function [result_CG,residual]  = B_solve_rum_CG(p_obs, n, init_k, max_iters, choice_sets, pricing_mode, chosen_alts,choice_set_list, IP, tol, time_limit_s, iptimes_path, run_tag)
% Optional 11th arg time_limit_s: wall-time budget in seconds for the
% entire CG run (Inf = no cap). Checked at iteration boundaries; the
% remaining budget is also passed to B_IP_pricing as Gurobi TimeLimit
% so individual IP solves cannot exceed it either.
%
% Optional 12th arg iptimes_path: path to a per-IP-call CSV. When provided,
% one row is appended after every IP pricing call (file is fclose'd between
% rows so a SLURM kill leaves a durable record). Columns:
%   run_tag, iter, ip_total_s, ip_solve_s, gurobi_runtime_s
% Optional 13th arg run_tag: string label written as the first column of
% iptimes CSV (e.g. 'group013_PaperTowels_store142__CG_IP'). Defaults to ''.
if nargin < 11 || isempty(time_limit_s), time_limit_s = Inf; end
if nargin < 12, iptimes_path = ''; end
if nargin < 13, run_tag = ''; end
% B_solve_rum_CG
% -------------------------------------------------------------------------
% Column generation solver for discrete choice RUM problem.
%  Added 'randominsertion' pricing mode 
% 
% Inputs:
%   p_obs        : observed choice probabilities (vector)
%   n            : number of alternatives
%   init_k       : number of initial columns (rankings) to start with
%   max_iters    : maximum number of iterations
%   error_imp_tol  : error improvement convergence tolerance
%   choice_sets  : cell array of choice sets to use (if empty, generates all)
%   pricing_mode : 'brute', 'bestinsertion', 'randominsertion', or 'IP' (exact IP pricing each iter)
%   chosen_alts  : vector of actually chosen alternatives for each observation (optional)
%   IP  : whether to use integer programming to exit

%
% Outputs:
%   lambda_full  : weights on selected columns (sums to 1)
%   V_sub        : matrix of selected choice vectors
%   subset_idx   : indices of selected rankings
%   rankings     : the actual selected rankings (matrix)
%   choice_sets_out : choice sets used in the algorithm
%   error_val    : final squared error
%   iter         : number of iterations performed
%   x_est        : estimated utilities (if computed)
% -------------------------------------------------------------------------

% Handle optional inputs
if nargin < 9 || isempty(chosen_alts)
    error('Error! Need chosen_alts');
end

fprintf('Starting column generation with %d initial columns...\n', init_k);
fprintf('Pricing mode: %s\n', pricing_mode);

% -------------------------------------------------------------------------
% Step 1: Only generate full rankings for brute force
% -------------------------------------------------------------------------

[V_full, all_rankings, choice_sets]=C_gen_V_full(n,choice_sets,pricing_mode,p_obs,chosen_alts);

% -------------------------------------------------------------------------
% INITIALIZATION: Start with init_k random rankings
% -------------------------------------------------------------------------

[V_sub, rankings, subset_idx] = C_gen_one_ranking(p_obs,choice_sets,chosen_alts,n,'random',[]);

% -------------------------------------------------------------------------
% COLUMN GENERATION MAIN LOOP
% -------------------------------------------------------------------------

prev_error = inf;

%control
iter=1;
exit=0;
timed_out = false;
ip_times           = [];   % per-call wall time around B_IP_pricing (s)
ip_solve_times     = [];   % per-call wall time of just gurobi() (s)
ip_gurobi_runtimes = [];   % per-call result.runtime (Gurobi-internal s)

% Build the IP pricing model once (structural part only — same across
% iterations). Only needed if we will actually call B_IP_pricing.
need_ip_model = strcmp(pricing_mode, 'IP') || IP == true;
if need_ip_model
    t_build = tic;
    ip_model = B_IP_pricing_build(n, choice_set_list);
    ip_build_time = toc(t_build);
    fprintf('B_IP_pricing_build: %.2fs (nvars=%d, ncons=%d)\n', ...
        ip_build_time, size(ip_model.A, 2), size(ip_model.A, 1));
else
    ip_model      = [];
    ip_build_time = 0;
end

solver_start = tic;
while and(exit==0, iter <= max_iters)
    if toc(solver_start) > time_limit_s
        timed_out = true;
        fprintf('Time limit reached (%.1f s); stopping at iter %d.\n', ...
            time_limit_s, iter);
        break;
    end
    
    % --- Solve Restricted Master Problem (RMP) ---
    % min ||V_sub * lambda - p_obs||^2
    % s.t. lambda >= 0, sum(lambda) = 1
    
    result=B_QP(V_sub,p_obs,false);
    error_val=result.QP.error;
    
    % --- Pricing Problem: Find best new column to add ---
    if strcmp(pricing_mode, 'brute')
        % ===================================================================
        % BRUTE FORCE PRICING
        % ===================================================================
        [V_sub,subset_idx,rankings,best_score]=B_CG_exact(V_sub,V_full,result.QP.residual,subset_idx,result.QP.optim_p,all_rankings,rankings);

    elseif strcmp(pricing_mode, 'bestinsertion')
        best_score=inf;
        
        for k=1:n
            [V_sub_temp,rankings_temp,best_score_temp]= B_CG_heuristic_best(k,V_sub,n,choice_sets,chosen_alts,result.QP.residual,result.QP.optim_p,rankings,p_obs);
            if best_score_temp < best_score
                V_sub=V_sub_temp;
                rankings = rankings_temp;
                best_score = best_score_temp;
            end
        end

    elseif strcmp(pricing_mode, 'bestinsertion_rand')

        best_score=inf;
        for k=1:10 %do random insertion 10 times
            [V_sub_temp,rankings_temp,best_score_temp]= B_CG_heuristic_best_rand_cmex(V_sub,n,choice_sets,chosen_alts,result.QP.residual,result.QP.optim_p,rankings,p_obs);
            if best_score_temp < best_score
                V_sub=V_sub_temp;
                rankings = rankings_temp;
                best_score = best_score_temp;
            end
        end

    elseif strcmp(pricing_mode, 'IP')
        % ===================================================================
        % EXACT IP PRICING (Gurobi) every iteration
        % ===================================================================
        ip_tic = tic;
        ip_budget = max(1, time_limit_s - toc(solver_start));
        [optim_value,~,V_sub,rankings,sw,gr] = B_IP_pricing(result.QP.residual, ...
            choice_sets, chosen_alts, choice_set_list, V_sub, rankings, result.QP.inner_product, ip_budget, ip_model);
        ip_time = toc(ip_tic);
        ip_times(end+1)           = ip_time;     %#ok<AGROW>
        ip_solve_times(end+1)     = sw;          %#ok<AGROW>
        ip_gurobi_runtimes(end+1) = gr;          %#ok<AGROW>
        fprintf('  IP pricing time: %.4f s (solve %.4f s, gurobi.runtime %.4f s)\n', ...
            ip_time, sw, gr);
        append_iptimes_row(iptimes_path, run_tag, iter, ip_time, sw, gr);
        best_score = optim_value - result.QP.inner_product;
    end

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%%%%%%%%%%%%%%Print Progres %%%%%%%%%%%%%%%%%%%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % --- Termination Criterion: hybrid (raw RC) AND (FW relative gap) ---
    % F(p) = ||p_obs - p||^2 is the master objective.
    %   UB  = sqrt(F_current)
    %   LB  = sqrt(max(F_current - 2*best_score, 0))   (convexity bound on F*)
    %   gap = (UB - LB)/LB                              (relative distance gap)
    % LB is clamped to 0 (instead of using sqrt of a possibly negative
    % argument); when LB == 0 we treat the relative gap as infinite. We
    % stop whenever the EITHER the raw best_score OR the relative gap is
    % below tol, whichever happens first.
    F_current = error_val;
    UB        = sqrt(max(F_current, 0));
    LB        = sqrt(max(F_current - 2*best_score, 0));
    if LB > 0
        fw_gap = (UB - LB) / LB;
    else
        fw_gap = Inf;
    end
    term_metric = min(best_score, fw_gap);
    fprintf(['Iter %d | error = %.6f | best_score = %.4e | FW gap = %.4e | ' ...
             'min = %.4e |\n'], iter, error_val, best_score, fw_gap, term_metric);

    if term_metric < tol
        if strcmp(pricing_mode, 'IP')
            % IP pricing already ran this iteration; best_score is an upper
            % bound on the exact reduced cost, so the gap above is honest.
            exit = 1;
            fprintf(['Convergence Criterion Achieved ' ...
                     '(min(best_score, FW gap) = %.4e < tol %.4e)\n'], ...
                    term_metric, tol);
        elseif IP==true
            fprintf('IP Pricing (verification)\n')
            ip_tic = tic;
            ip_budget = max(1, time_limit_s - toc(solver_start));
            [optim_value,optimizer,V_sub,rankings,sw,gr]=B_IP_pricing(result.QP.residual,choice_sets,chosen_alts,choice_set_list,V_sub,rankings,result.QP.inner_product,ip_budget,ip_model); %#ok<ASGLU>
            ip_time = toc(ip_tic);
            ip_times(end+1)           = ip_time;     %#ok<AGROW>
            ip_solve_times(end+1)     = sw;          %#ok<AGROW>
            ip_gurobi_runtimes(end+1) = gr;          %#ok<AGROW>
            fprintf('  IP-exit pricing time: %.4f s (solve %.4f s, gurobi.runtime %.4f s)\n', ...
                ip_time, sw, gr);
            append_iptimes_row(iptimes_path, run_tag, iter, ip_time, sw, gr);

            best_score_ip = optim_value - result.QP.inner_product;
            LB_ip = sqrt(max(F_current - 2*best_score_ip, 0));
            if LB_ip > 0
                fw_gap_ip = (sqrt(F_current) - LB_ip) / LB_ip;
            else
                fw_gap_ip = Inf;
            end
            term_metric_ip = min(best_score_ip, fw_gap_ip);

            if term_metric_ip < tol
                exit = 1;
                fprintf('best_score: %.4e | FW gap: %.4e | min: %.4e\n', ...
                    best_score_ip, fw_gap_ip, term_metric_ip);
                fprintf('Convergence Criterion Achieved (min < tol)\n');
            else
                fprintf(['best_score: %.4e | FW gap: %.4e | min: %.4e ' ...
                         '-- continuing\n'], best_score_ip, fw_gap_ip, term_metric_ip);
                exit = 0;
            end
        else
            fprintf(['Convergence Criterion Achieved ' ...
                     '(min(best_score, FW gap) = %.4e < tol %.4e)\n'], ...
                    term_metric, tol);
            exit = 1;
        end
    end

    
    iter = iter+1;

    

end

% -------------------------------------------------------------------------
% FINAL SOLVE: Recompute lambda with all selected columns
% -------------------------------------------------------------------------

result_CG=result;
result_CG.ip_build_time      = ip_build_time;       % one-shot structural build (s)
result_CG.ip_times           = ip_times;            % wall around B_IP_pricing (s)
result_CG.ip_solve_times     = ip_solve_times;      % wall around gurobi() only (s)
result_CG.ip_gurobi_runtimes = ip_gurobi_runtimes;  % result.runtime (s)
result_CG.n_iters            = iter - 1;
result_CG.timed_out          = timed_out;
residual=result.QP.residual;
end


% =========================================================================
% Append one per-IP-call row to the streaming CSV. Opens/closes the file
% for each row so that a SLURM kill leaves a durable record.
% =========================================================================
function append_iptimes_row(iptimes_path, run_tag, iter, ip_total, ip_solve, gurobi_runtime)
if isempty(iptimes_path), return; end
fid = fopen(iptimes_path, 'a');
if fid < 0
    warning('append_iptimes_row: could not open %s for append.', iptimes_path);
    return;
end
fprintf(fid, '%s,%d,%.6f,%.6f,%.6f\n', run_tag, iter, ip_total, ip_solve, gurobi_runtime);
fclose(fid);
end
