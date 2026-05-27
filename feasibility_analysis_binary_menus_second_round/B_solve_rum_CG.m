function [result_CG,residual]  = B_solve_rum_CG(p_obs, n, init_k, max_iters, choice_sets, pricing_mode, chosen_alts,choice_set_list, IP, tol_level, tol_relative, time_limit_s, iptimes_path, seed_id)
% Termination tolerances (args 10 and 11):
%   tol_level    : absolute  gap tolerance — stop if UB - LB     < tol_level
%   tol_relative : relative  gap tolerance — stop if (UB-LB)/LB  < tol_relative
% where UB = sqrt(F_current), LB = sqrt(max(F_current - 2*best_score, 0)),
% F_current is the master squared error. Either condition triggers
% termination (the more permissive one fires first).
%
% Optional 12th arg time_limit_s: wall-time budget in seconds for the
% entire CG run (Inf = no cap). Checked at iteration boundaries; the
% remaining budget is also passed to B_IP_pricing as Gurobi TimeLimit
% so individual IP solves cannot exceed it either.
%
% Optional 13th/14th args iptimes_path, seed_id: if iptimes_path is a
% non-empty char/string, this function appends one CSV row per IP
% pricing call to that file (columns: n,seed,iter,ip_total_s,
% ip_solve_s,gurobi_runtime_s). Each row is fclosed before the next
% gurobi() call, so a hard kill anywhere after B_IP_pricing returns
% still leaves a durable record of the IP cost up to that point.
if nargin < 12 || isempty(time_limit_s), time_limit_s = Inf; end
if nargin < 13, iptimes_path = ''; end
if nargin < 14 || isempty(seed_id), seed_id = -1; end
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
ip_times           = [];   % per-call total wall time inside B_IP_pricing (obj build + solve + ranking recovery)
ip_solve_times     = [];   % per-call wall time of just gurobi()
ip_gurobi_runtimes = [];   % per-call result.runtime (Gurobi-internal)

% Build the IP pricing model once (structural part only — same across
% iterations). Only needed if we'll actually call B_IP_pricing.
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
        ip_times(end+1)           = ip_time; %#ok<AGROW>
        ip_solve_times(end+1)     = sw;      %#ok<AGROW>
        ip_gurobi_runtimes(end+1) = gr;      %#ok<AGROW>
        fprintf('  IP pricing time: %.4f s (solve %.4f s, gurobi.runtime %.4f s)\n', ...
            ip_time, sw, gr);
        append_iptimes_row(iptimes_path, n, seed_id, iter, ip_time, sw, gr);
        best_score = optim_value - result.QP.inner_product;
    end

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%%%%%%%%%%%%%%Print Progres %%%%%%%%%%%%%%%%%%%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % --- Termination Criterion: absolute OR relative distance gap -------
    % F(p) = ||p_obs - p||^2 is the master objective.
    %   UB      = sqrt(F_current)
    %   LB      = sqrt(max(F_current - 2*best_score, 0))   (convexity bound on F*)
    %   abs_gap = UB - LB
    %   rel_gap = (UB - LB) / LB
    % LB is clamped to 0; when LB == 0 the relative gap is treated as
    % infinite. We stop when EITHER abs_gap < tol_level OR
    % rel_gap < tol_relative (whichever fires first).
    F_current = error_val;
    UB        = sqrt(max(F_current, 0));
    LB        = sqrt(max(F_current - 2*best_score, 0));
    abs_gap   = UB - LB;
    if LB > 0
        rel_gap = abs_gap / LB;
    else
        rel_gap = Inf;
    end
    fprintf(['Iter %d | error = %.6f | best_score = %.4e | abs_gap = %.4e | ' ...
             'rel_gap = %.4e |\n'], iter, error_val, best_score, abs_gap, rel_gap);

    converged = (abs_gap < tol_level) || (rel_gap < tol_relative);

    if converged
        if strcmp(pricing_mode, 'IP')
            % IP pricing already ran this iteration; best_score is the
            % exact reduced cost, so abs_gap / rel_gap above are honest.
            exit = 1;
            fprintf(['Convergence Criterion Achieved ' ...
                     '(abs_gap %.4e < tol_level %.4e || rel_gap %.4e < tol_relative %.4e)\n'], ...
                    abs_gap, tol_level, rel_gap, tol_relative);
        elseif IP==true
            fprintf('IP Pricing (verification)\n')
            ip_tic = tic;
            ip_budget = max(1, time_limit_s - toc(solver_start));
            [optim_value,optimizer,V_sub,rankings,sw,gr]=B_IP_pricing(result.QP.residual,choice_sets,chosen_alts,choice_set_list,V_sub,rankings,result.QP.inner_product,ip_budget,ip_model); %#ok<ASGLU>
            ip_time = toc(ip_tic);
            ip_times(end+1)           = ip_time; %#ok<AGROW>
            ip_solve_times(end+1)     = sw;      %#ok<AGROW>
            ip_gurobi_runtimes(end+1) = gr;      %#ok<AGROW>
            fprintf('  IP-exit pricing time: %.4f s (solve %.4f s, gurobi.runtime %.4f s)\n', ...
                ip_time, sw, gr);
            append_iptimes_row(iptimes_path, n, seed_id, iter, ip_time, sw, gr);

            best_score_ip = optim_value - result.QP.inner_product;
            LB_ip   = sqrt(max(F_current - 2*best_score_ip, 0));
            abs_gap_ip = UB - LB_ip;
            if LB_ip > 0
                rel_gap_ip = abs_gap_ip / LB_ip;
            else
                rel_gap_ip = Inf;
            end
            converged_ip = (abs_gap_ip < tol_level) || (rel_gap_ip < tol_relative);

            if converged_ip
                exit = 1;
                fprintf('best_score: %.4e | abs_gap: %.4e | rel_gap: %.4e\n', ...
                    best_score_ip, abs_gap_ip, rel_gap_ip);
                fprintf(['Convergence Criterion Achieved ' ...
                         '(abs_gap %.4e < tol_level %.4e || rel_gap %.4e < tol_relative %.4e)\n'], ...
                        abs_gap_ip, tol_level, rel_gap_ip, tol_relative);
            else
                fprintf(['best_score: %.4e | abs_gap: %.4e | rel_gap: %.4e ' ...
                         '-- continuing\n'], best_score_ip, abs_gap_ip, rel_gap_ip);
                exit = 0;
            end
        else
            fprintf(['Convergence Criterion Achieved ' ...
                     '(abs_gap %.4e < tol_level %.4e || rel_gap %.4e < tol_relative %.4e)\n'], ...
                    abs_gap, tol_level, rel_gap, tol_relative);
            exit = 1;
        end
    end

    
    iter = iter+1;

    

end

% -------------------------------------------------------------------------
% FINAL SOLVE: Recompute lambda with all selected columns
% -------------------------------------------------------------------------

result_CG=result;
result_CG.ip_build_time      = ip_build_time;        % one-shot structural build (s)
result_CG.ip_times           = ip_times;             % per-call total (B_IP_pricing) wall
result_CG.ip_solve_times     = ip_solve_times;       % per-call gurobi() wall only
result_CG.ip_gurobi_runtimes = ip_gurobi_runtimes;   % per-call result.runtime (Gurobi internal)
result_CG.n_iters            = iter - 1;
result_CG.timed_out          = timed_out;
residual=result.QP.residual;
end


% =========================================================================
%  Streaming per-IP-call CSV writer (durable across SLURM kill)
% =========================================================================
function append_iptimes_row(iptimes_path, n, seed_id, iter, ip_total, ip_solve, gurobi_runtime)
if isempty(iptimes_path), return; end
fid = fopen(iptimes_path, 'a');
if fid < 0
    warning('append_iptimes_row: could not open %s for append', iptimes_path);
    return;
end
fprintf(fid, '%d,%d,%d,%.6f,%.6f,%.6f\n', n, seed_id, iter, ip_total, ip_solve, gurobi_runtime);
fclose(fid);   % flush to disk before returning
end
