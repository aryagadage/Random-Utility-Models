function [result_CG,residual]  = B_solve_rum_CG(p_obs, n, init_k, max_iters, choice_sets, pricing_mode, chosen_alts,choice_set_list, IP, tol, time_limit_s)
% Optional 11th arg time_limit_s: wall-time budget in seconds for the
% entire CG run (Inf = no cap). Checked at iteration boundaries; the
% remaining budget is also passed to B_IP_pricing as Gurobi TimeLimit
% so individual IP solves cannot exceed it either.
if nargin < 11 || isempty(time_limit_s), time_limit_s = Inf; end
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
        best_score = optim_value - result.QP.inner_product;
    end

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%%%%%%%%%%%%%%Print Progres %%%%%%%%%%%%%%%%%%%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    fprintf('Iter %d | error = %.6f | best_score = %.4f |\n ', ...
    iter, error_val, best_score);
    % --- Termination Criterion: best_score < tol
    if best_score < tol
        if strcmp(pricing_mode, 'IP')
            % IP pricing already ran this iteration; reduced cost is exact
            exit = 1;
            fprintf('Convergence Criterion Achieved (reduced_cost %.4e < tol)\n', best_score);
        elseif IP==true
            fprintf('IP Pricing\n')
            ip_tic = tic;
            ip_budget = max(1, time_limit_s - toc(solver_start));
            [optim_value,optimizer,V_sub,rankings,sw,gr]=B_IP_pricing(result.QP.residual,choice_sets,chosen_alts,choice_set_list,V_sub,rankings,result.QP.inner_product,ip_budget,ip_model); %#ok<ASGLU>
            ip_time = toc(ip_tic);
            ip_times(end+1)           = ip_time; %#ok<AGROW>
            ip_solve_times(end+1)     = sw;      %#ok<AGROW>
            ip_gurobi_runtimes(end+1) = gr;      %#ok<AGROW>
            fprintf('  IP-exit pricing time: %.4f s (solve %.4f s, gurobi.runtime %.4f s)\n', ...
                ip_time, sw, gr);
            %optim_value

            if optim_value<result.QP.inner_product+tol
                exit=1;
                fprintf('best_score: %.4f',sqrt(optim_value-result.QP.inner_product) );
                fprintf('Convergence Criterion Achieved (best_score < tol)\n');

            else
                fprintf('best_score: %.4f\n',optim_value-result.QP.inner_product );
                exit=0;
            end
        else
            fprintf('Convergence Criterion Achieved (best_score < tol)\n');
            exit =1;
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
