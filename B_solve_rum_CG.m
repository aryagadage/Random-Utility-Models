function [result_CG,residual] ...
    = B_solve_rum_CG(p_obs, n, init_k, max_iters, tol, choice_sets, pricing_mode, chosen_alts,choice_set_list)
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
%   tol          : convergence tolerance
%   choice_sets  : cell array of choice sets to use (if empty, generates all)
%   pricing_mode : 'brute', 'bestinsertion', or 'randominsertion' (default: 'brute')
%   chosen_alts  : vector of actually chosen alternatives for each observation (optional)
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
if nargin < 8 || isempty(chosen_alts)
    error('Error! Need chosen_alts');
end
if nargin < 7
    pricing_mode = 'brute';
end
if nargin < 6 || isempty(choice_sets)
    choice_sets = [];
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
while and(exit==0, iter <= max_iters)
    
    % --- Solve Restricted Master Problem (RMP) ---
    % min ||V_sub * lambda - p_obs||^2
    % s.t. lambda >= 0, sum(lambda) = 1
    
    result=B_QP(V_sub,p_obs,false);
    error_val=result.QP.error;
    error_improvement = prev_error-error_val;
    
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
        for k=1:10
            [V_sub_temp,rankings_temp,best_score_temp]= B_CG_heuristic_best_rand(V_sub,n,choice_sets,chosen_alts,result.QP.residual,result.QP.optim_p,rankings,p_obs);
            if best_score_temp < best_score
                V_sub=V_sub_temp;
                rankings = rankings_temp;
                best_score = best_score_temp;
            end
        end
    end

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%%%%%%%%%%%%%%Print Progres %%%%%%%%%%%%%%%%%%%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    fprintf('Iter %d | error = %.6f | best_score = %.4f | error_improvement = %.2e |\n ', ...
    iter, error_val, best_score, error_improvement);
    % --- Termination Criterion: best_score < 0 (or a small tolerance)
    if best_score < tol
        fprintf('Convergence Criterion Achieved (best_score < tol)\n');

        [optim_value,optimizer,V_sub,rankings]=B_IP_pricing(result.QP.residual,choice_sets,chosen_alts,choice_set_list,tol,V_sub,rankings);
        if optim_value<tol
            exit=1;
        else
            exit=0;
        end
            
    end
    
    iter = iter+1;
    % --- Check convergence ---
    

    
    % Convergence condition: Error not improving significantly
    % ONLY for best insertion and random insertion modes (brute should run until best_score <= 0)
    %if ~strcmp(pricing_mode, 'brute') && iter > 5 && error_improvement < 1e-12
    %    fprintf('new_col = -1 | subset_size = %d\n', length(subset_idx));
    %    fprintf('\nConverged! Error not improving (improvement = %.2e < 1e-12).\n', error_improvement);
    %    break;
    %end
    
    % Update previous error for next iteration
    prev_error = error_val;
    
    % --- Add new column ---
    

end

% -------------------------------------------------------------------------
% FINAL SOLVE: Recompute lambda with all selected columns
% -------------------------------------------------------------------------

result_CG=result;
residual=result.QP.residual;
end
