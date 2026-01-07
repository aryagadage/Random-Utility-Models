function [result_FW,error_val] ...
    = B_solve_rum_FW(p_obs, n, max_iters, tol, choice_sets, chosen_alts,choice_set_list)
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
if isempty(chosen_alts)
    error('Error! Need chosen_alts');
end

fprintf('Frank Wolfe Method');
% -------------------------------------------------------------------------
% INITIALIZATION: Start with 1 random ordering
% -------------------------------------------------------------------------

[V_sub, rankings, subset_idx] = C_gen_one_ranking(p_obs,choice_sets,chosen_alts,n,'random',[]);
p_output = V_sub(:,1);
Deltaf = 2*(p_output-p_obs);
% -------------------------------------------------------------------------
% weighting function
% -------------------------------------------------------------------------

% alpha_k = 2/k+2
alpha=@(x) 2/(x+2);

prev_error = inf;

%control
iter=0;
exit=0;

while and(exit==0, iter <= max_iters)
    
    [~,optimizer,~,~]=B_IP_pricing(-Deltaf,choice_sets,chosen_alts,choice_set_list,V_sub,rankings,[]);

    p_output = (1-alpha(iter))*p_output + alpha(iter)*optimizer;
    
    Deltaf = 2*(p_output-p_obs);

    error_val =  sum((p_output-p_obs).^2);


    fprintf('Iter %d | error = %.6f \n ', ...
    iter, error_val);
      
    iter=iter+1;

end


% -------------------------------------------------------------------------
% FINAL SOLVE: Recompute lambda with all selected columns
% -------------------------------------------------------------------------
result_FW=p_obs;
end
