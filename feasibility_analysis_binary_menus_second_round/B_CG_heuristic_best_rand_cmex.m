function [V_sub, rankings, best_score] = B_CG_heuristic_best_rand_cmex(V_sub, n, choice_sets, chosen_alts, residual, p_optim, rankings, p_obs)
%-----------------------------------------------------------------------------------------------------------------------------
% MEX-accelerated Randomized Best Insertion Heuristic for Column Generation.
%
% Drop-in replacement for B_CG_heuristic_best_rand.
% Uses cg_heuristic_rand_mex (compiled C) for the greedy construction.
%
% Compile the MEX once:
%   mex -O cg_heuristic_rand_mex.c
%
% Input / Output: identical to B_CG_heuristic_best_rand.
%-----------------------------------------------------------------------------------------------------------------------------

% Generate random insertion order (equivalent to random seed + random draw order)
insertion_order = randperm(n);

% Call MEX core
[best_ranking, v_final] = cg_heuristic_rand_mex(n, choice_sets, ...
    double(chosen_alts(:)), residual(:), double(insertion_order));

% Append new column
V_sub    = [V_sub, v_final];
rankings = [rankings; best_ranking];

% Reduced cost (same formula as original)
best_score = v_final' * residual - p_optim' * residual;

end
