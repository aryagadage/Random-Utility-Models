load('p_obs.mat')  % loads the observed choice probabilities (80x1 vector)
n = 5;             % number of alternatives

[lambda_sub, V_sub, subset_idx, rankings, choice_sets, error_val, iter] = ...
    solve_rum_columngen(p_obs, n, 1, 200, 1e-8);
