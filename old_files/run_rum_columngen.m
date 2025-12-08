clear; clc;

% Load data
load('p_obs.mat');
n = 5;

% Handle table input
if istable(p_obs)
    if any(strcmp('Observed_Prob', p_obs.Properties.VariableNames))
        p_obs = p_obs.Observed_Prob;
    else
        % Get first numeric column
        numVars = varfun(@isnumeric, p_obs, 'OutputFormat', 'uniform');
        p_obs = table2array(p_obs(:, find(numVars, 1)));
    end
end

% Run column generation with brute force pricing
[lambda_full, V_sub, subset_idx, rankings, choice_sets, error_val, iter, x_est] = ...
    solve_rum_columngen(p_obs, n, 1, 200, 1e-8, 'brute');

% Display results
fprintf('\nFinal error: %.6f\n', error_val);
fprintf('Iterations: %d\n', iter);
fprintf('Columns selected: %d\n', length(subset_idx));
