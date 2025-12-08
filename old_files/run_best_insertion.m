% run_best_insertion.m
% --------------------------------------------------------------
% Run the column-generation solver using the Best Insertion heuristic
% for the pricing problem.
% --------------------------------------------------------------

clear; clc;

% --- Load observed probabilities ---
load('p_obs.mat');
n = 5;

% --- Handle table input ---
if istable(p_obs)
    if any(strcmp('Observed_Prob', p_obs.Properties.VariableNames))
        p_obs = p_obs.Observed_Prob;
    else
        % Get first numeric column
        numVars = varfun(@isnumeric, p_obs, 'OutputFormat', 'uniform');
        p_obs = table2array(p_obs(:, find(numVars, 1)));
    end
end

% --- Sanity check on input ---
if ~isnumeric(p_obs) || ~isvector(p_obs)
    error('p_obs must be a numeric vector of observed choice probabilities.');
end

% --- Run solver with Best Insertion pricing mode ---
[lambda_full, V_sub, subset_idx, rankings, choice_sets, error_val, iter, x_est] = ...
    solve_rum_columngen(p_obs, n, 1, 200, 1e-8, 'bestinsertion');

% --- Display summary ---
fprintf('\n=== Best Insertion Run Complete ===\n');
fprintf('Total iterations: %d\n', iter);
fprintf('Final squared error: %.6g\n', error_val);
fprintf('Selected %d columns out of %d possible.\n', length(subset_idx), size(rankings,1));

% --- Save results automatically ---
save('RUM_run_bestinsertion.mat', 'lambda_full', 'V_sub', 'subset_idx', ...
     'rankings', 'choice_sets', 'error_val', 'x_est');