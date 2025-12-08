function [lambda_full, V_sub, subset_idx, rankings, choice_sets, error_val, iter, x_est] = ...
    solve_rum_columngen(p_obs, n, init_k, max_iters, tol, pricing_mode)

% SOLVE_RUM_COLUMNGEN  Column-generation projection for discrete-choice RUM
%
% This function implements a column-generation (restricted master + pricing)
% approach to find a mixture of deterministic rankings that best fits the
% observed choice probabilities p_obs.
%
% Inputs:
%   p_obs    - stacked observed choice probabilities vector (rows match V rows)
%   n        - number of alternatives (for you: n = 5)
%   init_k   - initial number of columns to seed the restricted master (default 1)
%   max_iters- max column-generation iterations (default 200)
%   tol      - threshold for accepting a new column (default 1e-8)
%   pricing_mode - string: 'brute' or 'bestinsertion'
%
% Outputs:
%   lambda_full - optimal mixture weights for the final subset (size = #subset)
%   V_sub       - design matrix of chosen columns (rows x #subset)
%   subset_idx  - indices (into full V) of columns included in V_sub
%   rankings    - full list of rankings (permutations) from generate_choice_vectors
%   choice_sets - cell array of choice sets (same order as V row stacking)
%   error_val   - final squared error = norm(V_sub*lambda_full - p_obs)^2
%   iter        - number of CG iterations performed
% - This is intended for small n (n=5 is fine: 5! = 120 rankings). For larger n,
%   replace the exhaustive search in pricing_problem with a heuristic.
% - The pricing problem here scans all columns in V_full to find the best improving one.
%
% Example:
%   [lam, Vsub, idx, r, sets, err, it] = solve_rum_columngen(p_obs, 5, 1, 200, 1e-8);

%% ---- Defaults ----
if nargin < 3 || isempty(init_k); init_k = 1; end
if nargin < 4 || isempty(max_iters); max_iters = 200; end
if nargin < 5 || isempty(tol); tol = 1e-8; end
if nargin < 6 || isempty(pricing_mode); pricing_mode = 'brute'; end

%% ---- Build full deterministic matrix ----
[V_full, rankings, choice_sets] = generate_choice_vectors(n);
[num_rows, Kfull] = size(V_full);

if length(p_obs) ~= num_rows
    error('Length of p_obs (%d) must equal number of stacked rows in V (%d).', ...
        length(p_obs), num_rows);
end

%% ---- Initialize restricted master problem ----
subset_idx = 1:init_k;
V_sub = V_full(:, subset_idx);

options = optimoptions('quadprog','Display','off','TolFun',1e-12,'TolX',1e-12);

%% ---- Column-generation loop ----
iter = 0;
improved = true;

while iter < max_iters && improved
    iter = iter + 1;

    % --- (A) Solve Restricted Master Problem ---
    H = 2 * (V_sub' * V_sub);
    f = -2 * (V_sub' * p_obs);

    ksub = size(V_sub,2);
    A = -eye(ksub); b = zeros(ksub,1);
    Aeq = ones(1,ksub); beq = 1;

    [lambda_sub, fval, exitflag] = quadprog(H, f, A, b, Aeq, beq, [], [], [], options);

    if ~(exitflag == 1 || exitflag == 2 || exitflag == 0)
        warning('quadprog exitflag = %d at iter %d.', exitflag, iter);
    end

    % --- (B) Compute residual ---
    pred = V_sub * lambda_sub;
    residual = p_obs - pred;
    error_val = norm(residual)^2;

    % --- (C) Pricing problem switch ---
    switch lower(pricing_mode)
        case 'brute'
            [best_idx, best_score] = pricing_problem(V_full, residual, subset_idx);
        case 'bestinsertion'
            [best_idx, best_score] = pricing_bestinsertion( residual, ...
                rankings, choice_sets, subset_idx, 10);
        otherwise
            error('Unknown pricing_mode "%s". Use "brute" or "bestinsertion".', pricing_mode);
    end

    fprintf('Iter %d | error = %.8g | best_score = %.8g | new_col = %d | subset_size = %d\n', ...
            iter, error_val, best_score, best_idx, size(V_sub,2));

    % --- (D) Accept column if improvement significant ---
    if best_score > tol && ~isempty(best_idx)
        subset_idx = [subset_idx, best_idx];
        V_sub = V_full(:, subset_idx);
        improved = true;
    else
        improved = false;
    end
end

%% ---- Finalization ----
lambda_full = lambda_sub;
x_est = V_sub * lambda_full;
error_val = norm(p_obs - x_est)^2;

fprintf('\nFinal error = %.6f with %d columns selected (%d iterations).\n', ...
    error_val, size(V_sub,2), iter);

save('RUM_results.mat', 'lambda_full', 'V_sub', 'subset_idx', ...
     'rankings', 'choice_sets', 'error_val', 'x_est');

end
