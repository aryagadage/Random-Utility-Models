function [lambda_full, V_sub, subset_idx, rankings, choice_sets, error_val, iter] = ...
    solve_rum_columngen(p_obs, n, init_k, max_iters, tol)
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

%% ---- Input handling and defaults ----
if nargin < 3 || isempty(init_k); init_k = 1; end
if nargin < 4 || isempty(max_iters); max_iters = 200; end
if nargin < 5 || isempty(tol); tol = 1e-8; end

%% ---- Build full set of deterministic columns (V_full) ----
% For n=5 this creates 120 columns (one per strict ranking).
[V_full, rankings, choice_sets] = generate_choice_vectors(n);
[num_rows, Kfull] = size(V_full);

% check that p_obs length matches the stacked row count of V
if length(p_obs) ~= num_rows
    error('Length of p_obs (%d) must equal the number of stacked rows in V (%d).', length(p_obs), num_rows);
end

%% ---- Initialize the restricted master (subset of columns) ----
% You can start with one column (init_k = 1) or a small handful.
% Using a single column is permitted; the algorithm will iteratively discover more.
if init_k <= 0
    init_k = 1;
end
if init_k > Kfull
    init_k = Kfull;
end

% Simple deterministic choice: start with the first `init_k` permutations.
% You might prefer randperm(Kfull, init_k) to randomize the seed.
subset_idx = 1:init_k;
V_sub = V_full(:, subset_idx);   % restricted design matrix (rows x #subset)

%% ---- Quadprog options (QP solver) ----
% Use tight tolerances to reduce numerical noise when solving small QPs.
options = optimoptions('quadprog','Display','off','TolFun',1e-12,'TolX',1e-12);

%% ---- Column-generation loop ----
iter = 0;
improved = true;   % whether we added a new column in the last iteration

while iter < max_iters && improved
    iter = iter + 1;

    % ----- (A) Solve the Restricted Master Problem (RMP) -----
    % Minimize ||V_sub * lambda - p_obs||^2 s.t. lambda >= 0, sum(lambda)=1
    % Standard quadratic form: (V_sub*lambda - p)'(V_sub*lambda - p)
    H = 2 * (V_sub' * V_sub);      % quadratic term (symmetric positive semidefinite)
    f = -2 * (V_sub' * p_obs);     % linear term

    ksub = size(V_sub,2);
    A = -eye(ksub); b = zeros(ksub,1);   % inequality lambda >= 0 (written -I * lambda <= 0)
    Aeq = ones(1,ksub); beq = 1;         % equality: sum(lambda) = 1

    % Solve QP. quadprog returns lambda_sub and objective (fval).
    [lambda_sub, fval, exitflag, output, qp_lambda] = ...
        quadprog(H, f, A, b, Aeq, beq, [], [], [], options);

    % If quadprog signals numerical problems, warn but continue with result.
    if ~(exitflag == 1 || exitflag == 2 || exitflag == 0)
        warning('quadprog exitflag = %d at iter %d; continuing with current solution.', exitflag, iter);
    end

    % ----- (B) Compute residual and objective value -----
    pred = V_sub * lambda_sub;      % model-predicted probabilities
    residual = p_obs - pred;        % what the current model fails to explain
    error_val = norm(residual)^2;   % squared L2 error (objective)

    % ----- (C) Pricing problem (search for best new column) -----
    % The "score" of a candidate column v_j is v_j' * residual.
    % A large positive score means adding that column will reduce squared error.
    [best_idx, best_score] = pricing_problem(V_full, residual, subset_idx);

    % Print progress for debugging/demonstration (can be silenced)
    fprintf('Iter %d | error = %.8g | best_score = %.8g | new_col = %d | subset_size = %d\n', ...
            iter, error_val, best_score, best_idx, size(V_sub,2));

    % ----- (D) Acceptance rule: add column if sufficiently improving -----
    if best_score > tol
        % Add the best column to the restricted master set
        subset_idx = [subset_idx, best_idx];
        V_sub = V_full(:, subset_idx);   % expand RMP matrix
        improved = true;
    else
        % No column meaningfully improves the fit: stop CG
        improved = false;
    end
end

   %% ---- Final results and cleanup ----
lambda_full = lambda_sub;            % optimal weights (for last RMP)
V_sub = V_sub;                       % final restricted design matrix
error_val = norm(p_obs - V_sub*lambda_full)^2; % final squared error

fprintf('\nFinal error = %.6f with %d columns selected.\n', error_val, size(V_sub,2));

% Save results for later inspection
save('RUM_results.mat', 'lambda_full', 'V_sub', 'subset_idx', 'rankings', 'choice_sets', 'error_val');

end
