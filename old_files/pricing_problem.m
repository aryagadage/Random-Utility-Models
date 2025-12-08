function [best_idx, best_score] = pricing_problem(V_full, residual, subset_idx)
% PRICING_PROBLEM - find best new column and its score
%
% Inputs:
%   V_full     - full deterministic matrix (rows x Kfull)
%   residual   - current residual vector (p_obs - V_sub*lambda)
%   subset_idx - indices of columns already in the restricted master
%
% Outputs:
%   best_idx   - index in V_full of the best column to add
%   best_score - v' * residual value for that column

% Compute score for every column cheaply (matrix-vector product)
scores = V_full' * residual;   % size Kfull x 1

% Exclude columns already in RMP
scores(subset_idx) = -Inf;

% Find the single best candidate (ties broken arbitrarily by max)
[best_score, best_idx] = max(scores);

end
