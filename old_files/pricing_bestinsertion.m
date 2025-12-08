function [best_idx, best_score] = pricing_bestinsertion(residual, rankings, choice_sets, subset_idx, num_starts)
% PRICING_BESTINSERTION
% -------------------------------------------------------------------------
% Implements the pricing problem using a Best-Insertion heuristic.
% The goal is to find a *new ranking* (column) v_r that maximizes:
%       score = v_r' * residual
%
% Inputs:
%   p_obs       : observed choice probabilities (only for dimension checks)
%   residual    : current residual vector (p_obs - V_sub * lambda)
%   rankings    : all possible full rankings (n! x n)
%   choice_sets : cell array of all nonempty subsets of {1,...,n}
%   subset_idx  : indices of columns already in the restricted master problem
%   num_starts  : number of random restarts for the heuristic
%
% Outputs:
%   best_idx    : index (in `rankings`) of the best found ranking
%   best_score  : scalar improvement value (v_r' * residual)
% -------------------------------------------------------------------------

    if nargin < 5
        num_starts = 10;  % default number of random restarts
    end

    best_score = -Inf;
    best_idx   = -1;

    % ---------------------------------------------------------------------
    % Determine number of alternatives (n)
    % (Use flattening instead of cell2mat since choice_sets vary in length)
    % ---------------------------------------------------------------------
    n = length(unique([choice_sets{:}]));

    % ---------------------------------------------------------------------
    % Run several random starts of the Best Insertion heuristic
    % ---------------------------------------------------------------------
    for t = 1:num_starts
        % Each start constructs one promising ranking using residuals
        [ranking, score, ~] = best_insertion(residual, n, choice_sets);

        % Find the index of this ranking in the full permutation list
        idx = find(ismember(rankings, ranking, 'rows'), 1, 'first');

        % Skip if ranking already in the restricted master problem
        if isempty(idx) || ismember(idx, subset_idx)
            continue;
        end

        % Update the best found ranking if score improves
        if score > best_score
            best_score = score;
            best_idx   = idx;
        end
    end

    % ---------------------------------------------------------------------
    % If heuristic fails, just return with no improvement
    % ---------------------------------------------------------------------
    if best_idx == -1
        warning('Best-Insertion heuristic failed to find an improving column after %d starts. Stopping.', num_starts);
        best_score = -Inf;  % Signal no improvement
    end

end