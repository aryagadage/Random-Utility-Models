function score = evaluate_ranking_score(ranking, choice_sets, residual)
% EVALUATE_RANKING_SCORE
% -------------------------------------------------------------------------
% Computes the heuristic "score" of a ranking with respect to the current
% residual vector (Algorithm 6 of column-generation RUM).
%
% For each choice set:
%   - Identify the highest-ranked alternative in the subset (according to 'ranking').
%   - Add its corresponding residual entry to the total score.
%   - Skip subsets that don't overlap (safety check).
%
% Inputs:
%   ranking     : numeric vector, e.g. [3 1 5 2 4]
%   choice_sets : cell array of numeric vectors (each subset of {1,...,n})
%   residual    : stacked residual vector (same structure as p_obs)
%
% Output:
%   score       : scalar score = v_r' * residual
% -------------------------------------------------------------------------

    score = 0;   % total contribution from this ranking
    idx   = 0;   % pointer along residual vector

    for s = 1:length(choice_sets)
        subset = choice_sets{s};  % e.g. [2 4 5]

        % Skip subsets with no overlap (safety check)
        if isempty(intersect(subset, ranking))
            idx = idx + numel(subset);
            continue;
        end

        % Find top-ranked alternative within this subset
        top_alt = NaN;
        for alt = ranking
            if ismember(alt, subset)
                top_alt = alt;
                break;  % first one encountered in ranking order
            end
        end

        % Add residual value for top alternative
        for alt = subset
            idx = idx + 1;
            if alt == top_alt
                score = score + residual(idx);
            end
        end
    end
end
