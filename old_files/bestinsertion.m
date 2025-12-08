function [best_ranking, best_score, ranking_trace] = best_insertion(p_obs, n, choice_sets)
% BEST_INSERTION construction of a high-scoring ranking for discrete-choice RUM.
%
%   Inputs:
%     p_obs       - stacked vector of observed choice probabilities (as in your dataset)
%     n           - number of alternatives (e.g., n = 5)
%     choice_sets - cell array of all nonempty subsets of alternatives
%
%   Outputs:
%     best_ranking - the final full ordering of alternatives [i1, i2, ..., in]
%     best_score   - total score for the final ranking
%     ranking_trace- intermediate rankings and scores for diagnostics
%
%   Description:
%   --------------------------------------------------------
%   Implements a greedy "Best Insertion" heuristic inspired by
%   column generation (like Algorithm 4 in KS), adapted for
%   discrete-choice problems.
%
%   At each iteration:
%     1. Randomly pick an alternative not yet in the ranking.
%     2. Try inserting it at every possible position (front, middle, end).
%     3. For each insertion, compute how well the resulting ranking explains
%        the observed choice probabilities (sum of p_obs where the predicted
%        choice matches the observed chosen alternative).
%     4. Keep the insertion with the best total score.
%
%   The algorithm builds one ranking iteratively, from 1 alternative to n.
%   The total score measures the "fit" between the ranking and the observed data.
%
%   --------------------------------------------------------
%
%   Example: (run_best_insertion.m)
%       load('p_obs.mat'); % your 80x1 vector
%       n = 5;
%       [~, ~, choice_sets] = generate_choice_vectors(n);
%       [ranking, score, trace] = best_insertion(p_obs, n, choice_sets);
%

%% Setup of alternatives
alternatives = 1:n;                 % {1, 2, 3, 4, 5}
remaining = alternatives;           % list of alternatives not yet inserted
ranking_trace = {};                 % store history of ranking evolution

%%  Randomly choose the first alternative
first = remaining(randi(length(remaining)));  % pick one at random
best_ranking = first;                         % start partial ranking
remaining(remaining == first) = [];           % remove from pool

fprintf('Start: chosen initial alternative %d\n', first);

%% Iteratively add remaining alternatives
while ~isempty(remaining)
    % Pick next random alternative to insert
    c = remaining(randi(length(remaining)));
    remaining(remaining == c) = [];  % remove it from the remaining set
    
    fprintf('\nInserting alternative %d ...\n', c);

    % There are (length(best_ranking)+1) possible positions to insert
    n_positions = length(best_ranking) + 1;
    scores = zeros(1, n_positions);   % store insertion scores for each position

    % Try each possible insertion position
    for pos = 1:n_positions
        % Example: if current ranking = [4 5], c = 3:
        %   pos=1 â†’ [3 4 5], pos=2 â†’ [4 3 5], pos=3 â†’ [4 5 3]
        trial_ranking = [best_ranking(1:pos-1), c, best_ranking(pos:end)];

        % Evaluate this ranking's total score given p_obs and choice_sets
        scores(pos) = evaluate_ranking_score(trial_ranking, choice_sets, p_obs);
        fprintf('   Position %d -> score %.6f | ranking = [%s]\n', ...
                pos, scores(pos), num2str(trial_ranking));
    end

    % Pick the insertion position that gives the maximum score
    [best_pos_score, best_pos] = max(scores);

    % Update the ranking with the best insertion
    best_ranking = [best_ranking(1:best_pos-1), c, best_ranking(best_pos:end)];
    fprintf(' --> Inserted %d at position %d. Updated ranking = [%s], score = %.6f\n', ...
            c, best_pos, num2str(best_ranking), best_pos_score);

    % Store history (optional)
    ranking_trace{end+1} = struct('ranking', best_ranking, ...
                                  'score', best_pos_score, ...
                                  'inserted', c);
end


%% Compute final score
best_score = evaluate_ranking_score(best_ranking, choice_sets, p_obs);
fprintf('\nFinal Ranking: [%s] | Total Score: %.6f\n', num2str(best_ranking), best_score);

disp('--- Trace of insertions ---');
for t = 1:length(ranking_trace)
    fprintf('Step %2d: Added alt %d -> ranking = [%s], score = %.4f\n', ...
        t, ranking_trace{t}.inserted, num2str(ranking_trace{t}.ranking), ranking_trace{t}.score);
end
end
