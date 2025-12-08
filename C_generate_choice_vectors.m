function [V, rankings, choice_sets] = C_generate_choice_vectors(n)
%----------------------------------------------------------------------
% C_GENERATE_CHOICE_VECTORS - Generate all deterministic choice vectors
%----------------------------------------------------------------------
% Generate all deterministic choice vectors for n alternatives.
% Each column = deterministic ranking (a "type")
% Each row    = one observed choice probability (across all sets)
% INPUT:
%   n - Number of alternatives (e.g., n=3 for alternatives {1,2,3})
%
% OUTPUT:
%   V           - Choice probability matrix [num_obs x K]
%                 Each row: "Prob(alternative a chosen from set S)"
%                 Each column: deterministic choice vector for ranking i
%                 Entry V(row,col) = 1 if ranking col chooses the 
%                 alternative corresponding to row, 0 otherwise
%
%   rankings    - Matrix of all preference rankings [K x n]
%                 rankings(i,:) = [a b c ...] means "a preferred to b 
%                 preferred to c ..."
%
%   choice_sets - contains alternatives in the i-th set

    % --- Generate all preference rankings (n! permutations) ---
    alternatives = 1:n;
    rankings = perms(alternatives);
    rankings = flipud(rankings); % keep consistent ordering
    K = factorial(n);

    % --- Build all nonempty choice sets (powerset minus empty set) ---
    choice_sets = {};
    for r = 1:n
        subsets = nchoosek(alternatives, r);
        for j = 1:size(subsets,1)
            choice_sets{end+1} = subsets(j,:);
        end
    end

    % --- Count total number of "observations" ---
    % Each subset contributes as many rows as its size
    num_obs = sum(arrayfun(@(r) nchoosek(n,r)*r, 1:n));

    % --- Initialize V ---
    V = zeros(num_obs, K);

    % --- Fill V ---
    % Each row = "probability of alternative a being chosen in subset S"
    % Each column = deterministic choice vector under ranking i
    row_counter = 0;
    for c = 1:length(choice_sets)
        subset = choice_sets{c};
        % For each alternative in this choice set
        for alt = subset
            row_counter = row_counter + 1;
            % For each possible ranking (column of V)
            for i = 1:K
                rank = rankings(i,:);
                % Find top-ranked alt in this subset
                % V = 1 if this ranking chooses 'alt' from 'subset'
                top_alt = rank(find(ismember(rank, subset), 1, 'first'));
                V(row_counter, i) = double(alt == top_alt);
            end
        end
    end
    %----------------------------------------------------------------------
    % OUTPUT SUMMARY
    % - V is a binary matrix where each column sums to |choice_sets|
    %   (each ranking makes exactly one choice per choice set)
    %----------------------------------------------------------------------
end
