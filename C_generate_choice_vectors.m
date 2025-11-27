function [V, rankings, choice_sets] = C_generate_choice_vectors(n)
% Generate all deterministic choice vectors for n alternatives.
% Each column = deterministic ranking (a "type")
% Each row    = one observed choice probability (across all sets)

    % --- All preference rankings (n! permutations) ---
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
        for alt = subset
            row_counter = row_counter + 1;
            for i = 1:K
                rank = rankings(i,:);
                % Find top-ranked alt in this subset
                top_alt = rank(find(ismember(rank, subset), 1, 'first'));
                V(row_counter, i) = double(alt == top_alt);
            end
        end
    end
end
