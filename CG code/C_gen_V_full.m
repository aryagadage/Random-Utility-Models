function [V_full, all_rankings, choice_sets] = C_gen_V_full(n,choice_sets,pricing_mode,p_obs,chosen_alts)
	
%Purpose:

%This program prepares the objects needed for the DC-RUM algorithm.  

%- In pricing_mode=='brute'*, it **enumerates all preference rankings over n alternatives and builds the full matrix V_full', whose columns correspond to rankings and whose rows correspond to observed choice sets.  
%- In other pricing_modes  (`'bestinsertion'` or `'randominsertion'`), it only generates the choice sets and leaves rankings (and the corresponding columns of V_full) to be constructed on the fly by the heuristic.

% Input:
%   p_obs        : observed choice probabilities (vector)
%   pricing_mode : 'brute', 'bestinsertion', or 'randominsertion' (default: 'brute')
%   choice_sets  : cell array of choice sets to use (if empty, generates all)
%   n            : number of alternatives
%   chosen_alts  : vector of actually chosen alternatives for each observation 

% Output:
% V_full: matrices of deterministic rankings projected onto the observed
% choice sets.
% all_rankings: all permutations of 1:n, each row being a ranking. In heuristic modes: returned as [].
% choice_sets: The cell array of choice sets used.

if strcmp(pricing_mode, 'brute')

    % BRUTE FORCE MODE: Generate all rankings upfront
    fprintf('Generating all %d! = %d possible rankings...\n', n, factorial(n));
    
    if isempty(choice_sets)

        % Generate all choice sets if not provided
        [V_full, all_rankings, choice_sets] = C_generate_choice_vectors(n);

    else
        
        % Use provided choice sets
        fprintf('Using %d provided choice sets\n', length(choice_sets));
        all_rankings = perms(1:n); %all possible rankings
        V_full = zeros(length(p_obs), size(all_rankings, 1));
        num_rankings = size(all_rankings, 1);

        % Build V_full for provided choice sets
        for col = 1:size(all_rankings, 1)

            ranking = all_rankings(col, :);
            num_rankings=size(all_rankings,1);
            for s = 1:length(choice_sets)

                set_alts = choice_sets{s};
                
                % Find positions of each alternative in the ranking

                positions = zeros(1, length(set_alts));

                for j = 1:length(set_alts)
                    positions(j) = find(ranking == set_alts(j), 1, 'first');
                end
                
                [~, top_idx] = min(positions);

                top_alt = set_alts(top_idx); %top alternatives in the choice set
                
                % Use chosen_alts if provided, otherwise default to set_alts(1)
                if ~isempty(chosen_alts)
                    V_full(s, col) = (top_alt == chosen_alts(s));
                else
                    error('Incomplete Ranking!');
                end
            end
        if mod(col, 5000) == 0
            fprintf('  Progress: %d/%d rankings processed (%.1f%%)\n', ...
                col, num_rankings, 100*col/num_rankings);
        end
        end
    end
    fprintf('Full V matrix built. Size: %d x %d (%.2f MB)\n', ...
        size(V_full), 8*numel(V_full)/1e6);
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%%%For Heuristic Methods%%%%%%%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
else

    % BEST INSERTION or RANDOM INSERTION MODE: Only generate choice sets, build rankings on-the-fly
    fprintf('Generating choice sets only (rankings built on-demand)...\n');
    
    if isempty(choice_sets)
        % Generate only the choice sets (all nonempty subsets)
        alternatives = 1:n;
        num_sets = 2^n - 1;
        choice_sets = cell(num_sets, 1);
        idx = 1;
        for k = 1:n
            subsets = nchoosek(alternatives, k);
            for i = 1:size(subsets, 1)
                choice_sets{idx} = subsets(i, :);
                idx = idx + 1;
            end
        end
    else
        fprintf('Using %d provided choice sets\n', length(choice_sets));
    end
    
    % For best insertion / random insertion, we don't need V_full or all_rankings upfront
    V_full = [];
    all_rankings = [];
    fprintf('✓ %d choice sets generated (no ranking enumeration needed)\n\n', length(choice_sets));

end

end
