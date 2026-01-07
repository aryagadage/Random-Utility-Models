function [V_sub, rankings, subset_idx] = C_gen_one_ranking(p_obs,choice_sets,chosen_alts,n,mode,given_ranking)

% Input:
%   p_obs        : observed choice probabilities (vector)
%   n            : number of alternatives
%   choice_sets  : cell array of choice sets to use (if empty, generates all)
%   chosen_alts  : vector of actually chosen alternatives for each observation (optional)
%   mode         : random or deterministic, if deterministic, generate v
%   according to the given_ranking
%   given_ranking: user input for a determnistic ranking

% Output:
%   rankings: randomly generated rankings
%   V_sub: the corresponding choice probability vecotrs
%   sub_idx: an index set for the rankings

    V_sub = zeros(length(p_obs), 1);
    rankings = zeros(1, n);
    subset_idx = zeros(1, 1);
    
    % Generate a random ranking
    if strcmp(mode,'random')
        ranking_temp = randperm(n);
        rankings(1, :) = ranking_temp;
    elseif strcmp(mode,'deterministic')
        ranking_temp = given_ranking;
        rankings(1, :) = ranking_temp;
    end
    
    % Compute its choice vector
    v = zeros(length(p_obs), 1);

    %create  choice_vector
    for s = 1:length(choice_sets)
     
        set_alts = choice_sets{s};
            
        % Find positions of each alternative in random_ranking
        positions = zeros(1, length(set_alts));
        for j = 1:length(set_alts)
            positions(j) = find(ranking_temp == set_alts(j), 1, 'first');
        end
            
        [~, top_idx] = min(positions);
            
        top_alt = set_alts(top_idx);
            
        v(s) = (top_alt == chosen_alts(s));

    end

    V_sub(:, 1) = v;
    subset_idx(1) = 1;

end
