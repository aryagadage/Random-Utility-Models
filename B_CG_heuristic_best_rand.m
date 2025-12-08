function [V_sub,rankings,best_score]=B_CG_heuristic_best_rand(V_sub,n,choice_sets,chosen_alts,residual,p_optim,rankings,p_obs)
%-----------------------------------------------------------------------------------------------------------------------------
% Randomized Best Insertion Heuristic for Column Generation.

%  B_CG_heuristic_best_rand: picks a RANDOM alternative, finds best position for it
%-----------------------------------------------------------------------------------------------------------------------------
% Input:
%   k        : the initial element
%   n        : number of alternatives
%   p_obs        : observed choice probabilities (vector)
%   n            : number of alternatives
%   choice_sets  : cell array of choice sets to use (if empty, generates all)
%   chosen_alts  : vector of actually chosen alternatives for each observation (optional)

%   residual : current prices
%   rankings : current columns
%   p_optim  : current optimizers
%-----------------------------------------------------------------------------------------------------------------------------
% Start with random alternative (ensure it's a row vector)

% Initialize: start with a RANDOM alternative (unlike B_CG_heuristic_best which takes k as input)
best_ranking = randsample(1:n,1);
best_ranking = reshape(best_ranking, 1, []);  % Force row vector
remaining_alts = setdiff(1:n, best_ranking);

%iter=0;
% Greedily add remaining alternatives
while ~isempty(remaining_alts)
    %iter=iter+1;
    %fprintf('\n Iter %1d:',iter);
    best_pos = -1;
    best_pos_score = -Inf;
            
    % Try EACH remaining alternative
    alt = datasample(remaining_alts, 1, 'Replace', false);
        %fprintf('%1d',alt)
        % Try inserting at each position
    for pos = 1:(length(best_ranking) + 1)
       % Create candidate ranking (handle different cases explicitly)
       if pos == 1
           candidate = [alt, best_ranking];
       elseif pos > length(best_ranking)
           candidate = [best_ranking, alt];
       else
           candidate = [best_ranking(1:pos-1), alt, best_ranking(pos:end)];
       end
                    
       % Compute score for this candidate
        v_candidate = zeros(length(p_obs), 1);
        for s = 1:length(choice_sets)
            set_alts = choice_sets{s};
                        
                % Skip if any alternative in this choice set is not yet in candidate
            all_present = true;
            for j = 1:length(set_alts)
                if isempty(find(candidate == set_alts(j), 1))
                    all_present = false;
                break;
            end
            end
            if ~all_present
                continue;  % Skip this choice set
            end
                        
            % Find positions of each alternative in the candidate ranking
    
            % Where is {1,4,5} location in ranking [ 3 1 5 2 4]
            positions = zeros(1, length(set_alts));
            for j = 1:length(set_alts)
                positions(j) = find(candidate == set_alts(j), 1, 'first');
            end
                        
            [~, top_idx] = min(positions);
            top_alt = set_alts(top_idx);
                        
                % Use chosen_alts if provided, otherwise default to set_alts(1)

            v_candidate(s) = (top_alt == chosen_alts(s));

    end
        score = v_candidate' * residual;
                    
        if score > best_pos_score
            best_pos_score = score;
            best_pos = pos;
        end
    end

            
        % Insert best alternative at best position (handle cases explicitly)
    if best_pos == 1
        best_ranking = [alt, best_ranking];
    elseif best_pos > length(best_ranking)
        best_ranking = [best_ranking, alt];
    else
        best_ranking = [best_ranking(1:best_pos-1), alt, best_ranking(best_pos:end)];
    end
            
    remaining_alts = setdiff(remaining_alts, alt);
end
        
        % Compute final score
[v_final, ~, ~]=C_gen_one_ranking(p_obs,choice_sets,chosen_alts,n,'deterministic',best_ranking);
V_sub = [V_sub, v_final];
rankings = [rankings; best_ranking];
best_score = v_final' * residual - p_optim'*residual;
        

end
