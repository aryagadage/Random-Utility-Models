function [V_sub,rankings,best_score]=B_CG_heuristic_best(k,V_sub,n,choice_sets,chosen_alts,residual,p_optim,rankings,p_obs)
%----------------------------------------------------------------------------------------------------
% Best Insertion Heuristic for Column Generation.
%----------------------------------------------------------------------------------------------------
% Instead of enumerating all n! rankings (exponential), this heuristic constructs 1 ranking greedily by inserting alternatives
% one at a time into the position that maximizes the score.
%----------------------------------------------------------------------------------------------------

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
% Start with random alternative (ensure it's a row vector)

% Initialize: start with the given alternative(s)
best_ranking = k;
best_ranking = reshape(best_ranking, 1, []);  % Force row vector
remaining_alts = setdiff(1:n, best_ranking);

%iter=0;
% Greedily add remaining alternatives
while ~isempty(remaining_alts)

    
    best_pos = -1;
    best_pos_score = -Inf;
    best_alt_to_add = -1;

    % Try EACH remaining alternative at EACH possible position
    for alt = remaining_alts
        %fprintf('%1d',alt)
        % Try inserting at each position -> (1 = most preferred, end+1 = least)
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
                        
                % Skip if any alternative in this choice set is not yet in candidate (we can't determine the choice until ranking is complete for that set)
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
                
                % Top alternative = one with minimum position (highest ranked)   
                
                [~, top_idx] = min(positions);
                top_alt = set_alts(top_idx);
                        
                % Use chosen_alts if provided, otherwise default to set_alts(1)

                v_candidate(s) = (top_alt == chosen_alts(s));


            end
            %% Score = correlation with residual (how much this ranking helps)
                score = v_candidate' * residual;
                
               % Insert the best alternative at the best position     
                if score > best_pos_score
                    best_pos_score = score;
                    best_pos = pos;
                    best_alt_to_add = alt;
                end
            end
        end
            
        % Insert best alternative at best position (handle cases explicitly)
        if best_pos == 1
           best_ranking = [best_alt_to_add, best_ranking];
        elseif best_pos > length(best_ranking)
           best_ranking = [best_ranking, best_alt_to_add];
        else
           best_ranking = [best_ranking(1:best_pos-1), best_alt_to_add, best_ranking(best_pos:end)];
        end
        % Remove the inserted alternative from remaining set    
        remaining_alts = setdiff(remaining_alts, best_alt_to_add);
    end
        
        % Compute final score
[v_final, ~, ~]=C_gen_one_ranking(p_obs,choice_sets,chosen_alts,n,'deterministic',best_ranking);

% Update candidate set
V_sub = [V_sub, v_final];
rankings = [rankings; best_ranking];

% Reduced cost: positive means this ranking can improve the objective
best_score = v_final' * residual - p_optim'*residual;
        

end
