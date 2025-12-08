function [V_sub,subset_idx,rankings,best_score]=B_CG_exact(V_sub,V_full,residual,subset_idx,p_optim,all_rankings,rankings)
%----------------------------------------------------------------------------------------------------
% Column Generation pricing step: select the best ranking to add to the candidate set.
%----------------------------------------------------------------------------------------------------
% Input:
%   V_sub        : current subset of columns [num_obs x current_size]
%   V_full       : full choice matrix with all n! rankings [num_obs x K]
%   residual     : current prediction error, p_obs - V_sub*lambda [num_obs x 1]
%   subset_idx   : indices of rankings already in candidate set [current_size x 1]
%   p_optim      : current fitted probabilities, V_sub*lambda [num_obs x 1]
%   all_rankings : all preference rankings [K x n]
%   rankings     : rankings currently in candidate set [current_size x n]
% Output:
%   V_sub        : updated with new column appended [num_obs x (current_size+1)]
%   subset_idx   : updated with new index appended
%   rankings     : updated with new ranking appended
%   best_score   : reduced cost of the added column (positive = improvement possible)

    %select the columns to be added to the candidate set
    scores = V_full' * residual;
    scores(subset_idx) = -Inf; %omit the exisiting rankings
    [best_score, best_idx] = max(scores);

    % Find the ranking with the highest score (best candidate to add) - does the best score exceeds that of the candidate optimizer?
    best_score = best_score-p_optim' * residual;

    %update rankings
     V_sub = [V_sub, V_full(:, best_idx)];
     subset_idx = [subset_idx; best_idx];
     rankings = [rankings; all_rankings(best_idx, :)];
     %fprintf('new_col = %d | subset_size = %d\n', best_idx, length(subset_idx));

end
