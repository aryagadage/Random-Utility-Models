function [V_sub,subset_idx,rankings,best_score]=B_CG_exact(V_sub,V_full,residual,subset_idx,p_optim,all_rankings,rankings)

    %select the columns to be added to the candidate set
    scores = V_full' * residual;
    scores(subset_idx) = -Inf; %omit the exisiting rankings
    [best_score, best_idx] = max(scores);
    
    %does the best score exceeds that of the candidate optimizer?
    best_score = best_score-p_optim' * residual;

    %update rankings
     V_sub = [V_sub, V_full(:, best_idx)];
     subset_idx = [subset_idx; best_idx];
     rankings = [rankings; all_rankings(best_idx, :)];
     %fprintf('new_col = %d | subset_size = %d\n', best_idx, length(subset_idx));

end