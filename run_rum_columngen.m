load('p_obs.mat')  % loads the observed choice probabilities (80x1 vector)
n = 5;             % number of alternatives

%% Extract the numeric probability column
if istable(p_obs)
    if any(strcmp('Observed_Prob', p_obs.Properties.VariableNames))
        p_obs = p_obs.Observed_Prob;
    else
        error('Expected column "Observed_Prob" not found in p_obs table.');
    end
end

[lambda_sub, V_sub, subset_idx, rankings, choice_sets, error_val, iter] = ...
    solve_rum_columngen(p_obs, n, 1, 200, 1e-8);
