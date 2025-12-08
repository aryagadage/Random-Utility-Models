load("/Users/ag5276/Documents/Haoge/optimization stuff/p_obs.mat")   
n = 5;
%% Extract the numeric probability column
if istable(p_obs)
    if any(strcmp('Observed_Prob', p_obs.Properties.VariableNames))
        p_obs = p_obs.Observed_Prob;
    else
        error('Expected column "Observed_Prob" not found in p_obs table.');
    end
end
[lambda_opt, V, rankings, choice_sets, error_val] = solve_rum_projection(p_obs, n);
