load("/Users/ag5276/Documents/Haoge/optimization stuff/p_obs.mat")   
n = 5;

[lambda_opt, V, rankings, choice_sets, error_val] = solve_rum_projection(p_obs, n);
