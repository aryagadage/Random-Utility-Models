

*********************************************
**B_generate_fake_data_binarytenary.m:*******
*********************************************

	Input: n number of alternatives

        Output: p_obs ( column vector): choice probabilities for each (menu, alternative) row.

		choice_sets (cell array): each cell contains a vector of alternative indices defining the choice set for that row.

		chosen_alts (column vector): the alternative index corresponding to that row’s probability.

		choice_set_list (cell array): unique list of all sampled menus (binary, ternary, and the full set).

*************
**B_QP.m:****
*************

Purpose
This function estimates a random utility model (RUM) as a finite mixture over deterministic preference rankings using quadratic programming (least squares).
Given:

a matrix of predicted choice probabilities for each deterministic ranking, and

observed choice probabilities,

it finds mixture weights (lambdas) that best approximate the data in least-squares sense, subject to:

lambdas ≥ 0

sum of lambdas = 1

Formally, it solves:
min_λ ‖p_obs – V_unique * λ‖²
s.t. λ ≥ 0, 1'λ = 1.

