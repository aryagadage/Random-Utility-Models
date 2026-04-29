%% Column Generation Algoritm (CGA)
clear; clc;
fprintf('Comparing 4 methods - Pure brute, Colgen brute, Colgen Best Insertion, Colgen Random Insertion');

% --- Algorithm parameters (same for all methods) ---
init_k = 1; % number of initial rankings for CGA         
max_iters = 100; % maximum number of iterations for CGA     
tol = 1e-4;  % tolerance for CGA 

% Storage for results
results = struct();

%% generate choice probabilities
n=24;
[p_obs,choice_sets,chosen_alts,choice_set_list]=B_generate_fake_data_binarytenary(n);
%[p_obs,choice_sets,chosen_alts,choice_set_list]=B_generate_fake_data_binarytenary_incomplete(n);
%[p_obs2,choice_sets,chosen_alts,choice_set_list]=B_generate_fake_data_binarytenary_incomplete(n);

%p_obs=p_obs(chosen_alts==1);
%choice_sets=choice_sets(chosen_alts==1);
%p_obs2=p_obs2(chosen_alts==1);


%B_IP_pricing_incomplete(p_obs,choice_sets,chosen_alts,choice_set_list,[],[]);

% %% METHOD 1: PURE BRUTE FORCE with LSQLIN
% fprintf('\n[1/5] Pure Brute Force Method\n');
% fprintf('========================================================\n');
% 
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%%%%%%%%%%%%QP with all_possible rankings%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% % Generate all possible rankings (n! permutations)
%[V_full, all_rankings, choice_sets]=C_gen_V_full(n,choice_sets,'brute',p_obs,chosen_alts);
% 
% % Brutal Force QP
% result_QP = B_QP(V_full,p_obs,true);
% 
% result_QP.QP.error


%% METHOD 2: COLUMN GENERATION with BRUTE FORCE PRICING
%fprintf('\n[2/5] Column Generation with BRUTE FORCE Pricing\n');
%fprintf('========================================================\n');

% Brutal Force CG
%result_CG= B_solve_rum_CG(p_obs, n, init_k, max_iters, tol, choice_sets, 'brute', chosen_alts,choice_set_list,false);

%error_CG=result_CG.QP.error;

 
% %% METHOD 3: COLUMN GENERATION with BEST INSERTION PRICING
% fprintf('\n[3/5] Column Generation with BEST INSERTION Pricing\n');
% fprintf('========================================================\n');
% fprintf('How it works:\n');
% fprintf('  - Starts with %d initial ranking(s)\n', init_k);
% fprintf('  - Each iteration: uses HEURISTIC to construct a good ranking\n');
% tic;
% [result_CG_BI,residual] = ...
%    B_solve_rum_CG(p_obs, n, init_k, max_iters, tol, choice_sets, 'bestinsertion', chosen_alts,choice_set_list,IP=false);
% time2 = toc;
% 
% error_CG_BI=result_CG_BI.QP.error;



%% METHOD 4: COLUMN GENERATION with RANDOM INSERTION PRICING (without IP to check terminating condition)
fprintf('\n[4/5] Column Generation with RANDOM INSERTION Pricing\n');
[result_CG_BI_rand,residual] = B_solve_rum_CG(p_obs, n, init_k, max_iters, tol, choice_sets, 'bestinsertion_rand', chosen_alts,choice_set_list,false);

%% METHOD 5: COLUMN GENERATION with RANDOM INSERTION PRICING (with IP to check terminating condition)
fprintf('\n[5/5] Column Generation with RANDOM INSERTION Pricing\n');
[result_CG_BI_rand,residual] = B_solve_rum_CG(p_obs, n, init_k, max_iters, tol, choice_sets, 'bestinsertion_rand', chosen_alts,choice_set_list,true);
 
%% METHOD 6: Frank Wolfe
fprintf('Frank Wolfe Method with Determinisitc Step Size');
[result_CG_BI_rand,residual] = B_solve_rum_FW(p_obs, n, max_iters, tol, choice_sets,  chosen_alts,choice_set_list);
