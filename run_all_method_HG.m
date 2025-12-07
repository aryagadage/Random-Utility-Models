%%
clear; clc;

%%
fprintf('Comparing 4 methods - Pure brute, Colgen brute, Colgen Best Insertion, Colgen Random Insertion');

%%
% --- Algorithm parameters (same for all methods) ---
init_k = 1;          
max_iters = 100;     
tol = 1e-8;          

% Storage for results
results = struct();

%%
n=15;
[p_obs,choice_sets,chosen_alts,choice_set_list]=generate_fake_data_binarytenary(n);
%% METHOD 1: PURE BRUTE FORCE with LSQLIN
fprintf('\n[1/4] Pure Brute Force Method\n');
fprintf('========================================================\n');

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%QP with all_possible rankings%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Generate all possible rankings (n! permutations)
[V_full, all_rankings, choice_sets]=C_gen_V_full(n,choice_sets,'brute',p_obs,chosen_alts);
    
% Brutal Force QP
result_QP = B_QP(V_full,p_obs,true);

result_QP.QP.error

%% METHOD 2: COLUMN GENERATION with BRUTE FORCE PRICING
fprintf('\n[2/4] Column Generation with BRUTE FORCE Pricing\n');
fprintf('========================================================\n');

% Brutal Force CG
result_CG= B_solve_rum_CG(p_obs, n, init_k, max_iters, tol, choice_sets, 'brute', chosen_alts,choice_set_list);

error_CG=result_CG.QP.error;

 
%% METHOD 3: COLUMN GENERATION with BEST INSERTION PRICING
fprintf('\n[3/4] Column Generation with BEST INSERTION Pricing\n');
fprintf('========================================================\n');
fprintf('How it works:\n');
fprintf('  - Starts with %d initial ranking(s)\n', init_k);
fprintf('  - Each iteration: uses HEURISTIC to construct a good ranking\n');
tic;
[result_CG_BI,residual] = ...
   B_solve_rum_CG(p_obs, n, init_k, max_iters, tol, choice_sets, 'bestinsertion', chosen_alts,choice_set_list);
time2 = toc;

error_CG_BI=result_CG_BI.QP.error;

  

%% METHOD 4: COLUMN GENERATION with RANDOM INSERTION PRICING
fprintf('\n[4/4] Column Generation with RANDOM INSERTION Pricing\n');
[result_CG_BI_rand,residual] = B_solve_rum_CG(p_obs, n, init_k, max_iters, tol, choice_sets, 'bestinsertion_rand', chosen_alts,choice_set_list);

%% COMPARISON SUMMARY
fprintf('\n\n==================================================\n');
fprintf('              COMPARISON SUMMARY\n');
fprintf('==================================================\n\n');

fprintf('%-35s | %10s | %10s | %12s | %10s | %s\n', ...
    'Method', 'Time (s)', 'Iterations', 'Error', 'Columns', 'Status');
fprintf('-------------------------------------|------------|------------|--------------|------------|--------\n');

% Brute Force
if results.brute_force.success
    fprintf('%-35s | %10.2f | %10s | %12.2e | %10d | %s\n', ...
        'Brute Force (LSQLIN)', time_brute, 'N/A', error_brute, num_active, '✓ OK');
else
    fprintf('%-35s | %10.2f | %10s | %12s | %10s | %s\n', ...
        'Brute Force (LSQLIN)', time_brute, '-', '-', '-', '✗ FAIL');
end



% ColGen Brute
if results.colgen_brute.success
    fprintf('%-35s | %10.2f | %10d | %12.2e | %10d | %s\n', ...
        'ColGen (Brute Pricing)', time1, iter1, err1, length(idx1), '✓ OK');
else
    fprintf('%-35s | %10.2f | %10s | %12s | %10s | %s\n', ...
        'ColGen (Brute Pricing)', time1, '-', '-', '-', '✗ FAIL');
end

% Best Insertion
if results.best_insertion.success
    fprintf('%-35s | %10.2f | %10d | %12.2e | %10d | %s\n', ...
        'ColGen (Best Insertion)', time2, iter2, err2, length(idx2), '✓ OK');
else
    fprintf('%-35s | %10.2f | %10s | %12s | %10s | %s\n', ...
        'ColGen (Best Insertion)', time2, '-', '-', '-', '✗ FAIL');
end


% Random Insertion
if results.random_insertion.success
    fprintf('%-35s | %10.2f | %10d | %12.2e | %10d | %s\n', ...
        'ColGen (Random Insertion)', time3, iter3, err3, length(idx3), '✓ OK');
else
    fprintf('%-35s | %10.2f | %10s | %12s | %10s | %s\n', ...
        'ColGen (Random Insertion)', time3, '-', '-', '-', '✗ FAIL');
end
