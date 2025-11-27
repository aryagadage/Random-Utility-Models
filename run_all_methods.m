clear; clc;
fprintf('Comparing 4 methods - Pure brute, Colgen brute, Colgen Best Insertion, Colgen Random Insertion');
% --- Load data from CSV ---
% generate_fake_data file
csv_file = 'fake_data_n7_binary.csv'; 

fprintf('Loading data from CSV...\n');
data = readtable(csv_file);

% Extract the choice probability column
p_obs = data.choice_probability;

% Determine number of alternatives from the data
n = max(max(data.set_alt1), max(data.set_alt2));

% Convert ALL pairs to choice_sets AND extract chosen alternatives
choice_sets = cell(height(data), 1);
chosen_alts = zeros(height(data), 1);
for i = 1:height(data)
    choice_sets{i} = [data.set_alt1(i), data.set_alt2(i)];
    chosen_alts(i) = data.alt(i);  % the alternative corresponding to the row
end

fprintf('✓ Loaded data from: %s\n', csv_file);
fprintf('  - Number of observations: %d\n', length(p_obs));
fprintf('  - Number of alternatives: %d\n', n);
fprintf('  - Number of unique choice sets (pairs): %d\n', length(choice_sets));
fprintf('  - Choice probability range: [%.4f, %.4f]\n', min(p_obs), max(p_obs));
fprintf('  - Total possible rankings: %d! = %d\n', n, factorial(n));
fprintf('\n');

% --- Algorithm parameters (same for all methods) ---
init_k = 1;          
max_iters = 100;     
tol = 1e-8;          

% Storage for results
results = struct();


%% METHOD 1: PURE BRUTE FORCE with LSQLIN
fprintf('\n[1/4] Pure Brute Force Method\n');
fprintf('========================================================\n');
fprintf('How it works:\n');
fprintf('  - Generates ALL %d! = %d possible rankings upfront\n', n, factorial(n));
fprintf('  - Builds full V matrix (observations x rankings)\n');
fprintf('  - Solves: min ||V*lambda - p_obs||^2 s.t. sum(lambda)=1, lambda>=0\n');
fprintf('  - Uses LSQLIN for constrained least squares\n');
tic;
try
    % Generate all possible rankings (n! permutations)
    all_rankings = perms(1:n);
    num_rankings = size(all_rankings, 1);
    
    fprintf('Building V matrix (%d x %d)...\n', length(p_obs), num_rankings);
    
    % Build full V matrix: V(s,r) = 1 if ranking r selects chosen alt in choice set s
    V_full = zeros(length(p_obs), num_rankings);
    
    for col = 1:num_rankings
        ranking = all_rankings(col, :);
        for s = 1:length(choice_sets)
            set_alts = choice_sets{s};
            
            % Find which alternative in the set is ranked highest
            [~, top_idx] = min(arrayfun(@(alt) find(ranking == alt), set_alts));
            top_alt = set_alts(top_idx);
            
            % V = 1 if top alternative is the actually chosen alternative
            V_full(s, col) = (top_alt == chosen_alts(s));
        end
        
        % Progress indicator
        if mod(col, 5000) == 0
            fprintf('  Progress: %d/%d rankings processed (%.1f%%)\n', ...
                col, num_rankings, 100*col/num_rankings);
        end
    end
    
    fprintf('V matrix built. Size: %d x %d (%.2f MB)\n', ...
        size(V_full), 8*numel(V_full)/1e6);
    
    % Remove duplicate columns to reduce problem size
    fprintf('Removing duplicate columns...\n');
    V_unique = unique(V_full', 'rows', 'stable')';
    fprintf('Reduced from %d to %d unique columns\n', size(V_full,2), size(V_unique,2));
    
    % Solve optimization using LSQLIN
    fprintf('Solving optimization...\n');
    Aeq = ones(1, size(V_unique,2));  % sum(lambda) = 1
    beq = 1;
    lb = zeros(size(V_unique,2), 1);  % lambda >= 0
    ub = ones(size(V_unique,2), 1);   % lambda <= 1
    
    options = optimoptions('lsqlin', ...
        'Algorithm', 'interior-point', ...
        'Display', 'off', ...
        'MaxIterations', 500);
    
    lambda_brute = lsqlin(V_unique, p_obs, [], [], Aeq, beq, lb, ub, [], options);
    
    time_brute = toc;
    
    % Compute results
    error_brute = norm(p_obs - V_unique * lambda_brute)^2;
    num_active = sum(lambda_brute > 1e-8);
    
    results.brute_force.lambda = lambda_brute;
    results.brute_force.V = V_unique;
    results.brute_force.error = error_brute;
    results.brute_force.time = time_brute;
    results.brute_force.num_active = num_active;
    results.brute_force.total_rankings = num_rankings;
    results.brute_force.success = true;
    
    fprintf('✓ Completed in %.2f seconds\n', time_brute);
    fprintf('  - Final error: %.6e\n', error_brute);
    fprintf('  - Active rankings: %d (out of %d unique columns)\n', num_active, size(V_unique,2));
    fprintf('  - Sum of lambdas: %.10f\n', sum(lambda_brute));
    
    % Show top rankings
    fprintf('  - Top 5 rankings by weight:\n');
    [sorted_lambda, sort_idx] = sort(lambda_brute, 'descend');
    for i = 1:min(5, num_active)
        if sorted_lambda(i) > 1e-8
            fprintf('    Lambda = %.6f\n', sorted_lambda(i));
        end
    end
    
catch ME
    time_brute = toc;
    results.brute_force.success = false;
    results.brute_force.error_msg = ME.message;
    fprintf('✗ FAILED: %s\n', ME.message);
end


%% METHOD 2: COLUMN GENERATION with BRUTE FORCE PRICING
fprintf('\n[2/4] Column Generation with BRUTE FORCE Pricing\n');
fprintf('========================================================\n');
fprintf('How it works:\n');
fprintf('  - Starts with %d initial ranking(s)\n', init_k);
fprintf('  - Each iteration: checks ALL %d rankings to find best one to add\n', factorial(n));
tic;
try
    [lambda1, V1, idx1, rank1, cs1, err1, iter1, x1] = ...
        solve_rum_columngen3(p_obs, n, init_k, max_iters, tol, choice_sets, 'brute', chosen_alts);
    time1 = toc;
    
    results.colgen_brute.lambda = lambda1;
    results.colgen_brute.error = err1;
    results.colgen_brute.iterations = iter1;
    results.colgen_brute.time = time1;
    results.colgen_brute.num_columns = length(idx1);
    results.colgen_brute.rankings = rank1;
    results.colgen_brute.success = true;
    
    fprintf('✓ Completed in %.2f seconds\n', time1);
    fprintf('  - Iterations: %d\n', iter1);
    fprintf('  - Final error: %.6e\n', err1);
    fprintf('  - Rankings used: %d (out of %d possible)\n', length(idx1), factorial(n));
    fprintf('  - Selected rankings:\n');
    for i = 1:size(rank1, 1)
        fprintf('    Ranking %d (lambda=%.4f): [%s]\n', i, lambda1(i), num2str(rank1(i,:)));
    end
    
catch ME
    time1 = toc;
    results.colgen_brute.success = false;
    results.colgen_brute.error_msg = ME.message;
    fprintf('✗ FAILED: %s\n', ME.message);
end

%% METHOD 3: COLUMN GENERATION with BEST INSERTION PRICING
fprintf('\n[3/4] Column Generation with BEST INSERTION Pricing\n');
fprintf('========================================================\n');
fprintf('How it works:\n');
fprintf('  - Starts with %d initial ranking(s)\n', init_k);
fprintf('  - Each iteration: uses HEURISTIC to construct a good ranking\n');
tic;
try
    [lambda2, V2, idx2, rank2, cs2, err2, iter2, x2] = ...
        solve_rum_columngen3(p_obs, n, init_k, max_iters, tol, choice_sets, 'bestinsertion', chosen_alts);
    time2 = toc;
    
    results.best_insertion.lambda = lambda2;
    results.best_insertion.error = err2;
    results.best_insertion.iterations = iter2;
    results.best_insertion.time = time2;
    results.best_insertion.num_columns = length(idx2);
    results.best_insertion.rankings = rank2;
    results.best_insertion.success = true;
    
    fprintf('✓ Completed in %.2f seconds\n', time2);
    fprintf('  - Iterations: %d\n', iter2);
    fprintf('  - Final error: %.6e\n', err2);
    fprintf('  - Rankings used: %d\n', length(idx2));
    fprintf('  - Selected rankings:\n');
    for i = 1:size(rank2, 1)
        fprintf('    Ranking %d (lambda=%.4f): [%s]\n', i, lambda2(i), num2str(rank2(i,:)));
    end
    
catch ME
    time2 = toc;
    results.best_insertion.success = false;
    results.best_insertion.error_msg = ME.message;
    fprintf('✗ FAILED: %s\n', ME.message);
    fprintf('ERROR DETAILS:\n');
    fprintf('  File: %s\n', ME.stack(1).file);
    fprintf('  Line: %d\n', ME.stack(1).line);
    fprintf('  Full stack:\n');
    for i = 1:length(ME.stack)
        fprintf('    %s (line %d)\n', ME.stack(i).name, ME.stack(i).line);
    end
end

%% METHOD 4: COLUMN GENERATION with RANDOM INSERTION PRICING
fprintf('\n[4/4] Column Generation with RANDOM INSERTION Pricing\n');
fprintf('========================================================\n');
fprintf('How it works:\n');
fprintf('  - Starts with %d initial ranking(s)\n', init_k);
fprintf('  - Each iteration: uses RANDOMIZED HEURISTIC to construct a ranking\n');
fprintf('  - Picks ONE random alternative, tries ALL positions for that one\n');
fprintf('  - Much faster than best insertion, but may need more iterations\n');
tic;
try
    [lambda3, V3, idx3, rank3, cs3, err3, iter3, x3] = ...
        solve_rum_columngen3(p_obs, n, init_k, max_iters, tol, choice_sets, 'randominsertion', chosen_alts);
    time3 = toc;
    
    results.random_insertion.lambda = lambda3;
    results.random_insertion.error = err3;
    results.random_insertion.iterations = iter3;
    results.random_insertion.time = time3;
    results.random_insertion.num_columns = length(idx3);
    results.random_insertion.rankings = rank3;
    results.random_insertion.success = true;
    
    fprintf('✓ Completed in %.2f seconds\n', time3);
    fprintf('  - Iterations: %d\n', iter3);
    fprintf('  - Final error: %.6e\n', err3);
    fprintf('  - Rankings used: %d\n', length(idx3));
    fprintf('  - Top 5 rankings by weight:\n');
    [sorted_lambda, sort_idx] = sort(lambda3, 'descend');
    for i = 1:min(5, length(lambda3))
        if sorted_lambda(i) > 1e-8
            fprintf('    Lambda = %.6f | Ranking: [%s]\n', sorted_lambda(i), num2str(rank3(sort_idx(i),:)));
        end
    end
    
catch ME
    time3 = toc;
    results.random_insertion.success = false;
    results.random_insertion.error_msg = ME.message;
    fprintf('✗ FAILED: %s\n', ME.message);
    fprintf('ERROR DETAILS:\n');
    fprintf('  File: %s\n', ME.stack(1).file);
    fprintf('  Line: %d\n', ME.stack(1).line);
end


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
