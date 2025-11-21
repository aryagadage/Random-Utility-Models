% run_all_methods.m
% --------------------------------------------------------------
% Run Pure Brute Force and ColGen Brute methods
% csv_file variable should be set before calling this script
% Results stored in 'results' struct
% --------------------------------------------------------------

% Note: csv_file should be set by calling script
% Example: csv_file = 'data_n8_instances/fake_data_n8_1.csv';

% Load data from CSV
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
    chosen_alts(i) = data.alt(i);  % The actual chosen alternative
end

% --- Algorithm parameters (same for all methods) ---
init_k = 1;          
max_iters = 100;     
tol = 1e-8;          

% Storage for results
results = struct();


%% METHOD 1: PURE BRUTE FORCE with LSQLIN
tic;
try
    % Generate all possible rankings (n! permutations)
    all_rankings = perms(1:n);
    num_rankings = size(all_rankings, 1);
    
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
    end
    
    % Remove duplicate columns to reduce problem size
    V_unique = unique(V_full', 'rows', 'stable')';
    
    % Solve optimization using LSQLIN
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
    
catch ME
    time_brute = toc;
    results.brute_force.success = false;
    results.brute_force.error = NaN;
    results.brute_force.time = time_brute;
    results.brute_force.error_msg = ME.message;
end


%% METHOD 2: COLUMN GENERATION with BRUTE FORCE PRICING
tic;
try
    [lambda1, V1, idx1, rank1, cs1, err1, iter1, x1] = ...
        solve_rum_columngen2(p_obs, n, init_k, max_iters, tol, choice_sets, 'brute', chosen_alts);
    time1 = toc;
    
    results.colgen_brute.lambda = lambda1;
    results.colgen_brute.error = err1;
    results.colgen_brute.iterations = iter1;
    results.colgen_brute.time = time1;
    results.colgen_brute.num_columns = length(idx1);
    results.colgen_brute.rankings = rank1;
    results.colgen_brute.success = true;
    
catch ME
    time1 = toc;
    results.colgen_brute.success = false;
    results.colgen_brute.error = NaN;
    results.colgen_brute.time = time1;
    results.colgen_brute.error_msg = ME.message;
end