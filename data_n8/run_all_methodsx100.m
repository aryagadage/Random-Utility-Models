% run_100.m
% --------------------------------------------------------------
% Run Pure Brute and ColGen Brute on 100 fake data instances
% Calls run_all_methods.m for each CSV file
% Generates simple 5-column table with results
% --------------------------------------------------------------

clear; clc;

fprintf('RUNNING 100 INSTANCES: PURE BRUTE vs COLGEN BRUTE\n');

% Parameters
num_instances = 100;
data_dir = 'data_n8_instances';

% Initialize results table (5 columns exactly)
results_table = table();
results_table.dataset_name = cell(num_instances, 1);
results_table.brute_error = zeros(num_instances, 1);
results_table.brute_time = zeros(num_instances, 1);
results_table.colgen_error = zeros(num_instances, 1);
results_table.colgen_time = zeros(num_instances, 1);

% Start total timer
total_start = tic;

% Loop through all instances
for instance = 1:num_instances
    
    fprintf('[%d/%d] ', instance, num_instances);
    
    % Set csv_file for run_all_methods.m script
    csv_file = fullfile(data_dir, sprintf('fake_data_n8_%d.csv', instance));
    results_table.dataset_name{instance} = sprintf('fake_data_n8_%d', instance);
    
    try
        % Run the script - it will create 'results' struct in workspace
        run('run_all_methods_02.m');
        
        % Extract Pure Brute results
        if results.brute_force.success
            results_table.brute_error(instance) = results.brute_force.error;
            results_table.brute_time(instance) = results.brute_force.time;
            fprintf('Brute: ✓ (%.2fs) ', results.brute_force.time);
        else
            results_table.brute_error(instance) = NaN;
            results_table.brute_time(instance) = NaN;
            fprintf('Brute: ✗ ');
        end
        
        % Extract ColGen Brute results
        if results.colgen_brute.success
            results_table.colgen_error(instance) = results.colgen_brute.error;
            results_table.colgen_time(instance) = results.colgen_brute.time;
            fprintf('ColGen: ✓ (%.2fs)\n', results.colgen_brute.time);
        else
            results_table.colgen_error(instance) = NaN;
            results_table.colgen_time(instance) = NaN;
            fprintf('ColGen: ✗\n');
        end
        
    catch ME
        fprintf('✗ ERROR: %s\n', ME.message);
        results_table.brute_error(instance) = NaN;
        results_table.brute_time(instance) = NaN;
        results_table.colgen_error(instance) = NaN;
        results_table.colgen_time(instance) = NaN;
    end
    
    % Progress indicator every 10 instances
    if mod(instance, 10) == 0
        elapsed = toc(total_start);
        avg_time = elapsed / instance;
        remaining = avg_time * (num_instances - instance);
        fprintf('>>> Progress: %d/%d (%.1f%%) | Elapsed: %.1fs | ETA: %.1fs <<<\n', ...
            instance, num_instances, 100*instance/num_instances, elapsed, remaining);
    end
    
    % Checkpoint save every 25 instances
    if mod(instance, 25) == 0
        checkpoint_file = sprintf('results_checkpoint_%d.csv', instance);
        writetable(results_table(1:instance, :), checkpoint_file);
        fprintf('>>> Checkpoint saved: %s <<<\n', checkpoint_file);
    end
end

total_time = toc(total_start);

%% SAVE FINAL RESULTS
output_file = 'results_n8_100instances.csv';
writetable(results_table, output_file);

fprintf('\n==================================================\n');
fprintf('              EXPERIMENT COMPLETE\n');
fprintf('==================================================\n\n');
fprintf('✓ Total time: %.2f seconds (%.2f minutes)\n', total_time, total_time/60);
fprintf('✓ Average time per instance: %.2f seconds\n', total_time/num_instances);
fprintf('✓ Results saved to: %s\n\n', output_file);

%% SUMMARY STATISTICS
fprintf('==================================================\n');
fprintf('              SUMMARY STATISTICS\n');
fprintf('==================================================\n\n');

% Count successes (non-NaN values)
brute_success = sum(~isnan(results_table.brute_error));
colgen_success = sum(~isnan(results_table.colgen_error));

fprintf('PURE BRUTE FORCE:\n');
fprintf('  Success rate: %d/%d (%.1f%%)\n', brute_success, num_instances, 100*brute_success/num_instances);
if brute_success > 0
    brute_errors = results_table.brute_error(~isnan(results_table.brute_error));
    brute_times = results_table.brute_time(~isnan(results_table.brute_time));
    fprintf('  Mean error: %.6e\n', mean(brute_errors));
    fprintf('  Mean time: %.2f seconds\n', mean(brute_times));
    fprintf('  Min time: %.2f seconds\n', min(brute_times));
    fprintf('  Max time: %.2f seconds\n', max(brute_times));
end
fprintf('\n');

fprintf('COLGEN BRUTE:\n');
fprintf('  Success rate: %d/%d (%.1f%%)\n', colgen_success, num_instances, 100*colgen_success/num_instances);
if colgen_success > 0
    colgen_errors = results_table.colgen_error(~isnan(results_table.colgen_error));
    colgen_times = results_table.colgen_time(~isnan(results_table.colgen_time));
    fprintf('  Mean error: %.6e\n', mean(colgen_errors));
    fprintf('  Mean time: %.2f seconds\n', mean(colgen_times));
    fprintf('  Min time: %.2f seconds\n', min(colgen_times));
    fprintf('  Max time: %.2f seconds\n', max(colgen_times));
end
fprintf('\n');

% Comparison (only for instances where both succeeded)
both_success = ~isnan(results_table.brute_error) & ~isnan(results_table.colgen_error);
num_both = sum(both_success);

if num_both > 0
    fprintf('COMPARISON (both methods succeeded: %d instances):\n', num_both);
    
    brute_times_both = results_table.brute_time(both_success);
    colgen_times_both = results_table.colgen_time(both_success);
    speedup = brute_times_both ./ colgen_times_both;
    
    fprintf('  ColGen faster: %d/%d instances (%.1f%%)\n', ...
        sum(colgen_times_both < brute_times_both), num_both, ...
        100*sum(colgen_times_both < brute_times_both)/num_both);
    fprintf('  Mean speedup: %.2fx\n', mean(speedup));
    fprintf('  Median speedup: %.2fx\n', median(speedup));
    
    brute_errors_both = results_table.brute_error(both_success);
    colgen_errors_both = results_table.colgen_error(both_success);
    error_diff = abs(brute_errors_both - colgen_errors_both);
    
    fprintf('  Mean error difference: %.6e\n', mean(error_diff));
    fprintf('  Max error difference: %.6e\n', max(error_diff));
end

fprintf('\n==================================================\n');
fprintf('Results table preview (first 10 rows):\n\n');
disp(results_table(1:min(10, num_instances), :));

fprintf('\n✓ Full results saved to: %s\n', output_file);
fprintf('==================================================\n');