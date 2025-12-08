% generate_fake_data_n8.m
% Can you see this edit??
% --------------------------------------------------------------
% Generate fake pairwise choice data for n=8 alternatives
% Format matches the Haoge CSV structure
% --------------------------------------------------------------

clear; clc;

rng(41); % For reproducibility

n = 12;  % number of alternatives
max_possible = nchoosek(n, 2);  % Maximum possible pairwise comparisons
num_comparisons = min(276, max_possible);  % 25 comparisons (out of 28 possible)

fprintf('Generating fake data for n=%d alternatives...\n', n);
fprintf('Maximum possible pairwise comparisons: %d\n', max_possible);
fprintf('Generating %d comparisons...\n\n', num_comparisons);

% Generate true underlying utilities (what we're trying to recover)
true_utilities = randn(n, 1);
fprintf('True utilities: ');
fprintf('%.3f ', true_utilities);
fprintf('\n\n');

% Store all comparisons
data = [];
comparisons_used = zeros(0, 2); % Initialize as empty 0x2 matrix

% Generate random pairwise comparisons
attempts = 0;
max_attempts = num_comparisons * 100;  % Safety limit

while size(comparisons_used, 1) < num_comparisons && attempts < max_attempts
    attempts = attempts + 1;
    
    % Randomly select 2 different alternatives
    alts = randsample(n, 2);
    alt1 = alts(1);
    alt2 = alts(2);
    
    % Check if we already have this comparison
    comparison_key = sort([alt1, alt2]);
    if ismember(comparison_key, comparisons_used, 'rows')
        continue;
    end
    
   
    % Add to used comparisons
    comparisons_used = [comparisons_used; comparison_key];
    
    % Compute choice probability using logit model
    % P(choose alt1 | {alt1, alt2}) = exp(u1) / (exp(u1) + exp(u2))
    u1 = true_utilities(alt1);
    u2 = true_utilities(alt2);
    
    %prob_alt1 = rand(1,1);
    %prob_alt2 = 1-prob_alt1;
    
    prob_alt1 = normcdf(u1-u2)
    prob_alt2 = 1 - prob_alt1;
    
    % Add both rows (one for each alternative's probability)
    data = [data; prob_alt1, alt1, alt2, alt1];
    data = [data; prob_alt2, alt1, alt2, alt2];
    
end

% Create table with proper column names
fake_data = array2table(data, 'VariableNames', ...
    {'choice_probability', 'set_alt1', 'set_alt2', 'alt'});

% Save to CSV
name=strcat(['fake_data_n',num2str(n),'_binary.csv']);
writetable(fake_data, name, 'WriteRowNames', true);

fprintf('✓ Generated %d unique pairwise comparisons\n', num_comparisons);
fprintf('✓ Total rows in dataset: %d\n', size(data, 1));
fprintf(strcat('✓ Saved to: ',name,'\n'));

% Display first few rows
fprintf('First 10 rows:\n');
disp(fake_data(1:10, :));

% Summary statistics
fprintf('\nChoice probability statistics:\n');
fprintf('  Min:  %.4f\n', min(fake_data.choice_probability));
fprintf('  Max:  %.4f\n', max(fake_data.choice_probability));
fprintf('  Mean: %.4f\n', mean(fake_data.choice_probability));
fprintf('  Std:  %.4f\n', std(fake_data.choice_probability));

fprintf('\n✓ Fake data generation complete!\n');
