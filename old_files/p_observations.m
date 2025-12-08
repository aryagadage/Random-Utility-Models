%% ============================================================
% Preprocess observed probabilities into labeled matrix (A–E)
% ============================================================

clear; clc;

alts = {'A','B','C','D','E'};
n = numel(alts);

% --- Observed probabilities as vector (same as before) ---
p_obs = [
    % --- Singletons ---
    1; 1; 1; 1; 1;

    % --- Pairs (10*2=20 entries) ---
    0.6;0.4;  0.55;0.45;  0.5;0.5;  0.65;0.35;  0.4;0.6;
    0.7;0.3;  0.45;0.55;  0.6;0.4;  0.35;0.65;  0.5;0.5;

    % --- Triples (10*3=30 entries) ---
    0.5;0.3;0.2;   0.4;0.4;0.2;   0.45;0.25;0.3;   0.6;0.25;0.15;
    0.3;0.4;0.3;   0.5;0.2;0.3;   0.35;0.45;0.2;   0.25;0.5;0.25;
    0.2;0.3;0.5;   0.33;0.33;0.34;

    % --- Quadruples (5*4=20 entries) ---
    0.4;0.3;0.2;0.1;   0.25;0.25;0.25;0.25;
    0.5;0.2;0.2;0.1;   0.3;0.3;0.2;0.2;
    0.2;0.3;0.25;0.25;

    % --- Full set (1*5=5 entries) ---
    0.3;0.25;0.2;0.15;0.1
];

%% ============================================================
% Build structured labeled matrix (A–E columns)
% ============================================================

row_labels = {};      % e.g. "AB-A"
prob_rows = [];       % each row = [A B C D E]

idx = 1;
for k = 1:n
    combos = nchoosek(1:n, k);
    for i = 1:size(combos,1)
        subset = alts(combos(i,:));
        m = numel(subset);
        probs = p_obs(idx:idx+m-1);
        idx = idx + m;

        % for each alternative in this menu, make a separate labeled row
        for j = 1:m
            row_name = sprintf('%s-%s', strjoin(subset,''), subset{j});
            row = zeros(1,n);
            row(combos(i,:)) = probs;
            row_labels{end+1,1} = row_name;
            prob_rows = [prob_rows; row];
        end
    end
end

%% ============================================================
% Create table and save
% ============================================================

p_obs_table = array2table(prob_rows, 'VariableNames', alts);
p_obs_table.ChoiceSet = row_labels;
p_obs_table = movevars(p_obs_table, 'ChoiceSet', 'Before', 'A');

% Save as MAT and CSV
save('p_obs.mat', 'p_obs_table', 'p_obs');
writetable(p_obs_table, 'p_obs.csv');

disp('p_obs.csv created with ChoiceSet labels like "AB-A", "AB-B", etc.');
