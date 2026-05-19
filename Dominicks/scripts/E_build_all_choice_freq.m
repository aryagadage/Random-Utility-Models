%% E_build_all_choice_freq.m
% Builds the choice-frequency CSVs that A_run_all_method_HG.m consumes.
% Wraps D_build_choice_freq_per_group: one (STORE, category) draw per
% non-empty upc_menu complexity group, with one CSV + one *_upc_map.csv
% written per draw into scripts/results_choice_freq/.
%
% Reads (per-category) w*.csv files from Dominicks/data/<category>/, so
% this is the slow step — expect minutes per category. Run once; rerun
% only when you want a fresh random sample.
clear; clc;

script_dir  = fileparts(mfilename('fullpath'));
results_dir = fullfile(script_dir, 'results_choice_freq');

% Optional knobs (defaults inside D: seed=42, dir=<scripts>/results_choice_freq)
rng_seed = 42;
out_dir  = results_dir;

% Wipe stale outputs so the new draw isn't mixed with an old one.
% Comment this out if you'd rather keep both side-by-side.
if exist(results_dir, 'dir')
    old = [dir(fullfile(results_dir, 'group*_*_store*.csv')); ...
           dir(fullfile(results_dir, 'group*_*_store*_upc_map.csv'))];
    for ii = 1:numel(old)
        delete(fullfile(old(ii).folder, old(ii).name));
    end
end

tic;
D_build_choice_freq_per_group(rng_seed, out_dir);
fprintf('\nE_build_all_choice_freq: total time = %.1f s\n', toc);
