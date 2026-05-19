%% Column Generation on Dominick's choice-frequency data
% Loops over every CSV in scripts/results_choice_freq/ (one per non-empty
% upc_menu complexity group) and runs two RUM solvers on each:
%   (1) CG with IP pricing every iteration
%   (2) CG with random-insertion heuristic pricing + IP at convergence check
%
% For each (csv, method) it records:
%   - complexity group, category, STORE
%   - n alternatives, # observations, # unique menus
%   - total wall time
%   - per-iteration IP solve times (mean / max / total / count)
%   - final squared-error fit
%
% Outputs:
%   results_runs/run_summary.csv  — flat per-(csv,method) summary
%   results_runs/run_details.mat  — full ip_times vectors + per-CSV residuals
clear; clc;
fprintf('RUM estimation across all draws in results_choice_freq/\n');

% --- Algorithm parameters (same for all methods) ---
init_k        = 1;
max_iters     = 100;
tol           = 1e-4;
csv_budget_s  = 3600;   % wall-time cap per CSV (total across both methods)

% --- Paths -----------------------------------------------------------
script_dir  = fileparts(mfilename('fullpath'));
results_dir = fullfile(script_dir, 'results_choice_freq');
out_dir     = fullfile(script_dir, 'results_runs');
if ~exist(out_dir, 'dir'), mkdir(out_dir); end

% Mirror all console output to a text file so progress is visible while
% the script is still running and survives a Ctrl-C / kill.
log_path = fullfile(out_dir, 'run_log.txt');
if exist(log_path, 'file'), delete(log_path); end
diary(log_path);
diary on;
cleanup = onCleanup(@() diary('off'));

csv_files = dir(fullfile(results_dir, 'group*_*_store*.csv'));
csv_files = csv_files(~contains({csv_files.name}, '_upc_map'));
if isempty(csv_files)
    error(['No choice-freq CSVs in %s.\n' ...
           'Run E_build_all_choice_freq first.'], results_dir);
end
fprintf('Found %d CSV(s) to process.\n\n', numel(csv_files));

% --- Storage ---------------------------------------------------------
methods = {'CG_IP', 'CG_heurIP'};
nrows = numel(csv_files) * numel(methods);
csv_name_col  = strings(nrows, 1);
group_col     = zeros(nrows, 1);
category_col  = strings(nrows, 1);
store_col     = zeros(nrows, 1);
n_upcs_col    = zeros(nrows, 1);
nobs_col      = zeros(nrows, 1);
nmenus_col    = zeros(nrows, 1);
min_menu_col  = zeros(nrows, 1);
max_menu_col  = zeros(nrows, 1);
method_col    = strings(nrows, 1);
total_time    = nan(nrows, 1);
err_col       = nan(nrows, 1);
n_iters_col   = nan(nrows, 1);
n_ip_calls    = nan(nrows, 1);
ip_total      = nan(nrows, 1);
ip_mean       = nan(nrows, 1);
ip_max        = nan(nrows, 1);
status_col    = strings(nrows, 1);

details = struct('csv_name', {}, 'group', {}, 'category', {}, 'STORE', {}, ...
                 'method', {}, 'ip_times', {}, 'residual', {});

parse_set = @(s) cellfun(@str2double, regexp(s, '\d+', 'match')) + 1;
filename_re = 'group(\d+)_(.+)_store(\d+)\.csv';

row = 0;
for fi = 1:numel(csv_files)
    fname = csv_files(fi).name;
    tok   = regexp(fname, filename_re, 'tokens', 'once');
    if isempty(tok)
        warning('Skipping unrecognized filename: %s', fname);
        continue;
    end
    group_id  = str2double(tok{1});
    category  = tok{2};
    store_id  = str2double(tok{3});

    fprintf('\n========================================================\n');
    fprintf('[%d/%d] %s  (group %d, %s, STORE=%d)\n', ...
        fi, numel(csv_files), fname, group_id, category, store_id);
    fprintf('========================================================\n');

    % --- Load + parse this draw ------------------------------------------
    T = readtable(fullfile(results_dir, fname));

    choice_sets = cell(height(T), 1);
    for ii = 1:height(T)
        choice_sets{ii} = parse_set(T.choice_set{ii});
    end
    p_obs       = T.cp;
    chosen_alts = T.item + 1;
    n           = max(chosen_alts);

    [uniq_sets, ~, ~] = unique(string(T.choice_set), 'stable');
    choice_set_list = cell(numel(uniq_sets), 1);
    for ii = 1:numel(uniq_sets)
        choice_set_list{ii} = parse_set(char(uniq_sets(ii)));
    end

    nobs       = height(T);
    nmenus     = numel(choice_set_list);
    menu_sizes = cellfun(@numel, choice_set_list);
    min_menu   = min(menu_sizes);
    max_menu   = max(menu_sizes);
    fprintf('n = %d alternatives | %d observations | %d unique menus | menu sizes %d..%d\n', ...
        n, nobs, nmenus, min_menu, max_menu);

    % Sanity check: cp sums to 1 per menu
    [~, ~, gid] = unique(T.choice_set, 'stable');
    menu_sums   = accumarray(gid, T.cp);
    if max(abs(menu_sums - 1)) > 1e-6
        warning('  cp not summing to 1 per menu (max dev = %.3e).', ...
            max(abs(menu_sums - 1)));
    end

    % --- Run each method (sharing the per-CSV time budget) ---------------
    csv_start = tic;
    for mi = 1:numel(methods)
        row  = row + 1;
        meth = methods{mi};

        csv_name_col(row) = fname;
        group_col(row)    = group_id;
        category_col(row) = string(category);
        store_col(row)    = store_id;
        n_upcs_col(row)   = n;
        nobs_col(row)     = nobs;
        nmenus_col(row)   = nmenus;
        min_menu_col(row) = min_menu;
        max_menu_col(row) = max_menu;
        method_col(row)   = string(meth);

        remaining = csv_budget_s - toc(csv_start);
        if remaining <= 0
            status_col(row) = "BUDGET_EXHAUSTED";
            total_time(row) = 0;
            fprintf('\n  -- %s -- SKIPPED (CSV budget exhausted)\n', meth);
        else
            fprintf('\n  -- %s --  (budget left: %.1f s)\n', meth, remaining);
            t0 = tic;
            try
                switch meth
                    case 'CG_IP'
                        [res, residual] = B_solve_rum_CG(p_obs, n, init_k, max_iters, ...
                            choice_sets, 'IP', chosen_alts, choice_set_list, true, tol, remaining);
                    case 'CG_heurIP'
                        [res, residual] = B_solve_rum_CG(p_obs, n, init_k, max_iters, ...
                            choice_sets, 'bestinsertion_rand', chosen_alts, choice_set_list, true, tol, remaining);
                end
                total_time(row)  = toc(t0);
                err_col(row)     = res.QP.error;
                n_iters_col(row) = res.n_iters;
                n_ip_calls(row)  = numel(res.ip_times);
                if ~isempty(res.ip_times)
                    ip_total(row) = sum(res.ip_times);
                    ip_mean(row)  = mean(res.ip_times);
                    ip_max(row)   = max(res.ip_times);
                else
                    ip_total(row) = 0; ip_mean(row) = 0; ip_max(row) = 0;
                end
                if isfield(res, 'timed_out') && res.timed_out
                    status_col(row) = "TIMEOUT";
                else
                    status_col(row) = "ok";
                end

                details(end+1).csv_name = fname;       %#ok<SAGROW>
                details(end).group      = group_id;
                details(end).category   = category;
                details(end).STORE      = store_id;
                details(end).method     = meth;
                details(end).ip_times   = res.ip_times;
                details(end).residual   = residual;
                details(end).timed_out  = status_col(row) == "TIMEOUT";

                fprintf('  %-10s  status=%s  error=%.6e  total=%.2fs  iters=%d  IP_calls=%d  IP_total=%.2fs\n', ...
                    meth, status_col(row), err_col(row), total_time(row), n_iters_col(row), ...
                    n_ip_calls(row), ip_total(row));
            catch ME
                total_time(row) = toc(t0);
                status_col(row) = string(['ERR: ' ME.message]);
                fprintf(2, '  %s failed: %s\n', meth, ME.message);
            end
        end

        % --- Flush summary CSV after every method so a kill loses nothing ---
        summary_T = table(csv_name_col(1:row), group_col(1:row), category_col(1:row), ...
            store_col(1:row), n_upcs_col(1:row), nobs_col(1:row), nmenus_col(1:row), ...
            min_menu_col(1:row), max_menu_col(1:row), ...
            method_col(1:row), total_time(1:row), err_col(1:row), n_iters_col(1:row), ...
            n_ip_calls(1:row), ip_total(1:row), ip_mean(1:row), ip_max(1:row), status_col(1:row), ...
            'VariableNames', {'csv_name','complexity_group','category','STORE', ...
                              'n_upcs','n_obs','n_menus','min_menu_size','max_menu_size', ...
                              'method','total_time_s','error', ...
                              'n_iters','n_ip_calls','ip_total_s','ip_mean_s','ip_max_s','status'});
        writetable(summary_T, fullfile(out_dir, 'run_summary.csv'));
        diary off; diary on;   % force flush of run_log.txt
    end
end

save(fullfile(out_dir, 'run_details.mat'), 'details', '-v7.3');

fprintf('\n========================================================\n');
fprintf('Done. %d CSV(s) x %d method(s) = %d runs.\n', ...
    numel(csv_files), numel(methods), row);
fprintf('Summary: %s\n', fullfile(out_dir, 'run_summary.csv'));
fprintf('Details: %s\n', fullfile(out_dir, 'run_details.mat'));
fprintf('========================================================\n');
