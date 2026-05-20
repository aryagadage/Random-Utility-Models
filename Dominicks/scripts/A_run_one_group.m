function A_run_one_group(group_id)
% A_run_one_group  Run CG on the single Dominick's choice-frequency CSV
% whose filename starts with group{group_id:03d}_*. Designed for SLURM
% array jobs: each task processes exactly one group.
%
% Usage:
%   A_run_one_group(13)
%   matlab -batch "A_run_one_group($SLURM_ARRAY_TASK_ID)"
%
% Runs BOTH solvers (sharing a per-CSV wall-time budget), matching the
% schema of A_run_all_method_HG.m:
%   (1) CG_IP        — IP pricing every iteration
%   (2) CG_heurIP    — random-insertion heuristic pricing + IP at exit
%
% Per-group outputs (in results_runs/):
%   run_summary_g{NN}.csv   — one row per (csv, method)
%   run_log_g{NN}.txt       — diary of console output
%   run_details_g{NN}.mat   — full ip_times vectors + residuals + env info
%   run_iptimes_g{NN}.csv   — per-IP-call streaming CSV (run_tag,iter,...)
%
% Modeled on feasibility_analysis_binary_menus/A_run_all_method_HG.m so
% downstream summarisation scripts can be largely reused.

%% ---- Argument handling ------------------------------------------------
if nargin < 1 || isempty(group_id)
    error('Usage: A_run_one_group(group_id)');
end
if ischar(group_id) || isstring(group_id)
    group_id = str2double(group_id);
end
group_id = double(group_id);
assert(isfinite(group_id) && group_id == round(group_id) && group_id >= 1, ...
    'group_id must be a positive integer (got %g)', group_id);

fprintf('Dominick''s CG run — group %d\n', group_id);

script_dir = fileparts(mfilename('fullpath'));

%% ---- Gurobi MATLAB interface ------------------------------------------
gurobi_home = getenv('GUROBI_HOME');
if ~isempty(gurobi_home)
    addpath(fullfile(gurobi_home, 'matlab'));
end
if ~exist('gurobi', 'file')
    error('A_run_one_group:NoGurobi', ...
        ['gurobi() not on MATLAB path. On the cluster make sure ' ...
         '`module load Gurobi/...` ran before MATLAB started.']);
end

%% ---- Configuration ----------------------------------------------------
init_k       = 1;
max_iters    = Inf;
tol          = 1e-2;            % FW relative-distance-gap tolerance
csv_budget_s = 86400;           % 1-day wall-time cap shared across methods
methods      = {'CG_IP', 'CG_heurIP'};

%% ---- Paths ------------------------------------------------------------
results_dir = fullfile(script_dir, 'results_choice_freq');
out_dir     = fullfile(script_dir, 'results_runs');
if ~exist(out_dir, 'dir'), mkdir(out_dir); end

tag           = sprintf('g%03d', group_id);
summary_path  = fullfile(out_dir, ['run_summary_'  tag '.csv']);
log_path      = fullfile(out_dir, ['run_log_'      tag '.txt']);
details_path  = fullfile(out_dir, ['run_details_'  tag '.mat']);
iptimes_path  = fullfile(out_dir, ['run_iptimes_'  tag '.csv']);

% Initialise streaming per-IP-call CSV (one row per gurobi() call).
fid = fopen(iptimes_path, 'w');
fprintf(fid, 'run_tag,iter,ip_total_s,ip_solve_s,gurobi_runtime_s\n');
fclose(fid);

% Diary (per-group, no shared file)
if exist(log_path, 'file'), delete(log_path); end
diary(log_path);
diary on;
cleanup = onCleanup(@() diary('off'));

%% ---- Locate the CSV for this group ------------------------------------
csv_pattern = fullfile(results_dir, sprintf('group%03d_*_store*.csv', group_id));
matches     = dir(csv_pattern);
matches     = matches(~contains({matches.name}, '_upc_map'));
if isempty(matches)
    error('A_run_one_group:NoCSV', ...
          'No choice-freq CSV matching %s', csv_pattern);
end
if numel(matches) > 1
    fprintf('Multiple CSVs for group %d; using the first:\n', group_id);
    for kk = 1:numel(matches), fprintf('  %s\n', matches(kk).name); end
end
fname        = matches(1).name;
filename_re  = 'group(\d+)_(.+)_store(\d+)\.csv';
tok          = regexp(fname, filename_re, 'tokens', 'once');
category     = tok{2};
store_id     = str2double(tok{3});

fprintf('Found CSV: %s  (category=%s, store=%d)\n', fname, category, store_id);
fprintf('Outputs:\n  %s\n  %s\n  %s\n  %s\n\n', ...
    summary_path, log_path, details_path, iptimes_path);

%% ---- Environment info -------------------------------------------------
env_info = get_env_info();
fprintf('Host: %s\n', env_info.hostname);
fprintf('CPU:  %s\n', env_info.cpu_model);
fprintf('OS:   %s | logical cores: %d | MATLAB: %s\n\n', ...
    env_info.os, env_info.num_logical_cores, env_info.matlab_version);

%% ---- Load + parse this draw -------------------------------------------
parse_set = @(s) cellfun(@str2double, regexp(s, '\d+', 'match')) + 1;
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
fprintf(['n = %d alternatives | %d observations | %d unique menus | ' ...
         'menu sizes %d..%d\n'], n, nobs, nmenus, min_menu, max_menu);

% Sanity check: cp sums to 1 per menu
[~, ~, gid] = unique(T.choice_set, 'stable');
menu_sums   = accumarray(gid, T.cp);
if max(abs(menu_sums - 1)) > 1e-6
    warning('  cp not summing to 1 per menu (max dev = %.3e).', ...
        max(abs(menu_sums - 1)));
end

%% ---- Storage ----------------------------------------------------------
nrows = numel(methods);
group_col     = group_id * ones(nrows, 1);
category_col  = repmat(string(category), nrows, 1);
store_col     = store_id * ones(nrows, 1);
n_upcs_col    = n * ones(nrows, 1);
nobs_col      = nobs * ones(nrows, 1);
nmenus_col    = nmenus * ones(nrows, 1);
min_menu_col  = min_menu * ones(nrows, 1);
max_menu_col  = max_menu * ones(nrows, 1);
method_col    = strings(nrows, 1);
total_time    = nan(nrows, 1);
err_col       = nan(nrows, 1);
n_iters_col   = nan(nrows, 1);
n_ip_calls    = nan(nrows, 1);
ip_build_col  = nan(nrows, 1);
ip_total      = nan(nrows, 1);
ip_mean       = nan(nrows, 1);
ip_max        = nan(nrows, 1);
ip_solve_tot  = nan(nrows, 1);
ip_solve_mean = nan(nrows, 1);
ip_solve_max  = nan(nrows, 1);
ip_gur_tot    = nan(nrows, 1);
status_col    = strings(nrows, 1);
hostname_col  = repmat(string(env_info.hostname),  nrows, 1);
cpu_model_col = repmat(string(env_info.cpu_model), nrows, 1);
ncores_col    = env_info.num_logical_cores * ones(nrows, 1);

details = struct('group', {}, 'category', {}, 'STORE', {}, 'method', {}, ...
                 'ip_times', {}, 'ip_solve_times', {}, ...
                 'ip_gurobi_runtimes', {}, 'residual', {}, 'timed_out', {});

%% ---- Main loop --------------------------------------------------------
csv_start = tic;
for mi = 1:numel(methods)
    row  = mi;
    meth = methods{mi};
    method_col(row) = string(meth);
    run_tag         = sprintf('%s__%s', strrep(fname, '.csv', ''), meth);

    remaining = csv_budget_s - toc(csv_start);
    if remaining <= 0
        status_col(row) = "BUDGET_EXHAUSTED";
        total_time(row) = 0;
        fprintf('\n  -- %s -- SKIPPED (group budget exhausted)\n', meth);
        continue;
    end

    fprintf('\n========================================================\n');
    fprintf('  -- %s --  (budget left: %.1f s)\n', meth, remaining);
    fprintf('========================================================\n');

    t0 = tic;
    try
        switch meth
            case 'CG_IP'
                pricing_mode = 'IP';
            case 'CG_heurIP'
                pricing_mode = 'bestinsertion_rand';
            otherwise
                error('Unknown method %s', meth);
        end

        [res, residual] = B_solve_rum_CG(p_obs, n, init_k, max_iters, ...
            choice_sets, pricing_mode, chosen_alts, choice_set_list, ...
            true, tol, remaining, iptimes_path, run_tag);

        total_time(row)   = toc(t0);
        err_col(row)      = res.QP.error;
        n_iters_col(row)  = res.n_iters;
        n_ip_calls(row)   = numel(res.ip_times);
        if isfield(res, 'ip_build_time')
            ip_build_col(row) = res.ip_build_time;
        end
        if ~isempty(res.ip_times)
            ip_total(row)      = sum(res.ip_times);
            ip_mean(row)       = mean(res.ip_times);
            ip_max(row)        = max(res.ip_times);
            ip_solve_tot(row)  = sum(res.ip_solve_times);
            ip_solve_mean(row) = mean(res.ip_solve_times);
            ip_solve_max(row)  = max(res.ip_solve_times);
            ip_gur_tot(row)    = sum(res.ip_gurobi_runtimes);
        else
            ip_total(row)      = 0; ip_mean(row)       = 0; ip_max(row)       = 0;
            ip_solve_tot(row)  = 0; ip_solve_mean(row) = 0; ip_solve_max(row) = 0;
            ip_gur_tot(row)    = 0;
        end
        if isfield(res, 'timed_out') && res.timed_out
            status_col(row) = "TIMEOUT";
        else
            status_col(row) = "ok";
        end

        details(end+1).group        = group_id;       %#ok<AGROW>
        details(end).category       = category;
        details(end).STORE          = store_id;
        details(end).method         = meth;
        details(end).ip_times       = res.ip_times;
        details(end).ip_solve_times = res.ip_solve_times;
        details(end).ip_gurobi_runtimes = res.ip_gurobi_runtimes;
        details(end).residual       = residual;
        details(end).timed_out      = status_col(row) == "TIMEOUT";

        fprintf(['  %s  status=%s  error=%.6e  total=%.2fs  iters=%d  ' ...
                 'IP_calls=%d  solve_tot=%.2fs  gur_tot=%.2fs\n'], ...
            meth, status_col(row), err_col(row), total_time(row), ...
            n_iters_col(row), n_ip_calls(row), ip_solve_tot(row), ip_gur_tot(row));
    catch ME
        total_time(row) = toc(t0);
        status_col(row) = string(['ERR: ' ME.message]);
        fprintf(2, '  %s failed: %s\n', meth, ME.message);
    end

    % --- Flush summary CSV after every method ---------------------------
    summary_T = table(group_col(1:row), category_col(1:row), store_col(1:row), ...
        n_upcs_col(1:row), nobs_col(1:row), nmenus_col(1:row), ...
        min_menu_col(1:row), max_menu_col(1:row), method_col(1:row), ...
        total_time(1:row), err_col(1:row), n_iters_col(1:row), n_ip_calls(1:row), ...
        ip_build_col(1:row), ...
        ip_total(1:row), ip_mean(1:row), ip_max(1:row), ...
        ip_solve_tot(1:row), ip_solve_mean(1:row), ip_solve_max(1:row), ...
        ip_gur_tot(1:row), status_col(1:row), ...
        hostname_col(1:row), cpu_model_col(1:row), ncores_col(1:row), ...
        'VariableNames', {'group','category','STORE', ...
                          'n_upcs','n_obs','n_menus', ...
                          'min_menu_size','max_menu_size', ...
                          'method','total_time_s','error', ...
                          'n_iters','n_ip_calls', ...
                          'ip_build_s', ...
                          'ip_total_s','ip_mean_s','ip_max_s', ...
                          'ip_solve_total_s','ip_solve_mean_s','ip_solve_max_s', ...
                          'ip_gurobi_total_s','status', ...
                          'hostname','cpu_model','n_logical_cores'});
    writetable(summary_T, summary_path);
    diary off; diary on;   % force flush of run_log
end

save(details_path, 'details', 'env_info', '-v7.3');

fprintf('\n========================================================\n');
fprintf('Done. group %d (%s, store %d).\n', group_id, category, store_id);
fprintf('Summary: %s\n', summary_path);
fprintf('Details: %s\n', details_path);
fprintf('========================================================\n');

end


% =========================================================================
%  Environment info helper (hostname, CPU, cores, OS, MATLAB version)
% =========================================================================
function info = get_env_info()
info = struct();

[~, host] = system('hostname');
info.hostname = strtrim(host);

if ispc
    info.cpu_model = getenv('PROCESSOR_IDENTIFIER');
elseif ismac
    [~, cpu] = system('sysctl -n machdep.cpu.brand_string');
    info.cpu_model = strtrim(cpu);
else
    [~, cpu] = system('grep -m1 "model name" /proc/cpuinfo | sed "s/^[^:]*: //"');
    info.cpu_model = strtrim(cpu);
end
if isempty(info.cpu_model), info.cpu_model = 'unknown'; end

info.num_logical_cores = feature('numcores');
info.matlab_version    = version;
info.os                = computer;

info.slurm_job_id        = getenv('SLURM_JOB_ID');
info.slurm_array_task_id = getenv('SLURM_ARRAY_TASK_ID');
info.slurm_cpus_per_task = getenv('SLURM_CPUS_PER_TASK');
info.slurm_nodelist      = getenv('SLURM_NODELIST');
end
