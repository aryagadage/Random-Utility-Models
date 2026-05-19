function A_run_all_method_HG(n)
% A_run_all_method_HG  Run CG_IP on all-binary-menu synthetic data for
% one given n, sweeping n_seeds seeds.
%
% Usage:
%   A_run_all_method_HG(20)
%   matlab -batch "A_run_all_method_HG(20)"     % CLI / SLURM
%
% Per-(n) outputs (all in results_runs/):
%   run_summary_n{N}.csv   — one row per seed
%   run_log_n{N}.txt       — diary
%   run_details_n{N}.mat   — per-call ip_times / solve_times / runtimes / residuals
%
% Designed so 100 jobs covering n = 20..100 (one job per n) can run
% concurrently on a cluster without clobbering each other's files.

%% ---- Argument handling ------------------------------------------------
if nargin < 1 || isempty(n)
    error('Usage: A_run_all_method_HG(n)  where n >= 2');
end
if ischar(n) || isstring(n)
    n = str2double(n);   % allow `matlab -batch "A_run_all_method_HG('20')"`
end
n = double(n);
assert(isfinite(n) && n == round(n) && n >= 2, ...
    'n must be an integer >= 2 (got %g)', n);

fprintf('RUM estimation on all-binary-menu synthetic data (n = %d)\n', n);

script_dir = fileparts(mfilename('fullpath'));

%% ---- Gurobi MATLAB interface (cluster: set by `module load Gurobi/...`) -
gurobi_home = getenv('GUROBI_HOME');
if ~isempty(gurobi_home)
    addpath(fullfile(gurobi_home, 'matlab'));
end
if ~exist('gurobi', 'file')
    error('A_run_all_method_HG:NoGurobi', ...
        ['gurobi() not on MATLAB path. On the cluster make sure ' ...
         '`module load Gurobi/...` ran before MATLAB started.']);
end

%% ---- Configuration ----------------------------------------------------
n_seeds      = 5;

% Algorithm parameters
init_k       = 1;
max_iters    = Inf;
tol          = 1e-4;
csv_budget_s = 86400 * 7;   % per-seed wall-time cap (7 days)
pricing_mode = 'IP';

%% ---- Paths ------------------------------------------------------------
out_dir    = fullfile(script_dir, 'results_runs');
if ~exist(out_dir, 'dir'), mkdir(out_dir); end

tag          = sprintf('n%03d', n);
summary_path = fullfile(out_dir, ['run_summary_'  tag '.csv']);
log_path     = fullfile(out_dir, ['run_log_'      tag '.txt']);
details_path = fullfile(out_dir, ['run_details_'  tag '.mat']);
iptimes_path = fullfile(out_dir, ['run_iptimes_'  tag '.csv']);

% Initialise streaming per-IP-call CSV (one row per gurobi() call, flushed
% immediately so a SLURM kill leaves a durable record of IP cost).
fid = fopen(iptimes_path, 'w');
fprintf(fid, 'n,seed,iter,ip_total_s,ip_solve_s,gurobi_runtime_s\n');
fclose(fid);

% Diary (per-n, no shared file)
if exist(log_path, 'file'), delete(log_path); end
diary(log_path);
diary on;
cleanup = onCleanup(@() diary('off'));

fprintf('Running %d seeds. Outputs:\n  %s\n  %s\n  %s\n  %s\n\n', ...
    n_seeds, summary_path, log_path, details_path, iptimes_path);

%% ---- Environment info (constant across seeds) -------------------------
env_info = get_env_info();
fprintf('Host: %s\n', env_info.hostname);
fprintf('CPU:  %s\n', env_info.cpu_model);
fprintf('OS:   %s | logical cores: %d | MATLAB: %s\n\n', ...
    env_info.os, env_info.num_logical_cores, env_info.matlab_version);

%% ---- Storage ----------------------------------------------------------
nrows = n_seeds;
n_col         = zeros(nrows, 1);
seed_col      = zeros(nrows, 1);
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
ip_build_col  = nan(nrows, 1);
ip_solve_tot  = nan(nrows, 1);
ip_solve_mean = nan(nrows, 1);
ip_solve_max  = nan(nrows, 1);
ip_gur_tot    = nan(nrows, 1);
status_col    = strings(nrows, 1);
hostname_col  = strings(nrows, 1);
cpu_model_col = strings(nrows, 1);
ncores_col    = zeros(nrows, 1);

details = struct('n', {}, 'seed', {}, 'method', {}, ...
                 'ip_build_time', {}, 'ip_times', {}, ...
                 'ip_solve_times', {}, 'ip_gurobi_runtimes', {}, ...
                 'residual', {}, 'timed_out', {});

%% ---- Menu-size spec (all binary, deterministic given n) ---------------
menu_size_spec    = zeros(n, 1);
menu_size_spec(2) = 1.0;

%% ---- Main loop --------------------------------------------------------
for seed = 1:n_seeds
    row = seed;

    fprintf('\n========================================================\n');
    fprintf('[%d/%d] n = %d, seed = %d  (all binary menus)\n', ...
        seed, n_seeds, n, seed);
    fprintf('========================================================\n');

    % --- Generate this draw ---------------------------------------------
    [p_obs, choice_sets, chosen_alts, choice_set_list] = ...
        B_generate_fake_data_all_menus_outside(n, 'fractions', ...
                                               menu_size_spec, seed);

    nobs       = numel(p_obs);
    nmenus     = numel(choice_set_list);
    menu_sizes = cellfun(@numel, choice_set_list);
    min_menu   = min(menu_sizes);
    max_menu   = max(menu_sizes);
    fprintf('n = %d | %d observations | %d unique menus | menu sizes %d..%d\n', ...
        n, nobs, nmenus, min_menu, max_menu);

    % Sanity check: p_obs sums to 1 per menu
    menu_id  = zeros(nobs, 1);
    cur_menu = 0;
    last_set = [];
    for ii = 1:nobs
        if ~isequal(choice_sets{ii}, last_set)
            cur_menu = cur_menu + 1;
            last_set = choice_sets{ii};
        end
        menu_id(ii) = cur_menu;
    end
    menu_sums = accumarray(menu_id, p_obs);
    if max(abs(menu_sums - 1)) > 1e-6
        warning('  p_obs not summing to 1 per menu (max dev = %.3e).', ...
            max(abs(menu_sums - 1)));
    end

    % --- Bookkeeping -----------------------------------------------------
    n_col(row)         = n;
    seed_col(row)      = seed;
    nobs_col(row)      = nobs;
    nmenus_col(row)    = nmenus;
    min_menu_col(row)  = min_menu;
    max_menu_col(row)  = max_menu;
    method_col(row)    = "CG_IP";
    hostname_col(row)  = string(env_info.hostname);
    cpu_model_col(row) = string(env_info.cpu_model);
    ncores_col(row)    = env_info.num_logical_cores;

    % --- Run CG_IP -------------------------------------------------------
    fprintf('\n  -- CG_IP --  (budget: %.1f s)\n', csv_budget_s);
    t0 = tic;
    try
        [res, residual] = B_solve_rum_CG(p_obs, n, init_k, max_iters, ...
            choice_sets, pricing_mode, chosen_alts, choice_set_list, ...
            true, tol, csv_budget_s, iptimes_path, seed);

        total_time(row)   = toc(t0);
        err_col(row)      = res.QP.error;
        n_iters_col(row)  = res.n_iters;
        n_ip_calls(row)   = numel(res.ip_times);
        ip_build_col(row) = res.ip_build_time;
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

        details(end+1).n              = n; %#ok<AGROW>
        details(end).seed             = seed;
        details(end).method           = 'CG_IP';
        details(end).ip_build_time    = res.ip_build_time;
        details(end).ip_times         = res.ip_times;
        details(end).ip_solve_times   = res.ip_solve_times;
        details(end).ip_gurobi_runtimes = res.ip_gurobi_runtimes;
        details(end).residual         = residual;
        details(end).timed_out        = status_col(row) == "TIMEOUT";

        fprintf(['  CG_IP  status=%s  error=%.6e  total=%.2fs  iters=%d  ' ...
                 'IP_calls=%d  build=%.2fs  solve_tot=%.2fs  gur_tot=%.2fs\n'], ...
            status_col(row), err_col(row), total_time(row), n_iters_col(row), ...
            n_ip_calls(row), ip_build_col(row), ip_solve_tot(row), ip_gur_tot(row));
    catch ME
        total_time(row) = toc(t0);
        status_col(row) = string(['ERR: ' ME.message]);
        fprintf(2, '  CG_IP failed: %s\n', ME.message);
    end

    % --- Flush summary CSV after every seed -----------------------------
    summary_T = table(n_col(1:row), seed_col(1:row), nobs_col(1:row), ...
        nmenus_col(1:row), min_menu_col(1:row), max_menu_col(1:row), ...
        method_col(1:row), total_time(1:row), err_col(1:row), ...
        n_iters_col(1:row), n_ip_calls(1:row), ...
        ip_build_col(1:row), ...
        ip_total(1:row), ip_mean(1:row), ip_max(1:row), ...
        ip_solve_tot(1:row), ip_solve_mean(1:row), ip_solve_max(1:row), ...
        ip_gur_tot(1:row), status_col(1:row), ...
        hostname_col(1:row), cpu_model_col(1:row), ncores_col(1:row), ...
        'VariableNames', {'n','seed','n_obs','n_menus', ...
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
fprintf('Done. n = %d, %d seed(s).\n', n, n_seeds);
fprintf('Summary: %s\n', summary_path);
fprintf('Details: %s\n', details_path);
fprintf('========================================================\n');

end


% =========================================================================
%  Environment info helper (hostname, CPU, cores, OS, MATLAB version)
% =========================================================================
function info = get_env_info()
info = struct();

% Hostname
[~, host] = system('hostname');
info.hostname = strtrim(host);

% CPU model — platform-specific
if ispc
    info.cpu_model = getenv('PROCESSOR_IDENTIFIER');
elseif ismac
    [~, cpu] = system('sysctl -n machdep.cpu.brand_string');
    info.cpu_model = strtrim(cpu);
else  % Linux / HPC
    [~, cpu] = system('grep -m1 "model name" /proc/cpuinfo | sed "s/^[^:]*: //"');
    info.cpu_model = strtrim(cpu);
end
if isempty(info.cpu_model), info.cpu_model = 'unknown'; end

% Cores / runtime
info.num_logical_cores = feature('numcores');
info.matlab_version    = version;
info.os                = computer;

% SLURM context (empty strings when not running under SLURM)
info.slurm_job_id        = getenv('SLURM_JOB_ID');
info.slurm_array_task_id = getenv('SLURM_ARRAY_TASK_ID');
info.slurm_cpus_per_task = getenv('SLURM_CPUS_PER_TASK');
info.slurm_nodelist      = getenv('SLURM_NODELIST');
end

