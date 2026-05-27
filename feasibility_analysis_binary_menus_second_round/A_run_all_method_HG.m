function A_run_all_method_HG(n)
% A_run_all_method_HG  For each seed, generate an OUTSIDE and an INSIDE
% endpoint on the same all-binary-menus skeleton, sweep 11 convex
% combinations w*p_in + (1-w)*p_out (w = 0, 0.1, ..., 1.0), and project
% each combined point with TWO methods:
%   1. CG_IP                 — exact IP pricing every iteration.
%   2. CG_heur_IPverify      — randomized best-insertion pricing
%                              ('bestinsertion_rand'), with IP verification
%                              at termination (same min(best_score,fw_gap)
%                              < tol rule).
%
% Both methods share tolerance, time budget, and termination logic.
%
% Usage:
%   A_run_all_method_HG(20)
%   matlab -batch "A_run_all_method_HG(20)"     % CLI / SLURM
%
% Per-(n) outputs (all in results_runs/):
%   run_summary_n{N}.csv   — one row per (seed, weight, method)
%   run_log_n{N}.txt       — diary
%   run_details_n{N}.mat   — per-call ip_times / solve_times / runtimes / residuals
%   run_iptimes_n{N}.csv   — streamed per-IP-call rows (with seed/weight/method)

%% ---- Argument handling ------------------------------------------------
if nargin < 1 || isempty(n)
    error('Usage: A_run_all_method_HG(n)  where n >= 2');
end
if ischar(n) || isstring(n)
    n = str2double(n);
end
n = double(n);
assert(isfinite(n) && n == round(n) && n >= 2, ...
    'n must be an integer >= 2 (got %g)', n);

fprintf('RUM estimation on all-binary-menu data, inside<->outside sweep (n = %d)\n', n);

script_dir = fileparts(mfilename('fullpath'));

%% ---- Gurobi MATLAB interface -----------------------------------------
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
weights      = 1.0:-0.1:0.0;               % weight on INSIDE point (descending: easy -> hard)
n_weights    = numel(weights);

methods      = {'CG_IP', 'CG_heur_IPverify'};
n_methods    = numel(methods);

init_k       = 1;
max_iters    = Inf;
tol_level    = 1e-3;                       % absolute  gap tolerance (UB-LB)
tol_relative = 1e-2;                       % relative  gap tolerance ((UB-LB)/LB)
csv_budget_s = 86400 * 7;                  % per-call wall-time cap (7 days)

%% ---- Paths ------------------------------------------------------------
out_dir = fullfile(script_dir, 'results_runs');
if ~exist(out_dir, 'dir'), mkdir(out_dir); end

tag          = sprintf('n%03d', n);
summary_path = fullfile(out_dir, ['run_summary_'  tag '.csv']);
log_path     = fullfile(out_dir, ['run_log_'      tag '.txt']);
details_path = fullfile(out_dir, ['run_details_'  tag '.mat']);
iptimes_path = fullfile(out_dir, ['run_iptimes_'  tag '.csv']);

% Streaming per-IP-call CSV (now keyed by seed/weight/method)
fid = fopen(iptimes_path, 'w');
fprintf(fid, 'n,seed,weight,method,iter,ip_total_s,ip_solve_s,gurobi_runtime_s\n');
fclose(fid);

if exist(log_path, 'file'), delete(log_path); end
diary(log_path);
diary on;
cleanup = onCleanup(@() diary('off')); %#ok<NASGU>

fprintf('Running %d seeds x %d weights x %d methods = %d projections.\n', ...
    n_seeds, n_weights, n_methods, n_seeds*n_weights*n_methods);
fprintf('Outputs:\n  %s\n  %s\n  %s\n  %s\n\n', ...
    summary_path, log_path, details_path, iptimes_path);

%% ---- Environment info -------------------------------------------------
env_info = get_env_info();
fprintf('Host: %s\n', env_info.hostname);
fprintf('CPU:  %s\n', env_info.cpu_model);
fprintf('OS:   %s | logical cores: %d | MATLAB: %s\n\n', ...
    env_info.os, env_info.num_logical_cores, env_info.matlab_version);

%% ---- Storage (one row per (seed,weight,method)) ----------------------
nrows         = n_seeds * n_weights * n_methods;
n_col         = zeros(nrows, 1);
seed_col      = zeros(nrows, 1);
weight_col    = nan(nrows, 1);
method_col    = strings(nrows, 1);
nobs_col      = zeros(nrows, 1);
nmenus_col    = zeros(nrows, 1);
min_menu_col  = zeros(nrows, 1);
max_menu_col  = zeros(nrows, 1);
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
hostname_col  = strings(nrows, 1);
cpu_model_col = strings(nrows, 1);
ncores_col    = zeros(nrows, 1);

details = struct('n', {}, 'seed', {}, 'weight', {}, 'method', {}, ...
                 'ip_build_time', {}, 'ip_times', {}, ...
                 'ip_solve_times', {}, 'ip_gurobi_runtimes', {}, ...
                 'residual', {}, 'timed_out', {});

%% ---- Menu-size spec (all binary) -------------------------------------
menu_size_spec    = zeros(n, 1);
menu_size_spec(2) = 1.0;

%% ---- Main loop --------------------------------------------------------
row = 0;
for seed = 1:n_seeds
    fprintf('\n########################################################\n');
    fprintf('# Seed %d / %d\n', seed, n_seeds);
    fprintf('########################################################\n');

    % --- Generate OUTSIDE endpoint (Dirichlet on simplex) ---------------
    [p_out, choice_sets, chosen_alts, choice_set_list] = ...
        B_generate_fake_data_all_menus_outside(n, 'fractions', ...
                                               menu_size_spec, seed);

    % --- Generate INSIDE endpoint (MNL) on the SAME menu skeleton -------
    [p_in, choice_sets_in, chosen_alts_in, choice_set_list_in] = ...
        B_generate_fake_data_all_menus_inside(n, 'fractions', ...
                                              menu_size_spec, seed);

    % Both generators sample size-2 menus with menu_size_spec(2)=1.0,
    % so after lex-sort the menu skeleton is identical. Sanity-check:
    assert(isequal(choice_sets, choice_sets_in), ...
        'inside/outside choice_sets do not match (seed=%d)', seed);
    assert(isequal(chosen_alts, chosen_alts_in), ...
        'inside/outside chosen_alts do not match (seed=%d)', seed);
    assert(isequal(choice_set_list, choice_set_list_in), ...
        'inside/outside choice_set_list do not match (seed=%d)', seed);

    nobs       = numel(p_out);
    nmenus     = numel(choice_set_list);
    menu_sizes = cellfun(@numel, choice_set_list);
    min_menu   = min(menu_sizes);
    max_menu   = max(menu_sizes);
    fprintf('n = %d | %d obs | %d unique menus | menu sizes %d..%d\n', ...
        n, nobs, nmenus, min_menu, max_menu);

    % Build menu_id once per seed for the per-menu prob-sum sanity check
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

    % Endpoints themselves should each sum to 1 per menu
    for ep_name = {'p_in','p_out'}
        if strcmp(ep_name{1},'p_in'), v = p_in; else, v = p_out; end
        s = accumarray(menu_id, v);
        if max(abs(s - 1)) > 1e-6
            warning('  %s not summing to 1 per menu (max dev = %.3e).', ...
                ep_name{1}, max(abs(s - 1)));
        end
    end

    % --- Sweep weights ---------------------------------------------------
    for wi = 1:n_weights
        w = weights(wi);
        p_obs = w * p_in + (1 - w) * p_out;

        fprintf('\n--------------------------------------------------------\n');
        fprintf('seed=%d  w(inside)=%.1f\n', seed, w);
        fprintf('--------------------------------------------------------\n');

        for mi = 1:n_methods
            method = methods{mi};
            row = row + 1;

            fprintf('\n  -- %s --  (budget: %.1f s)\n', method, csv_budget_s);

            % All methods use IP=true so termination requires an IP
            % certificate (either IP-as-pricer in 'IP' mode, or IP
            % verification when the heuristic claims convergence).
            switch method
                case 'CG_IP'
                    pricing_mode = 'IP';
                case 'CG_heur_IPverify'
                    pricing_mode = 'bestinsertion_rand';
                otherwise
                    error('unknown method %s', method);
            end

            % Bookkeeping that's the same regardless of how the run goes
            n_col(row)         = n;
            seed_col(row)      = seed;
            weight_col(row)    = w;
            method_col(row)    = string(method);
            nobs_col(row)      = nobs;
            nmenus_col(row)    = nmenus;
            min_menu_col(row)  = min_menu;
            max_menu_col(row)  = max_menu;
            hostname_col(row)  = string(env_info.hostname);
            cpu_model_col(row) = string(env_info.cpu_model);
            ncores_col(row)    = env_info.num_logical_cores;

            % Compose a method/weight tag for the streaming iptimes file
            tag_iptimes = sprintf('s%02d_w%03d_%s', ...
                seed, round(w*100), method);

            t0 = tic;
            try
                [res, residual] = B_solve_rum_CG(p_obs, n, init_k, max_iters, ...
                    choice_sets, pricing_mode, chosen_alts, choice_set_list, ...
                    true, tol_level, tol_relative, csv_budget_s, ...
                    '', ...   % don't double-stream — wrapper writes its own row
                    seed);

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

                % Append per-IP-call rows to the streaming CSV (after the
                % run finishes; durable because solver writes nothing to it).
                append_iptimes_block(iptimes_path, n, seed, w, method, ...
                    res.ip_times, res.ip_solve_times, res.ip_gurobi_runtimes);

                details(end+1).n              = n; %#ok<AGROW>
                details(end).seed             = seed;
                details(end).weight           = w;
                details(end).method           = method;
                details(end).ip_build_time    = res.ip_build_time;
                details(end).ip_times         = res.ip_times;
                details(end).ip_solve_times   = res.ip_solve_times;
                details(end).ip_gurobi_runtimes = res.ip_gurobi_runtimes;
                details(end).residual         = residual;
                details(end).timed_out        = status_col(row) == "TIMEOUT";

                fprintf(['  %s  status=%s  error=%.6e  total=%.2fs  iters=%d  ' ...
                         'IP_calls=%d  build=%.2fs  solve_tot=%.2fs  gur_tot=%.2fs\n'], ...
                    method, status_col(row), err_col(row), total_time(row), ...
                    n_iters_col(row), n_ip_calls(row), ip_build_col(row), ...
                    ip_solve_tot(row), ip_gur_tot(row));
            catch ME
                total_time(row) = toc(t0);
                status_col(row) = string(['ERR: ' ME.message]);
                fprintf(2, '  %s failed: %s\n', method, ME.message);
            end

            % Flush summary CSV after every projection
            summary_T = make_summary_table(row, ...
                n_col, seed_col, weight_col, method_col, nobs_col, ...
                nmenus_col, min_menu_col, max_menu_col, total_time, err_col, ...
                n_iters_col, n_ip_calls, ip_build_col, ip_total, ip_mean, ip_max, ...
                ip_solve_tot, ip_solve_mean, ip_solve_max, ip_gur_tot, status_col, ...
                hostname_col, cpu_model_col, ncores_col);
            writetable(summary_T, summary_path);
            diary off; diary on;
        end
    end
end

save(details_path, 'details', 'env_info', '-v7.3');

fprintf('\n========================================================\n');
fprintf('Done. n = %d, %d seed(s) x %d weights x %d methods.\n', ...
    n, n_seeds, n_weights, n_methods);
fprintf('Summary: %s\n', summary_path);
fprintf('Details: %s\n', details_path);
fprintf('========================================================\n');

end


% =========================================================================
%  Summary-table builder (kept here to avoid repeating the column list)
% =========================================================================
function T = make_summary_table(row, ...
    n_col, seed_col, weight_col, method_col, nobs_col, ...
    nmenus_col, min_menu_col, max_menu_col, total_time, err_col, ...
    n_iters_col, n_ip_calls, ip_build_col, ip_total, ip_mean, ip_max, ...
    ip_solve_tot, ip_solve_mean, ip_solve_max, ip_gur_tot, status_col, ...
    hostname_col, cpu_model_col, ncores_col)
T = table(n_col(1:row), seed_col(1:row), weight_col(1:row), method_col(1:row), ...
    nobs_col(1:row), nmenus_col(1:row), min_menu_col(1:row), max_menu_col(1:row), ...
    total_time(1:row), err_col(1:row), n_iters_col(1:row), n_ip_calls(1:row), ...
    ip_build_col(1:row), ip_total(1:row), ip_mean(1:row), ip_max(1:row), ...
    ip_solve_tot(1:row), ip_solve_mean(1:row), ip_solve_max(1:row), ...
    ip_gur_tot(1:row), status_col(1:row), ...
    hostname_col(1:row), cpu_model_col(1:row), ncores_col(1:row), ...
    'VariableNames', {'n','seed','weight_inside','method', ...
                      'n_obs','n_menus','min_menu_size','max_menu_size', ...
                      'total_time_s','error','n_iters','n_ip_calls', ...
                      'ip_build_s', ...
                      'ip_total_s','ip_mean_s','ip_max_s', ...
                      'ip_solve_total_s','ip_solve_mean_s','ip_solve_max_s', ...
                      'ip_gurobi_total_s','status', ...
                      'hostname','cpu_model','n_logical_cores'});
end


% =========================================================================
%  Append per-IP-call rows for one (seed,weight,method) block
% =========================================================================
function append_iptimes_block(iptimes_path, n, seed, w, method, ...
                              ip_times, ip_solve_times, ip_gurobi_runtimes)
if isempty(iptimes_path) || isempty(ip_times), return; end
fid = fopen(iptimes_path, 'a');
if fid < 0
    warning('append_iptimes_block: could not open %s for append', iptimes_path);
    return;
end
for k = 1:numel(ip_times)
    fprintf(fid, '%d,%d,%.2f,%s,%d,%.6f,%.6f,%.6f\n', ...
        n, seed, w, method, k, ip_times(k), ip_solve_times(k), ip_gurobi_runtimes(k));
end
fclose(fid);
end


% =========================================================================
%  Environment info helper
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
