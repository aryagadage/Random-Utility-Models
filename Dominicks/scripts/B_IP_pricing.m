function [optim_value,optimizer,V_sub,rankings,solve_wall,gurobi_runtime] = B_IP_pricing(price, choice_sets, chosen_alts, choice_set_list, V_sub, rankings, lb, time_limit_s, ip_model) %#ok<INUSL>
% B_IP_PRICING  Solve the IP pricing problem of the CG RUM solver
% (Dominick's version).
%
% Given a residual (`price`) from the RMP, search over deterministic
% rankings (+ menu probabilities for menus of size >= 3) to maximise the
% reduced cost.
%
% Required input ip_model:
%   Pre-built Gurobi model struct from B_IP_pricing_build(n,
%   choice_set_list) containing vtype, lb, ub, A, rhs, sense. The
%   objective vector is set here per call.
%
% Returns (extra outputs vs. earlier versions):
%   solve_wall      wall time of just the gurobi() call (s)
%   gurobi_runtime  Gurobi-internal solve time (= result.runtime, s)
%
% Optional 8th arg time_limit_s: Gurobi TimeLimit in seconds (Inf = no cap).
% 9th arg ip_model is required; build it once with
% B_IP_pricing_build(n, choice_set_list) in the caller.

if nargin < 8 || isempty(time_limit_s), time_limit_s = Inf; end
if nargin < 9 || isempty(ip_model)
    error('B_IP_pricing:MissingModel', ...
        ['ip_model is required. Build it once with ' ...
         'B_IP_pricing_build(n, choice_set_list).']);
end

n  = max(chosen_alts);
nm = size(choice_set_list, 1);
nvars = size(ip_model.A, 2);

%% Objective (depends on `price`; changes every call) -------------------
% `price` is grouped by menu in choice_set_list order: for each menu j the
% next |D_j| entries of `price` are that menu's observation residuals (in
% the same item-within-menu order used by the build, which is sorted).
% Walk through `price` and choice_set_list together; do not split into a
% "all size-2 first, then size-≥3 in bulk" pass — menus aren't sorted by
% size, so that splitting silently mis-pairs residuals with variables.
index = @(i, j) n*(i-1) + j;
obj   = zeros(1, nvars);
counter   = 1;          % running index into `price`
p_offset  = n*n + 1;    % running index into the p(D,a) section of `obj`
for j = 1:nm
    cs = choice_set_list{j};
    s  = size(cs, 2);
    if s == 2
        obj(index(cs(1), cs(2))) = price(counter);
        obj(index(cs(2), cs(1))) = price(counter + 1);
        counter = counter + 2;
    else
        obj(p_offset:p_offset+s-1) = price(counter:counter+s-1);
        counter  = counter  + s;
        p_offset = p_offset + s;
    end
end

ip_model.obj        = obj;
ip_model.modelsense = 'max';

%% Solve ----------------------------------------------------------------
params = [];
params.OutputFlag = 0;
if isfinite(time_limit_s)
    params.TimeLimit = time_limit_s;
end

% Pin Gurobi to the cores actually allocated to this SLURM task (else
% Gurobi defaults to all visible cores and may oversubscribe the cgroup).
cpt = getenv('SLURM_CPUS_PER_TASK');
if ~isempty(cpt)
    params.Threads = str2double(cpt);
end

% --- Frank-Wolfe gap-based early termination ---------------------------
% The master QP minimizes F(p) = ||p_obs - p||^2 over the RUM polytope.
% Let cp = max_{v in P} price'*v - lb be the reduced cost. By convexity
%   F* >= F_current - 2*cp,
% so the relative distance gap is <= tol_gap whenever
%   cp <= cp_thr := F_current * (1 - 1/(1+tol_gap)^2) / 2.
% Stopping Gurobi via BestBdStop = lb + cp_thr therefore certifies that
% the outer FW gap is below tol_gap, without solving the IP to optimality.
tol_gap   = 0.01;
F_current = price(:)' * price(:);
cp_thr    = 0.5 * F_current * (1 - 1/(1+tol_gap)^2);
if isfinite(lb) && cp_thr > 0
    params.BestBdStop = lb + cp_thr;
end
% -----------------------------------------------------------------------

t_solve        = tic;
result         = gurobi(ip_model, params);
solve_wall     = toc(t_solve);                 % wall time of gurobi() only
gurobi_runtime = result.runtime;               % Gurobi-internal solve time

% Optimal value (upper bound on the IP optimum, certified by Gurobi)
optim_value = result.objbound;

% If Gurobi early-stopped via BestBdStop before finding any feasible
% incumbent, result.x can be empty. In that case the FW gap is already
% certified small (objbound <= lb + cp_thr), so the outer loop will
% terminate and we simply return V_sub / rankings unchanged.
if isfield(result, 'x') && ~isempty(result.x)
    %% Recover ranking from rank-var solution ---------------------------
    ranking   = zeros(1, n);
    counter   = 1;
    ranking_x = result.x(1:n^2);
    for i = 1:n
        rank_temp = round(n - sum(ranking_x(counter:(counter+n-1))) + ranking_x(counter+i-1));
        ranking(rank_temp) = i;
        counter = counter + n;
    end

    [optimizer, ~, ~] = C_gen_one_ranking(price, choice_sets, chosen_alts, n, 'deterministic', ranking);

    V_sub    = [V_sub, optimizer];
    rankings = [rankings; ranking];
else
    fprintf(['B_IP_pricing: Gurobi stopped via BestBdStop before producing ' ...
             'an incumbent; FW gap is already certified <= tol_gap.\n']);
end

end
