function [optim_value,optimizer,V_sub,rankings,solve_wall,gurobi_runtime] = B_IP_pricing(price, choice_sets, chosen_alts, choice_set_list, V_sub, rankings, lb, time_limit_s, ip_model) %#ok<INUSL>
% B_IP_PRICING  Solve the IP pricing problem of the CG RUM solver.
%
% Given a residual (`price`) from the RMP, search over deterministic
% rankings (+ menu probabilities for menus of size >= 3) to maximise
% reduced cost.
%
% Required input ip_model:
%   Pre-built Gurobi model struct from B_IP_pricing_build(n,
%   choice_set_list) containing vtype, lb, ub, A, rhs, sense.
%   The objective vector is set here per call.
%
% Optional time_limit_s (8th arg): Gurobi TimeLimit in seconds.
% `lb` is currently unused (reserved for BestObjStop).

if nargin < 8 || isempty(time_limit_s), time_limit_s = Inf; end
if nargin < 9 || isempty(ip_model)
    error('B_IP_pricing:MissingModel', ...
        'ip_model is required. Build it once with B_IP_pricing_build(n, choice_set_list).');
end

n  = max(chosen_alts);
nm = size(choice_set_list, 1);
nvars = size(ip_model.A, 2);

%% Objective (depends on `price`; changes every call) -------------------
index = @(i,j) n*(i-1) + j;
obj   = zeros(1, nvars);
counter = 1;
for j = 1:nm
    cs = choice_set_list{j};
    if size(cs, 2) == 2
        obj(index(cs(1), cs(2))) = price(counter);
        obj(index(cs(2), cs(1))) = price(counter + 1);
        counter = counter + 2;
    end
end
obj((n*n+1):end) = price(counter:end);

ip_model.obj        = obj;
ip_model.modelsense = 'max';

%% Solve ----------------------------------------------------------------
params = [];
params.OutputFlag = 0;   % suppress per-call Gurobi log on cluster
if isfinite(time_limit_s)
    params.TimeLimit = time_limit_s;
end

% Pin Gurobi to the cores actually allocated to this SLURM task (else
% Gurobi defaults to all visible cores and may oversubscribe the cgroup).
cpt = getenv('SLURM_CPUS_PER_TASK');
if ~isempty(cpt)
    params.Threads = str2double(cpt);
end
t_solve = tic;
result         = gurobi(ip_model, params);
solve_wall     = toc(t_solve);          % wall time of gurobi() only (incl. marshaling)
gurobi_runtime = result.runtime;        % Gurobi-internal solve time (machine-portable)

%% Recover ranking from rank-var solution -------------------------------
ranking   = zeros(1, n);
counter   = 1;
ranking_x = result.x(1:n^2);
for i = 1:n
    rank_temp = round(n - sum(ranking_x(counter:(counter+n-1))) + ranking_x(counter+i-1));
    ranking(rank_temp) = i;
    counter = counter + n;
end

optim_value = result.objbound;
[optimizer, ~, ~] = C_gen_one_ranking(price, choice_sets, chosen_alts, n, 'deterministic', ranking);

V_sub    = [V_sub, optimizer];
rankings = [rankings; ranking];

end
