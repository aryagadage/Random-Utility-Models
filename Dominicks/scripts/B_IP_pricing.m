function [optim_value,optimizer,V_sub,rankings]=B_IP_pricing(price,choice_sets,chosen_alts,choice_set_list,V_sub,rankings,lb,time_limit_s)
% Optional 8th arg time_limit_s: Gurobi TimeLimit in seconds (Inf = no cap).
if nargin < 8 || isempty(time_limit_s), time_limit_s = Inf; end


%Purpose:
% This function solves an integer programming pricing problem used in the column generation algorithm for RUM estimation.

%Given a current residual (dual prices) from the restricted master problem, the list of observed choice sets,
% it searches over all deterministic preference rankings and associated menu probabilities to maximize the reduced cost (the "price" objective).
% If the optimal objective value is small (below a tolerance in the calling code), this certifies that no improving column remains and the column generation procedure has converged.


% price: vector of "prices" (usually the residuals or dual-like quantities) coming from the restricted master problem. This is the coefficient vector in the objective that the IP maximizes.

% choice_sets: Cell array of choice sets (used for constructing the final optimizer column via C_gen_one_ranking).

%chosen_alts: Vector of actually chosen alternatives for each observation.

% choice_set_list: Cell array of all choice sets over which the probability variables are defined. This is used to count how many choice sets of each size exist,
% define probability variables for menus of size >= 3, build the linking and simplex constraints on these probabilities.

%tol: Tolerance passed in from the caller.

%V_sub: Current matrix of selected columns in the restricted master problem. Size: (#observations) x (#current_columns).

%rankings: Current list/matrix of selected deterministic rankings corresponding to the columns in V_sub.

n  = max(chosen_alts);        % number of alternatives
nm = size(choice_set_list,1); % number of menus

% Menu sizes (column vector)
menu_sizes = cellfun(@(s) size(s,2), choice_set_list);

% Histogram of menu sizes used for nvars
num_choice_set_size = accumarray(menu_sizes(:), 1, [n 1]).';

% Number of binary variables: n^2 rank vars + per-menu-prob vars for menus of size >= 3
nvars = n^2 + sum(num_choice_set_size(3:end) .* (3:n));
num_D = sum(num_choice_set_size); %#ok<NASGU>

model.vtype = repmat('B', 1, nvars);
model.lb    = zeros(1, nvars);
model.ub    = ones(1, nvars);

index = @(i,j) n*(i-1) + j;   % column index for rank var x_{ij} (1..n^2)

% --- Build A in triplet (COO) form, then sparse(...) at the end. The
% previous loop-with-vertcat pattern is O(rows^2); for n ~ 100+ the
% triangle-inequality block alone takes minutes.
I_cells     = {};
J_cells     = {};
V_cells     = {};
rhs_cells   = {};
sense_cells = {};
row_offset  = 0;

%% Block 1: pairwise equality   x_{ij} + x_{ji} = 1   for i < j
[ii, jj] = ndgrid(1:n, 1:n);
mask = ii < jj;
ii = ii(mask); jj = jj(mask);
n_eq = numel(ii);
rows = (1:n_eq).' + row_offset;
I_cells{end+1}     = [rows; rows];
J_cells{end+1}     = [index(ii, jj); index(jj, ii)];
V_cells{end+1}     = ones(2*n_eq, 1);
rhs_cells{end+1}   = ones(n_eq, 1);
sense_cells{end+1} = repmat('=', n_eq, 1);
row_offset = row_offset + n_eq;

%% Block 2: triangle inequality   x_{ij} + x_{jk} + x_{ki} <= 2
%   for i < j, k > i, k ~= j  (same iteration domain as the original loops)
% Loop over i to bound peak memory at O(n^2); ndgrid over (j, k) inside.
for i_val = 1:n
    rng_jk = (i_val+1):n;
    if numel(rng_jk) < 2, continue; end
    [jj, kk] = ndgrid(rng_jk, rng_jk);
    mask = jj ~= kk;
    jj = jj(mask); kk = kk(mask);
    nrows = numel(jj);
    if nrows == 0, continue; end
    rows = (1:nrows).' + row_offset;
    i_col = i_val * ones(nrows, 1);
    I_cells{end+1}     = [rows; rows; rows];                               %#ok<AGROW>
    J_cells{end+1}     = [index(i_col, jj); index(jj, kk); index(kk, i_col)]; %#ok<AGROW>
    V_cells{end+1}     = ones(3*nrows, 1);                                 %#ok<AGROW>
    rhs_cells{end+1}   = 2 * ones(nrows, 1);                               %#ok<AGROW>
    sense_cells{end+1} = repmat('<', nrows, 1);                            %#ok<AGROW>
    row_offset = row_offset + nrows;
end

%% Block 3: zero constraints     -x_{a,b} + p(D, a) <= 0
%   For each menu D of size s >= 3 and each (a, b) with a, b in D, a ~= b.
%   The probability vars for menu D occupy columns counter..counter+s-1
%   (one per element of D, in the listed order of choice_set_list{j}).
counter = n*n + 1;
for j_cs = 1:nm
    cs = choice_set_list{j_cs}(:);   % force column so cs(idx) preserves column shape
    s  = numel(cs);
    if s < 3
        % Binary menus contribute no zero constraints (and no prob vars).
        continue;
    end
    [k_loc, l_loc] = ndgrid(1:s, 1:s);
    mask = k_loc ~= l_loc;
    k_loc = k_loc(mask); l_loc = l_loc(mask);
    nrows = numel(k_loc);
    rows  = (1:nrows).' + row_offset;
    rank_cols = index(cs(k_loc), cs(l_loc));   % -1 columns (rank vars)
    prob_cols = counter + k_loc - 1;           % +1 columns (per-element prob var)
    I_cells{end+1}     = [rows; rows];                            %#ok<AGROW>
    J_cells{end+1}     = [rank_cols; prob_cols];                  %#ok<AGROW>
    V_cells{end+1}     = [-ones(nrows, 1); ones(nrows, 1)];       %#ok<AGROW>
    rhs_cells{end+1}   = zeros(nrows, 1);                         %#ok<AGROW>
    sense_cells{end+1} = repmat('<', nrows, 1);                   %#ok<AGROW>
    row_offset = row_offset + nrows;
    counter = counter + s;
end

%% Block 4: probability simplex   sum_k p(D, cs(k)) = 1   (per menu of size >= 3)
counter = n*n + 1;
for j_cs = 1:nm
    cs = choice_set_list{j_cs};
    s  = size(cs, 2);
    if s < 3, continue; end
    cols = (counter:counter+s-1).';
    row_offset = row_offset + 1;
    I_cells{end+1}     = repmat(row_offset, s, 1); %#ok<AGROW>
    J_cells{end+1}     = cols;                     %#ok<AGROW>
    V_cells{end+1}     = ones(s, 1);               %#ok<AGROW>
    rhs_cells{end+1}   = 1;                        %#ok<AGROW>
    sense_cells{end+1} = '=';                      %#ok<AGROW>
    counter = counter + s;
end

% --- Assemble sparse A and stacked rhs/sense ---
I_idx = vertcat(I_cells{:});
J_idx = vertcat(J_cells{:});
V_idx = vertcat(V_cells{:});
A     = sparse(I_idx, J_idx, V_idx, row_offset, nvars);
rhs   = vertcat(rhs_cells{:});
sense = vertcat(sense_cells{:});

model.A     = A;
model.rhs   = rhs;
model.sense = sense;

%% Objective (i.e. price)
obj = zeros(1, nvars);
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

params = [];
%if isempty(lb)==0
%    params.BestObjStop=lb;
%end
params.OutputFlag = 1;
if isfinite(time_limit_s)
    params.TimeLimit = time_limit_s;
end
model.obj         = obj;
model.modelsense  = 'max';
result            = gurobi(model, params);

%% Returning the rank for the best objective function
ranking   = zeros(1, n);
counter   = 1;
ranking_x = result.x(1:n^2);
for i = 1:n
    rank_temp = round(n - sum(ranking_x(counter:(counter+n-1))) + ranking_x(counter+i-1));
    ranking(rank_temp) = i;
    counter = counter + n;
end

% Optimal value and the implied deterministic ranking of the optimizer
optim_value = result.objbound;
[optimizer, ~, ~] = C_gen_one_ranking(price, choice_sets, chosen_alts, n, 'deterministic', ranking);

% Update choice probability matrix and the rankings
V_sub    = [V_sub, optimizer];
rankings = [rankings; ranking];

end
