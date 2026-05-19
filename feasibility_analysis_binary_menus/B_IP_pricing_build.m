function ip_model = B_IP_pricing_build(n, choice_set_list)
% B_IP_PRICING_BUILD  Build the price-independent IP pricing model.
%
%   ip_model = B_IP_pricing_build(n, choice_set_list)
%
% Builds the parts of the Gurobi model that depend only on (n,
% choice_set_list): variable bounds/types and the constraint matrix
% (pairwise rank equalities, triangle inequalities, zero constraints
% and probability-simplex constraints for menus of size >= 3).
%
% The caller (B_IP_pricing) sets ip_model.obj and ip_model.modelsense
% each iteration before calling gurobi().
%
% Inputs:
%   n               - number of alternatives.
%   choice_set_list - cell array of menus (row vectors).
%
% Output struct ip_model has fields:
%   vtype, lb, ub, A, rhs, sense

nm = size(choice_set_list, 1); %#ok<NASGU>  % used by commented Blocks 3/4

% Menu sizes (column vector)
menu_sizes = cellfun(@(s) size(s,2), choice_set_list);

% Histogram of menu sizes
num_choice_set_size = accumarray(menu_sizes(:), 1, [n 1]).';

% nvars = n^2 rank vars + per-menu-prob vars for menus of size >= 3
nvars = n^2 + sum(num_choice_set_size(3:end) .* (3:n));

ip_model.vtype = repmat('B', 1, nvars);
ip_model.lb    = zeros(1, nvars);
ip_model.ub    = ones(1, nvars);

index = @(i,j) n*(i-1) + j;   % rank var column index (1..n^2)

% Triplet (COO) storage
I_cells = {}; J_cells = {}; V_cells = {};
rhs_cells = {}; sense_cells = {};
row_offset = 0;

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
%   for i < j, k > i, k ~= j
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
    I_cells{end+1}     = [rows; rows; rows];                                  %#ok<AGROW>
    J_cells{end+1}     = [index(i_col, jj); index(jj, kk); index(kk, i_col)]; %#ok<AGROW>
    V_cells{end+1}     = ones(3*nrows, 1);                                    %#ok<AGROW>
    rhs_cells{end+1}   = 2 * ones(nrows, 1);                                  %#ok<AGROW>
    sense_cells{end+1} = repmat('<', nrows, 1);                               %#ok<AGROW>
    row_offset = row_offset + nrows;
end

% Blocks 3 and 4 are inactive for binary-only menus (every menu has s=2,
% so the s>=3 guards skip every iteration and nvars collapses to n^2).
% Kept here, commented out, so they can be re-enabled if menus of size
% >= 3 are reintroduced.
%
% %% Block 3: zero constraints   -x_{a,b} + p(D, a) <= 0   (menus of size >= 3)
% counter = n*n + 1;
% for j_cs = 1:nm
%     cs = choice_set_list{j_cs}(:);
%     s  = numel(cs);
%     if s < 3, continue; end
%     [k_loc, l_loc] = ndgrid(1:s, 1:s);
%     mask = k_loc ~= l_loc;
%     k_loc = k_loc(mask); l_loc = l_loc(mask);
%     nrows = numel(k_loc);
%     rows  = (1:nrows).' + row_offset;
%     rank_cols = index(cs(k_loc), cs(l_loc));
%     prob_cols = counter + k_loc - 1;
%     I_cells{end+1}     = [rows; rows];
%     J_cells{end+1}     = [rank_cols; prob_cols];
%     V_cells{end+1}     = [-ones(nrows, 1); ones(nrows, 1)];
%     rhs_cells{end+1}   = zeros(nrows, 1);
%     sense_cells{end+1} = repmat('<', nrows, 1);
%     row_offset = row_offset + nrows;
%     counter = counter + s;
% end
%
% %% Block 4: probability simplex   sum_k p(D, cs(k)) = 1   (menus of size >= 3)
% counter = n*n + 1;
% for j_cs = 1:nm
%     cs = choice_set_list{j_cs};
%     s  = size(cs, 2);
%     if s < 3, continue; end
%     cols = (counter:counter+s-1).';
%     row_offset = row_offset + 1;
%     I_cells{end+1}     = repmat(row_offset, s, 1);
%     J_cells{end+1}     = cols;
%     V_cells{end+1}     = ones(s, 1);
%     rhs_cells{end+1}   = 1;
%     sense_cells{end+1} = '=';
%     counter = counter + s;
% end

% Assemble
I_idx = vertcat(I_cells{:});
J_idx = vertcat(J_cells{:});
V_idx = vertcat(V_cells{:});
ip_model.A     = sparse(I_idx, J_idx, V_idx, row_offset, nvars);
ip_model.rhs   = vertcat(rhs_cells{:});
ip_model.sense = vertcat(sense_cells{:});

end
