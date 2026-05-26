function [p_obs, choice_sets, chosen_alts, choice_set_list, sampled_ids] = ...
    B_load_zhirkov_subsample(n_sub, rng_seed, csv_path)
% B_LOAD_ZHIRKOV_SUBSAMPLE  Load Zhirkov binary-choice frequencies and
% restrict to a random subsample of n_sub products (out of 32).
%
% Returns data in the same format as
% B_generate_fake_data_all_menus_outside, so it can be fed directly into
% B_solve_rum_CG. All binary menus over the sampled products are
% included (C(n_sub,2) menus, 2 rows per menu).
%
% INPUTS
%   n_sub     - integer in [2,32]; number of products to sample.
%   rng_seed  - seed for the subsample draw (NOT for the data).
%   csv_path  - (optional) path to cleaned_choice_frequency.csv.
%               Default: ../data/cleaned_choice_frequency.csv relative to
%               this file (i.e. Zhirkov/data/cleaned_choice_frequency.csv).
%
% OUTPUTS
%   p_obs           - column vec; choice prob per (menu, alt) row.
%   choice_sets     - cell array; choice_sets{i} = the menu (1..n_sub) for row i.
%   chosen_alts     - column vec; chosen_alts(i) = alternative for row i.
%   choice_set_list - cell array of unique menus (sorted by size then lex).
%   sampled_ids     - row vec of the original (1..32) product IDs that were
%                     mapped to local IDs 1..n_sub (sampled_ids(k) = original ID
%                     of local product k).

%% ---- Defaults / validation --------------------------------------------
if nargin < 3 || isempty(csv_path)
    here = fileparts(mfilename('fullpath'));   % .../Zhirkov/code
    csv_path = fullfile(here, '..', 'data', 'cleaned_choice_frequency.csv');
end
assert(exist(csv_path, 'file') == 2, ...
    'B_load_zhirkov_subsample:FileNotFound', ...
    'cleaned_choice_frequency.csv not found at: %s', csv_path);

assert(isscalar(n_sub) && n_sub == round(n_sub) && n_sub >= 2 && n_sub <= 32, ...
    'n_sub must be an integer in [2,32] (got %g)', n_sub);

%% ---- Read CSV ---------------------------------------------------------
T = readtable(csv_path);
% Columns: row index (first), choice_probability, set_alt1, set_al2, alt
required = {'choice_probability', 'set_alt1', 'set_al2', 'alt'};
for k = 1:numel(required)
    assert(any(strcmp(T.Properties.VariableNames, required{k})), ...
        'Column %s missing from %s', required{k}, csv_path);
end

prob = T.choice_probability;
s1   = T.set_alt1;     % low ID in pair
s2   = T.set_al2;      % high ID in pair (note original typo in column name)
alt  = T.alt;          % which of (s1,s2) this row's probability refers to

all_ids = unique([s1; s2]);
assert(numel(all_ids) == 32 && all(all_ids == (1:32)'), ...
    'Expected products labelled 1..32; got %d unique IDs.', numel(all_ids));

%% ---- Subsample n_sub products -----------------------------------------
rng(rng_seed);
sampled_ids = sort(randperm(32, n_sub));   % row vector, ascending

% Keep only rows whose pair is entirely inside the subsample
keep = ismember(s1, sampled_ids) & ismember(s2, sampled_ids);
prob = prob(keep);
s1   = s1(keep);
s2   = s2(keep);
alt  = alt(keep);

% Remap original IDs -> local IDs 1..n_sub
id_map               = zeros(32, 1);
id_map(sampled_ids)  = 1:n_sub;
s1_loc   = id_map(s1);
s2_loc   = id_map(s2);
alt_loc  = id_map(alt);

%% ---- Build outputs -----------------------------------------------------
% Each (s1_loc, s2_loc) is a binary menu; two rows per pair (one per alt).
% Group by pair, then expand back into per-(menu,alt) rows in canonical order.
[pairs_uniq, ~, ic] = unique([s1_loc, s2_loc], 'rows');   % rows are sorted lex
n_menus = size(pairs_uniq, 1);
assert(n_menus == nchoosek(n_sub, 2), ...
    'Expected C(%d,2)=%d binary menus in subsample, got %d.', ...
    n_sub, nchoosek(n_sub, 2), n_menus);

total_rows  = 2 * n_menus;
p_obs       = zeros(total_rows, 1);
choice_sets = cell(total_rows, 1);
chosen_alts = zeros(total_rows, 1);

row_idx = 0;
for m = 1:n_menus
    S = pairs_uniq(m, :);                  % [lo, hi], local IDs, already sorted
    rows_m = find(ic == m);                % the (up to 2) input rows for this pair
    % Build alt -> prob map for this menu
    p_lo = NaN; p_hi = NaN;
    for r = rows_m'
        if alt_loc(r) == S(1)
            p_lo = prob(r);
        elseif alt_loc(r) == S(2)
            p_hi = prob(r);
        end
    end
    assert(~isnan(p_lo) && ~isnan(p_hi), ...
        'Menu (%d,%d) missing one of its two alternative rows.', S(1), S(2));
    % Renormalize defensively (input already sums to 1 modulo float noise)
    Z = p_lo + p_hi;
    p_lo = p_lo / Z;  p_hi = p_hi / Z;

    row_idx = row_idx + 1;
    choice_sets{row_idx} = S;
    chosen_alts(row_idx) = S(1);
    p_obs(row_idx)       = p_lo;

    row_idx = row_idx + 1;
    choice_sets{row_idx} = S;
    chosen_alts(row_idx) = S(2);
    p_obs(row_idx)       = p_hi;
end

% choice_set_list: same canonical order (binary menus sorted lex by lo,hi)
choice_set_list = cell(n_menus, 1);
for m = 1:n_menus
    choice_set_list{m} = pairs_uniq(m, :);
end

%% ---- Summary -----------------------------------------------------------
fprintf(['Loaded Zhirkov subsample: %d / 32 products (seed=%d), ' ...
         '%d binary menus, %d rows.\n'], ...
        n_sub, rng_seed, n_menus, total_rows);
fprintf('Sampled original IDs: %s\n', mat2str(sampled_ids));

end
