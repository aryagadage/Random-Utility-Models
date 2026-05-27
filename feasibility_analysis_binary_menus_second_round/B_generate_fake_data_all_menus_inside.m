function [p_obs, choice_sets, chosen_alts, choice_set_list, true_utilities] = ...
    B_generate_fake_data_all_menus_inside(n, mode, menu_size_spec, rng_seed)
% B_GENERATE_FAKE_DATA_ALL_MENUS  Generate synthetic choice data for menus
% of arbitrary sizes from a multinomial logit model.
%
%   [p_obs, choice_sets, chosen_alts, choice_set_list, true_utilities] = ...
%       B_generate_fake_data_all_menus(n)
%
%   [p_obs, choice_sets, chosen_alts, choice_set_list, true_utilities] = ...
%       B_generate_fake_data_all_menus(n, mode, menu_size_spec, rng_seed)
%
%   INPUTS
%     n              - (required) integer >= 2, number of alternatives.
%
%     mode           - (optional) string: 'counts' or 'fractions'.
%                      'counts'    – menu_size_spec(s) is the exact number
%                                    of menus of size s to include.
%                                    Example: [0 10 5 3]
%                      'fractions' – menu_size_spec(s) is the fraction of
%                                    all C(n,s) possible menus of size s
%                                    to include.  counts(s) = round(frac(s)*C(n,s)).
%                                    Example: [0 0.5 0.3 0.2]
%                      Default: 'fractions'.
%
%     menu_size_spec - (optional) vector of length <= n.
%                      Default: 0.5 for sizes 2..n, 0 for size 1.
%
%     rng_seed       - (optional) seed for rng.  Default: 41.
%
%   n_menus is always derived from menu_size_spec and is not a separate
%   input argument.
%
%   OUTPUTS
%     p_obs          - column vector; choice probabilities, one row per
%                      (menu, alternative) pair.
%     choice_sets    - cell array same length as p_obs; choice_sets{i} is
%                      the row vector of alternatives in the menu for row i.
%     chosen_alts    - column vector same length as p_obs; chosen_alts(i)
%                      is the alternative associated with p_obs(i).
%     choice_set_list - column cell array of unique menus (sorted row vectors).
%     true_utilities  - column vector of length n; ground-truth utilities.
%
%   The output structure is compatible with B_generate_fake_data_binarytenary.

%% ---- Argument defaults ------------------------------------------------
if nargin < 4 || isempty(rng_seed)
    rng_seed = 41;
end
if nargin < 3 || isempty(menu_size_spec)
    % Default: 50% of possible menus for each size 2..n, none for size 1
    menu_size_spec = zeros(n, 1);
    menu_size_spec(2:n) = 0.5;
end
if nargin < 2 || isempty(mode)
    mode = 'fractions';
end

%% ---- Input validation --------------------------------------------------
assert(n >= 2, 'n must be >= 2.');
assert(ischar(mode) || isstring(mode), 'mode must be a string.');
mode = lower(char(mode));
assert(ismember(mode, {'counts', 'fractions'}), ...
    'mode must be ''counts'' or ''fractions''.');

% Make column vector and truncate / pad to length n
menu_size_spec = menu_size_spec(:);
if length(menu_size_spec) > n
    menu_size_spec = menu_size_spec(1:n);
elseif length(menu_size_spec) < n
    menu_size_spec(end+1:n) = 0;
end
assert(all(menu_size_spec >= 0), 'menu_size_spec entries must be non-negative.');

switch mode
    case 'counts'
        %--- COUNTS MODE: entries are exact integer counts ---
        assert(all(menu_size_spec == round(menu_size_spec)), ...
            'In counts mode all entries must be non-negative integers.');
        counts  = menu_size_spec;
        n_menus = sum(counts);
        fprintf('[counts mode] exact counts (total %d menus).\n', n_menus);

    case 'fractions'
        %--- FRACTION MODE: fraction(s) * C(n,s) menus of size s ---
        assert(all(menu_size_spec <= 1), ...
            'In fractions mode all entries must be in [0, 1].');
        counts = zeros(n, 1);
        for s = 1:n
            counts(s) = round(menu_size_spec(s) * nchoosek(n, s));
        end
        n_menus = sum(counts);
        fprintf('[fractions mode] counts(s) = round(frac(s) * C(n,s))  (total %d menus).\n', n_menus);
end

%% ---- Set RNG seed ------------------------------------------------------
rng(rng_seed);

%% ---- Generate ground-truth utilities -----------------------------------
true_utilities = randn(n, 1) * 2;
fprintf('Generating fake data for n=%d alternatives, %d menus...\n', n, n_menus);
fprintf('True utilities: ');
fprintf('%.3f ', true_utilities);
fprintf('\n\n');

%% ---- Draw menus and compute choice probabilities -----------------------
% Validate: requested count per size must not exceed available menus
for s = 1:n
    max_menus_s = nchoosek(n, s);
    if counts(s) > max_menus_s
        error('Requested %d menus of size %d but only %d distinct menus exist (C(%d,%d)).', ...
            counts(s), s, max_menus_s, n, s);
    end
end

% Build the vector of menu sizes (deterministic)
drawn_sizes = zeros(n_menus, 1);
idx = 0;
for s = 1:n
    drawn_sizes(idx+1 : idx+counts(s)) = s;
    idx = idx + counts(s);
end
% Menus are built in ascending size order (no shuffle) so that
% the output rows match the layout expected by B_IP_pricing.

% Pre-allocate cell array for menus (one per sampled menu)
menus_cell = cell(n_menus, 1);
menu_sizes = zeros(n_menus, 1);

% Sample menus without replacement (reject duplicates)
seen = containers.Map('KeyType', 'char', 'ValueType', 'logical');
for t = 1:n_menus
    s = drawn_sizes(t);
    if s == n
        % Only one possible menu of size n
        perm = 1:n;
    elseif s == 1
        % Only n possible singletons; pick deterministically from unseen
        perm = drawn_sizes(t);  % placeholder, overwritten below
        % Find an unseen singleton
        candidates = randperm(n);
        for c = 1:n
            perm = candidates(c);
            key_tmp = mat2str(perm);
            if ~seen.isKey(key_tmp)
                break;
            end
        end
    else
        while true
            perm = sort(randperm(n, s));
            key_tmp = mat2str(perm);
            if ~seen.isKey(key_tmp)
                break;
            end
        end
    end
    key = mat2str(perm);
    seen(key) = true;
    menus_cell{t} = perm;
    menu_sizes(t) = s;
end

% Sort menus lexicographically within each size group so that
% row order matches the choice_set_list produced by unique+sortrows.
max_s = max(menu_sizes);
idx = 0;
for s = 1:n
    group_idx = find(menu_sizes == s);
    if isempty(group_idx), continue; end
    M_group = zeros(length(group_idx), max_s);
    for g = 1:length(group_idx)
        S_g = menus_cell{group_idx(g)};
        M_group(g, 1:length(S_g)) = S_g;
    end
    [~, lex_order] = sortrows(M_group);
    menus_cell(group_idx) = menus_cell(group_idx(lex_order));
    menu_sizes(group_idx) = menu_sizes(group_idx(lex_order));
end

% Total number of rows = sum of menu sizes
total_rows = sum(menu_sizes);

% Pre-allocate outputs
p_obs       = zeros(total_rows, 1);
choice_sets = cell(total_rows, 1);
chosen_alts = zeros(total_rows, 1);

row_idx = 0;   % running row counter
for t = 1:n_menus
    S = menus_cell{t};
    s = menu_sizes(t);

    % Multinomial logit probabilities for this menu
    u_S = true_utilities(S);           % utilities of alternatives in S
    exp_u = exp(u_S - max(u_S));       % numerically stable softmax
    probs = exp_u / sum(exp_u);        % P(i|S) for each i in S

    % Fill s rows (one per alternative in the menu)
    for j = 1:s
        row_idx = row_idx + 1;
        choice_sets{row_idx} = S;
        chosen_alts(row_idx)  = S(j);
        p_obs(row_idx)        = probs(j);
    end
end

%% ---- Build choice_set_list (unique menus, sorted by size then lex) -----
max_s = max(menu_sizes);
M = zeros(n_menus, max_s);            % zero-padded matrix of menus
for t = 1:n_menus
    S = menus_cell{t};
    M(t, 1:length(S)) = S;
end
[M_unique, ~, ~] = unique(M, 'rows');
num_unique = size(M_unique, 1);

% Sort by menu size (ascending) so binary menus come first
menu_sizes_unique = sum(M_unique > 0, 2);
[~, size_sort_idx] = sortrows([menu_sizes_unique, M_unique]);
M_unique = M_unique(size_sort_idx, :);

choice_set_list = cell(num_unique, 1);
for k = 1:num_unique
    row_k = M_unique(k, :);
    choice_set_list{k} = row_k(row_k > 0);   % remove zero padding
end

%% ---- Summary -----------------------------------------------------------
fprintf('Generated %d rows from %d menus (%d unique).\n', ...
    total_rows, n_menus, num_unique);
fprintf('Menu size distribution actually drawn:\n');
for s = 1:n
    cnt = sum(drawn_sizes == s);
    if cnt > 0
        fprintf('  size %d: %d menus (%.1f%%)\n', s, cnt, 100*cnt/n_menus);
    end
end
fprintf('\n');

end
