function D_build_choice_freq_per_group(rng_seed, out_dir)
%D_BUILD_CHOICE_FREQ_PER_GROUP  Choice-frequency CSV per complexity group.
%
%   For each non-empty upc_menu complexity group (1..121), draw one
%   (STORE, category) row from store_summary_all_categories_with_complexity_group.csv
%   and write a flat choice-frequency CSV built from that store's movement data.
%   No top-K restriction — every UPC offered at the drawn store is used.
%
%   For each (STORE, WEEK) the "offered" rule is OK == 1 & PRICE > 0
%   (matching UPC_Store_exploratory_analysis.R). Each unit of MOVE counts as
%   one customer's choice, so the empirical choice probability of item i on
%   menu S is
%       sum_w MOVE_i(w)  /  sum_w sum_{j in S} MOVE_j(w)
%   aggregated across weeks where the offered set equals S. Singleton menus
%   are excluded.
%
%   Items at each store are indexed 0..n-1 (0-based, to match the schema in
%   Dean_Ravindran_Stoye.csv). The ordering is by # weeks offered (desc),
%   total MOVE (desc), UPC code (asc) — so index 0 is the "anchor" UPC at
%   that store.
%
%   Outputs per draw (in <out_dir>):
%     group<gg>_<category>_store<sss>.csv
%         choice_set       — sorted list of 0-based item indices, e.g. "[0,3,7]"
%         item             — 0-based item index (one row per (menu,item))
%         n_choices        — total MOVE on this menu across matching weeks
%         n_chosen         — MOVE of this item on this menu
%         cp               — n_chosen / n_choices
%         n_weeks_offered  — # weeks the menu appeared at this store
%
%     group<gg>_<category>_store<sss>_upc_map.csv
%         local_idx UPC n_weeks_offered tot_move
%
%   Inputs (both optional):
%     rng_seed — RNG seed. Default 42.
%     out_dir  — output dir. Default <scripts>/results_choice_freq.

    if nargin < 1 || isempty(rng_seed), rng_seed = 42; end

    script_dir = fileparts(mfilename('fullpath'));
    if nargin < 2 || isempty(out_dir)
        out_dir = fullfile(script_dir, 'results_choice_freq');
    end
    if ~exist(out_dir, 'dir'), mkdir(out_dir); end

    rng(rng_seed);

    % --- Paths -----------------------------------------------------------
    dom_dir   = fileparts(script_dir);
    csv_path  = fullfile(dom_dir, ...
        'store_summary_all_categories_with_complexity_group.csv');
    data_root = fullfile(dom_dir, 'data');
    if ~exist(csv_path, 'file')
        error('Summary CSV not found: %s', csv_path);
    end

    % --- Sample one (STORE, category) per non-empty complexity group -----
    T = readtable(csv_path);
    T.category = string(T.category);

    groups  = unique(T.upc_menu_complexity_group);
    groups  = groups(~isnan(groups));
    sampled = cell(numel(groups), 1);

    fprintf('Sampling 1 (STORE, category) per non-empty complexity group ...\n');
    for gi = 1:numel(groups)
        g    = groups(gi);
        idx  = find(T.upc_menu_complexity_group == g);
        pick = idx(randi(numel(idx)));
        sampled{gi} = struct( ...
            'group',    g, ...
            'category', char(T.category(pick)), ...
            'STORE',    T.STORE(pick), ...
            'n_upcs',   T.n_upcs(pick), ...
            'n_menus',  T.n_menus(pick));
    end

    % --- Group draws by category so each w*.csv is loaded at most once --
    cat_names = cellfun(@(s) s.category, sampled, 'UniformOutput', false);
    cats      = unique(cat_names);

    for ci = 1:numel(cats)
        cat_name = cats{ci};
        folder   = fullfile(data_root, cat_name);
        w_files  = dir(fullfile(folder, 'w*.csv'));
        if isempty(w_files)
            warning('Skipping %s (no w*.csv).', cat_name);
            continue;
        end
        w_path = fullfile(w_files(1).folder, w_files(1).name);

        sel           = find(strcmp(cat_names, cat_name));
        stores_needed = unique(cellfun(@(s) s.STORE, sampled(sel)));

        fprintf('\nLoading %s  (%d stores needed from %s) ...\n', ...
            cat_name, numel(stores_needed), w_files(1).name);

        opts = detectImportOptions(w_path);
        keep_vars = intersect(opts.VariableNames, ...
            {'STORE','UPC','WEEK','MOVE','PRICE','OK'});
        opts.SelectedVariableNames = keep_vars;
        W = readtable(w_path, opts);

        % Offered rule, matching UPC_Store_exploratory_analysis.R
        if all(ismember({'OK','PRICE'}, W.Properties.VariableNames))
            W = W(W.OK == 1 & W.PRICE > 0, :);
        elseif ismember('MOVE', W.Properties.VariableNames)
            W = W(W.MOVE > 0, :);
        end
        W = W(ismember(W.STORE, stores_needed), :);

        for jj = sel(:).'
            s = sampled{jj};
            try
                build_one(W(W.STORE == s.STORE, :), s, out_dir);
            catch ME
                warning('Group %d (%s, STORE=%d) failed: %s', ...
                    s.group, s.category, s.STORE, ME.message);
            end
        end
    end

    fprintf('\nDone. Outputs in: %s\n', out_dir);
end


function build_one(W, s, out_dir)
    if isempty(W)
        warning('  group %d: no rows for STORE=%d in %s — skipping.', ...
            s.group, s.STORE, s.category);
        return;
    end

    % --- All UPCs at this store, ordered for stable local indexing ------
    upc_stats = groupsummary(W, 'UPC', {'numunique','sum'}, {'WEEK','MOVE'});
    nw_col = find(startsWith(upc_stats.Properties.VariableNames, 'numunique'), 1);
    sm_col = find(startsWith(upc_stats.Properties.VariableNames, 'sum'),       1);
    upc_stats.Properties.VariableNames{nw_col} = 'n_weeks_offered';
    upc_stats.Properties.VariableNames{sm_col} = 'tot_move';
    upc_stats = sortrows(upc_stats, ...
        {'n_weeks_offered','tot_move','UPC'}, {'descend','descend','ascend'});

    n = height(upc_stats);
    if n < 2
        warning('  group %d: < 2 UPCs at STORE=%d (%s) — skipping.', ...
            s.group, s.STORE, s.category);
        return;
    end

    upcs = upc_stats.UPC;            % local idx (1-based in MATLAB) -> global UPC
    [~, W.local] = ismember(W.UPC, upcs);

    % --- Per-week projected menu (sorted local idx, 1-based) -------------
    [g_week, weeks] = findgroups(W.WEEK);
    week_items = splitapply(@(x) {sort(unique(x))}, W.local, g_week);
    keep       = cellfun(@numel, week_items) >= 2;
    weeks      = weeks(keep);
    week_items = week_items(keep);
    if isempty(weeks)
        warning('  group %d: no menu with >=2 items at STORE=%d (%s).', ...
            s.group, s.STORE, s.category);
        return;
    end

    % Menu signature in 0-based notation, e.g. "[0,3,7]"
    week_sigs = cellfun(@(it) sprintf('[%s]', strjoin(string(it - 1), ',')), ...
        week_items, 'UniformOutput', false);

    [u_sigs, ~, sig_grp] = unique(string(week_sigs));
    n_menus = numel(u_sigs);

    % --- Build flat (menu, item, prob) rows ------------------------------
    set_col      = strings(0, 1);
    item_col     = zeros(0, 1);
    nc_col       = zeros(0, 1);
    nchosen_col  = zeros(0, 1);
    cp_col       = zeros(0, 1);
    nweeks_col   = zeros(0, 1);

    for m_i = 1:n_menus
        sig   = u_sigs(m_i);
        wk    = weeks(sig_grp == m_i);
        items = week_items{find(sig_grp == m_i, 1)};      % 1-based local idx, sorted

        Wsub = W(ismember(W.WEEK, wk) & ismember(W.local, items), :);
        agg  = groupsummary(Wsub, 'local', 'sum', 'MOVE');
        sm   = find(startsWith(agg.Properties.VariableNames, 'sum'), 1);
        agg.Properties.VariableNames{sm} = 'sum_move';

        [~, ord] = ismember(items, agg.local);             % map items -> agg rows
        moves    = zeros(numel(items), 1);
        moves(ord > 0) = agg.sum_move(ord(ord > 0));

        total = sum(moves);
        if total <= 0, continue; end

        K = numel(items);
        set_col     = [set_col;     repmat(sig, K, 1)];        %#ok<AGROW>
        item_col    = [item_col;    items(:) - 1];             %#ok<AGROW>  0-based
        nc_col      = [nc_col;      repmat(total, K, 1)];      %#ok<AGROW>
        nchosen_col = [nchosen_col; moves];                    %#ok<AGROW>
        cp_col      = [cp_col;      moves ./ total];           %#ok<AGROW>
        nweeks_col  = [nweeks_col;  repmat(numel(wk), K, 1)];  %#ok<AGROW>
    end

    if isempty(set_col)
        warning('  group %d: no usable menus at STORE=%d (%s).', ...
            s.group, s.STORE, s.category);
        return;
    end

    out_T = table(set_col, item_col, nc_col, nchosen_col, cp_col, nweeks_col, ...
        'VariableNames', ...
        {'choice_set','item','n_choices','n_chosen','cp','n_weeks_offered'});

    safe_cat = regexprep(s.category, '[^A-Za-z0-9]', '');
    base = sprintf('group%03d_%s_store%03d', s.group, safe_cat, s.STORE);
    writetable(out_T, fullfile(out_dir, [base '.csv']));

    % Sidecar: local_idx -> UPC mapping
    map_T = table((0:n-1).', upcs, upc_stats.n_weeks_offered, upc_stats.tot_move, ...
        'VariableNames', {'local_idx','UPC','n_weeks_offered','tot_move'});
    writetable(map_T, fullfile(out_dir, [base '_upc_map.csv']));

    fprintf('  group %3d  %-22s STORE=%3d  n_upcs=%d  menus=%d  rows=%d\n', ...
        s.group, s.category, s.STORE, n, n_menus, height(out_T));
end
