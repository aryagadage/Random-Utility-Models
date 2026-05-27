function D_build_choice_freq_per_group(rng_seed, out_dir)
%D_BUILD_CHOICE_FREQ_PER_GROUP  Choice-frequency CSV per complexity group.
%
%   For each non-empty upc_menu complexity group (1..121), draw one
%   (STORE, category) row from store_summary_all_categories_with_complexity_group.csv
%   and write a flat choice-frequency CSV built from that store's movement data.
%   No top-K restriction — every effective UPC offered at the drawn store is
%   used.
%
%   For each (STORE, WEEK) the "offered" rule is OK == 1 (matching
%   UPC_Store_exploratory_analysis.R / compute_min_menu_size.R). Each unit of
%   MOVE counts as one customer's choice, so the empirical choice probability
%   of effective item i on menu S is
%       sum_w sum_{u in class(i)} MOVE_u(w)
%       ----------------------------------------------
%       sum_w sum_{j in S} sum_{u in class(j)} MOVE_u(w)
%   aggregated across weeks where the offered set equals S. Singleton menus
%   (in effective units) are excluded.
%
%   "Effective UPC" = equivalence class of raw UPCs that are offered in
%   exactly the same set of weeks at this store; the canonical representative
%   is the smallest UPC in the class. Two raw UPCs that always co-offer at
%   the same store carry no choice-set variation to identify their preferences
%   separately, so we sum their MOVE into a single effective UPC.
%
%   Effective items at each store are indexed 0..n-1 (0-based, to match the
%   schema in Dean_Ravindran_Stoye.csv). The ordering is by # weeks offered
%   (desc), total MOVE summed over the class (desc), canonical UPC code (asc).
%
%   Outputs per draw (in <out_dir>):
%     group<gg>_<category>_store<sss>.csv
%         choice_set       — sorted list of 0-based effective-item indices, e.g. "[0,3,7]"
%         item             — 0-based effective-item index (one row per (menu,item))
%         n_choices        — total MOVE on this menu across matching weeks
%         n_chosen         — MOVE summed over the class on this menu
%         cp               — n_chosen / n_choices
%         n_weeks_offered  — # weeks the menu appeared at this store
%
%     group<gg>_<category>_store<sss>_upc_map.csv
%         local_idx canonical_UPC n_raw_upcs constituent_UPCs
%                   n_weeks_offered tot_move
%       (constituent_UPCs is a ';'-separated list of the raw UPCs collapsed
%        into this effective UPC; n_raw_upcs is its length.)
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
            'group',           g, ...
            'category',        char(T.category(pick)), ...
            'STORE',           T.STORE(pick), ...
            'n_effective_upc', T.n_effective_upc(pick), ...
            'n_menus',         T.n_menus(pick));
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
        if ismember('OK', W.Properties.VariableNames)
            W = W(W.OK == 1, :);
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

    % --- Effective-UPC equivalence classes (per store) -------------------
    % Two raw UPCs are equivalent iff they are offered in exactly the same
    % set of weeks at this store. Canonical representative = smallest UPC
    % in the class. All downstream indexing / menus / choice frequencies
    % are at the effective-UPC level.
    [u_upcs, ~, upc_g] = unique(W.UPC);
    n_raw = numel(u_upcs);
    sigs  = strings(n_raw, 1);
    for ii = 1:n_raw
        wks = sort(unique(W.WEEK(upc_g == ii)));
        sigs(ii) = sprintf('%d_', wks);
    end
    [~, ~, class_g] = unique(sigs);
    n_classes = max(class_g);

    canonical_upcs    = zeros(n_classes, 1);
    class_members_str = strings(n_classes, 1);
    n_per_class       = zeros(n_classes, 1);
    for cc = 1:n_classes
        mems = sort(u_upcs(class_g == cc));
        canonical_upcs(cc)    = mems(1);
        class_members_str(cc) = strjoin(string(mems), ';');
        n_per_class(cc)       = numel(mems);
    end
    % Map each row of W to the canonical UPC of its class
    canon_per_raw = canonical_upcs(class_g);    % length n_raw, in order of u_upcs
    W.EFF_UPC = canon_per_raw(upc_g);

    % --- Per-(effective UPC) stats, ordered for stable local indexing ---
    upc_stats = groupsummary(W, 'EFF_UPC', {'numunique','sum'}, {'WEEK','MOVE'});
    nw_col = find(startsWith(upc_stats.Properties.VariableNames, 'numunique'), 1);
    sm_col = find(startsWith(upc_stats.Properties.VariableNames, 'sum'),       1);
    upc_stats.Properties.VariableNames{nw_col} = 'n_weeks_offered';
    upc_stats.Properties.VariableNames{sm_col} = 'tot_move';
    upc_stats = sortrows(upc_stats, ...
        {'n_weeks_offered','tot_move','EFF_UPC'}, {'descend','descend','ascend'});

    n = height(upc_stats);
    if n < 2
        warning('  group %d: < 2 effective UPCs at STORE=%d (%s) — skipping.', ...
            s.group, s.STORE, s.category);
        return;
    end

    eff_list = upc_stats.EFF_UPC;     % local idx (1-based in MATLAB) -> canonical UPC
    [~, W.local] = ismember(W.EFF_UPC, eff_list);

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

    % Sidecar: local_idx -> effective-UPC mapping
    % Look up class members (in canonical-UPC order) for each row of upc_stats
    [~, eff_pos] = ismember(eff_list, canonical_upcs);
    map_T = table((0:n-1).', eff_list, n_per_class(eff_pos), ...
                  class_members_str(eff_pos), ...
                  upc_stats.n_weeks_offered, upc_stats.tot_move, ...
        'VariableNames', {'local_idx','canonical_UPC','n_raw_upcs', ...
                          'constituent_UPCs','n_weeks_offered','tot_move'});
    writetable(map_T, fullfile(out_dir, [base '_upc_map.csv']));

    fprintf(['  group %3d  %-22s STORE=%3d  n_eff_upc=%d (raw=%d)  ' ...
             'menus=%d  rows=%d\n'], ...
        s.group, s.category, s.STORE, n, n_raw, n_menus, height(out_T));
end
