function [p_obs, w_BM, menu_sizes, M] = B_generate_random_menus_L(n, L, rng_seed)
%B_GENERATE_RANDOM_MENUS_L  Random L menus per size, with choice probabilities.
%
%   For each menu size k = 2, 3, ..., n-1:
%     select min(L, C(n,k)) menus uniformly at random (without replacement).
%   The full menu (k=n) is ALWAYS included.
%   Singletons (k=1) are EXCLUDED.
%
%   Inputs:
%     n        — number of alternatives
%     L        — max menus per size (cap per k)
%     rng_seed — (optional) random seed
%
%   Outputs:
%     p_obs      — observed probability vector (only observed coordinates)
%     w_BM       — m2 x 1 binary mask (1 = observed coordinate)
%     menu_sizes — n x 1 vector: menu_sizes(k) = number of observed size-k menus
%     M          — map_get(n) struct

    if nargin >= 3 && ~isempty(rng_seed)
        rng(rng_seed);
    end

    M = map_get(n);
    w_BM = zeros(M.m2, 1);
    menu_sizes = zeros(n, 1);

    % --- Always include the full menu (k=n): exactly 1 row ---
    rows_kn = find(M.k_per_row == n);
    for i = 1:length(rows_kn)
        r = rows_kn(i);
        k = M.k_per_row(r);
        w_BM(M.ptr(r) : M.ptr(r)+k-1) = 1;
    end
    menu_sizes(n) = length(rows_kn);

    % --- For each size k = 2, ..., n-1: select min(L, C(n,k)) random menus ---
    for k = 2:n-1
        rows_k = find(M.k_per_row == k);
        nk = length(rows_k);               % = C(n,k)
        num_select = min(L, nk);
        perm = randperm(nk, num_select);    % random selection without replacement
        selected = rows_k(perm);
        for i = 1:num_select
            r = selected(i);
            w_BM(M.ptr(r) : M.ptr(r)+k-1) = 1;
        end
        menu_sizes(k) = num_select;
    end

    % --- Generate full BM probabilities and extract observed ---
    p = make_choice_prob_BM_random(n);
    p_obs = p(w_BM == 1);
end
