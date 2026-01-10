import pandas as pd
from collections import Counter
from math import sqrt, inf

PATH = "c:/Users/hgcha/OneDrive/Documents/clicks_test.csv"
TOP_M = 25          # <-- adjust this (e.g., 50, 100)
CHUNKSIZE = 5_000_000

# ---------- Pass 1: count ad_id occurrences to get top-M ----------
ad_counts = Counter()
for chunk in pd.read_csv(PATH, usecols=["ad_id"], chunksize=CHUNKSIZE):
    ad_counts.update(chunk["ad_id"].value_counts().to_dict())

topM = [ad for ad, _ in ad_counts.most_common(TOP_M)]
topM_set = set(topM)
ad_to_bit = {ad: i for i, ad in enumerate(topM)}  # maps ad_id -> 0..TOP_M-1

def popcount(x: int) -> int:
    try:
        return x.bit_count()
    except AttributeError:
        return bin(x).count("1")

# Online mean/std (Welford)
n = 0
mean = 0.0
M2 = 0.0
min_k = inf
max_k = 0

# Count identical menus via bitmask (works up to large TOP_M; Python int is unbounded)
menu_counter = Counter()

# ---------- Pass 2: stream menus; assumes file is sorted by display_id ----------
reader = pd.read_csv(PATH, usecols=["display_id", "ad_id"], chunksize=CHUNKSIZE)

cur_display = None
cur_valid = True
cur_mask = 0

def finalize_menu(valid: bool, mask: int):
    global n, mean, M2, min_k, max_k
    if not valid:
        return
    k = popcount(mask)
    menu_counter[mask] += 1

    if k < min_k:
        min_k = k
    if k > max_k:
        max_k = k

    n += 1
    delta = k - mean
    mean += delta / n
    M2 += delta * (k - mean)

for chunk in reader:
    for d, a in zip(chunk["display_id"].values, chunk["ad_id"].values):
        if cur_display is None:
            cur_display = d

        if d != cur_display:
            finalize_menu(cur_valid, cur_mask)
            cur_display = d
            cur_valid = True
            cur_mask = 0

        if a in topM_set:
            cur_mask |= (1 << ad_to_bit[a])
        else:
            cur_valid = False

if cur_display is not None:
    finalize_menu(cur_valid, cur_mask)

# ---------- Report ----------
if n == 0:
    print(f"No menus found that contain ONLY top-{TOP_M} ads.")
else:
    var = M2 / (n - 1) if n > 1 else 0.0
    std = sqrt(var)

    num_valid_menus = n
    num_unique_menus = len(menu_counter)
    num_duplicate_menus = num_valid_menus - num_unique_menus
    num_patterns_with_duplicates = sum(1 for c in menu_counter.values() if c > 1)

    print(f"=== Screened menus: menu ⊆ top-{TOP_M} ===")
    print(f"Valid menus (display_id) count: {num_valid_menus:,}")
    print(f"Average menu size: {mean:.4f}")
    print(f"Std of menu size: {std:.4f}")
    print(f"Min menu size: {min_k}")
    print(f"Max menu size: {max_k}")

    print("\n=== Exact same menus (menu patterns) ===")
    print(f"Unique menu patterns: {num_unique_menus:,}")
    print(f"Total duplicated menus (valid_menus - unique_patterns): {num_duplicate_menus:,}")
    print(f"Number of patterns appearing >=2 times: {num_patterns_with_duplicates:,}")

    print("\nTop 10 most common menu patterns (count, menu_size):")
    for mask, cnt in menu_counter.most_common(10):
        print(f"count={cnt:,}, menu_size={popcount(mask)}")
