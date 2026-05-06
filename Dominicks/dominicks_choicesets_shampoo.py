"""
Dominick's Shampoo — DC RUM Data Preparation & Variation Checks
"""

import pandas as pd
import numpy as np
import itertools
from collections import defaultdict

# ─────────────────────────────────────────
# LOAD
# ─────────────────────────────────────────
print("Loading data...")
w = pd.read_csv("wsha.csv", usecols=["UPC","STORE","WEEK","MOVE","PRICE","OK"])
u = pd.read_csv("upcsha.csv", usecols=["UPC","DESCRIP"], encoding="latin-1")

print(f"  wsha rows: {len(w):,}  |  upcsha rows: {len(u):,}")

# ─────────────────────────────────────────
# STEP 1 — BUILD OCCASIONS
# ─────────────────────────────────────────
print("\nSTEP 1: Building occasions...")

df = w[(w["OK"] == 1) & (w["PRICE"] > 0)].copy()
print(f"  Rows after OK==1 & PRICE>0: {len(df):,}")

df["occasion_id"] = df["STORE"].astype(str) + "_" + df["WEEK"].astype(str)

# Compute share = MOVE / sum(MOVE) within each occasion
df["total_move"] = df.groupby("occasion_id")["MOVE"].transform("sum")
# Drop occasions where total_move == 0 (all zero sales — can't form shares)
df = df[df["total_move"] > 0].copy()
df["share"] = df["MOVE"] / df["total_move"]

print(f"  Occasions (STORE_WEEK): {df['occasion_id'].nunique():,}")
print(f"  Unique UPCs: {df['UPC'].nunique():,}")

# ─────────────────────────────────────────
# STEP 2 — BUILD CHOICE SETS
# ─────────────────────────────────────────
print("\nSTEP 2: Building choice sets...")

# For each occasion, the choice set = all UPCs present
# set_id = sorted UPCs joined by underscore
occ_upcs = df.groupby("occasion_id")["UPC"].apply(
    lambda x: "_".join(str(u) for u in sorted(x.unique()))
).rename("set_id").reset_index()

df = df.merge(occ_upcs, on="occasion_id")

# set_size = number of unique UPCs in that set
df["set_size"] = df["set_id"].apply(lambda s: len(s.split("_")))

print(f"  Distinct set_ids: {df['set_id'].nunique():,}")
print(f"  set_size range: {df['set_size'].min()} – {df['set_size'].max()}")
print(f"  Median set_size: {df['set_size'].median():.0f}")

# ─────────────────────────────────────────
# STEP 3 — INTEGER UPC INDEX & PER-SET AGGREGATION
# ─────────────────────────────────────────
print("\nSTEP 3: Indexing UPCs & aggregating by (set_id, UPC)...")

all_upcs = sorted(df["UPC"].unique())
upc_to_idx = {u: i for i, u in enumerate(all_upcs)}
df["upc_idx"] = df["UPC"].map(upc_to_idx)

# Aggregate: avg_share, avg_price, n_occasions per (set_id, upc_idx, UPC)
agg = (df.groupby(["set_id","upc_idx","UPC"])
         .agg(avg_share=("share","mean"),
              avg_price=("PRICE","mean"),
              n_occasions=("occasion_id","nunique"))
         .reset_index())

# Attach set_size
set_sizes = df[["set_id","set_size"]].drop_duplicates()
agg = agg.merge(set_sizes, on="set_id")

# Attach description
agg = agg.merge(u[["UPC","DESCRIP"]], on="UPC", how="left")

print(f"  choice_probs rows: {len(agg):,}")

# ─────────────────────────────────────────
# STEP 3b — FIND CONTAINED SUBSETS
# ─────────────────────────────────────────
print("\nSTEP 3b: Finding contained subset pairs...")

# Build set_id → frozenset of UPCs
set_id_to_upcs = {}
for sid, grp in df.groupby("set_id"):
    set_id_to_upcs[sid] = frozenset(grp["UPC"].unique())

# Sort set_ids by size descending
sorted_sets = sorted(set_id_to_upcs.items(), key=lambda x: len(x[1]), reverse=True)

# Focus on large sets (top quartile by size), find subsets among all sets
sizes = np.array([len(v) for _, v in sorted_sets])
q75_size = int(np.percentile(sizes, 75))
print(f"  75th-percentile set_size: {q75_size}")

large_sets  = [(sid, upcs) for sid, upcs in sorted_sets if len(upcs) >= q75_size]
all_sets_d  = dict(sorted_sets)

containment_pairs = []
for large_sid, large_upcs in large_sets:
    for other_sid, other_upcs in all_sets_d.items():
        if other_sid == large_sid:
            continue
        if other_upcs < large_upcs:          # strict subset
            containment_pairs.append((large_sid, other_sid,
                                      len(large_upcs), len(other_upcs)))

contained_df = pd.DataFrame(containment_pairs,
    columns=["large_set_id","subset_id","large_size","subset_size"])
print(f"  Containment pairs found: {len(contained_df):,}")

# ─────────────────────────────────────────
# STEP 4 — DC RUM VARIATION CHECKS
# ─────────────────────────────────────────
print("\n" + "="*60)
print("STEP 4: DC RUM Variation Checks")
print("="*60)

checks_summary = {}

# ── CHECK A: REGULARITY ──────────────────
print("\n── CHECK A: Regularity (share vs set_size) ──")

reg = (df.groupby(["UPC","set_size"])["share"]
         .mean()
         .reset_index()
         .rename(columns={"share":"mean_share"}))

# For each UPC, check monotonicity across set_sizes
violations = []
for upc, grp in reg.groupby("UPC"):
    grp_s = grp.sort_values("set_size")
    if len(grp_s) < 2:
        continue
    shares = grp_s["mean_share"].values
    sizes  = grp_s["set_size"].values
    for i in range(len(shares)-1):
        if shares[i+1] > shares[i]:
            violations.append({
                "UPC": upc,
                "set_size_low":  sizes[i],
                "set_size_high": sizes[i+1],
                "share_low":     shares[i],
                "share_high":    shares[i+1],
            })

# Print summary table (pivot: UPC × set_size, limited to top 20 UPCs by volume)
top_upcs = df.groupby("UPC")["MOVE"].sum().nlargest(20).index
reg_top = reg[reg["UPC"].isin(top_upcs)].pivot(
    index="UPC", columns="set_size", values="mean_share")
reg_top = reg_top.merge(u[["UPC","DESCRIP"]].set_index("UPC"), left_index=True, right_index=True, how="left")

print(f"\n  Mean share by set_size (top 20 UPCs by volume):")
pd.set_option("display.max_columns", 20)
pd.set_option("display.width", 140)
pd.set_option("display.float_format", "{:.5f}".format)
print(reg_top.to_string())

n_viol_upcs = len(set(v["UPC"] for v in violations))
print(f"\n  Regularity violations: {len(violations)} UPC-pair(s) across {n_viol_upcs} UPC(s)")
if violations:
    viol_df = pd.DataFrame(violations).head(10)
    viol_df = viol_df.merge(u[["UPC","DESCRIP"]], on="UPC", how="left")
    print(viol_df.to_string(index=False))

checks_summary["regularity_violations"] = len(violations)
checks_summary["regularity_violation_upcs"] = n_viol_upcs

# ── CHECK B: MENU VARIATION ──────────────
print("\n── CHECK B: Menu Variation ──")

n_distinct_sets = df["set_id"].nunique()
occ_per_set = df.groupby("set_id")["occasion_id"].nunique()
avg_occ_per_set = occ_per_set.mean()

size_dist = df.drop_duplicates("set_id")["set_size"].value_counts().sort_index()

print(f"  Distinct set_ids:          {n_distinct_sets:,}")
print(f"  Avg occasions per set_id:  {avg_occ_per_set:.1f}")
print(f"  Min occasions per set_id:  {occ_per_set.min()}")
print(f"  Max occasions per set_id:  {occ_per_set.max()}")
print(f"\n  set_size distribution:")
print(size_dist.to_string())

flag_b1 = n_distinct_sets < 50
flag_b2 = avg_occ_per_set < 5
if flag_b1:
    print(f"\n  *** FLAG: Only {n_distinct_sets} distinct set_ids (<50 threshold)")
if flag_b2:
    print(f"\n  *** FLAG: Avg occasions/set_id = {avg_occ_per_set:.1f} (<5 threshold)")

checks_summary["n_distinct_set_ids"]   = n_distinct_sets
checks_summary["avg_occasions_per_set"] = round(avg_occ_per_set, 2)
checks_summary["flag_few_set_ids"]     = flag_b1
checks_summary["flag_few_occasions"]   = flag_b2

# ── CHECK C: CONTAINMENT PAIRS ───────────
print("\n── CHECK C: Containment Pairs ──")

n_pairs = len(contained_df)
print(f"  Total (large_set, subset) pairs: {n_pairs:,}")

if n_pairs > 0:
    pair_dist = contained_df["subset_size"].value_counts().sort_index()
    print(f"\n  Distribution of subset sizes within pairs:")
    print(pair_dist.to_string())

    # Distinct subsets covered
    n_distinct_subsets = contained_df["subset_id"].nunique()
    print(f"\n  Distinct subset set_ids covered: {n_distinct_subsets:,}")

checks_summary["n_containment_pairs"]    = n_pairs
checks_summary["n_distinct_subsets"]     = contained_df["subset_id"].nunique() if n_pairs > 0 else 0

# ─────────────────────────────────────────
# STEP 5 — SAVE OUTPUTS
# ─────────────────────────────────────────
print("\nSTEP 5: Saving CSVs...")

# 1) occasions.csv
occ_out = df[["occasion_id","UPC","share","PRICE","set_id","set_size"]].copy()
occ_out.rename(columns={"PRICE":"price"}, inplace=True)
occ_out.to_csv("occasions.csv", index=False)
print(f"  occasions.csv: {len(occ_out):,} rows")

# 2) choice_probs.csv
cp_out = agg[["set_id","upc_idx","UPC","DESCRIP","avg_share","avg_price","n_occasions","set_size"]]
cp_out.to_csv("choice_probs.csv", index=False)
print(f"  choice_probs.csv: {len(cp_out):,} rows")

# 3) dc_rum_checks.csv
chk_df = pd.DataFrame([checks_summary])
chk_df.to_csv("dc_rum_checks.csv", index=False)
print(f"  dc_rum_checks.csv: 1 row")

# ─────────────────────────────────────────
# FINAL VERDICT
# ─────────────────────────────────────────
print("\n" + "="*60)
print("FINAL VERDICT")
print("="*60)

ok_sets  = n_distinct_sets >= 50
ok_occ   = avg_occ_per_set >= 5
ok_pairs = n_pairs >= 100

print(f"  ✓ Distinct set_ids ≥ 50:          {'YES' if ok_sets  else 'NO'}  ({n_distinct_sets})")
print(f"  ✓ Avg occasions/set_id ≥ 5:       {'YES' if ok_occ   else 'NO'}  ({avg_occ_per_set:.1f})")
print(f"  ✓ Containment pairs ≥ 100:        {'YES' if ok_pairs else 'NO'}  ({n_pairs})")

if ok_sets and ok_occ and ok_pairs:
    print("\n  >> YES — sufficient menu variation for DC RUM estimation.")
else:
    print("\n  >> NO — insufficient variation on one or more dimensions.")
    if not ok_sets:
        print("     - Too few distinct choice menus.")
    if not ok_occ:
        print("     - Too few repeated observations per menu.")
    if not ok_pairs:
        print("     - Too few containment pairs for DC RUM identification.")

print("\nDone.")
