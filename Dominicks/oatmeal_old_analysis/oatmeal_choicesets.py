"""
Oatmeal DC RUM Analysis:
  1. Download woat.csv + upcoat.csv from Chicago Booth
  2. Zero-purchase chart
  3. Greedy universal set (4a): largest single choice set U,
     then keep occasions whose full menu ⊆ U AND every UPC in that menu has MOVE > 0
"""

import urllib.request, ssl, io, zipfile
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import os

# ─────────────────────────────────────────
# CONFIG
# ─────────────────────────────────────────
BASE    = "https://www.chicagobooth.edu/-/media/enterprise/centers/kilts/datasets/dominicks-dataset"
UPC_URL = f"{BASE}/upc_csv-files/upcoat.csv"
MOV_URL = f"{BASE}/movement_csv-files/woat.zip"
OUT_DIR = "oatmeal csv"
os.makedirs(OUT_DIR, exist_ok=True)

SSL_CTX = ssl.create_default_context()
SSL_CTX.check_hostname = False
SSL_CTX.verify_mode    = ssl.CERT_NONE

# ─────────────────────────────────────────
# STEP 0 — DOWNLOAD
# ─────────────────────────────────────────
def fetch(url, label):
    print(f"  Downloading {label}...")
    req = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0"})
    with urllib.request.urlopen(req, timeout=300, context=SSL_CTX) as r:
        return r.read()

upc_path = f"{OUT_DIR}/upcoat.csv"
mov_path = f"{OUT_DIR}/woat.csv"

if not os.path.exists(upc_path):
    data = fetch(UPC_URL, "upcoat.csv")
    with open(upc_path, "wb") as f:
        f.write(data)
    print(f"  Saved {upc_path}")
else:
    print(f"  {upc_path} already exists, skipping download.")

if not os.path.exists(mov_path):
    zip_bytes = fetch(MOV_URL, "woat.zip")
    with zipfile.ZipFile(io.BytesIO(zip_bytes)) as zf:
        csv_name = [n for n in zf.namelist() if n.lower().endswith(".csv")][0]
        with open(mov_path, "wb") as f:
            f.write(zf.read(csv_name))
    print(f"  Saved {mov_path}")
else:
    print(f"  {mov_path} already exists, skipping download.")

# ─────────────────────────────────────────
# LOAD
# ─────────────────────────────────────────
print("\nLoading data...")
w = pd.read_csv(mov_path, usecols=["UPC","STORE","WEEK","MOVE","PRICE","OK"])
u = pd.read_csv(upc_path, usecols=["UPC","DESCRIP"], encoding="latin-1")
print(f"  woat rows: {len(w):,}  |  upcoat rows: {len(u):,}")
print(f"  Unique UPCs in movement: {w['UPC'].nunique():,}")

# ─────────────────────────────────────────
# PART 1 — ZERO-PURCHASE ANALYSIS
# ─────────────────────────────────────────
print("\nPART 1: Zero-purchase analysis...")

total_move = w.groupby("UPC")["MOVE"].sum().reset_index(name="total_move")
total_move = total_move.merge(u[["UPC","DESCRIP"]], on="UPC", how="left")

n_zero    = (total_move["total_move"] == 0).sum()
n_nonzero = (total_move["total_move"] >  0).sum()
print(f"  UPCs with zero total MOVE:  {n_zero:,} / {len(total_move):,}")
print(f"  UPCs with at least 1 sale:  {n_nonzero:,}")

fig, axes = plt.subplots(1, 2, figsize=(12, 5))

axes[0].bar(["Never purchased", "At least 1 sale"], [n_zero, n_nonzero],
            color=["#d62728", "#1f77b4"])
axes[0].set_title("Oatmeal UPCs: Purchase Status")
axes[0].set_ylabel("Number of UPCs")
for i, v in enumerate([n_zero, n_nonzero]):
    axes[0].text(i, v + 0.3, str(v), ha="center", fontweight="bold")

nz = total_move[total_move["total_move"] > 0]["total_move"]
axes[1].hist(np.log10(nz), bins=30, color="#1f77b4", edgecolor="white")
axes[1].set_title("Distribution of Total MOVE (log10, nonzero UPCs)")
axes[1].set_xlabel("log10(total units sold)")
axes[1].set_ylabel("Number of UPCs")
axes[1].axvline(np.log10(nz.median()), color="red", linestyle="--",
                label=f"Median = {nz.median():.0f}")
axes[1].legend()

plt.tight_layout()
plt.savefig("oatmeal_zero_purchase.png", dpi=150)
plt.close()
print("  Saved: oatmeal_zero_purchase.png")

# ─────────────────────────────────────────
# PART 2 — GREEDY UNIVERSAL SET (4a)
# ─────────────────────────────────────────
print("\nPART 2: Greedy universal set...")

# Filter to offered products
offered = w[(w["OK"] == 1) & (w["PRICE"] > 0)].copy()
offered["occasion_id"] = offered["STORE"].astype(str) + "_" + offered["WEEK"].astype(str)
print(f"  Offered rows (OK==1, PRICE>0): {len(offered):,}")
print(f"  Occasions (STORE_WEEK):         {offered['occasion_id'].nunique():,}")

# Build occasion → set of UPCs
occ_sets = offered.groupby("occasion_id")["UPC"].apply(frozenset)

# Step 1: Find the single largest choice set → U
set_sizes   = occ_sets.apply(len)
largest_occ = set_sizes.idxmax()
U           = occ_sets[largest_occ]
print(f"\n  Largest single choice set:")
print(f"    occasion_id: {largest_occ}")
print(f"    |U| = {len(U)} UPCs")

# Step 2: Among ALL occasions, keep those where:
#   (a) their UPC set ⊆ U  (contained in universal set)
#   (b) every UPC in that occasion has MOVE > 0  (fully purchased menu)

# For condition (b), build a per-occasion set of UPCs with MOVE > 0
occ_purchased = (offered[offered["MOVE"] > 0]
                 .groupby("occasion_id")["UPC"]
                 .apply(frozenset))

# RELAXED: menu ⊆ U only (no purchase filter)
valid_occasions = []
for occ_id, upc_set in occ_sets.items():
    if not upc_set <= U:
        continue
    valid_occasions.append({
        "occasion_id": occ_id,
        "set_size":    len(upc_set),
        "upcs":        "_".join(str(x) for x in sorted(upc_set)),
    })

valid_df = pd.DataFrame(valid_occasions)
print(f"\n  Occasions with menu ⊆ U (relaxed, no purchase filter):")
print(f"    Count: {len(valid_df):,}")
if len(valid_df) > 0:
    print(f"    set_size range:  {valid_df['set_size'].min()} – {valid_df['set_size'].max()}")
    print(f"    Median set_size: {valid_df['set_size'].median():.0f}")
    print(f"\n  set_size distribution:")
    print(valid_df["set_size"].value_counts().sort_index().to_string())

# Step 3: Attach full occasion data for valid occasions
valid_occ_ids = set(valid_df["occasion_id"])
clean = offered[offered["occasion_id"].isin(valid_occ_ids)].copy()
clean["total_move"] = clean.groupby("occasion_id")["MOVE"].transform("sum")
clean["share"]      = clean["MOVE"] / clean["total_move"]
clean = clean.merge(u[["UPC","DESCRIP"]], on="UPC", how="left")
clean[["occasion_id","UPC","DESCRIP","MOVE","PRICE","share"]].to_csv(
    "oatmeal_clean_occasions.csv", index=False)
print(f"\n  Saved: oatmeal_clean_occasions.csv  ({len(clean):,} rows)")

# Step 4: Menu repetition
n_distinct_menus = valid_df["upcs"].nunique() if len(valid_df) > 0 else 0
occ_per_menu     = valid_df.groupby("upcs")["occasion_id"].count() if len(valid_df) > 0 else pd.Series(dtype=int)
print(f"\n  Distinct menus within U:  {n_distinct_menus:,}")
if len(occ_per_menu) > 0:
    print(f"  Avg occasions per menu:   {occ_per_menu.mean():.1f}")
    print(f"  Max occasions per menu:   {occ_per_menu.max()}")

# Step 5: Zero-purchase check WITHIN valid occasions
# For each of the 51 UPCs in U, what fraction have MOVE==0 across ALL valid occasions?
print(f"\n── Zero-purchase check within valid occasions ──")
upc_move = (clean.groupby("UPC")["MOVE"].sum().reset_index(name="total_move_in_valid"))
u_upcs   = pd.DataFrame({"UPC": list(U)})
upc_move = u_upcs.merge(upc_move, on="UPC", how="left").fillna({"total_move_in_valid": 0})
upc_move = upc_move.merge(u[["UPC","DESCRIP"]], on="UPC", how="left")

n_zero_in_valid    = (upc_move["total_move_in_valid"] == 0).sum()
pct_zero_in_valid  = n_zero_in_valid / len(U) * 100
print(f"  UPCs in U with zero purchases across all valid occasions: "
      f"{n_zero_in_valid} / {len(U)}  ({pct_zero_in_valid:.1f}%)")
print(f"  UPCs with at least 1 purchase in valid occasions:         "
      f"{len(U) - n_zero_in_valid} / {len(U)}  ({100 - pct_zero_in_valid:.1f}%)")
if n_zero_in_valid > 0:
    zero_upcs = upc_move[upc_move["total_move_in_valid"] == 0][["UPC","DESCRIP"]]
    print(f"\n  Zero-purchase UPCs within valid occasions:")
    print(zero_upcs.to_string(index=False))

# Step 6: DC RUM verdict
print("\n" + "="*55)
print("DC RUM VERDICT (relaxed: menu ⊆ U)")
print("="*55)
ok_menus = n_distinct_menus >= 10
ok_occ   = (occ_per_menu.mean() >= 5) if len(occ_per_menu) > 0 else False
print(f"  Distinct menus ≥ 10:     {'YES' if ok_menus else 'NO'}  ({n_distinct_menus})")
avg_occ_str = f"{occ_per_menu.mean():.1f}" if len(occ_per_menu) > 0 else "0"
print(f"  Avg occasions/menu ≥ 5:  {'YES' if ok_occ else 'NO'}  ({avg_occ_str})")
print(f"  % zero-purchase UPCs in valid occ: {pct_zero_in_valid:.1f}%")
if ok_menus and ok_occ:
    print("\n  >> YES — usable for DC RUM estimation.")
else:
    print("\n  >> NO — insufficient menu repetition.")

print("\nDone.")
