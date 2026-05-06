"""
Shampoo UPC Co-occurrence Matrix + Zero-Purchase Analysis
"""

import pandas as pd
import numpy as np
from scipy import sparse
import matplotlib.pyplot as plt
import os, sys

DATA_DIR = "shampoo csv"

# ─────────────────────────────────────────
# SANITY CHECK HELPERS
# ─────────────────────────────────────────
def mb(arr):
    return arr.nbytes / 1e6

def check_size(label, obj, limit_mb=500):
    if hasattr(obj, "nbytes"):
        size = obj.nbytes / 1e6
    elif hasattr(obj, "data"):
        size = (obj.data.nbytes + obj.indices.nbytes + obj.indptr.nbytes) / 1e6
    else:
        size = 0
    print(f"  [{label}] size: {size:.1f} MB")
    if size > limit_mb:
        sys.exit(f"  ABORT: {label} exceeds {limit_mb} MB limit.")
    return size

# ─────────────────────────────────────────
# LOAD
# ─────────────────────────────────────────
print("Loading data...")
w = pd.read_csv(f"{DATA_DIR}/wsha.csv", usecols=["UPC","STORE","WEEK","MOVE","PRICE","OK"])
u = pd.read_csv(f"{DATA_DIR}/upcsha.csv", usecols=["UPC","DESCRIP"], encoding="latin-1")
print(f"  wsha: {len(w):,} rows  |  upcsha: {len(u):,} rows")

# ─────────────────────────────────────────
# PART 1 — ZERO-PURCHASE ANALYSIS
# ─────────────────────────────────────────
print("\nPART 1: Zero-purchase analysis...")

total_move = w.groupby("UPC")["MOVE"].sum().reset_index(name="total_move")
total_move = total_move.merge(u[["UPC","DESCRIP"]], on="UPC", how="left")

n_zero    = (total_move["total_move"] == 0).sum()
n_nonzero = (total_move["total_move"] >  0).sum()
print(f"  UPCs with zero total MOVE: {n_zero:,} / {len(total_move):,}")
print(f"  UPCs with ≥1 sale:         {n_nonzero:,}")

fig, axes = plt.subplots(1, 2, figsize=(12, 5))

# Bar: zero vs nonzero
axes[0].bar(["Never purchased", "At least 1 sale"], [n_zero, n_nonzero],
            color=["#d62728","#1f77b4"])
axes[0].set_title("Shampoo UPCs: Purchase Status")
axes[0].set_ylabel("Number of UPCs")
for i, v in enumerate([n_zero, n_nonzero]):
    axes[0].text(i, v + 1, str(v), ha="center", fontweight="bold")

# Histogram of log total MOVE (nonzero only)
nz = total_move[total_move["total_move"] > 0]["total_move"]
axes[1].hist(np.log10(nz), bins=40, color="#1f77b4", edgecolor="white")
axes[1].set_title("Distribution of Total MOVE (log10, nonzero UPCs)")
axes[1].set_xlabel("log10(total units sold)")
axes[1].set_ylabel("Number of UPCs")
axes[1].axvline(np.log10(nz.median()), color="red", linestyle="--",
                label=f"Median={nz.median():.0f}")
axes[1].legend()

plt.tight_layout()
plt.savefig("zero_purchase_analysis.png", dpi=150)
plt.close()
print("  Saved: zero_purchase_analysis.png")

# ─────────────────────────────────────────
# PART 2 — CO-OCCURRENCE MATRIX
# ─────────────────────────────────────────
print("\nPART 2: Co-occurrence matrix...")

# Filter to offered products only
offered = w[(w["OK"] == 1) & (w["PRICE"] > 0)][["UPC","STORE","WEEK"]].copy()
print(f"  Offered rows (OK==1, PRICE>0): {len(offered):,}")

# Integer index for all UPCs
all_upcs = sorted(offered["UPC"].unique())
n        = len(all_upcs)
upc2idx  = {u: i for i, u in enumerate(all_upcs)}
print(f"  Unique UPCs in offered set: {n:,}")

# SANITY CHECK: expected matrix size
dense_mb = n * n * 4 / 1e6   # int32
print(f"\n  SANITY — dense int32 matrix: {dense_mb:.1f} MB")
if dense_mb > 500:
    sys.exit("  ABORT: dense matrix would exceed 500 MB.")

# Expected number of pairs per occasion (rough)
avg_set_size = offered.groupby(["STORE","WEEK"])["UPC"].count().mean()
n_occasions  = offered.groupby(["STORE","WEEK"]).ngroups
est_pairs    = n_occasions * (avg_set_size * (avg_set_size - 1) / 2)
print(f"  Avg set size: {avg_set_size:.1f}  |  Occasions: {n_occasions:,}")
print(f"  Estimated pair increments: {est_pairs/1e6:.1f}M  (sanity: should be <1B)")
if est_pairs > 1e9:
    sys.exit("  ABORT: too many pair increments (>1B).")

# Build sparse co-occurrence matrix
print("\n  Building co-occurrence matrix (sparse accumulation)...")
mat = sparse.lil_matrix((n, n), dtype=np.int32)

occasion_groups = offered.groupby(["STORE","WEEK"])["UPC"].apply(list)
total_occ = len(occasion_groups)

for k, (key, upcs) in enumerate(occasion_groups.items()):
    if k % 2000 == 0:
        print(f"    {k:,}/{total_occ:,} occasions processed...", end="\r")
    idx = [upc2idx[u] for u in set(upcs)]   # deduplicate within occasion
    for i in range(len(idx)):
        for j in range(i, len(idx)):         # upper triangle + diagonal
            mat[idx[i], idx[j]] += 1
            if i != j:
                mat[idx[j], idx[i]] += 1

print(f"\n  Done. Converting to CSR...")
mat_csr = mat.tocsr()
check_size("sparse CSR matrix", mat_csr)

# Convert to dense for CSV export
print("  Converting to dense array...")
dense = mat_csr.toarray()
check_size("dense array", dense)

print("  Saving co-occurrence matrix CSV...")
df_mat = pd.DataFrame(dense, index=all_upcs, columns=all_upcs)
df_mat.index.name = "UPC"
df_mat.to_csv("cooccurrence_matrix.csv")
csv_size = os.path.getsize("cooccurrence_matrix.csv") / 1e6
print(f"  Saved: cooccurrence_matrix.csv  ({csv_size:.1f} MB)")

# ─────────────────────────────────────────
# SANITY CHECKS ON MATRIX
# ─────────────────────────────────────────
print("\nSANITY CHECKS on matrix:")
diag = dense.diagonal()
print(f"  Diagonal (times UPC appears): min={diag.min()}, max={diag.max()}, mean={diag.mean():.1f}")
print(f"  Matrix symmetric: {np.allclose(dense, dense.T)}")
offdiag = dense[np.triu_indices(n, k=1)]
print(f"  Off-diagonal max co-occurrence: {offdiag.max()}")
print(f"  Off-diagonal mean (nonzero pairs): {offdiag[offdiag>0].mean():.1f}")
print(f"  Pairs that never co-occur (=0): {(offdiag==0).sum():,} / {len(offdiag):,}")
print(f"  Pairs that co-occur ≥1 time:    {(offdiag>0).sum():,} / {len(offdiag):,}")

# ─────────────────────────────────────────
# HEATMAP (top 50 UPCs by total appearances)
# ─────────────────────────────────────────
print("\nPlotting heatmap (top 50 UPCs by diagonal)...")
top50_idx = np.argsort(diag)[-50:][::-1]
top50_mat = dense[np.ix_(top50_idx, top50_idx)]
top50_upc = [all_upcs[i] for i in top50_idx]

fig, ax = plt.subplots(figsize=(14, 12))
im = ax.imshow(np.log1p(top50_mat), cmap="YlOrRd", aspect="auto")
ax.set_xticks(range(50))
ax.set_yticks(range(50))
ax.set_xticklabels(top50_upc, rotation=90, fontsize=6)
ax.set_yticklabels(top50_upc, fontsize=6)
plt.colorbar(im, ax=ax, label="log1p(co-occurrences)")
ax.set_title("Co-occurrence Matrix — Top 50 UPCs by Appearances\n(color = log1p of count)")
plt.tight_layout()
plt.savefig("cooccurrence_heatmap.png", dpi=150)
plt.close()
print("  Saved: cooccurrence_heatmap.png")

print("\nDone.")
