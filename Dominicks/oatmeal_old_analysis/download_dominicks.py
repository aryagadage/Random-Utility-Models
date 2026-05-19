"""
Download Dominick's UPC CSVs and movement ZIP files from Chicago Booth,
count UPCs per category, and compute movement statistics for top-8 categories.
"""

import urllib.request
import ssl
import io
import csv
import zipfile
import math

# Chicago Booth's cert chain is self-signed
_SSL_CTX = ssl.create_default_context()
_SSL_CTX.check_hostname = False
_SSL_CTX.verify_mode = ssl.CERT_NONE

BASE = "https://www.chicagobooth.edu/-/media/enterprise/centers/kilts/datasets/dominicks-dataset"
UPC_BASE = f"{BASE}/upc_csv-files/"
MOV_BASE = f"{BASE}/movement_csv-files/"

# (upc_filename, movement_zip, category_label)
# Analgesics movement zip has a different name: wana_csv.zip
CATEGORIES = [
    ("upcana.csv", "wana_csv.zip", "Analgesics"),
    ("upcbat.csv", "wbat.zip",     "Bathrm Tiss"),
    ("upcber.csv", "wber.zip",     "Beer"),
    ("upcbjc.csv", "wbjc.zip",     "Bottled Juice"),
    ("upccer.csv", "wcer.zip",     "Cereals"),
    ("upcche.csv", "wche.zip",     "Cheese"),
    ("upccig.csv", "wcig.zip",     "Cigarettes"),
    ("upccoo.csv", "wcoo.zip",     "Cookies"),
    ("upccra.csv", "wcra.zip",     "Crackers"),
    ("upccso.csv", "wcso.zip",     "Canned Soup"),
    ("upcdid.csv", "wdid.zip",     "Dish Deterg"),
    ("upcfec.csv", "wfec.zip",     "Front-End Candy"),
    ("upcfrd.csv", "wfrd.zip",     "Frozen Dinners"),
    ("upcfre.csv", "wfre.zip",     "Frozen Entrees"),
    ("upcfrj.csv", "wfrj.zip",     "Frozen Juices"),
    ("upcfsf.csv", "wfsf.zip",     "Fabric Softeners"),
    ("upcgro.csv", "wgro.zip",     "Grooming Prod"),
    ("upclnd.csv", "wlnd.zip",     "Laundry Det"),
    ("upcoat.csv", "woat.zip",     "Oatmeal"),
    ("upcptw.csv", "wptw.zip",     "Paper Towels"),
    ("upcsdr.csv", "wsdr.zip",     "Soft Drinks"),
    ("upcsha.csv", "wsha.zip",     "Shampoo"),
    ("upcsna.csv", "wsna.zip",     "Snack Crackers"),
    ("upcsoa.csv", "wsoa.zip",     "Soaps"),
    ("upctbr.csv", "wtbr.zip",     "Toothbrushes"),
    ("upctna.csv", "wtna.zip",     "Tuna"),
    ("upctpa.csv", "wtpa.zip",     "Toothpaste"),
    ("upctti.csv", "wtti.zip",     "Toilet Tissue"),
]

SAMPLE_ROWS = 100_000

def fetch_bytes(url, label=""):
    print(f"  GET {label} ...", flush=True)
    req = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0"})
    with urllib.request.urlopen(req, timeout=300, context=_SSL_CTX) as resp:
        return resp.read()

def count_csv_rows(data_bytes):
    text = data_bytes.decode("latin-1", errors="replace")
    n = 0
    for i, _ in enumerate(csv.reader(io.StringIO(text))):
        if i > 0:
            n += 1
    return n

def extract_csv_from_zip(zip_bytes):
    """Return bytes of the first .csv file inside a zip archive."""
    with zipfile.ZipFile(io.BytesIO(zip_bytes)) as zf:
        csv_names = [n for n in zf.namelist() if n.lower().endswith(".csv")]
        if not csv_names:
            raise ValueError("No CSV found inside zip")
        return zf.read(csv_names[0])

def sample_movement(csv_bytes, n=SAMPLE_ROWS):
    text = csv_bytes.decode("latin-1", errors="replace")
    reader = csv.DictReader(io.StringIO(text))
    rows = []
    for i, row in enumerate(reader):
        if i >= n:
            break
        rows.append(row)
    return rows

def movement_stats(rows):
    # Dominick's movement columns: STORE, WEEK, UPC, MOVE, QU, PRICE, ...
    # Try both upper and lower case
    def col(row, *names):
        for n in names:
            if n in row:
                return row[n]
            if n.lower() in row:
                return row[n.lower()]
        return None

    prices, zero_count, stores, weeks = [], 0, set(), set()
    store_upcs = {}  # Track UPCs per store
    
    for r in rows:
        move_s = col(r, "MOVE", "move") or "0"
        try:
            move_val = float(move_s)
        except ValueError:
            move_val = 0.0
        if move_val == 0:
            zero_count += 1

        price_s = col(r, "PRICE", "price") or "0"
        try:
            price_val = float(price_s)
        except ValueError:
            price_val = 0.0
        if price_val > 0:
            prices.append(price_val)

        store = col(r, "STORE", "store") or ""
        week  = col(r, "WEEK", "week") or ""
        upc = col(r, "UPC", "upc") or ""
        
        if store:
            stores.add(store)
            # Track UPCs per store
            if store not in store_upcs:
                store_upcs[store] = set()
            if upc:
                store_upcs[store].add(upc)
                
        if week:
            weeks.add(week)

    if len(prices) >= 2:
        mean_p = sum(prices) / len(prices)
        sd = math.sqrt(sum((p - mean_p) ** 2 for p in prices) / (len(prices) - 1))
    else:
        sd = float("nan")

    total = len(rows)
    pct_zero = zero_count / total if total else float("nan")
    
    # Calculate products per store statistics
    if store_upcs:
        products_per_store = [len(upcs) for upcs in store_upcs.values()]
        avg_products_per_store = sum(products_per_store) / len(products_per_store)
        if len(products_per_store) >= 2:
            sd_products = math.sqrt(sum((p - avg_products_per_store) ** 2 for p in products_per_store) / (len(products_per_store) - 1))
        else:
            sd_products = float("nan")
    else:
        avg_products_per_store = float("nan")
        sd_products = float("nan")
    
    return sd, pct_zero, len(stores), len(weeks), avg_products_per_store, sd_products

# ── Step 1: UPC counts ────────────────────────────────────────────────────────
print("\n=== Step 1: Downloading UPC files ===\n")
upc_counts = []
for upc_file, mov_zip, label in CATEGORIES:
    url = UPC_BASE + upc_file
    try:
        data = fetch_bytes(url, f"{label} ({upc_file})")
        n = count_csv_rows(data)
        print(f"    -> {n:,} UPCs")
        upc_counts.append((label, upc_file, mov_zip, n))
    except Exception as e:
        print(f"    ERROR: {e}")
        upc_counts.append((label, upc_file, mov_zip, -1))

upc_counts.sort(key=lambda x: x[3], reverse=True)

print("\n=== UPC Count Table (ranked) ===\n")
print(f"{'Rank':<5} {'Category':<20} {'N_UPCs':>10}")
print("-" * 38)
for rank, (label, _, _, n) in enumerate(upc_counts, 1):
    print(f"{rank:<5} {label:<20} {n:>10,}")

# ── Step 2: Movement stats for top 8 (ORIGINAL CODE - COMMENTED OUT) ──────────
# top8 = [x for x in upc_counts if x[3] > 0][:8]
# print(f"\n=== Top 8: {[x[0] for x in top8]} ===\n")
# print("=== Step 2: Downloading movement ZIPs (sampling 100k rows each) ===\n")
#
# results = []
# for label, upc_file, mov_zip, n_upcs in top8:
#     url = MOV_BASE + mov_zip
#     try:
#         zip_bytes = fetch_bytes(url, f"{label} movement ({mov_zip})")
#         csv_bytes = extract_csv_from_zip(zip_bytes)
#         rows = sample_movement(csv_bytes)
#         print(f"    -> {len(rows):,} rows sampled")
#         sd, pct_zero, n_stores, n_weeks, avg_products_store, sd_products_store = movement_stats(rows)
#         results.append({
#             "Category": label,
#             "N_UPCs": n_upcs,
#             "Price_SD": round(sd, 4) if not math.isnan(sd) else "NA",
#             "Pct_Zero_Sales": round(pct_zero, 4),
#             "N_Stores": n_stores,
#             "N_Weeks": n_weeks,
#         })
#     except Exception as e:
#         print(f"    ERROR: {e}")
#         results.append({
#             "Category": label,
#             "N_UPCs": n_upcs,
#             "Price_SD": "ERROR",
#             "Pct_Zero_Sales": "ERROR",
#             "N_Stores": "ERROR",
#             "N_Weeks": "ERROR",
#         })
#
# # ── Step 3: Print and save final table ───────────────────────────────────────
# results.sort(key=lambda x: x["N_UPCs"] if isinstance(x["N_UPCs"], int) else 0, reverse=True)
#
# print("\n=== Final Ranked Table ===\n")
# hdr = f"{'Category':<20} {'N_UPCs':>8} {'Price_SD':>10} {'Pct_Zero_Sales':>15} {'N_Stores':>9} {'N_Weeks':>8}"
# print(hdr)
# print("-" * len(hdr))
# for r in results:
#     print(
#         f"{r['Category']:<20} {r['N_UPCs']:>8,} "
#         f"{str(r['Price_SD']):>10} {str(r['Pct_Zero_Sales']):>15} "
#         f"{str(r['N_Stores']):>9} {str(r['N_Weeks']):>8}"
#     )
#
# out_path = "category_comparison.csv"
# with open(out_path, "w", newline="") as f:
#     writer = csv.DictWriter(
#         f, fieldnames=["Category", "N_UPCs", "Price_SD", "Pct_Zero_Sales", "N_Stores", "N_Weeks"]
#     )
#     writer.writeheader()
#     writer.writerows(results)
#
# print(f"\nSaved to {out_path}\n")

# ── Step 3: Movement stats for all categories ────────────────────────────────
all_cats = [x for x in upc_counts if x[3] > 0]
print(f"\n=== Processing all {len(all_cats)} categories ===\n")
print("=== Step 3: Downloading movement ZIPs (sampling 100k rows each) ===\n")

results = []
store_results = []
for label, upc_file, mov_zip, n_upcs in all_cats:
    url = MOV_BASE + mov_zip
    try:
        zip_bytes = fetch_bytes(url, f"{label} movement ({mov_zip})")
        csv_bytes = extract_csv_from_zip(zip_bytes)
        rows = sample_movement(csv_bytes)
        print(f"    -> {len(rows):,} rows sampled")
        sd, pct_zero, n_stores, n_weeks, avg_products_store, sd_products_store = movement_stats(rows)
        results.append({
            "Category": label,
            "N_UPCs": n_upcs,
            "Price_SD": round(sd, 4) if not math.isnan(sd) else "NA",
            "Pct_Zero_Sales": round(pct_zero, 4),
            "N_Stores": n_stores,
            "N_Weeks": n_weeks,
        })
        store_results.append({
            "Category": label,
            "N_UPCs": n_upcs,
            "N_Stores": n_stores,
            "Avg_Products_Per_Store": round(avg_products_store, 2) if not math.isnan(avg_products_store) else "NA",
            "SD_Products_Per_Store": round(sd_products_store, 2) if not math.isnan(sd_products_store) else "NA",
        })
    except Exception as e:
        print(f"    ERROR: {e}")
        results.append({
            "Category": label,
            "N_UPCs": n_upcs,
            "Price_SD": "ERROR",
            "Pct_Zero_Sales": "ERROR",
            "N_Stores": "ERROR",
            "N_Weeks": "ERROR",
        })
        store_results.append({
            "Category": label,
            "N_UPCs": n_upcs,
            "N_Stores": "ERROR",
            "Avg_Products_Per_Store": "ERROR",
            "SD_Products_Per_Store": "ERROR",
        })

# ── Step 4: Print and save final table ───────────────────────────────────────
results.sort(key=lambda x: x["N_UPCs"] if isinstance(x["N_UPCs"], int) else 0, reverse=True)

print("\n=== Final Ranked Table ===\n")
hdr = f"{'Category':<20} {'N_UPCs':>8} {'Price_SD':>10} {'Pct_Zero_Sales':>15} {'N_Stores':>9} {'N_Weeks':>8}"
print(hdr)
print("-" * len(hdr))
for r in results:
    print(
        f"{r['Category']:<20} {r['N_UPCs']:>8,} "
        f"{str(r['Price_SD']):>10} {str(r['Pct_Zero_Sales']):>15} "
        f"{str(r['N_Stores']):>9} {str(r['N_Weeks']):>8}"
    )

out_path = "category_comparison_all.csv"
with open(out_path, "w", newline="") as f:
    writer = csv.DictWriter(
        f, fieldnames=["Category", "N_UPCs", "Price_SD", "Pct_Zero_Sales", "N_Stores", "N_Weeks"]
    )
    writer.writeheader()
    writer.writerows(results)

print(f"\nSaved comprehensive overview to {out_path}\n")

# ── Step 5: Save store-level product statistics ──────────────────────────────
store_results.sort(key=lambda x: x["N_Stores"] if isinstance(x["N_Stores"], int) else 0, reverse=True)

print("\n=== Store-Level Product Statistics ===\n")
store_hdr = f"{'Category':<20} {'N_UPCs':>8} {'N_Stores':>9} {'Avg_Products/Store':>18} {'SD_Products/Store':>18}"
print(store_hdr)
print("-" * len(store_hdr))
for r in store_results:
    print(
        f"{r['Category']:<20} {str(r['N_UPCs']):>8} {str(r['N_Stores']):>9} "
        f"{str(r['Avg_Products_Per_Store']):>18} {str(r['SD_Products_Per_Store']):>18}"
    )

store_out_path = "store_product_variation.csv"
with open(store_out_path, "w", newline="") as f:
    writer = csv.DictWriter(
        f, fieldnames=["Category", "N_UPCs", "N_Stores", "Avg_Products_Per_Store", "SD_Products_Per_Store"]
    )
    writer.writeheader()
    writer.writerows(store_results)

print(f"\nSaved store-level statistics to {store_out_path}\n")
