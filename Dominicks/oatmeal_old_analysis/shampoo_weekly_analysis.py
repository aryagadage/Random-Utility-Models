"""
Analyze weekly product variation for shampoo category.
Shows how many products are housed per week in each store.
"""

import urllib.request
import ssl
import io
import csv
import zipfile
import math
import matplotlib.pyplot as plt
import pandas as pd

# Chicago Booth's cert chain is self-signed
_SSL_CTX = ssl.create_default_context()
_SSL_CTX.check_hostname = False
_SSL_CTX.verify_mode = ssl.CERT_NONE

BASE = "https://www.chicagobooth.edu/-/media/enterprise/centers/kilts/datasets/dominicks-dataset"
MOV_BASE = f"{BASE}/movement_csv-files/"

def fetch_bytes(url, label=""):
    print(f"  GET {label} ...", flush=True)
    req = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0"})
    with urllib.request.urlopen(req, timeout=300, context=_SSL_CTX) as resp:
        return resp.read()

def extract_csv_from_zip(zip_bytes):
    """Return bytes of the first .csv file inside a zip archive."""
    with zipfile.ZipFile(io.BytesIO(zip_bytes)) as zf:
        csv_names = [n for n in zf.namelist() if n.lower().endswith(".csv")]
        if not csv_names:
            raise ValueError("No CSV found inside zip")
        return zf.read(csv_names[0])

def sample_movement(csv_bytes, n=100_000):
    text = csv_bytes.decode("latin-1", errors="replace")
    reader = csv.DictReader(io.StringIO(text))
    rows = []
    for i, row in enumerate(reader):
        if i >= n:
            break
        rows.append(row)
    return rows

def analyze_weekly_variation(rows):
    """Analyze how many products are housed per week in each store."""
    # Group by store and week, count unique UPCs per store per week
    store_weekly_upcs = {}

    def col(row, *names):
        for n in names:
            if n in row:
                return row[n]
            if n.lower() in row:
                return row[n.lower()]
        return None

    for r in rows:
        store = col(r, "STORE", "store") or ""
        week = col(r, "WEEK", "week") or ""
        upc = col(r, "UPC", "upc") or ""

        if store and week and upc:
            key = (store, week)
            if key not in store_weekly_upcs:
                store_weekly_upcs[key] = set()
            store_weekly_upcs[key].add(upc)

    # Calculate per-store stats across all weeks
    store_stats = {}
    for (store, week), upcs in store_weekly_upcs.items():
        if store not in store_stats:
            store_stats[store] = []
        store_stats[store].append(len(upcs))

    # Calculate stats per store
    results = []
    for store in sorted(store_stats.keys()):
        products_per_week = store_stats[store]
        if products_per_week:
            min_products = min(products_per_week)
            max_products = max(products_per_week)
            avg_products = sum(products_per_week) / len(products_per_week)
            if len(products_per_week) >= 2:
                sd_products = math.sqrt(sum((p - avg_products) ** 2 for p in products_per_week) / (len(products_per_week) - 1))
            else:
                sd_products = 0.0
            results.append({
                "Store": store,
                "N_Weeks": len(products_per_week),
                "Avg_Products_Per_Week": round(avg_products, 2),
                "SD_Products_Per_Week": round(sd_products, 2),
                "Min_Products": min_products,
                "Max_Products": max_products
            })

    return results

def main():
    print("=== Analyzing Weekly Product Variation for Shampoo ===\n")

    # Download shampoo movement data
    mov_zip = "wsha.zip"
    url = MOV_BASE + mov_zip
    print(f"Downloading shampoo movement data from {url}")

    try:
        zip_bytes = fetch_bytes(url, "shampoo movement")
        csv_bytes = extract_csv_from_zip(zip_bytes)
        rows = sample_movement(csv_bytes)
        print(f"    -> {len(rows):,} rows sampled")

        # Analyze store variation
        store_results = analyze_weekly_variation(rows)

        # Save to CSV
        csv_path = "shampoo_store_variation.csv"
        with open(csv_path, "w", newline="") as f:
            if store_results:
                writer = csv.DictWriter(f, fieldnames=store_results[0].keys())
                writer.writeheader()
                writer.writerows(store_results)

        print(f"\nSaved store variation data to {csv_path}")

        # Create figure
        if store_results:
            df = pd.DataFrame(store_results)

            plt.figure(figsize=(12, 8))

            # Plot average products per week for each store
            plt.subplot(2, 1, 1)
            plt.hist(df['Avg_Products_Per_Week'], bins=20, alpha=0.7, color='blue', edgecolor='black')
            plt.axvline(df['Avg_Products_Per_Week'].mean(), color='red', linestyle='--', linewidth=2, label=f'Mean: {df["Avg_Products_Per_Week"].mean():.2f}')
            plt.title('Shampoo: Distribution of Average Products per Week by Store', fontsize=14, fontweight='bold')
            plt.xlabel('Average Products per Week')
            plt.ylabel('Number of Stores')
            plt.legend()
            plt.grid(True, alpha=0.3)

            # Plot min vs max products per store
            plt.subplot(2, 1, 2)
            plt.scatter(df['Min_Products'], df['Max_Products'], alpha=0.6, color='green', s=50)
            plt.plot([df['Min_Products'].min(), df['Max_Products'].max()],
                    [df['Min_Products'].min(), df['Max_Products'].max()],
                    'r--', alpha=0.7, label='Equal min/max line')
            plt.title('Shampoo: Min vs Max Products per Store Across All Weeks', fontsize=14, fontweight='bold')
            plt.xlabel('Minimum Products in Any Week')
            plt.ylabel('Maximum Products in Any Week')
            plt.legend()
            plt.grid(True, alpha=0.3)

            plt.tight_layout()
            plt.savefig('shampoo_store_variation.png', dpi=300, bbox_inches='tight')
            print("Saved figure to shampoo_store_variation.png")

            # Print summary stats
            print("\n=== Summary Statistics ===")
            print(f"Total stores analyzed: {len(store_results)}")
            print(f"Average products per week per store: {df['Avg_Products_Per_Week'].mean():.2f}")
            print(f"Average weeks per store: {df['N_Weeks'].mean():.2f}")
            print(f"Overall minimum products in any store/week: {df['Min_Products'].min()}")
            print(f"Overall maximum products in any store/week: {df['Max_Products'].max()}")
            print(f"Stores with most consistent assortment (lowest SD): {df.nsmallest(3, 'SD_Products_Per_Week')[['Store', 'SD_Products_Per_Week']].to_string(index=False)}")
            print(f"Stores with most variable assortment (highest SD): {df.nlargest(3, 'SD_Products_Per_Week')[['Store', 'SD_Products_Per_Week']].to_string(index=False)}")

        else:
            print("No store data found to analyze")

    except Exception as e:
        print(f"ERROR: {e}")

    except Exception as e:
        print(f"ERROR: {e}")

if __name__ == "__main__":
    main()