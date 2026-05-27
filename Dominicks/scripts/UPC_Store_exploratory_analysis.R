# ─────────────────────────────────────────────────────────────────────────────
# UPC_Store_exploratory_analysis.R
#
# For each Dominick's category folder (Oat, Cereal, Cheese, ...):
#   1. Total # of UPCs in the catalog (upc<xxx>.csv).
#   2. Per-store summary from the movement file (w<xxx>.csv):
#        - # distinct UPCs ever offered at the store
#        - # distinct menus (a "menu" = distinct set of UPCs offered in a
#          (STORE, WEEK) occasion, where "offered" means OK == 1)
#          Rows with PRICE == 0 / MOVE == 0 are kept as "available but not
#          chosen" alternatives rather than excluded.
#
# Output: pretty-printed tables to console + tidy CSV summaries.
# ─────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

DATA_ROOT <- file.path(".", "data")        # script lives in Dominicks/, so ./data
OUT_DIR   <- "."

# Discover category folders that contain BOTH a upc*.csv and a w*.csv
categories <- list.dirs(DATA_ROOT, recursive = FALSE, full.names = FALSE)

find_one <- function(folder, pattern) {
  files <- list.files(folder, pattern = pattern, ignore.case = TRUE,
                      full.names = TRUE)
  files <- files[!grepl("\\.zip$", files, ignore.case = TRUE)]
  if (length(files) == 0) return(NA_character_)
  files[1]
}

# ─────────────────────────────────────────────────────────────────────────────
# Pretty-printing helpers
# ─────────────────────────────────────────────────────────────────────────────
hr <- function(char = "─", width = 78) cat(strrep(char, width), "\n", sep = "")

banner <- function(txt) {
  hr("═")
  cat(" ", txt, "\n", sep = "")
  hr("═")
}

fmt_int <- function(x) formatC(x, format = "d", big.mark = ",")

print_df <- function(df, max_rows = Inf) {
  if (nrow(df) == 0) { cat("  (empty)\n"); return(invisible()) }
  if (nrow(df) > max_rows) df <- head(df, max_rows)
  print(as.data.frame(df), row.names = FALSE)
}

# ─────────────────────────────────────────────────────────────────────────────
# Per-category processing
# ─────────────────────────────────────────────────────────────────────────────
cat_summary  <- list()   # one row per category
store_long   <- list()   # per (category, store)

for (cat_name in categories) {

  folder   <- file.path(DATA_ROOT, cat_name)
  upc_file <- find_one(folder, "^upc.*\\.csv$")
  w_file   <- find_one(folder, "^w.*\\.csv$")

  banner(sprintf("Category: %s", cat_name))
  cat("  upc file: ", if (is.na(upc_file)) "(missing)" else basename(upc_file), "\n")
  cat("  move file:", if (is.na(w_file))   "(missing)" else basename(w_file),   "\n")

  if (is.na(upc_file) || is.na(w_file)) {
    cat("  Skipping (one or both files missing).\n\n")
    next
  }

  upc_df <- tryCatch(
    suppressWarnings(read_csv(upc_file, show_col_types = FALSE,
                              progress = FALSE)),
    error = function(e) { cat("  ERROR reading UPC file:", conditionMessage(e),"\n"); NULL }
  )
  w_df <- tryCatch(
    suppressWarnings(read_csv(w_file, show_col_types = FALSE,
                              progress = FALSE)),
    error = function(e) { cat("  ERROR reading move file:", conditionMessage(e),"\n"); NULL }
  )
  if (is.null(upc_df) || is.null(w_df)) { cat("\n"); next }

  # --- Catalog UPC / NITEM counts -----------------------------------------
  n_upc_catalog   <- dplyr::n_distinct(upc_df$UPC)
  has_nitem_cat   <- "NITEM" %in% names(upc_df)
  n_nitem_catalog <- if (has_nitem_cat)
    dplyr::n_distinct(upc_df$NITEM[!is.na(upc_df$NITEM)]) else NA_integer_

  # --- Movement-based offerings -------------------------------------------
  # "Offered" = OK == 1.  Zero-sale rows are kept (they represent UPCs the
  # store carried that week but that had no sales).  If OK is absent, fall
  # back to MOVE > 0.
  has_ok   <- "OK"   %in% names(w_df)
  has_move <- "MOVE" %in% names(w_df)

  if (has_ok) {
    offered <- w_df %>% filter(OK == 1)
    offer_def <- "OK == 1"
  } else if (has_move) {
    offered <- w_df %>% filter(MOVE > 0)
    offer_def <- "MOVE > 0  (fallback: OK not present)"
  } else {
    offered <- w_df
    offer_def <- "all rows  (fallback: no OK/MOVE)"
  }

  cat("  offered rule:", offer_def, "\n")
  cat("  rows in move file: ", fmt_int(nrow(w_df)),
      " | offered rows: ", fmt_int(nrow(offered)), "\n", sep = "")

  # --- Attach NITEM from the catalog --------------------------------------
  # Multiple UPCs can map to the same NITEM (e.g., regular and PREPRICE
  # variants). UPCs not present in the catalog or with NA NITEM are dropped
  # from the NITEM-level views.
  if (has_nitem_cat) {
    upc_to_nitem <- upc_df %>% distinct(UPC, NITEM)
    offered_n <- offered %>%
      inner_join(upc_to_nitem, by = "UPC") %>%
      filter(!is.na(NITEM))
    n_unmatched <- nrow(offered) - nrow(offered_n)
    cat("  offered rows w/ NITEM: ", fmt_int(nrow(offered_n)),
        "  (dropped ", fmt_int(n_unmatched),
        " w/o NITEM in catalog)\n", sep = "")
  } else {
    offered_n <- offered[0, , drop = FALSE]
    cat("  NITEM column not in catalog — NITEM stats skipped.\n")
  }

  # --- Per-store table ----------------------------------------------------
  # 1. (STORE, WEEK) -> menu signature  (one row per occasion)
  occ <- offered %>%
    distinct(STORE, WEEK, UPC) %>%
    arrange(STORE, WEEK, UPC) %>%
    group_by(STORE, WEEK) %>%
    summarise(menu_sig = paste(UPC, collapse = "_"), .groups = "drop")

  # 2. # weeks per store  (denominator for "always / >=95% of choice sets")
  store_weeks <- offered %>%
    distinct(STORE, WEEK) %>%
    count(STORE, name = "n_weeks")

  # 3. (STORE, UPC) -> # weeks that UPC was offered at that store
  store_upc_freq <- offered %>%
    distinct(STORE, WEEK, UPC) %>%
    count(STORE, UPC, name = "n_weeks_offered") %>%
    left_join(store_weeks, by = "STORE") %>%
    mutate(share = n_weeks_offered / n_weeks)

  # 4. Always / >=95% counts per store
  always_95 <- store_upc_freq %>%
    group_by(STORE) %>%
    summarise(
      n_upcs_always = sum(share >= 1.0 - 1e-12),
      n_upcs_95pct  = sum(share >= 0.95),
      .groups       = "drop"
    )

  # 5. Distinct menus per store
  menus_per_store <- occ %>%
    group_by(STORE) %>%
    summarise(n_menus = dplyr::n_distinct(menu_sig), .groups = "drop")

  # 6. Distinct UPCs ever offered per store
  upcs_per_store <- offered %>%
    distinct(STORE, UPC) %>%
    count(STORE, name = "n_upcs")

  # 7. Effective UPCs: collapse UPCs that are present in exactly the same
  # set of menus at this store (i.e. they are never separable in the data,
  # so the choice model can't tell them apart).  Each UPC's "signature" is
  # its sorted set of offered weeks at the store; UPCs with identical
  # signatures form one equivalence class and count as a single product.
  upc_signatures <- offered %>%
    distinct(STORE, WEEK, UPC) %>%
    arrange(STORE, UPC, WEEK) %>%
    group_by(STORE, UPC) %>%
    summarise(week_sig = paste(WEEK, collapse = "_"), .groups = "drop")

  effective_upcs <- upc_signatures %>%
    group_by(STORE) %>%
    summarise(n_effective_upc = dplyr::n_distinct(week_sig), .groups = "drop")

  # --- NITEM-level analogs (parallel to UPC block above) ------------------
  if (nrow(offered_n) > 0) {
    occ_n <- offered_n %>%
      distinct(STORE, WEEK, NITEM) %>%
      arrange(STORE, WEEK, NITEM) %>%
      group_by(STORE, WEEK) %>%
      summarise(menu_sig_n = paste(NITEM, collapse = "_"), .groups = "drop")

    store_nitem_freq <- offered_n %>%
      distinct(STORE, WEEK, NITEM) %>%
      count(STORE, NITEM, name = "n_weeks_offered") %>%
      left_join(store_weeks, by = "STORE") %>%
      mutate(share = n_weeks_offered / n_weeks)

    always_95_n <- store_nitem_freq %>%
      group_by(STORE) %>%
      summarise(
        n_nitems_always = sum(share >= 1.0 - 1e-12),
        n_nitems_95pct  = sum(share >= 0.95),
        .groups         = "drop"
      )

    menus_per_store_n <- occ_n %>%
      group_by(STORE) %>%
      summarise(n_menus_nitem = dplyr::n_distinct(menu_sig_n),
                .groups = "drop")

    nitems_per_store <- offered_n %>%
      distinct(STORE, NITEM) %>%
      count(STORE, name = "n_nitems")
  } else {
    occ_n             <- data.frame(STORE = integer(), menu_sig_n = character())
    nitems_per_store  <- data.frame(STORE = integer(), n_nitems = integer())
    menus_per_store_n <- data.frame(STORE = integer(), n_menus_nitem = integer())
    always_95_n       <- data.frame(STORE = integer(),
                                    n_nitems_always = integer(),
                                    n_nitems_95pct  = integer())
  }

  per_store <- store_weeks %>%
    left_join(upcs_per_store,    by = "STORE") %>%
    left_join(effective_upcs,    by = "STORE") %>%
    left_join(menus_per_store,   by = "STORE") %>%
    left_join(always_95,         by = "STORE") %>%
    left_join(nitems_per_store,  by = "STORE") %>%
    left_join(menus_per_store_n, by = "STORE") %>%
    left_join(always_95_n,       by = "STORE") %>%
    select(STORE, n_weeks,
           n_menus, n_upcs, n_effective_upc, n_upcs_always, n_upcs_95pct,
           n_menus_nitem, n_nitems, n_nitems_always, n_nitems_95pct) %>%
    arrange(STORE)

  # --- Print category summary --------------------------------------------
  cat("\n  Catalog: ",  fmt_int(n_upc_catalog), " UPCs",
      if (!is.na(n_nitem_catalog))
        sprintf(" / %s NITEMs", fmt_int(n_nitem_catalog)) else "",
      "\n", sep = "")
  cat("  Stores:  ",  fmt_int(nrow(per_store)),
      " | total store-week occasions: ",
      fmt_int(sum(per_store$n_weeks)), "\n\n", sep = "")

  cat("  Per-store summary (first 10 stores shown; full table in CSV):\n")
  print_df(per_store, max_rows = 10)

  cat("\n  Across-store distribution:\n")
  cat("    n_upcs          : "); print(summary(per_store$n_upcs))
  cat("    n_effective_upc : "); print(summary(per_store$n_effective_upc))
  cat("    n_menus         : "); print(summary(per_store$n_menus))
  cat("    n_weeks         : "); print(summary(per_store$n_weeks))
  cat("    n_upcs_always   : "); print(summary(per_store$n_upcs_always))
  cat("    n_upcs_95pct    : "); print(summary(per_store$n_upcs_95pct))
  cat("    n_nitems        : "); print(summary(per_store$n_nitems))
  cat("    n_menus_nitem   : "); print(summary(per_store$n_menus_nitem))
  cat("    n_nitems_always : "); print(summary(per_store$n_nitems_always))
  cat("    n_nitems_95pct  : "); print(summary(per_store$n_nitems_95pct))

  # --- Save per-store CSV ------------------------------------------------
  csv_name <- file.path(OUT_DIR,
                        sprintf("store_summary_%s.csv", tolower(cat_name)))
  write.csv(per_store, csv_name, row.names = FALSE)
  cat("\n  Wrote: ", csv_name, "\n\n", sep = "")

  # --- Stash for the cross-category roll-up ------------------------------
  cat_summary[[cat_name]] <- data.frame(
    category                 = cat_name,
    n_upc_catalog            = n_upc_catalog,
    n_nitem_catalog          = n_nitem_catalog,
    n_stores                 = nrow(per_store),
    n_storeweeks             = sum(per_store$n_weeks),
    median_upcs_store        = stats::median(per_store$n_upcs),
    median_effective_upc_store = stats::median(per_store$n_effective_upc),
    median_menus_store       = stats::median(per_store$n_menus),
    median_always_store      = stats::median(per_store$n_upcs_always),
    median_95pct_store       = stats::median(per_store$n_upcs_95pct),
    median_nitems_store      = stats::median(per_store$n_nitems),
    median_menus_nitem_store = stats::median(per_store$n_menus_nitem),
    median_always_nitem_store= stats::median(per_store$n_nitems_always),
    median_95pct_nitem_store = stats::median(per_store$n_nitems_95pct),
    total_distinct_menus     = dplyr::n_distinct(occ$menu_sig),
    total_distinct_menus_nitem = if (nrow(occ_n) > 0)
      dplyr::n_distinct(occ_n$menu_sig_n) else NA_integer_
  )

  store_long[[cat_name]] <- per_store %>% mutate(category = cat_name, .before = 1)
}

# ─────────────────────────────────────────────────────────────────────────────
# Cross-category roll-up
# ─────────────────────────────────────────────────────────────────────────────
banner("CROSS-CATEGORY SUMMARY")

if (length(cat_summary) > 0) {
  roll <- do.call(rbind, cat_summary)
  rownames(roll) <- NULL
  roll <- roll[order(-roll$n_upc_catalog), ]
  print_df(roll)
  write.csv(roll, file.path(OUT_DIR, "category_overview.csv"), row.names = FALSE)
  cat("\nWrote: category_overview.csv\n")
}

if (length(store_long) > 0) {
  all_stores <- do.call(rbind, store_long)
  write.csv(all_stores, file.path(OUT_DIR, "store_summary_all_categories.csv"),
            row.names = FALSE)
  cat("Wrote: store_summary_all_categories.csv  (",
      fmt_int(nrow(all_stores)), " rows)\n", sep = "")
}

cat("\nDone.\n")
