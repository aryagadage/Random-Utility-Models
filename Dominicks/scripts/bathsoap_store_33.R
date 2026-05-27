# bathsoap_store_33.R
# -----------------------------------------------------------------------------
# Loads everything for the (BathSoap, STORE = 33) draw.
#
# "Offered" rule: OK == 1.  We treat any UPC the store carried that week
# (i.e. appears in the rectangular weekly file with OK == 1) as part of the
# choice set, regardless of whether it had any sales.  Rows with PRICE == 0 /
# MOVE == 0 are then "available but not chosen" alternatives — not exclusions.
#
# Produces these data frames in the workspace:
#   move_raw    — raw movement rows for STORE 33 (all weeks/UPCs)
#   move_off    — same, after the "offered" rule  (OK == 1)
#   catalog     — upcbat.csv (UPC descriptions / sizes / NITEMs)
#   move_off_w_desc — move_off left-joined to catalog
#   week_menus  — one row per (WEEK), with the sorted list of offered UPCs
#   cf          — D's choice-frequency CSV (one row per (menu, item))
#   upc_map     — D's local_idx -> UPC mapping sidecar
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

dom_dir <- "/Users/haoge/Dropbox/Research/Random-Utility-Models/Dominicks"
setwd(dom_dir)

CATEGORY <- "BathSoap"
STORE_ID <- 33

data_dir <- file.path(".", "data", CATEGORY)
w_path   <- file.path(data_dir, "wbat.csv")
upc_path <- file.path(data_dir, "upcbat.csv")

cf_dir       <- file.path(".", "scripts", "results_choice_freq")
cf_path      <- file.path(cf_dir, sprintf("group013_%s_store%03d.csv", CATEGORY, STORE_ID))
upc_map_path <- file.path(cf_dir, sprintf("group013_%s_store%03d_upc_map.csv", CATEGORY, STORE_ID))

cat(sprintf("Loading %s STORE %d ...\n", CATEGORY, STORE_ID))

# --- Movement file: filter to this store on read for memory ---
move_raw <- read_csv(w_path, show_col_types = FALSE, progress = FALSE) %>%
  filter(STORE == STORE_ID)

move_off <- move_raw %>% filter(OK == 1)

catalog  <- read_csv(upc_path, show_col_types = FALSE, progress = FALSE)
move_off_w_desc <- move_off %>%
  left_join(catalog %>% select(UPC, DESCRIP, SIZE, NITEM), by = "UPC")

# --- One row per (WEEK) with the offered menu ---
week_menus <- move_off %>%
  distinct(WEEK, UPC) %>%
  arrange(WEEK, UPC) %>%
  group_by(WEEK) %>%
  summarise(
    menu_size = n(),
    menu_upcs = list(UPC),
    .groups   = "drop"
  )

# --- D's choice-freq output for this store (may not exist if D failed) ---
if (file.exists(cf_path)) {
  cf      <- read_csv(cf_path,      show_col_types = FALSE)
  upc_map <- read_csv(upc_map_path, show_col_types = FALSE)
} else {
  cf      <- NULL
  upc_map <- NULL
  cat(sprintf("(no choice-freq CSV at %s)\n", cf_path))
}

# --- Sanity check ---
# Note: the summary CSV's n_weeks / n_upcs / n_menus were built under the old
# "OK == 1 & PRICE > 0" rule, so they will not match the counts below.
cat("\n----- Sanity check (OK == 1 offered rule) -----\n")
cat(sprintf("  n_weeks (offered)     : %d\n",
            length(unique(move_off$WEEK))))
cat(sprintf("  n_upcs (offered)      : %d\n",
            length(unique(move_off$UPC))))
cat(sprintf("  n_menus (distinct)    : %d\n",
            length(unique(sapply(week_menus$menu_upcs,
                                 function(u) paste(u, collapse = "_"))))))
cat(sprintf("  min / max menu size   : %d / %d\n",
            min(week_menus$menu_size), max(week_menus$menu_size)))
if (!is.null(cf)) {
  cat(sprintf("  cf rows               : %d\n", nrow(cf)))
  cat(sprintf("  cf distinct menus     : %d\n",
              length(unique(cf$choice_set))))
}
cat("----------------------------------------\n")
