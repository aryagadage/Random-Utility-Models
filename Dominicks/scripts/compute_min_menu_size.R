# compute_min_menu_size.R
# -----------------------------------------------------------------------------
# Builds store_summary_all_categories_min_cs.csv: the existing
# store_summary_all_categories_with_complexity_group.csv (or the plain
# store_summary_all_categories.csv if the complexity version is missing)
# augmented with two columns:
#   min_menu_size — smallest weekly choice set at (STORE, category)
#   max_menu_size — largest  weekly choice set at (STORE, category)
#
# Menus use the project's "offered" rule: OK == 1 (fallback MOVE > 0).  Menu
# size is counted in *effective UPCs*: UPCs that are offered in exactly the
# same set of weeks at a store collapse into one class, since the data can't
# separate them.  See UPC_Store_exploratory_analysis.R (column
# n_effective_upc) for the same equivalence relation.
#
# Slow step: re-reads each category's w*.csv.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

dom_dir <- "/Users/haoge/Dropbox/Research/Random-Utility-Models/Dominicks"
setwd(dom_dir)

DATA_ROOT   <- file.path(".", "data")
SUMMARY_IN  <- file.path(".", "store_summary_all_categories_with_complexity_group.csv")
if (!file.exists(SUMMARY_IN)) {
  SUMMARY_IN <- file.path(".", "store_summary_all_categories.csv")
}
SUMMARY_OUT <- file.path(".", "store_summary_all_categories_min_cs.csv")
cat(sprintf("Base summary: %s\n", SUMMARY_IN))

summary_T  <- read_csv(SUMMARY_IN, show_col_types = FALSE)
categories <- list.dirs(DATA_ROOT, recursive = FALSE, full.names = FALSE)

find_one <- function(folder, pattern) {
  files <- list.files(folder, pattern = pattern, ignore.case = TRUE,
                      full.names = TRUE)
  files <- files[!grepl("\\.zip$", files, ignore.case = TRUE)]
  if (length(files) == 0) return(NA_character_)
  files[1]
}

apply_offered_rule <- function(df) {
  if ("OK" %in% names(df)) {
    df %>% filter(OK == 1)
  } else if ("MOVE" %in% names(df)) {
    df %>% filter(MOVE > 0)
  } else {
    df
  }
}

all_min <- list()
for (cat_name in categories) {
  folder <- file.path(DATA_ROOT, cat_name)
  w_file <- find_one(folder, "^w.*\\.csv$")
  if (is.na(w_file)) {
    cat(sprintf("  %s: no w*.csv — skipping\n", cat_name)); next
  }

  cat(sprintf("Processing %s (%s) ...\n", cat_name, basename(w_file)))
  w_df    <- suppressWarnings(read_csv(w_file, show_col_types = FALSE,
                                       progress = FALSE))
  offered <- apply_offered_rule(w_df)

  # Each (STORE, UPC) has a "signature" = sorted set of offered weeks.
  # UPCs with identical signatures at the same store are inseparable and
  # collapse to a single effective UPC.  We label each class by its
  # smallest constituent UPC (eff_id) and count distinct eff_ids per menu.
  upc_sig <- offered %>%
    distinct(STORE, WEEK, UPC) %>%
    arrange(STORE, UPC, WEEK) %>%
    group_by(STORE, UPC) %>%
    summarise(week_sig = paste(WEEK, collapse = "_"), .groups = "drop") %>%
    group_by(STORE, week_sig) %>%
    mutate(eff_id = min(UPC)) %>%
    ungroup() %>%
    select(STORE, UPC, eff_id)

  per_store_min <- offered %>%
    distinct(STORE, WEEK, UPC) %>%
    inner_join(upc_sig, by = c("STORE", "UPC")) %>%
    distinct(STORE, WEEK, eff_id) %>%
    count(STORE, WEEK, name = "menu_size") %>%
    group_by(STORE) %>%
    summarise(
      min_menu_size = min(menu_size),
      max_menu_size = max(menu_size),
      .groups       = "drop"
    ) %>%
    mutate(category = cat_name)

  all_min[[cat_name]] <- per_store_min
}

all_min_df <- bind_rows(all_min)

out <- summary_T %>%
  left_join(all_min_df, by = c("category", "STORE"))

write_csv(out, SUMMARY_OUT)
cat(sprintf("\nWrote: %s  (%d rows, %d cols)\n",
            SUMMARY_OUT, nrow(out), ncol(out)))
