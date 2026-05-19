# compute_min_menu_size.R
# -----------------------------------------------------------------------------
# Builds store_summary_all_categories_min_cs.csv: the existing
# store_summary_all_categories_with_complexity_group.csv (or the plain
# store_summary_all_categories.csv if the complexity version is missing)
# augmented with two columns:
#   min_menu_size — smallest weekly choice set at (STORE, category)
#   max_menu_size — largest  weekly choice set at (STORE, category)
# Menus use the project's "offered" rule: OK == 1 & PRICE > 0 (fallback MOVE > 0).
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
  if (all(c("OK","PRICE") %in% names(df))) {
    df %>% filter(OK == 1, PRICE > 0)
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

  per_store_min <- offered %>%
    distinct(STORE, WEEK, UPC) %>%
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
