#!/usr/bin/env Rscript
# Summarize runtime behavior of the binary-menu feasibility runs.
#   (1) Average total running time for cases that have a run_summary file (finished).
#   (2) For each n, summary stats (mean, p10, p25, p50, p75, p90) of IP pricing time.

script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) NULL)
if (is.null(script_dir) || !nzchar(script_dir)) script_dir <- "."
results_dir <- file.path(script_dir, "results_runs")
if (!dir.exists(results_dir)) results_dir <- "results_runs"
if (!dir.exists(results_dir)) {
  results_dir <- "/Users/haoge/Dropbox/Research/Random-Utility-Models/feasibility_analysis_binary_menus/results_runs"
}

# ----------------------------------------------------------------------------
# (1) Finished runs: per-n and overall average total runtime
# ----------------------------------------------------------------------------
summary_files <- list.files(results_dir, pattern = "^run_summary_n\\d+\\.csv$",
                            full.names = TRUE)

summary_df <- do.call(rbind, lapply(summary_files, function(f) {
  d <- tryCatch(read.csv(f, stringsAsFactors = FALSE),
                error = function(e) NULL)
  if (is.null(d) || nrow(d) == 0) return(NULL)
  d
}))

cat("=========================================================\n")
cat(" (1) Finished runs (those with run_summary): total_time_s\n")
cat("=========================================================\n")

if (is.null(summary_df) || nrow(summary_df) == 0) {
  cat("No run_summary files found in", results_dir, "\n")
} else {
  per_n <- aggregate(total_time_s ~ n, data = summary_df,
                     FUN = function(x) c(mean = mean(x),
                                         min = min(x),
                                         max = max(x),
                                         n_seeds = length(x)))
  per_n <- do.call(data.frame, per_n)
  names(per_n) <- c("n", "mean_total_s", "min_total_s",
                    "max_total_s", "n_seeds")
  per_n <- per_n[order(per_n$n), ]
  print(per_n, row.names = FALSE, digits = 5)

  cat("\nOverall average total_time_s across all finished seeds:",
      sprintf("%.4f s (n_seeds = %d, n_distinct_n = %d)\n",
              mean(summary_df$total_time_s),
              nrow(summary_df),
              length(unique(summary_df$n))))
}

# ----------------------------------------------------------------------------
# (2) IP pricing-time percentiles per n
# ----------------------------------------------------------------------------
iptime_files <- list.files(results_dir, pattern = "^run_iptimes_n\\d+\\.csv$",
                           full.names = TRUE)

ip_df <- do.call(rbind, lapply(iptime_files, function(f) {
  d <- tryCatch(read.csv(f, stringsAsFactors = FALSE),
                error = function(e) NULL)
  if (is.null(d) || nrow(d) == 0) return(NULL)
  d
}))

cat("\n=========================================================\n")
cat(" (2) IP pricing time per n  (ip_total_s, seconds)\n")
cat("=========================================================\n")

if (is.null(ip_df) || nrow(ip_df) == 0) {
  cat("No run_iptimes files found in", results_dir, "\n")
} else {
  qs <- c(0.10, 0.25, 0.50, 0.75, 0.90)
  ip_by_n <- do.call(rbind, lapply(split(ip_df, ip_df$n), function(g) {
    p <- quantile(g$ip_total_s, probs = qs, na.rm = TRUE, names = FALSE)
    data.frame(
      n        = g$n[1],
      n_iters  = nrow(g),
      n_seeds  = length(unique(g$seed)),
      mean_s   = mean(g$ip_total_s, na.rm = TRUE),
      p10_s    = p[1],
      p25_s    = p[2],
      p50_s    = p[3],
      p75_s    = p[4],
      p90_s    = p[5]
    )
  }))
  ip_by_n <- ip_by_n[order(ip_by_n$n), ]
  print(ip_by_n, row.names = FALSE, digits = 5)
}

# ----------------------------------------------------------------------------
# Optional: write tidy CSVs next to the inputs
# ----------------------------------------------------------------------------
if (exists("per_n")) {
  write.csv(per_n, file.path(results_dir, "summary_total_time_by_n.csv"),
            row.names = FALSE)
}
if (exists("ip_by_n")) {
  write.csv(ip_by_n, file.path(results_dir, "summary_ip_pricing_by_n.csv"),
            row.names = FALSE)
}

cat("\nWrote:\n")
cat("  ", file.path(results_dir, "summary_total_time_by_n.csv"), "\n")
cat("  ", file.path(results_dir, "summary_ip_pricing_by_n.csv"), "\n")
