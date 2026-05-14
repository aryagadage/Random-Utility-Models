upcoat=read.csv("/Users/haoge/Dropbox/Research/Random-Utility-Models/Dominicks/oatmeal csv/upcoat.csv")
woat=read.csv("/Users/haoge/Dropbox/Research/Random-Utility-Models/Dominicks/oatmeal csv/woat.csv")

if (interactive()) View(woat)

# UPCs in the catalog with NO sales record in any store/week
upc_with_sales <- unique(woat$UPC[woat$MOVE > 0])
upc_no_sales   <- setdiff(woat$UPC, upc_with_sales)
length(upc_no_sales)
upcoat[upcoat$UPC %in% upc_no_sales, ]

# (Optional) UPCs that appear in woat but only with MOVE == 0 in every record
upc_only_zero <- setdiff(unique(woat$UPC), upc_with_sales)
length(upc_only_zero)

# ─────────────────────────────────────────────────────────────────────────────
# Build store-week choice sets
#   "Offered" = OK == 1 & PRICE > 0  (clean record, actually priced/stocked)
#   Occasion  = (STORE, WEEK)
#   Choice set = set of UPCs offered in that occasion
# ─────────────────────────────────────────────────────────────────────────────
library(dplyr)

# 1. Long form: one row per (STORE, WEEK, UPC) that was offered, with attrs.
offered <- woat %>%
  filter(OK == 1, PRICE > 0) %>%
  select(STORE, WEEK, UPC, MOVE, PRICE, SALE) %>%
  left_join(upcoat %>% select(UPC, DESCRIP, SIZE, NITEM), by = "UPC") %>%
  group_by(STORE, WEEK) %>%
  mutate(
    set_size   = n(),                                # # of UPCs in choice set
    total_move = sum(MOVE),                          # total units sold in occasion
    share      = ifelse(total_move > 0, MOVE / total_move, 0)
  ) %>%
  ungroup()

# 2. Wide / compact summary: one row per store-week occasion
choicesets <- offered %>%
  arrange(STORE, WEEK, UPC) %>%
  group_by(STORE, WEEK) %>%
  summarise(
    set_size   = n(),
    total_move = sum(MOVE),
    upc_list   = paste(UPC, collapse = "_"),        # canonical menu signature
    .groups    = "drop"
  )

cat("Offered (long) rows:        ", nrow(offered),    "\n")
cat("Store-week occasions:       ", nrow(choicesets), "\n")
cat("Distinct menus (UPC sets):  ", n_distinct(choicesets$upc_list), "\n")
cat("Choice set size — summary:\n"); print(summary(choicesets$set_size))

# 3. Save
write.csv(offered,    "oatmeal_offered_long.csv",   row.names = FALSE)
write.csv(choicesets, "oatmeal_storeweek_sets.csv", row.names = FALSE)

# ─────────────────────────────────────────────────────────────────────────────
# Distribution of choice-set cardinality (|C_t|)
# ─────────────────────────────────────────────────────────────────────────────
size_dist <- as.data.frame(table(choicesets$set_size))
names(size_dist) <- c("set_size", "n_occasions")
size_dist$set_size    <- as.integer(as.character(size_dist$set_size))
size_dist$share       <- size_dist$n_occasions / sum(size_dist$n_occasions)
size_dist$cum_share   <- cumsum(size_dist$share)

cat("\nChoice-set cardinality distribution:\n")
print(size_dist)

cat("\nQuantiles of |C_t|:\n")
print(quantile(choicesets$set_size, c(0, .05, .25, .5, .75, .95, 1)))

write.csv(size_dist, "oatmeal_choiceset_size_dist.csv", row.names = FALSE)

png("oatmeal_choiceset_size_hist.png", width = 900, height = 500)
hist(choicesets$set_size, breaks = seq(0, max(choicesets$set_size) + 1, 1),
     col = "#1f77b4", border = "white",
     main = "Distribution of store-week choice-set cardinality",
     xlab = "|C_t| (# UPCs offered)", ylab = "# store-weeks")
abline(v = median(choicesets$set_size), col = "red", lty = 2, lwd = 2)
dev.off()

# ─────────────────────────────────────────────────────────────────────────────
# Drop small choice sets:  keep only occasions with |C_t| > 30
# ─────────────────────────────────────────────────────────────────────────────
MIN_SIZE <- 30
keep_occ <- choicesets %>% filter(set_size > MIN_SIZE) %>% select(STORE, WEEK)

cat("\nFiltering choice sets with |C_t| > ", MIN_SIZE, "\n", sep = "")
cat("  Occasions before:", nrow(choicesets), "\n")
cat("  Occasions after: ", nrow(keep_occ),  "\n")

offered    <- offered    %>% semi_join(keep_occ, by = c("STORE","WEEK"))
choicesets <- choicesets %>% semi_join(keep_occ, by = c("STORE","WEEK"))

cat("  Offered (long) rows after filter:", nrow(offered), "\n")

# Overwrite saved files with the filtered versions
write.csv(offered,    "oatmeal_offered_long.csv",   row.names = FALSE)
write.csv(choicesets, "oatmeal_storeweek_sets.csv", row.names = FALSE)

# ─────────────────────────────────────────────────────────────────────────────
# Are there (STORE, WEEK) occasions where >1 UPC share the same NITEM?
# ─────────────────────────────────────────────────────────────────────────────
nitem_dup <- offered %>%
  filter(!is.na(NITEM)) %>%
  group_by(STORE, WEEK, NITEM) %>%
  summarise(n_upc = n_distinct(UPC), upcs = paste(sort(unique(UPC)), collapse = "|"),
            .groups = "drop") %>%
  filter(n_upc > 1)

cat("\nOccasions with >1 UPC sharing the same NITEM:\n")
cat("  Rows (STORE,WEEK,NITEM with collision):", nrow(nitem_dup), "\n")
cat("  Distinct occasions affected:           ",
    n_distinct(paste(nitem_dup$STORE, nitem_dup$WEEK)), "\n")
cat("  Distinct NITEMs that ever collide:     ",
    n_distinct(nitem_dup$NITEM), "\n")
if (nrow(nitem_dup) > 0) {
  cat("\n  Top NITEMs by # collisions:\n")
  print(nitem_dup %>% count(NITEM, sort = TRUE) %>% head(10))
}

# ─────────────────────────────────────────────────────────────────────────────
# Most frequent products
#   - by # store-week occasions offered  (presence on shelf)
#   - by total MOVE                      (units sold)
# ─────────────────────────────────────────────────────────────────────────────
n_occ_total <- n_distinct(paste(offered$STORE, offered$WEEK))

product_freq <- offered %>%
  group_by(UPC, DESCRIP, SIZE, NITEM) %>%
  summarise(
    n_occasions = n(),
    occ_share   = n() / n_occ_total,
    total_move  = sum(MOVE),
    avg_price   = mean(PRICE),
    .groups     = "drop"
  ) %>%
  arrange(desc(n_occasions))

cat("\nTop 15 UPCs by # store-week occasions offered:\n")
print(product_freq %>% select(UPC, DESCRIP, SIZE, n_occasions, occ_share, total_move) %>% head(15))

cat("\nTop 15 UPCs by total units sold (MOVE):\n")
print(product_freq %>% arrange(desc(total_move)) %>%
      select(UPC, DESCRIP, SIZE, total_move, n_occasions) %>% head(15))

write.csv(product_freq, "oatmeal_product_frequency.csv", row.names = FALSE)

# ─────────────────────────────────────────────────────────────────────────────
# Restrict alternatives: keep only the K best-selling UPCs as named alternatives;
# everything else gets aggregated into a single "outside option" within each
# store-week. Outside option is treated as ALWAYS available.
# ─────────────────────────────────────────────────────────────────────────────
K <- 23
top_upcs <- product_freq %>% arrange(desc(total_move)) %>% slice_head(n = K) %>% pull(UPC)
cat("\nTop", K, "UPCs by MOVE (kept as named alternatives):\n")
print(product_freq %>% filter(UPC %in% top_upcs) %>%
      arrange(desc(total_move)) %>% select(UPC, DESCRIP, SIZE, total_move))

# In each occasion, the choice set among NAMED alternatives is the subset
# of top_upcs that is offered there (outside option is always present).
restricted <- offered %>%
  filter(UPC %in% top_upcs) %>%
  arrange(STORE, WEEK, UPC) %>%
  group_by(STORE, WEEK) %>%
  summarise(
    n_named  = n(),
    upc_list = paste(UPC, collapse = "_"),
    .groups  = "drop"
  )

cat("\nWith K =", K, "best-selling alternatives + 1 outside option:\n")
cat("  Store-week occasions covered:        ", nrow(restricted),
    "/", n_occ_total, "\n")
cat("  Distinct choice sets (over named):   ", n_distinct(restricted$upc_list), "\n")
cat("  # named alternatives offered — summary:\n")
print(summary(restricted$n_named))

cat("\n  Top 10 most common restricted choice sets:\n")
print(restricted %>% count(upc_list, sort = TRUE) %>% head(10) %>%
      mutate(set_size = lengths(strsplit(upc_list, "_"))) %>%
      select(set_size, n, upc_list))





# ─────────────────────────────────────────────────────────────────────────────
# UPC co-occurrence graph
#   For each pair (i, j) of UPCs, count store-week occasions in which both
#   appear in the choice set. Implemented as A' A on a sparse incidence matrix
#   I[occasion, UPC] = 1 iff UPC offered in that occasion.
# ─────────────────────────────────────────────────────────────────────────────
library(Matrix)

offered <- offered %>%
  mutate(occasion = paste(STORE, WEEK, sep = "_"))

occ_levels <- unique(offered$occasion)
upc_levels <- sort(unique(offered$UPC))

I <- sparseMatrix(
  i = match(offered$occasion, occ_levels),
  j = match(offered$UPC,      upc_levels),
  x = 1,
  dims = c(length(occ_levels), length(upc_levels)),
  dimnames = list(NULL, as.character(upc_levels))
)

# Co-occurrence matrix: diag = # occasions UPC appears, off-diag = # joint occasions
C <- crossprod(I)               # symmetric, length(upc_levels) x length(upc_levels)

cat("Co-occurrence matrix:        ", nrow(C), "x", ncol(C), "\n")
cat("Total UPCs in graph:         ", nrow(C), "\n")
cat("Max pair co-occurrence:      ", max(C - Diagonal(nrow(C), diag(C))), "\n")

# Edge list (long form): UPC_i, UPC_j, count, with i < j (no self-loops)
Ct <- as(C, "TsparseMatrix")
keep <- Ct@i < Ct@j                              # i,j are 0-indexed
edges <- data.frame(
  UPC_i = upc_levels[Ct@i[keep] + 1],
  UPC_j = upc_levels[Ct@j[keep] + 1],
  count = Ct@x[keep]
)
edges <- edges[order(-edges$count), ]
cat("Co-occurrence edges (pairs): ", nrow(edges), "\n")

# Save: full matrix (UPCs as row/col names) and tidy edge list
write.csv(as.matrix(C), "oatmeal_cooccurrence_matrix.csv")
write.csv(edges,        "oatmeal_cooccurrence_edges.csv", row.names = FALSE)

# ─────────────────────────────────────────────────────────────────────────────
# Largest independent set in the co-occurrence graph
#   Nodes  = UPCs
#   Edge (i,j) iff UPCs i and j ever co-appear in some store-week choice set
#   Independent set = UPCs that are pairwise NEVER offered together
# ─────────────────────────────────────────────────────────────────────────────
library(igraph)

A <- as.matrix(C)
diag(A) <- 0
A <- (A > 0) * 1                                  # binary adjacency

g <- graph_from_adjacency_matrix(A, mode = "undirected", diag = FALSE)
cat("Co-occurrence graph: ", vcount(g), "nodes,", ecount(g), "edges\n")
cat("Graph density:       ", round(edge_density(g), 3), "\n")

# Exact maximum independent set (= maximum clique in the complement graph).
# 96 nodes is feasible for igraph's exact solver in most cases.
mis_list <- largest_ivs(g)                        # list of all maximum IS
mis      <- mis_list[[1]]
mis_upcs <- as.numeric(names(V(g))[mis])

cat("\nLargest independent set size: ", length(mis), "\n")
cat("Number of maximum IS found:   ", length(mis_list), "\n")

mis_df <- upcoat[upcoat$UPC %in% mis_upcs, c("UPC","DESCRIP","SIZE","NITEM")]
print(mis_df)

write.csv(mis_df, "oatmeal_max_independent_set.csv", row.names = FALSE)

# ─────────────────────────────────────────────────────────────────────────────
# Intersection of all store-week menus
#   = UPCs offered in EVERY occasion. Equivalently, UPCs whose # occasions
#   offered equals the total number of occasions.
# ─────────────────────────────────────────────────────────────────────────────
n_occ_total <- n_distinct(paste(offered$STORE, offered$WEEK))

upc_occ_counts <- offered %>%
  group_by(UPC) %>%
  summarise(n_occasions = n_distinct(paste(STORE, WEEK)), .groups = "drop")

intersection_upcs <- upc_occ_counts$UPC[upc_occ_counts$n_occasions == n_occ_total]

cat("\nTotal store-week occasions:        ", n_occ_total, "\n")
cat("Intersection of all menus — size:  ", length(intersection_upcs), "\n")
if (length(intersection_upcs) == 0) {
  cat("Intersection is EMPTY.\n")
  cat("Closest-to-universal UPCs (top 10 by # occasions offered):\n")
  print(upc_occ_counts %>%
          arrange(desc(n_occasions)) %>%
          mutate(occ_share = n_occasions / n_occ_total) %>%
          left_join(upcoat %>% select(UPC, DESCRIP, SIZE), by = "UPC") %>%
          head(10))
} else {
  print(upcoat[upcoat$UPC %in% intersection_upcs,
               c("UPC","DESCRIP","SIZE","NITEM")])
}

# ─────────────────────────────────────────────────────────────────────────────
# Top-K coverage curve
#   Rank UPCs by diagonal frequency (# occasions offered, descending).
#   For k = 1..20, count occasions whose menu contains ALL of the top-k UPCs.
#   Uses the incidence matrix I [occasion x UPC]: an occasion contains the
#   top-k iff rowSums(I[, top_k]) == k.
# ─────────────────────────────────────────────────────────────────────────────
diag_freq <- diag(C)                              # named by UPC string
ord_upcs  <- names(sort(diag_freq, decreasing = TRUE))
K_TOP     <- 50

topk_cov <- data.frame(
  k           = integer(),
  upc_added   = character(),
  descrip     = character(),
  size        = character(),
  n_alone     = integer(),         # # occasions containing this UPC
  n_topk      = integer(),         # # occasions containing ALL top-k UPCs
  share_topk  = numeric()
)

cum_have <- rep(TRUE, nrow(I))                    # logical mask over occasions
for (k in seq_len(K_TOP)) {
  u  <- ord_upcs[k]
  has_u   <- as.logical(I[, u] > 0)
  cum_have <- cum_have & has_u
  meta <- upcoat[match(as.numeric(u), upcoat$UPC), ]
  topk_cov <- rbind(topk_cov, data.frame(
    k          = k,
    upc_added  = u,
    descrip    = meta$DESCRIP,
    size       = meta$SIZE,
    n_alone    = sum(has_u),
    n_topk     = sum(cum_have),
    share_topk = sum(cum_have) / nrow(I)
  ))
}

cat("\nTop-20 UPCs (ranked by # occasions offered) and cumulative coverage:\n")
print(topk_cov, row.names = FALSE)

write.csv(topk_cov, "oatmeal_topk_coverage.csv", row.names = FALSE)

# ─────────────────────────────────────────────────────────────────────────────
# Filter occasions to those whose menu contains BOTH:
#   - the TOP-8 most frequent UPCs overall (all happen to be Quaker), AND
#   - the TOP-6 most frequent Nabisco / Cream-of-Wheat UPCs (prefix 13130)
# ─────────────────────────────────────────────────────────────────────────────
K_Q <- 8
K_N <- 6
top8_q <- as.numeric(ord_upcs[seq_len(K_Q)])

nab_freq <- offered %>%
  group_by(UPC) %>%
  summarise(n_occ = n_distinct(paste(STORE, WEEK)), .groups = "drop") %>%
  mutate(prefix = substr(sprintf("%010.0f", UPC), 1, 5)) %>%
  filter(prefix == "13130") %>%
  arrange(desc(n_occ))
top6_n <- nab_freq$UPC[seq_len(K_N)]

keep_upcs <- c(top8_q, top6_n)
cat("\nRequiring top-", K_Q, " Quaker (by frequency) + top-", K_N,
    " Nabisco UPCs in every menu:\n", sep = "")
cat("  Quaker:  ", paste(top8_q, collapse = ", "), "\n")
cat("  Nabisco: ", paste(top6_n, collapse = ", "), "\n")

keep_occ <- offered %>%
  filter(UPC %in% keep_upcs) %>%
  group_by(STORE, WEEK) %>%
  summarise(n_keep = n_distinct(UPC), .groups = "drop") %>%
  filter(n_keep == length(keep_upcs)) %>%
  select(STORE, WEEK)

n_before <- n_distinct(paste(offered$STORE, offered$WEEK))
n_after  <- nrow(keep_occ)
cat("  Occasions before: ", n_before, "\n")
cat("  Occasions after:  ", n_after,  "\n")
cat("  Occasions dropped:", n_before - n_after, "\n")

offered    <- offered    %>% semi_join(keep_occ, by = c("STORE","WEEK"))
choicesets <- choicesets %>% semi_join(keep_occ, by = c("STORE","WEEK"))

cat("  Offered (long) rows after filter:", nrow(offered), "\n")
cat("  Distinct menus after filter:     ", n_distinct(choicesets$upc_list), "\n")

write.csv(offered,    "oatmeal_offered_long.csv",   row.names = FALSE)
write.csv(choicesets, "oatmeal_storeweek_sets.csv", row.names = FALSE)

# ─────────────────────────────────────────────────────────────────────────────
# Connectivity of the co-occurrence graph on the FILTERED choice sets
# ─────────────────────────────────────────────────────────────────────────────
occ_levels2 <- unique(paste(offered$STORE, offered$WEEK, sep = "_"))
upc_levels2 <- sort(unique(offered$UPC))

I2 <- sparseMatrix(
  i = match(paste(offered$STORE, offered$WEEK, sep = "_"), occ_levels2),
  j = match(offered$UPC, upc_levels2),
  x = 1,
  dims = c(length(occ_levels2), length(upc_levels2)),
  dimnames = list(NULL, as.character(upc_levels2))
)
C2 <- crossprod(I2)
A2 <- as.matrix(C2); diag(A2) <- 0; A2 <- (A2 > 0) * 1
g2 <- graph_from_adjacency_matrix(A2, mode = "undirected", diag = FALSE)

cat("\n── Co-occurrence graph (filtered choice sets) ──\n")
cat("Nodes (UPCs):        ", vcount(g2), "\n")
cat("Edges:               ", ecount(g2), "\n")
cat("Density:             ", round(edge_density(g2), 4), "\n")
cat("Connected:           ", is_connected(g2), "\n")
cat("# components:        ", components(g2)$no, "\n")
cat("Largest component:   ", max(components(g2)$csize), "\n")
cat("Diameter (unweighted):", diameter(g2, directed = FALSE), "\n")
cat("Mean shortest path:  ", round(mean_distance(g2, directed = FALSE), 3), "\n")
cat("Global clustering:   ", round(transitivity(g2, type = "global"), 4), "\n")

deg2 <- degree(g2)
cat("Degree summary:\n"); print(summary(deg2))
cat("# isolated nodes (deg 0):  ", sum(deg2 == 0), "\n")
cat("# fully-connected (deg n-1):", sum(deg2 == vcount(g2) - 1), "\n")

W2 <- as.matrix(C2); diag(W2) <- 0
ut <- W2[upper.tri(W2)]
cat("\nEdge-weight (joint-offering count) summary over realized edges:\n")
print(summary(ut[ut > 0]))
cat("# unrealized pairs (never co-offered):", sum(ut == 0),
    "of", length(ut), "\n")

gw2 <- graph_from_adjacency_matrix(W2, mode = "undirected",
                                   weighted = TRUE, diag = FALSE)
mc2 <- min_cut(gw2, capacity = E(gw2)$weight, value.only = FALSE)
cat("\nGlobal min-cut value (weighted):", mc2$value, "\n")
cat("Min-cut partition sizes:        ", sapply(mc2$partition, length), "\n")

# ─────────────────────────────────────────────────────────────────────────────
# Co-occurrence graph after collapsing the 14 required UPCs (top-8 Quaker +
# top-6 Nabisco) into one synthetic node "CORE14".
# ─────────────────────────────────────────────────────────────────────────────
cat("\n── Co-occurrence graph (CORE14 collapsed) ──\n")

offered_c14 <- offered %>%
  mutate(node = ifelse(UPC %in% keep_upcs, "CORE14", as.character(UPC))) %>%
  distinct(STORE, WEEK, node)

occ_c14  <- offered_c14 %>% mutate(occ = paste(STORE, WEEK, sep = "_"))
nodes_c14 <- sort(unique(occ_c14$node))
occs_c14  <- sort(unique(occ_c14$occ))
I_c14 <- sparseMatrix(
  i = match(occ_c14$occ,  occs_c14),
  j = match(occ_c14$node, nodes_c14),
  x = 1, dims = c(length(occs_c14), length(nodes_c14)),
  dimnames = list(occs_c14, nodes_c14)
)
C_c14 <- crossprod(I_c14)
W_c14 <- as.matrix(C_c14); diag(W_c14) <- 0
g_c14 <- graph_from_adjacency_matrix(W_c14 > 0, mode = "undirected", diag = FALSE)

cat("Nodes:                ", vcount(g_c14), "\n")
cat("Edges:                ", ecount(g_c14), "\n")
cat("Density:              ", round(edge_density(g_c14), 4), "\n")
cat("Connected:            ", is_connected(g_c14), "\n")
cat("# components:         ", components(g_c14)$no, "\n")
cat("Diameter (unweighted):", diameter(g_c14, weights = NA), "\n")
cat("Mean shortest path:   ", round(mean_distance(g_c14), 3), "\n")
cat("Global clustering:    ", round(transitivity(g_c14, type = "global"), 4), "\n")
deg_c14 <- degree(g_c14)
cat("Degree summary:\n"); print(summary(deg_c14))
cat("# isolated nodes (deg 0):  ", sum(deg_c14 == 0), "\n")
cat("# fully-connected (deg n-1):", sum(deg_c14 == vcount(g_c14) - 1), "\n")

ut_c14 <- W_c14[upper.tri(W_c14)]
cat("\nEdge-weight summary over realized edges:\n")
print(summary(ut_c14[ut_c14 > 0]))
cat("# unrealized pairs (never co-offered):", sum(ut_c14 == 0),
    "of", length(ut_c14), "\n")

# Edges incident to the CORE14 super-node
core_idx <- which(nodes_c14 == "CORE14")
cat("\nCORE14 super-node degree:", deg_c14[core_idx], "\n")
cat("CORE14 weight to neighbors — summary:\n")
print(summary(W_c14[core_idx, -core_idx]))

gw_c14 <- graph_from_adjacency_matrix(W_c14, mode = "undirected",
                                      weighted = TRUE, diag = FALSE)
mc_c14 <- min_cut(gw_c14, capacity = E(gw_c14)$weight, value.only = FALSE)
cat("\nGlobal min-cut value (weighted):", mc_c14$value, "\n")
cat("Min-cut partition sizes:        ", sapply(mc_c14$partition, length), "\n")

# Choice-set sizes after CORE14 collapse
sizes_c14 <- offered_c14 %>%
  group_by(STORE, WEEK) %>%
  summarise(set_size = n_distinct(node), .groups = "drop")
cat("\nCollapsed (CORE14) choice-set size summary:\n")
print(summary(sizes_c14$set_size))
cat("\nDistribution:\n")
print(as.data.frame(table(sizes_c14$set_size)))

# Distinct collapsed menus
menus_c14 <- offered_c14 %>%
  arrange(STORE, WEEK, node) %>%
  group_by(STORE, WEEK) %>%
  summarise(upc_list = paste(node, collapse = "_"), .groups = "drop")
cat("\nDistinct collapsed menus (CORE14):", n_distinct(menus_c14$upc_list), "\n")

