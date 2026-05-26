setwd('/Users/haoge/Dropbox/Research/Random-Utility-Models/data/ConjointAnalysis/Zhirkov')

data=read.csv('./data/conjoint_processed_data.csv')


########################################################
######Recode Attributes ################################
########################################################

data$lang_bin <- factor(data$lang_bin, levels = c("Poor","Good"))
data$trip_bin <- factor(data$trip_bin, levels = c("No violation","Violation"))
data$race_cat <- factor(data$race_cat, levels = c("White","Black","Hispanic","Asian"))
data$educ_bin <- factor(data$educ_bin, levels = c("Less than college","College or higher"))

attrs <- c("race_cat","educ_bin","lang_bin","trip_bin")

profiles <- expand.grid(lapply(data[attrs], levels), KEEP.OUT.ATTRS = FALSE,
                        stringsAsFactors = FALSE)
profiles$profile_id <- seq_len(nrow(profiles))

data <- merge(data, profiles, by = attrs, all.x = TRUE, sort = FALSE)
data <- data[order(data$respid, data$scenario, data$profile), ]

stopifnot(nrow(profiles) == 32, !anyNA(data$profile_id))

########################################################
###### Wide form: one row per (respid, scenario) #######
########################################################

wide <- reshape(
  data[, c("respid","scenario","profile","profile_id","rate")],
  idvar     = c("respid","scenario"),
  timevar   = "profile",
  direction = "wide",
  sep       = "_"
)
names(wide) <- sub("^profile_id_", "profile_id_", names(wide))
wide <- wide[order(wide$respid, wide$scenario), ]
rownames(wide) <- NULL

stopifnot(nrow(wide) == nrow(data) / 2, !anyNA(wide$profile_id_1), !anyNA(wide$profile_id_2))

########################################################
###### Choice probabilities by binary choice set #######
########################################################
##
## Choice rule: higher `rate` wins; equal rates count as a tie.
## P(i over j) = (wins_i + 0.5 * ties) / n_total   (half-credit for ties)

cmp <- wide[!is.na(wide$rate_1) & !is.na(wide$rate_2), ]

cmp$lo <- pmin(cmp$profile_id_1, cmp$profile_id_2)
cmp$hi <- pmax(cmp$profile_id_1, cmp$profile_id_2)
# which side is the "lo" profile, and rate-comparison outcome
cmp$rate_lo <- ifelse(cmp$profile_id_1 == cmp$lo, cmp$rate_1, cmp$rate_2)
cmp$rate_hi <- ifelse(cmp$profile_id_1 == cmp$hi, cmp$rate_1, cmp$rate_2)

pair_prob <- aggregate(
  cbind(n_total = 1,
        wins_lo = as.integer(cmp$rate_lo >  cmp$rate_hi),
        wins_hi = as.integer(cmp$rate_lo <  cmp$rate_hi),
        ties    = as.integer(cmp$rate_lo == cmp$rate_hi)) ~ lo + hi,
  data = cmp, FUN = sum
)
pair_prob$P_lo <- with(pair_prob, (wins_lo + 0.5 * ties) / n_total)
pair_prob$P_hi <- 1 - pair_prob$P_lo

## 32x32 choice-probability matrix: P_mat[i, j] = P(profile i chosen over j)
nP <- nrow(profiles)
P_mat   <- matrix(NA_real_, nP, nP, dimnames = list(seq_len(nP), seq_len(nP)))
N_mat   <- matrix(0L,       nP, nP, dimnames = list(seq_len(nP), seq_len(nP)))
for (k in seq_len(nrow(pair_prob))) {
  i <- pair_prob$lo[k]; j <- pair_prob$hi[k]
  P_mat[i, j] <- pair_prob$P_lo[k]; P_mat[j, i] <- pair_prob$P_hi[k]
  N_mat[i, j] <- N_mat[j, i] <- pair_prob$n_total[k]
}

## Long-form CSV matching Feldman_Rehbeck_QE_2022/cleaned_choice_frequency.csv
## Two rows per binary choice set {lo, hi} (lo < hi): one per alternative.
pp <- pair_prob[pair_prob$lo < pair_prob$hi, ]
cleaned_choice_frequency <- data.frame(
  choice_probability = as.numeric(rbind(pp$P_lo, pp$P_hi)),
  set_alt1           = rep(pp$lo, each = 2),
  set_al2            = rep(pp$hi, each = 2),
  alt                = as.integer(rbind(pp$lo, pp$hi))
)
write.csv(cleaned_choice_frequency,
          "./data/cleaned_choice_frequency.csv", row.names = TRUE)

