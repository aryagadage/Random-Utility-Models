## partial R2 results w/ bootstrap

set.seed(2139)
svyx <- read.csv("../../data/cand_s2_SSI.csv")
source("../helper_scripts/cand_s2_datapreprocess_SSI.R")

Ks <- sort(unique(svyx$condition))
R2s <- rep(NA, length(Ks))
names(R2s) <- paste0("K", Ks)

for(l in 1:length(Ks)){
  out <- lm(pref ~ cpartyp + cmarriagep + chealthcarep + cagep,
            data = svyx, subset = condition == Ks[l] & partisan == 1)
  R2s[l] <- summary(out)$r.squared
}

# block bootstrap for uncertainty estimates
B <- 10000 # n of bootstrap draws
library(parallel)
boot.getR2s <- function(x, data){
  a <- unique(data$r_id)
  rid.b <- sample(a, length(a), repl = TRUE)
  ind.b <- c()
  for(i in 1:length(rid.b)) ind.b <- c(ind.b, which(data$r_id %in% rid.b[i]))
  DD.b <- data[ind.b,]

  R2s.b <- rep(NA, length(Ks))
  names(R2s.b) <- paste0("K", Ks)

  for(l in 1:length(Ks)){
    out.b <- lm(pref ~ cpartyp + cmarriagep + chealthcarep + cagep,
                data = DD.b, subset = condition == Ks[l] & partisan == 1)
    R2s.b[l] <- summary(out.b)$r.squared
  }

  return(R2s.b)
}

out.R2.b <- mclapply(1:B, boot.getR2s, data = svyx, mc.cores = 10)
R2s.sims <- sapply(out.R2.b, function(x) x)

# save
save(R2s.sims, file = "../../results/boots/cand_s2_bootresults_SSI.RData")

