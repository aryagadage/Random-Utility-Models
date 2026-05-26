## partial R2 results w/ bootstrap

set.seed(2139)
svyx <- read.csv("../../data/cand_s2_MTurk.csv")
source("../helper_scripts/cand_s2_datapreprocess_MTurk.R")

Ks <- sort(unique(svyx$condition))
R2s <- rep(NA, length(Ks))
names(R2s) <- paste0("K", Ks)

nwavs <- length(unique(svyx$wave))
Ks.s <- lapply(1:nwavs, function(x) sort(unique(subset(svyx, wave==x)$condition)))
R2s.s <- lapply(Ks.s, function(x) rep(NA, length(x)))
for(i in 1:nwavs){
  names(R2s.s[[i]]) <- paste0("K", Ks.s[[i]])
}

for(l in 1:length(Ks)){
  out <- lm(pref ~ cpartyp + cmarriagep + chealthcarep + cagep,
            data = svyx, subset = condition == Ks[l] & partisan == 1)
  R2s[l] <- summary(out)$r.squared
}

for(i in 1:nwavs){
  for(l in 1:length(Ks.s[[i]])){
    out <- lm(pref ~ cpartyp + cmarriagep + chealthcarep + cagep,
              data = svyx, subset = condition == Ks.s[[i]][l] & wave == i & partisan == 1)
    R2s.s[[i]][l] <- summary(out)$r.squared
  }
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

  R2s.s.b <- lapply(Ks.s, function(x) rep(NA, length(x)))
  for(i in 1:nwavs){
    names(R2s.s.b[[i]]) <- paste0("K", Ks.s[[i]])
  }

  for(l in 1:length(Ks)){
    out.b <- lm(pref ~ cpartyp + cmarriagep + chealthcarep + cagep,
                data = DD.b, subset = condition == Ks[l] & partisan == 1)
    R2s.b[l] <- summary(out.b)$r.squared
  }

  for(i in 1:nwavs){
    for(l in 1:length(Ks.s[[i]])){
      out.b <- lm(pref ~ cpartyp + cmarriagep + chealthcarep + cagep,
                  data = DD.b, subset = condition == Ks.s[[i]][l] & wave == i & partisan == 1)
      R2s.s.b[[i]][l] <- summary(out.b)$r.squared
    }
  }
  R2s.b <- list(R2s.b, R2s.s.b)

  return(R2s.b)
}

out.R2.b <- mclapply(1:B, boot.getR2s, data = svyx, mc.cores = 10)

R2s.sims <- sapply(out.R2.b, function(x) x[[1]])
R2s.s.sims.tmp <- lapply(out.R2.b, function(x) x[[2]])
R2s.s.sims <- as.list(rep(NA, nwavs))
for(i in 1:nwavs){
  R2s.s.sims[[i]] <- sapply(R2s.s.sims.tmp, function(x) x[[i]])
}


# save
save(R2s.sims, file = "../../results/boots/cand_s2_bootresults_MTurk.RData")
save(R2s.s.sims, file = "../../results/boots/cand_s2_bootresults_MTurkbyWaves.RData")

