library(lmtest)
library(sandwich)
library(texreg)
source("../helper_scripts/vcovCluster.R")


# Load and pre-process data -----------------------------------------------

svyx <- read.csv("../../data/cand_s2_SSI.csv")
source("../helper_scripts/cand_s2_datapreprocess_SSI.R")
fixed <- c("cpartyp","cmarriagep","chealthcarep","cagep")


# Estimation --------------------------------------------------------------

conds <- sort(unique(svyx$condition))
mods <- list()
dats <- list()

for (i in 1:length(conds)){
  dats[[i]] <- tdat <- subset(svyx,svyx$partisan == 1 & svyx$condition == conds[i])
  mods[[i]] <- lm(paste("pref~",paste(fixed,collapse="+")), tdat)
}

clusteredSEs <- list()
clusterpvals <- list()

for (i in 1:length(conds)){
  
  mod <- mods[[i]]
  svyxx <- dats[[i]]
  out <- coeftest(mod,vcov = vcovCluster(mod,factor(svyxx$r_id)))
  clusteredSEs[[i]] <- out[,2]
  clusterpvals[[i]] <- out[,4]
  
}


# Results -----------------------------------------------------------------

screenreg(l=mods,
          override.se = clusteredSEs,override.pvalues = clusterpvals,
          stars = 0.05,digits=3, custom.model.names = as.character(conds))

texreg(l=mods, file = "../../results/tables/tableA4.tex",
       override.se = clusteredSEs,override.pvalues = clusterpvals,
       stars = 0.05,digits=3, custom.model.names = as.character(conds))
