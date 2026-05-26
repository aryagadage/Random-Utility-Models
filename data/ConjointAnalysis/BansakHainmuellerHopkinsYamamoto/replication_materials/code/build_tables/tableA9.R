library(lmtest)
library(sandwich)
library(texreg)
source("../helper_scripts/vcovCluster.R")


# Load and pre-process data -----------------------------------------------

svyx <- read.csv("../../data/hotel_s2.csv")
source("../helper_scripts/hotel_s2_datapreprocess.R")
fixed <- c("cview","cfurniture","cinternet","cfloor")


# Estimation --------------------------------------------------------------

conds <- sort(unique(svyx$condition))
mods <- list()
dats <- list()

for (i in 1:length(conds)){
  dats[[i]] <- tdat <- subset(svyx,svyx$condition == conds[i])
  mods[[i]] <- lm(paste("pref~",paste(fixed,collapse="+")), tdat)
}

estimates <- list()
clusteredSEs <- list()
clusterpvals <- list()

for (i in 1:length(conds)){
  
  mod <- mods[[i]]
  svyxx <- dats[[i]]
  out <- coeftest(mod,vcov = vcovCluster(mod,factor(svyxx$r_id)))
  estimates[[i]] <- out[,1]
  clusteredSEs[[i]] <- out[,2]
  clusterpvals[[i]] <- out[,4]
  
}


# Results -----------------------------------------------------------------

screenreg(l=mods,
          override.se = clusteredSEs,override.pvalues = clusterpvals,
          stars = 0.05,digits=3, custom.model.names = as.character(conds))

texreg(l=mods, file = "../../results/tables/tableA9.tex",
       override.se = clusteredSEs,override.pvalues = clusterpvals,
       stars = 0.05,digits=3, custom.model.names = as.character(conds))

