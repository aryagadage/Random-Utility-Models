library(lmtest)
library(sandwich)
library(ggplot2)
library(ggthemes)
source("../helper_scripts/vcovCluster.R")


# Load and pre-process data -----------------------------------------------

svyx <- read.csv("../../data/cand_s2_MTurk.csv")
source("../helper_scripts/cand_s2_datapreprocess_MTurk.R")
fixed <- c("cpartyp","cmarriagep","chealthcarep","cagep")


# Estimation --------------------------------------------------------------

conds <- sort(unique(svyx$condition))
mods <- list()
dats <- list()

for (i in 1:length(conds)){
  dats[[i]] <- tdat <- subset(svyx,svyx$partisan == 1 & 
                                svyx$condition == conds[i])
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

party.est <- as.numeric(lapply(estimates, "[[", 2))
party.se <- as.numeric(lapply(clusteredSEs, "[[", 2))
party.low <- party.est - 1.96*party.se
party.high <- party.est + 1.96*party.se

ssmarr.est <- as.numeric(lapply(estimates, "[[", 3))
ssmarr.se <- as.numeric(lapply(clusteredSEs, "[[", 3))
ssmarr.low <- ssmarr.est - 1.96*ssmarr.se
ssmarr.high <- ssmarr.est + 1.96*ssmarr.se

health.est <- as.numeric(lapply(estimates, "[[", 4))
health.se <- as.numeric(lapply(clusteredSEs, "[[", 4))
health.low <- health.est - 1.96*health.se
health.high <- health.est + 1.96*health.se

age54.est <- as.numeric(lapply(estimates, "[[", 5))
age54.se <- as.numeric(lapply(clusteredSEs, "[[", 5))
age54.low <- age54.est - 1.96*age54.se
age54.high <- age54.est + 1.96*age54.se

age72.est <- as.numeric(lapply(estimates, "[[", 6))
age72.se <- as.numeric(lapply(clusteredSEs, "[[", 6))
age72.low <- age72.est - 1.96*age72.se
age72.high <- age72.est + 1.96*age72.se


# View results ------------------------------------------------------------

sdat <- data.frame(ests = c(party.est,ssmarr.est,health.est,age54.est,age72.est),
                   lows = c(party.low,ssmarr.low,health.low,age54.low,age72.low),
                   highs = c(party.high,ssmarr.high,health.high,age54.high,age72.high),
                   condition = rep(factor(paste("K",conds,sep=""),levels = paste("K",conds,sep="")),5),
                   conds = rep(conds,5),
                   Attribute = factor(rep(c("Own Party","Own SS Marriage Position","Own Healthcare Position", "Age 54", "Age 72"),each=length(conds)),
                                      levels = c("Own Party","Own SS Marriage Position","Own Healthcare Position", "Age 54", "Age 72")))
sdat


# Shaping data improve plotting -------------------------------------------

#Creating dummy data for better plotting aesthetics
ssdat <- sdat
ssdat$fakelow <- NA
ssdat$fakehigh <- NA
levs <- unique(ssdat$Attribute)
for (i in 1:length(levs)){
  subdf <- subset(ssdat,ssdat$Attribute == levs[i])
  lowest <- min(subdf$lows)
  highest <- max(subdf$highs)
  delt <- highest - lowest
  spread <- 0.17
  to.add <- (spread - delt)/2
  ssdat$fakelow[ssdat$Attribute == levs[i]] <- lowest - to.add
  ssdat$fakehigh[ssdat$Attribute == levs[i]] <- highest + to.add
}
ssdat$agedummy <- NA
ssdat$agedummy[ssdat$Attribute == "Age 54" | ssdat$Attribute == "Age 72"] <- 0
ssdat$agedummy2 <- NA
ssdat$agedummy2[ssdat$Attribute == "Age 54" | ssdat$Attribute == "Age 72"] <- -10
ssdat$agedummy2[ssdat$Attribute == "Age 54"][1] <- 50
ssdat$agedummy2[ssdat$Attribute == "Age 72"][1] <- 50


# Plotting ----------------------------------------------------------------

the.color <- "dodgerblue"
pdf("../../results/figures/fig4.pdf",width=7,height=9)
ggplot(ssdat,aes(y=ests,x=conds)) + stat_smooth(aes(x=agedummy2,y=agedummy),fullrange=TRUE,method="lm",color="black",size=0.65) +
  geom_point(size=1.75,position=position_dodge(width=.5),color=the.color) + theme_economist(dkpanel = T) + 
  geom_errorbar(aes(ymin=lows,ymax=highs,width=0.5),position=position_dodge(width=c(0.5)),size=0.35,color=the.color) +
  ylab("AMCEs with 95% CI") + theme(axis.text.x = element_text(size=10,face='bold'), axis.title.x = element_text(size=10)) +
  theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=10)) +
  theme(legend.text = element_text(size=10), legend.title = element_text(size=10)) +
  facet_wrap(~Attribute,ncol=1,scales = "free_y") + theme(panel.margin=unit(0.35, "lines")) +
  theme(strip.background = element_rect(color="black",fill="#6794a7"), strip.text = element_text(size=10,face="bold")) +
  scale_x_continuous(breaks = conds) + xlab("Total Number of Fillers Shown") +
  geom_line(color=the.color,linetype=2) + geom_blank(aes(y=fakelow)) + geom_blank(aes(y=fakehigh)) + coord_cartesian(xlim=c(0,35))
dev.off()
