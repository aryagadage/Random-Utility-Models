library(lmtest)
library(sandwich)
library(ggplot2)
library(ggthemes)
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

view.est <- as.numeric(lapply(estimates, "[[", 2))
view.se <- as.numeric(lapply(clusteredSEs, "[[", 2))
view.low <- view.est - 1.96*view.se
view.high <- view.est + 1.96*view.se

furniture.est <- as.numeric(lapply(estimates, "[[", 3))
furniture.se <- as.numeric(lapply(clusteredSEs, "[[", 3))
furniture.low <- furniture.est - 1.96*furniture.se
furniture.high <- furniture.est + 1.96*furniture.se

internet.est <- as.numeric(lapply(estimates, "[[", 4))
internet.se <- as.numeric(lapply(clusteredSEs, "[[", 4))
internet.low <- internet.est - 1.96*internet.se
internet.high <- internet.est + 1.96*internet.se

floor10.est <- as.numeric(lapply(estimates, "[[", 5))
floor10.se <- as.numeric(lapply(clusteredSEs, "[[", 5))
floor10.low <- floor10.est - 1.96*floor10.se
floor10.high <- floor10.est + 1.96*floor10.se

floor20.est <- as.numeric(lapply(estimates, "[[", 6))
floor20.se <- as.numeric(lapply(clusteredSEs, "[[", 6))
floor20.low <- floor20.est - 1.96*floor20.se
floor20.high <- floor20.est + 1.96*floor20.se


# View results ------------------------------------------------------------

sdat <- data.frame(ests = c(view.est,furniture.est,internet.est,floor10.est,floor20.est),
                   lows = c(view.low,furniture.low,internet.low,floor10.low,floor20.low),
                   highs = c(view.high,furniture.high,internet.high,floor10.high,floor20.high),
                   condition = rep(factor(paste("K",conds,sep=""),levels = paste("K",conds,sep="")),5),
                   conds = rep(conds,5),
                   Attribute = factor(rep(c("Ocean View","King Bed","Pay for Wireless", "10th Floor", "20th Floor"),each=length(conds)),
                                      levels = c("Ocean View","King Bed","Pay for Wireless", "10th Floor", "20th Floor")))
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
  spread <- 0.3
  to.add <- (spread - delt)/2
  ssdat$fakelow[ssdat$Attribute == levs[i]] <- min(-0.01,lowest - to.add)
  ssdat$fakehigh[ssdat$Attribute == levs[i]] <- max(0.01,highest + to.add)
}


# Plotting ----------------------------------------------------------------

the.color <- "dodgerblue"
pdf("../../results/figures/fig8.pdf",width=7,height=9)
ggplot(ssdat,aes(y=ests,x=conds)) + geom_hline(yintercept = 0) + 
  geom_point(size=1.75,position=position_dodge(width=.5),color=the.color) + theme_economist(dkpanel = T) + 
  geom_errorbar(aes(ymin=lows,ymax=highs,width=0.5),position=position_dodge(width=c(0.5)),size=0.35,color=the.color) +
  ylab("AMCEs with 95% CI") + theme(axis.text.x = element_text(size=10,face='bold'), axis.title.x = element_text(size=10)) +
  theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=10)) +
  theme(legend.text = element_text(size=10), legend.title = element_text(size=10)) +
  facet_wrap(~Attribute,ncol=1,scales = "free_y") + theme(panel.margin=unit(0.35, "lines")) +
  theme(strip.background = element_rect(color="black",fill="#6794a7"), strip.text = element_text(size=10,face="bold")) +
  scale_x_continuous(breaks = conds) + xlab("Total Number of Fillers Shown") +
  geom_line(color=the.color,linetype=2) + geom_blank(aes(y=fakelow)) + geom_blank(aes(y=fakehigh)) + coord_cartesian(xlim=c(0,18))
dev.off()

