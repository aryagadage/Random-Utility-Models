library(ggplot2)
library(ggthemes)


# Load and pre-process data -----------------------------------------------

load("../../results/boots/hotel_s2_bootresults.RData")
svyx <- read.csv("../../data/hotel_s2.csv")
source("../helper_scripts/hotel_s2_datapreprocess.R")
fixed <- c("cview","cfurniture","cinternet","cfloor")


# Compute and calculate stats ---------------------------------------------

Ks <- sort(unique(svyx$condition))
R2s <- rep(NA, length(Ks))
names(R2s) <- paste0("K", Ks)

for(l in 1:length(Ks)){
  out <- lm(pref ~ cview + cfurniture + cinternet + cfloor,
            data = svyx, subset = condition == Ks[l])
  R2s[l] <- summary(out)$r.squared
}

# calculate stats
clevel <- .95
R2s.up <- apply(R2s.sims, 1, quantile, 1 - (1-clevel)/2)
R2s.lo <- apply(R2s.sims, 1, quantile, (1-clevel)/2)
R2s.K0diff <- cbind(rep(-1,nrow(R2s.sims)-1), diag(rep(1, nrow(R2s.sims)-1))) %*% R2s.sims
rownames(R2s.K0diff) <- rownames(R2s.sims)[-1]


# View results and run tests ----------------------------------------------

pdat <- data.frame(Ks,R2s,R2s.lo,R2s.up)
pdat

(apply(R2s.K0diff, 1, function(x) quantile(x, c(.95,.975,.995)) < 0)) # two-sided tests at .1, .05, .01
(apply(R2s.K0diff, 1, function(x) sum(x>0)/length(x)))  # one-sided percentile p-vals


# Plotting ----------------------------------------------------------------

the.color <- "dodgerblue"
pdf("../../results/figures/fig9.pdf",width=6,height=4)
ggplot(pdat,aes(y=R2s,x=Ks)) +
  geom_point(size=1.75,position=position_dodge(width=.6), color = the.color) + theme_economist(dkpanel = T) +
  geom_errorbar(aes(ymin=R2s.lo,ymax=R2s.up,width=0.5),position=position_dodge(width=c(0.6,0.6)),size=0.35, color = the.color) +
  ylab("Partial R Squares") + theme(axis.text.x = element_text(size=10,face='bold'), axis.title.x = element_text(size=10)) +
  theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=10)) +
  theme(legend.text = element_text(size=8), legend.title = element_text(size=10)) +
  scale_x_continuous(breaks = Ks) + xlab("Total Number of Fillers Shown") +
  geom_line(color=the.color,linetype=2) + ylim(0,0.2)
dev.off()
