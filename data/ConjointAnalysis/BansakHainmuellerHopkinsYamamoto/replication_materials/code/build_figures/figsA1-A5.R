library(lmtest)
library(sandwich)
library(ggplot2)
library(ggthemes)
source("../helper_scripts/vcovCluster.R")


# Load and pre-process data -----------------------------------------------

svyx <- read.csv("../../data/cand_s1.csv")
svyx$cage <- as.factor(svyx$cage)
svyx$chealthcare <- as.factor(svyx$chealthcare)
svyx$cmarriage <- as.factor(svyx$cmarriage)
svyx$cparty <- as.factor(svyx$cparty)


# Function for running all dichotomized difference-in-means tests ---------

all.dichotomized.tests <- function(filler.attribute,data = svyx){
  
  differences.in.means <- c()
  standard.errors <- c()
  dichotomization <- c()
  
  if (length(unique(filler.attribute)) == 2){
    
    dv <- as.numeric(filler.attribute == 2)
    mod1 <- lm(dv ~ cage + chealthcare + cmarriage + cparty, data = data)
    differences.in.means <- c(differences.in.means,coef(mod1)[2:6])
    standard.errors <- c(standard.errors,coeftest(mod1, vcov = vcovCluster(mod1, factor(data$r_id)))[2:6,2])
    dichotomization <- c(dichotomization, rep("1v2",5))
    
    #Now add the final piece: cage3-cage2
    differences.in.means <- c(differences.in.means,coef(mod1)[3]-coef(mod1)[2])
    names(differences.in.means)[length(differences.in.means)] <- "cage3 - cage2"
    standard.errors <- c(standard.errors,
                         sqrt(vcovCluster(mod1, factor(data$r_id))["cage2","cage2"] + vcovCluster(mod1, factor(data$r_id))["cage3","cage3"] - 2*vcovCluster(mod1, factor(data$r_id))["cage2","cage3"])
    )
    dichotomization <- c(dichotomization, "1v2")
    
  }
  
  if (length(unique(filler.attribute)) == 3){
    
    dv1 <- as.numeric(filler.attribute == 1)
    mod1 <- lm(dv1 ~ cage + chealthcare + cmarriage + cparty, data = data)
    differences.in.means <- c(differences.in.means,coef(mod1)[2:6])
    standard.errors <- c(standard.errors,coeftest(mod1, vcov = vcovCluster(mod1, factor(data$r_id)))[2:6,2])
    dichotomization <- c(dichotomization, rep("1vRest",5))
    #final piece: cage3-cage2
    differences.in.means <- c(differences.in.means,coef(mod1)[3]-coef(mod1)[2])
    names(differences.in.means)[length(differences.in.means)] <- "cage3 - cage2"
    standard.errors <- c(standard.errors,
                         sqrt(vcovCluster(mod1, factor(data$r_id))["cage2","cage2"] + vcovCluster(mod1, factor(data$r_id))["cage3","cage3"] - 2*vcovCluster(mod1, factor(data$r_id))["cage2","cage3"])
    )
    dichotomization <- c(dichotomization, "1vRest")
    
    dv2 <- as.numeric(filler.attribute == 2)
    mod2 <- lm(dv2 ~ cage + chealthcare + cmarriage + cparty, data = data)
    differences.in.means <- c(differences.in.means,coef(mod2)[2:6])
    standard.errors <- c(standard.errors,coeftest(mod2, vcov = vcovCluster(mod2, factor(data$r_id)))[2:6,2])
    dichotomization <- c(dichotomization, rep("2vRest",5))
    #final piece: cage3-cage2
    differences.in.means <- c(differences.in.means,coef(mod2)[3]-coef(mod2)[2])
    names(differences.in.means)[length(differences.in.means)] <- "cage3 - cage2"
    standard.errors <- c(standard.errors,
                         sqrt(vcovCluster(mod2, factor(data$r_id))["cage2","cage2"] + vcovCluster(mod2, factor(data$r_id))["cage3","cage3"] - 2*vcovCluster(mod2, factor(data$r_id))["cage2","cage3"])
    )
    dichotomization <- c(dichotomization, "2vRest")
    
    dv3 <- as.numeric(filler.attribute == 3)
    mod3 <- lm(dv3 ~ cage + chealthcare + cmarriage + cparty, data = data)
    differences.in.means <- c(differences.in.means,coef(mod3)[2:6])
    standard.errors <- c(standard.errors,coeftest(mod3, vcov = vcovCluster(mod3, factor(data$r_id)))[2:6,2])
    dichotomization <- c(dichotomization, rep("3vRest",5))
    #final piece: cage3-cage2
    differences.in.means <- c(differences.in.means,coef(mod3)[3]-coef(mod3)[2])
    names(differences.in.means)[length(differences.in.means)] <- "cage3 - cage2"
    standard.errors <- c(standard.errors,
                         sqrt(vcovCluster(mod3, factor(data$r_id))["cage2","cage2"] + vcovCluster(mod3, factor(data$r_id))["cage3","cage3"] - 2*vcovCluster(mod3, factor(data$r_id))["cage2","cage3"])
    )
    dichotomization <- c(dichotomization, "3vRest")
    
  }
  
  if (length(unique(filler.attribute)) == 4){
    
    dv1 <- as.numeric(filler.attribute == 1)
    mod1 <- lm(dv1 ~ cage + chealthcare + cmarriage + cparty, data = data)
    differences.in.means <- c(differences.in.means,coef(mod1)[2:6])
    standard.errors <- c(standard.errors,coeftest(mod1, vcov = vcovCluster(mod1, factor(data$r_id)))[2:6,2])
    dichotomization <- c(dichotomization, rep("1vRest",5))
    #final piece: cage3-cage2
    differences.in.means <- c(differences.in.means,coef(mod1)[3]-coef(mod1)[2])
    names(differences.in.means)[length(differences.in.means)] <- "cage3 - cage2"
    standard.errors <- c(standard.errors,
                         sqrt(vcovCluster(mod1, factor(data$r_id))["cage2","cage2"] + vcovCluster(mod1, factor(data$r_id))["cage3","cage3"] - 2*vcovCluster(mod1, factor(data$r_id))["cage2","cage3"])
    )
    dichotomization <- c(dichotomization, "1vRest")
    
    dv2 <- as.numeric(filler.attribute == 2)
    mod2 <- lm(dv2 ~ cage + chealthcare + cmarriage + cparty, data = data)
    differences.in.means <- c(differences.in.means,coef(mod2)[2:6])
    standard.errors <- c(standard.errors,coeftest(mod2, vcov = vcovCluster(mod2, factor(data$r_id)))[2:6,2])
    dichotomization <- c(dichotomization, rep("2vRest",5))
    #final piece: cage3-cage2
    differences.in.means <- c(differences.in.means,coef(mod2)[3]-coef(mod2)[2])
    names(differences.in.means)[length(differences.in.means)] <- "cage3 - cage2"
    standard.errors <- c(standard.errors,
                         sqrt(vcovCluster(mod2, factor(data$r_id))["cage2","cage2"] + vcovCluster(mod2, factor(data$r_id))["cage3","cage3"] - 2*vcovCluster(mod2, factor(data$r_id))["cage2","cage3"])
    )
    dichotomization <- c(dichotomization, "2vRest")
    
    dv3 <- as.numeric(filler.attribute == 3)
    mod3 <- lm(dv3 ~ cage + chealthcare + cmarriage + cparty, data = data)
    differences.in.means <- c(differences.in.means,coef(mod3)[2:6])
    standard.errors <- c(standard.errors,coeftest(mod3, vcov = vcovCluster(mod3, factor(data$r_id)))[2:6,2])
    dichotomization <- c(dichotomization, rep("3vRest",5))
    #final piece: cage3-cage2
    differences.in.means <- c(differences.in.means,coef(mod3)[3]-coef(mod3)[2])
    names(differences.in.means)[length(differences.in.means)] <- "cage3 - cage2"
    standard.errors <- c(standard.errors,
                         sqrt(vcovCluster(mod3, factor(data$r_id))["cage2","cage2"] + vcovCluster(mod3, factor(data$r_id))["cage3","cage3"] - 2*vcovCluster(mod3, factor(data$r_id))["cage2","cage3"])
    )
    dichotomization <- c(dichotomization, "3vRest")
    
    dv4 <- as.numeric(filler.attribute == 4)
    mod4 <- lm(dv4 ~ cage + chealthcare + cmarriage + cparty, data = data)
    differences.in.means <- c(differences.in.means,coef(mod4)[2:6])
    standard.errors <- c(standard.errors,coeftest(mod4, vcov = vcovCluster(mod4, factor(data$r_id)))[2:6,2])
    dichotomization <- c(dichotomization, rep("4vRest",5))
    #final piece: cage3-cage2
    differences.in.means <- c(differences.in.means,coef(mod4)[3]-coef(mod4)[2])
    names(differences.in.means)[length(differences.in.means)] <- "cage3 - cage2"
    standard.errors <- c(standard.errors,
                         sqrt(vcovCluster(mod4, factor(data$r_id))["cage2","cage2"] + vcovCluster(mod4, factor(data$r_id))["cage3","cage3"] - 2*vcovCluster(mod4, factor(data$r_id))["cage2","cage3"])
    )
    dichotomization <- c(dichotomization, "4vRest")
    
    dv5 <- as.numeric(filler.attribute == 1 | filler.attribute == 2)
    mod5 <- lm(dv5 ~ cage + chealthcare + cmarriage + cparty, data = data)
    differences.in.means <- c(differences.in.means,coef(mod5)[2:6])
    standard.errors <- c(standard.errors,coeftest(mod5, vcov = vcovCluster(mod5, factor(data$r_id)))[2:6,2])
    dichotomization <- c(dichotomization, rep("12v34",5))
    #final piece: cage3-cage2
    differences.in.means <- c(differences.in.means,coef(mod5)[3]-coef(mod5)[2])
    names(differences.in.means)[length(differences.in.means)] <- "cage3 - cage2"
    standard.errors <- c(standard.errors,
                         sqrt(vcovCluster(mod5, factor(data$r_id))["cage2","cage2"] + vcovCluster(mod5, factor(data$r_id))["cage3","cage3"] - 2*vcovCluster(mod5, factor(data$r_id))["cage2","cage3"])
    )
    dichotomization <- c(dichotomization, "12v34")
    
    dv6 <- as.numeric(filler.attribute == 1 | filler.attribute == 3)
    mod6 <- lm(dv6 ~ cage + chealthcare + cmarriage + cparty, data = data)
    differences.in.means <- c(differences.in.means,coef(mod6)[2:6])
    standard.errors <- c(standard.errors,coeftest(mod6, vcov = vcovCluster(mod6, factor(data$r_id)))[2:6,2])
    dichotomization <- c(dichotomization, rep("13v24",5))
    #final piece: cage3-cage2
    differences.in.means <- c(differences.in.means,coef(mod6)[3]-coef(mod6)[2])
    names(differences.in.means)[length(differences.in.means)] <- "cage3 - cage2"
    standard.errors <- c(standard.errors,
                         sqrt(vcovCluster(mod6, factor(data$r_id))["cage2","cage2"] + vcovCluster(mod6, factor(data$r_id))["cage3","cage3"] - 2*vcovCluster(mod6, factor(data$r_id))["cage2","cage3"])
    )
    dichotomization <- c(dichotomization, "13v24")
    
    dv7 <- as.numeric(filler.attribute == 1 | filler.attribute == 4)
    mod7 <- lm(dv7 ~ cage + chealthcare + cmarriage + cparty, data = data)
    differences.in.means <- c(differences.in.means,coef(mod7)[2:6])
    standard.errors <- c(standard.errors,coeftest(mod7, vcov = vcovCluster(mod7, factor(data$r_id)))[2:6,2])
    dichotomization <- c(dichotomization, rep("14v23",5))
    #final piece: cage3-cage2
    differences.in.means <- c(differences.in.means,coef(mod7)[3]-coef(mod7)[2])
    names(differences.in.means)[length(differences.in.means)] <- "cage3 - cage2"
    standard.errors <- c(standard.errors,
                         sqrt(vcovCluster(mod7, factor(data$r_id))["cage2","cage2"] + vcovCluster(mod7, factor(data$r_id))["cage3","cage3"] - 2*vcovCluster(mod7, factor(data$r_id))["cage2","cage3"])
    )
    dichotomization <- c(dichotomization, "14v23")
    
  }
  
  fixed.effects <- names(differences.in.means)
  t.stats <- differences.in.means/standard.errors
  p.values <- pt(abs(t.stats),df=length(filler.attribute)-6,lower.tail = FALSE)*2
  
  differences.in.means <- round(differences.in.means, digits = 4)
  standard.errors <- round(standard.errors, digits = 4)
  t.stats <- round(t.stats, digits = 4)
  p.values <- round(p.values, digits = 4)
  
  df <- data.frame(differences.in.means,standard.errors,t.stats,p.values,fixed.effects,dichotomization)
  
  k <- order(abs(differences.in.means), decreasing = TRUE)
  
  dff <- df[k,]
  dff$index <- seq(1:length(differences.in.means))
  
  return(dff)
  
}


# For Plotting ------------------------------------------------------------

t.school <- all.dichotomized.tests(filler.attribute = svyx$school)
t.highway <- all.dichotomized.tests(filler.attribute = svyx$highway)
t.relative <- all.dichotomized.tests(filler.attribute = svyx$relative)
t.vacation <- all.dichotomized.tests(filler.attribute = svyx$vacation)
t.marital <- all.dichotomized.tests(filler.attribute = svyx$marital)

t.school$filler <- "School"
t.highway$filler <- "Highway"
t.relative$filler <- "Relative"
t.vacation$filler <- "Vacation"
t.marital$filler <- "Marital"

tbind <- rbind(t.school,t.highway,t.relative,t.vacation,t.marital)
tbind$lows <- tbind$differences.in.means - 1.96*tbind$standard.errors
tbind$highs <- tbind$differences.in.means + 1.96*tbind$standard.errors

the.color <- "dodgerblue"


# Figure A1 ---------------------------------------------------------------

pdf("../../results/figures/figA1.pdf",width=7,height=9)
ggplot(tbind,aes(y=differences.in.means,x=index)) + geom_hline(yintercept = 0,size=0.65) +
  geom_point(size=0.5,position=position_dodge(width=.5),color=the.color) + theme_economist(dkpanel = T) + 
  geom_errorbar(aes(ymin=lows,ymax=highs,width=0.35),position=position_dodge(width=c(0.5)),size=0.35,color=the.color) +
  ylab("Dichotomized Effect with 95% CI") + theme(axis.text.x = element_text(size=10,face='bold'), axis.title.x = element_text(size=10)) +
  theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=10)) +
  theme(legend.text = element_text(size=10), legend.title = element_text(size=10)) +
  ylim(-0.3,0.3) + xlab("Index") +
  facet_wrap(~filler,ncol=1,scales = "free_x") + theme(panel.margin=unit(0.35, "lines")) +
  theme(strip.background = element_rect(color="black",fill="#6794a7"), strip.text = element_text(size=10,face="bold"))
dev.off()


# Comparison to Simulated Uncorrelated Fillers ----------------------------

set.seed(124)

svyx$fake2 <- sample(c(1,2),nrow(svyx),replace = TRUE)
svyx$fake3 <- sample(c(1,2,3),nrow(svyx),replace = TRUE)
svyx$fake4 <- sample(c(1,2,3,4),nrow(svyx),replace = TRUE)

t.fake2 <- all.dichotomized.tests(filler.attribute = svyx$fake2)
t.fake3 <- all.dichotomized.tests(filler.attribute = svyx$fake3)
t.fake4 <- all.dichotomized.tests(filler.attribute = svyx$fake4)

t.fake2$filler <- "2-Level Simulated Filler"
t.fake3$filler <- "3-Level Simulated Filler"
t.fake4$filler <- "4-Level Simulated Filler"

fbind <- rbind(t.fake2,t.fake3,t.fake4)
fbind$lows <- fbind$differences.in.means - 1.96*fbind$standard.errors
fbind$highs <- fbind$differences.in.means + 1.96*fbind$standard.errors


# Figure A2 ---------------------------------------------------------------

pdf("../../results/figures/figA2.pdf",width=7,height=6)
ggplot(fbind,aes(y=differences.in.means,x=index)) + geom_hline(yintercept = 0,size=0.65) +
  geom_point(size=0.5,position=position_dodge(width=.5),color=the.color) + theme_economist(dkpanel = T) + 
  geom_errorbar(aes(ymin=lows,ymax=highs,width=0.35),position=position_dodge(width=c(0.5)),size=0.35,color=the.color) +
  ylab("Dichotomized Effect with 95% CI") + theme(axis.text.x = element_text(size=10,face='bold'), axis.title.x = element_text(size=10)) +
  theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=10)) +
  theme(legend.text = element_text(size=10), legend.title = element_text(size=10)) +
  ylim(-0.3,0.3) + xlab("Index") +
  facet_wrap(~filler,ncol=1,scales = "free_x") + theme(panel.margin=unit(0.35, "lines")) +
  theme(strip.background = element_rect(color="black",fill="#6794a7"), strip.text = element_text(size=10,face="bold"))
dev.off()


# Correlated Fillers ------------------------------------------------------

t.educ <- all.dichotomized.tests(filler.attribute = svyx$educ)
t.exp <- all.dichotomized.tests(filler.attribute = svyx$exp)
t.income <- all.dichotomized.tests(filler.attribute = svyx$income)
t.kids <- all.dichotomized.tests(filler.attribute = svyx$kids)

t.gender <- all.dichotomized.tests(filler.attribute = svyx$gender)
t.job <- all.dichotomized.tests(filler.attribute = svyx$job)
t.military <- all.dichotomized.tests(filler.attribute = svyx$military)
t.race <- all.dichotomized.tests(filler.attribute = svyx$race)

t.ideology <- all.dichotomized.tests(filler.attribute = svyx$ideology)
t.immigration <- all.dichotomized.tests(filler.attribute = svyx$immigration)
t.guns <- all.dichotomized.tests(filler.attribute = svyx$guns)

t.educ$filler <- "Education"
t.exp$filler <- "Political Experience"
t.income$filler <- "Income"
t.kids$filler <- "Children"

t.gender$filler <- "Gender"
t.job$filler <- "Previous Occupation"
t.military$filler <- "Military Experience"
t.race$filler <- "Race/Ethnicity"

t.ideology$filler <- "Political Ideology"
t.immigration$filler <- "Position on Immigration"
t.guns$filler <- "Position on Gun Control"

tbind1 <- rbind(t.educ,t.exp,t.income,t.kids)
tbind1$lows <- tbind1$differences.in.means - 1.96*tbind1$standard.errors
tbind1$highs <- tbind1$differences.in.means + 1.96*tbind1$standard.errors

tbind2 <- rbind(t.gender,t.job,t.military,t.race)
tbind2$lows <- tbind2$differences.in.means - 1.96*tbind2$standard.errors
tbind2$highs <- tbind2$differences.in.means + 1.96*tbind2$standard.errors

tbind3 <- rbind(t.ideology,t.immigration,t.guns)
tbind3$lows <- tbind3$differences.in.means - 1.96*tbind3$standard.errors
tbind3$highs <- tbind3$differences.in.means + 1.96*tbind3$standard.errors


# Figure A3 ---------------------------------------------------------------

pdf("../../results/figures/figA3.pdf",width=7,height=7.5)
ggplot(tbind1,aes(y=differences.in.means,x=index)) + geom_hline(yintercept = 0,size=0.65) +
  geom_point(size=0.5,position=position_dodge(width=.5),color=the.color) + theme_economist(dkpanel = T) + 
  geom_errorbar(aes(ymin=lows,ymax=highs,width=0.35),position=position_dodge(width=c(0.5)),size=0.35,color=the.color) +
  ylab("Dichotomized Effect with 95% CI") + theme(axis.text.x = element_text(size=10,face='bold'), axis.title.x = element_text(size=10)) +
  theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=10)) +
  theme(legend.text = element_text(size=10), legend.title = element_text(size=10)) +
  ylim(-0.3,0.3) + xlab("Index") +
  facet_wrap(~filler,ncol=1,scales = "free_x") + theme(panel.margin=unit(0.35, "lines")) +
  theme(strip.background = element_rect(color="black",fill="#6794a7"), strip.text = element_text(size=10,face="bold"))
dev.off()


# Figure A4 ---------------------------------------------------------------

pdf("../../results/figures/figA4.pdf",width=7,height=7.5)
ggplot(tbind2,aes(y=differences.in.means,x=index)) + geom_hline(yintercept = 0,size=0.65) +
  geom_point(size=0.5,position=position_dodge(width=.5),color=the.color) + theme_economist(dkpanel = T) + 
  geom_errorbar(aes(ymin=lows,ymax=highs,width=0.35),position=position_dodge(width=c(0.5)),size=0.35,color=the.color) +
  ylab("Dichotomized Effect with 95% CI") + theme(axis.text.x = element_text(size=10,face='bold'), axis.title.x = element_text(size=10)) +
  theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=10)) +
  theme(legend.text = element_text(size=10), legend.title = element_text(size=10)) +
  ylim(-0.3,0.3) + xlab("Index") +
  facet_wrap(~filler,ncol=1,scales = "free_x") + theme(panel.margin=unit(0.35, "lines")) +
  theme(strip.background = element_rect(color="black",fill="#6794a7"), strip.text = element_text(size=10,face="bold"))
dev.off()


# Figure A5 ---------------------------------------------------------------

pdf("../../results/figures/figA5.pdf",width=7,height=6)
ggplot(tbind3,aes(y=differences.in.means,x=index)) + geom_hline(yintercept = 0,size=0.65) +
  geom_point(size=0.5,position=position_dodge(width=.5),color=the.color) + theme_economist(dkpanel = T) + 
  geom_errorbar(aes(ymin=lows,ymax=highs,width=0.35),position=position_dodge(width=c(0.5)),size=0.35,color=the.color) +
  ylab("Dichotomized Effect with 95% CI") + theme(axis.text.x = element_text(size=10,face='bold'), axis.title.x = element_text(size=10)) +
  theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=10)) +
  theme(legend.text = element_text(size=10), legend.title = element_text(size=10)) +
  ylim(-0.5,0.5) + xlab("Index") +
  facet_wrap(~filler,ncol=1,scales = "free_x") + theme(panel.margin=unit(0.35, "lines")) +
  theme(strip.background = element_rect(color="black",fill="#6794a7"), strip.text = element_text(size=10,face="bold"))
dev.off()
