library(lmtest)
library(sandwich)
library(ggplot2)
library(ggthemes)
source("../helper_scripts/vcovCluster.R")


# Load and pre-process data -----------------------------------------------

svyx <- read.csv("../../data/hotel_s1.csv")
svyx$cfloor <- as.factor(svyx$cfloor)
svyx$cview <- as.factor(svyx$cview)
svyx$cfurniture <- as.factor(svyx$cfurniture)
svyx$cinternet <- as.factor(svyx$cinternet)

#removing a few NAs:
svyx <- subset(svyx, !is.na(svyx$cfloor) &
                 !is.na(svyx$cview) &
                 !is.na(svyx$cfurniture) &
                 !is.na(svyx$cinternet))


# Function for running all dichotomized difference-in-means tests ---------

all.dichotomized.tests <- function(filler.attribute,data = svyx,only.top = FALSE){
  
  differences.in.means <- c()
  standard.errors <- c()
  dichotomization <- c()
  
  if (length(unique(filler.attribute)) == 2){
    
    dv <- as.numeric(filler.attribute == 2)
    mod1 <- lm(dv ~ cfloor + cview + cfurniture + cinternet, data = data)
    differences.in.means <- c(differences.in.means,coef(mod1)[2:6])
    standard.errors <- c(standard.errors,coeftest(mod1, vcov = vcovCluster(mod1, factor(data$r_id)))[2:6,2])
    dichotomization <- c(dichotomization, rep("1v2",5))
    
    #Now add the final piece: cfloor3-cfloor2
    differences.in.means <- c(differences.in.means,coef(mod1)[3]-coef(mod1)[2])
    names(differences.in.means)[length(differences.in.means)] <- "cfloor3 - cfloor2"
    standard.errors <- c(standard.errors,
                         sqrt(vcovCluster(mod1, factor(data$r_id))["cfloor2","cfloor2"] + vcovCluster(mod1, factor(data$r_id))["cfloor3","cfloor3"] - 2*vcovCluster(mod1, factor(data$r_id))["cfloor2","cfloor3"])
    )
    dichotomization <- c(dichotomization, "1v2")
    
  }
  
  if (length(unique(filler.attribute)) == 3){
    
    dv1 <- as.numeric(filler.attribute == 1)
    mod1 <- lm(dv1 ~ cfloor + cview + cfurniture + cinternet, data = data)
    differences.in.means <- c(differences.in.means,coef(mod1)[2:6])
    standard.errors <- c(standard.errors,coeftest(mod1, vcov = vcovCluster(mod1, factor(data$r_id)))[2:6,2])
    dichotomization <- c(dichotomization, rep("1vRest",5))
    #final piece: cfloor3-cfloor2
    differences.in.means <- c(differences.in.means,coef(mod1)[3]-coef(mod1)[2])
    names(differences.in.means)[length(differences.in.means)] <- "cfloor3 - cfloor2"
    standard.errors <- c(standard.errors,
                         sqrt(vcovCluster(mod1, factor(data$r_id))["cfloor2","cfloor2"] + vcovCluster(mod1, factor(data$r_id))["cfloor3","cfloor3"] - 2*vcovCluster(mod1, factor(data$r_id))["cfloor2","cfloor3"])
    )
    dichotomization <- c(dichotomization, "1vRest")
    
    dv2 <- as.numeric(filler.attribute == 2)
    mod2 <- lm(dv2 ~ cfloor + cview + cfurniture + cinternet, data = data)
    differences.in.means <- c(differences.in.means,coef(mod2)[2:6])
    standard.errors <- c(standard.errors,coeftest(mod2, vcov = vcovCluster(mod2, factor(data$r_id)))[2:6,2])
    dichotomization <- c(dichotomization, rep("2vRest",5))
    #final piece: cfloor3-cfloor2
    differences.in.means <- c(differences.in.means,coef(mod2)[3]-coef(mod2)[2])
    names(differences.in.means)[length(differences.in.means)] <- "cfloor3 - cfloor2"
    standard.errors <- c(standard.errors,
                         sqrt(vcovCluster(mod2, factor(data$r_id))["cfloor2","cfloor2"] + vcovCluster(mod2, factor(data$r_id))["cfloor3","cfloor3"] - 2*vcovCluster(mod2, factor(data$r_id))["cfloor2","cfloor3"])
    )
    dichotomization <- c(dichotomization, "2vRest")
    
    dv3 <- as.numeric(filler.attribute == 3)
    mod3 <- lm(dv3 ~ cfloor + cview + cfurniture + cinternet, data = data)
    differences.in.means <- c(differences.in.means,coef(mod3)[2:6])
    standard.errors <- c(standard.errors,coeftest(mod3, vcov = vcovCluster(mod3, factor(data$r_id)))[2:6,2])
    dichotomization <- c(dichotomization, rep("3vRest",5))
    #final piece: cfloor3-cfloor2
    differences.in.means <- c(differences.in.means,coef(mod3)[3]-coef(mod3)[2])
    names(differences.in.means)[length(differences.in.means)] <- "cfloor3 - cfloor2"
    standard.errors <- c(standard.errors,
                         sqrt(vcovCluster(mod3, factor(data$r_id))["cfloor2","cfloor2"] + vcovCluster(mod3, factor(data$r_id))["cfloor3","cfloor3"] - 2*vcovCluster(mod3, factor(data$r_id))["cfloor2","cfloor3"])
    )
    dichotomization <- c(dichotomization, "3vRest")
    
  }
  
  if (length(unique(filler.attribute)) == 4){
    
    dv1 <- as.numeric(filler.attribute == 1)
    mod1 <- lm(dv1 ~ cfloor + cview + cfurniture + cinternet, data = data)
    differences.in.means <- c(differences.in.means,coef(mod1)[2:6])
    standard.errors <- c(standard.errors,coeftest(mod1, vcov = vcovCluster(mod1, factor(data$r_id)))[2:6,2])
    dichotomization <- c(dichotomization, rep("1vRest",5))
    #final piece: cfloor3-cfloor2
    differences.in.means <- c(differences.in.means,coef(mod1)[3]-coef(mod1)[2])
    names(differences.in.means)[length(differences.in.means)] <- "cfloor3 - cfloor2"
    standard.errors <- c(standard.errors,
                         sqrt(vcovCluster(mod1, factor(data$r_id))["cfloor2","cfloor2"] + vcovCluster(mod1, factor(data$r_id))["cfloor3","cfloor3"] - 2*vcovCluster(mod1, factor(data$r_id))["cfloor2","cfloor3"])
    )
    dichotomization <- c(dichotomization, "1vRest")
    
    dv2 <- as.numeric(filler.attribute == 2)
    mod2 <- lm(dv2 ~ cfloor + cview + cfurniture + cinternet, data = data)
    differences.in.means <- c(differences.in.means,coef(mod2)[2:6])
    standard.errors <- c(standard.errors,coeftest(mod2, vcov = vcovCluster(mod2, factor(data$r_id)))[2:6,2])
    dichotomization <- c(dichotomization, rep("2vRest",5))
    #final piece: cfloor3-cfloor2
    differences.in.means <- c(differences.in.means,coef(mod2)[3]-coef(mod2)[2])
    names(differences.in.means)[length(differences.in.means)] <- "cfloor3 - cfloor2"
    standard.errors <- c(standard.errors,
                         sqrt(vcovCluster(mod2, factor(data$r_id))["cfloor2","cfloor2"] + vcovCluster(mod2, factor(data$r_id))["cfloor3","cfloor3"] - 2*vcovCluster(mod2, factor(data$r_id))["cfloor2","cfloor3"])
    )
    dichotomization <- c(dichotomization, "2vRest")
    
    dv3 <- as.numeric(filler.attribute == 3)
    mod3 <- lm(dv3 ~ cfloor + cview + cfurniture + cinternet, data = data)
    differences.in.means <- c(differences.in.means,coef(mod3)[2:6])
    standard.errors <- c(standard.errors,coeftest(mod3, vcov = vcovCluster(mod3, factor(data$r_id)))[2:6,2])
    dichotomization <- c(dichotomization, rep("3vRest",5))
    #final piece: cfloor3-cfloor2
    differences.in.means <- c(differences.in.means,coef(mod3)[3]-coef(mod3)[2])
    names(differences.in.means)[length(differences.in.means)] <- "cfloor3 - cfloor2"
    standard.errors <- c(standard.errors,
                         sqrt(vcovCluster(mod3, factor(data$r_id))["cfloor2","cfloor2"] + vcovCluster(mod3, factor(data$r_id))["cfloor3","cfloor3"] - 2*vcovCluster(mod3, factor(data$r_id))["cfloor2","cfloor3"])
    )
    dichotomization <- c(dichotomization, "3vRest")
    
    dv4 <- as.numeric(filler.attribute == 4)
    mod4 <- lm(dv4 ~ cfloor + cview + cfurniture + cinternet, data = data)
    differences.in.means <- c(differences.in.means,coef(mod4)[2:6])
    standard.errors <- c(standard.errors,coeftest(mod4, vcov = vcovCluster(mod4, factor(data$r_id)))[2:6,2])
    dichotomization <- c(dichotomization, rep("4vRest",5))
    #final piece: cfloor3-cfloor2
    differences.in.means <- c(differences.in.means,coef(mod4)[3]-coef(mod4)[2])
    names(differences.in.means)[length(differences.in.means)] <- "cfloor3 - cfloor2"
    standard.errors <- c(standard.errors,
                         sqrt(vcovCluster(mod4, factor(data$r_id))["cfloor2","cfloor2"] + vcovCluster(mod4, factor(data$r_id))["cfloor3","cfloor3"] - 2*vcovCluster(mod4, factor(data$r_id))["cfloor2","cfloor3"])
    )
    dichotomization <- c(dichotomization, "4vRest")
    
    dv5 <- as.numeric(filler.attribute == 1 | filler.attribute == 2)
    mod5 <- lm(dv5 ~ cfloor + cview + cfurniture + cinternet, data = data)
    differences.in.means <- c(differences.in.means,coef(mod5)[2:6])
    standard.errors <- c(standard.errors,coeftest(mod5, vcov = vcovCluster(mod5, factor(data$r_id)))[2:6,2])
    dichotomization <- c(dichotomization, rep("12v34",5))
    #final piece: cfloor3-cfloor2
    differences.in.means <- c(differences.in.means,coef(mod5)[3]-coef(mod5)[2])
    names(differences.in.means)[length(differences.in.means)] <- "cfloor3 - cfloor2"
    standard.errors <- c(standard.errors,
                         sqrt(vcovCluster(mod5, factor(data$r_id))["cfloor2","cfloor2"] + vcovCluster(mod5, factor(data$r_id))["cfloor3","cfloor3"] - 2*vcovCluster(mod5, factor(data$r_id))["cfloor2","cfloor3"])
    )
    dichotomization <- c(dichotomization, "12v34")
    
    dv6 <- as.numeric(filler.attribute == 1 | filler.attribute == 3)
    mod6 <- lm(dv6 ~ cfloor + cview + cfurniture + cinternet, data = data)
    differences.in.means <- c(differences.in.means,coef(mod6)[2:6])
    standard.errors <- c(standard.errors,coeftest(mod6, vcov = vcovCluster(mod6, factor(data$r_id)))[2:6,2])
    dichotomization <- c(dichotomization, rep("13v24",5))
    #final piece: cfloor3-cfloor2
    differences.in.means <- c(differences.in.means,coef(mod6)[3]-coef(mod6)[2])
    names(differences.in.means)[length(differences.in.means)] <- "cfloor3 - cfloor2"
    standard.errors <- c(standard.errors,
                         sqrt(vcovCluster(mod6, factor(data$r_id))["cfloor2","cfloor2"] + vcovCluster(mod6, factor(data$r_id))["cfloor3","cfloor3"] - 2*vcovCluster(mod6, factor(data$r_id))["cfloor2","cfloor3"])
    )
    dichotomization <- c(dichotomization, "13v24")
    
    dv7 <- as.numeric(filler.attribute == 1 | filler.attribute == 4)
    mod7 <- lm(dv7 ~ cfloor + cview + cfurniture + cinternet, data = data)
    differences.in.means <- c(differences.in.means,coef(mod7)[2:6])
    standard.errors <- c(standard.errors,coeftest(mod7, vcov = vcovCluster(mod7, factor(data$r_id)))[2:6,2])
    dichotomization <- c(dichotomization, rep("14v23",5))
    #final piece: cfloor3-cfloor2
    differences.in.means <- c(differences.in.means,coef(mod7)[3]-coef(mod7)[2])
    names(differences.in.means)[length(differences.in.means)] <- "cfloor3 - cfloor2"
    standard.errors <- c(standard.errors,
                         sqrt(vcovCluster(mod7, factor(data$r_id))["cfloor2","cfloor2"] + vcovCluster(mod7, factor(data$r_id))["cfloor3","cfloor3"] - 2*vcovCluster(mod7, factor(data$r_id))["cfloor2","cfloor3"])
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
  dff$index <- seq(1:nrow(dff))
  
  if (only.top==TRUE){
    dff <- dff[1,]
  }
  
  return(dff)
  
}


# Summary top effect results ----------------------------------------------

fillers <- c("pillows","channels","lamps","service","color","shower","sinks","balcony",
             "closet","decor","theme","hallway","coffee","office","scent","kitchen",
             "call","parking","towels","linens","flooring","lights","access","elevators",
             "food","cars","bar","tv","door","windows","fan","thermo",
             "valet","hours","menu","chocolate","boats","size")

outframe <- NULL
for (i in 1:length(fillers)){
  outframe <- rbind(outframe,all.dichotomized.tests(filler.attribute = svyx[,fillers[i]],only.top = TRUE))
}
outframe <- cbind(fillers,outframe)
names(outframe)[2] <- "max.diff.in.means"
ord.outframe <- outframe[order(abs(outframe$max.diff.in.means)),]
rownames(ord.outframe) <- NULL

ord.outframe


# For Plotting ------------------------------------------------------------

good.fillers <- c("office","lamps","menu","hallway","thermo","elevators",
                  "fan","call","closet","channels","towels","chocolate",
                  "service","bar","linens","kitchen","pillows","sinks")
good.fillers.names <- c("Office","Lamps","Menu","Hallway","Thermo","Elevators",
                        "Fan","Call","Closet","Channels","Towels","Chocolate",
                        "Service","Bar","Linens","Kitchen","Pillows","Sinks")

bad.fillers <- c("door","decor","tv","lights","coffee","shower","food",
                 "flooring","hours","parking","valet","cars","windows",
                 "scent","theme","color","balcony","access","size","boats")
bad.filler.names <- c("Bathroom Door","Decor","TV","Lights","Coffee",
                      "Shower","Food","Flooring","Hours","Parking",
                      "Valet","Car","Windows","Scent","Theme",
                      "Color","Balcony","Access","Size","Boats")

effects.list <- list(NULL)
for (i in 1:length(good.fillers)){
  effects.list[[i]] <- 
    all.dichotomized.tests(filler.attribute = svyx[,good.fillers[i]])
  effects.list[[i]]$filler <- good.fillers.names[i]
}


# Plotting good fillers (Figure A10) --------------------------------------

tbind <- data.frame(NULL)
for (i in 1:18){
  tbind <- rbind(tbind,effects.list[[i]])
}

tbind$lows <- tbind$differences.in.means - 1.96*tbind$standard.errors
tbind$highs <- tbind$differences.in.means + 1.96*tbind$standard.errors

the.color <- "dodgerblue"
pdf("../../results/figures/figA10.pdf",width=9,height=12)
ggplot(tbind,aes(y=differences.in.means,x=index)) + geom_hline(yintercept = 0,size=0.65) +
  geom_point(size=0.5,position=position_dodge(width=.5),color=the.color) + theme_economist(dkpanel = T) + 
  geom_errorbar(aes(ymin=lows,ymax=highs,width=0.35),position=position_dodge(width=c(0.5)),size=0.35,color=the.color) +
  ylab("Dichotomized Effect with 95% CI") + theme(axis.text.x = element_text(size=10,face='bold'), axis.title.x = element_text(size=10)) +
  theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=10)) +
  theme(legend.text = element_text(size=10), legend.title = element_text(size=10)) +
  ylim(-0.3,0.3) + xlab("Index") +
  facet_wrap(~filler,ncol=3,scales = "free_x") + theme(panel.margin=unit(0.35, "lines")) +
  theme(strip.background = element_rect(color="black",fill="#6794a7"), strip.text = element_text(size=10,face="bold"))
dev.off()


# Plotting bad fillers, except attention checks (Figure A11) --------------

effects.list <- list(NULL)
for (i in 1:length(bad.fillers)){
  effects.list[[i]] <- all.dichotomized.tests(filler.attribute = svyx[,bad.fillers[i]])
  effects.list[[i]]$filler <- bad.filler.names[i]
}

tbind <- data.frame(NULL)
for (i in 1:20){
  tbind <- rbind(tbind,effects.list[[i]])
}
tbind <- subset(tbind,tbind$filler != "Access" &
                  tbind$filler != "Boats" &
                  tbind$filler != "Size")

tbind$lows <- tbind$differences.in.means - 1.96*tbind$standard.errors
tbind$highs <- tbind$differences.in.means + 1.96*tbind$standard.errors

the.color <- "dodgerblue"
pdf("../../results/figures/figA11.pdf",width=9,height=12)
ggplot(tbind,aes(y=differences.in.means,x=index)) + geom_hline(yintercept = 0,size=0.65) +
  geom_point(size=0.5,position=position_dodge(width=.5),color=the.color) + theme_economist(dkpanel = T) + 
  geom_errorbar(aes(ymin=lows,ymax=highs,width=0.35),position=position_dodge(width=c(0.5)),size=0.35,color=the.color) +
  ylab("Dichotomized Effect with 95% CI") + theme(axis.text.x = element_text(size=10,face='bold'), axis.title.x = element_text(size=10)) +
  theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=10)) +
  theme(legend.text = element_text(size=10), legend.title = element_text(size=10)) +
  ylim(-0.3,0.3) + xlab("Index") +
  facet_wrap(~filler,ncol=3,scales = "free_x") + theme(panel.margin=unit(0.35, "lines")) +
  theme(strip.background = element_rect(color="black",fill="#6794a7"), strip.text = element_text(size=10,face="bold"))
dev.off()


# Plotting attention check fillers (Figure A12) ---------------------------

tbind <- data.frame(NULL)
for (i in 1:20){
  tbind <- rbind(tbind,effects.list[[i]])
}
tbind <- subset(tbind,tbind$filler == "Access" |
                  tbind$filler == "Boats" |
                  tbind$filler == "Size")

tbind$lows <- tbind$differences.in.means - 1.96*tbind$standard.errors
tbind$highs <- tbind$differences.in.means + 1.96*tbind$standard.errors

the.color <- "dodgerblue"
pdf("../../results/figures/figA12.pdf",width=9,height=3)
ggplot(tbind,aes(y=differences.in.means,x=index)) + geom_hline(yintercept = 0,size=0.65) +
  geom_point(size=0.5,position=position_dodge(width=.5),color=the.color) + theme_economist(dkpanel = T) + 
  geom_errorbar(aes(ymin=lows,ymax=highs,width=0.35),position=position_dodge(width=c(0.5)),size=0.35,color=the.color) +
  ylab("Dichotomized Effect with 95% CI") + theme(axis.text.x = element_text(size=10,face='bold'), axis.title.x = element_text(size=10)) +
  theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=10)) +
  theme(legend.text = element_text(size=10), legend.title = element_text(size=10)) +
  ylim(-1,1) + xlab("Index") +
  facet_wrap(~filler,ncol=3,scales = "free_x") + theme(panel.margin=unit(0.35, "lines")) +
  theme(strip.background = element_rect(color="black",fill="#6794a7"), strip.text = element_text(size=10,face="bold"))
dev.off()
