library(lmtest)
library(ggplot2)
library(ggthemes)


# Load and pre-process data -----------------------------------------------

svyx <- read.csv("../../data/hotel_s2.csv")
source("../helper_scripts/hotel_s2_datapreprocess.R")
fixed <- c("cview","cfurniture","cinternet","cfloor")
filler <- c("cpillowsm","cchannelsm","clampsm","cservicem","cclosetm",
            "challwaym","cofficem","ckitchenm","ccallm","ctowelsm",
            "clinensm","celevatorsm","cbarm","csinksm","cfanm",
            "cthermom","cmenum","cchocolatem")

# delete conditions where no filler was added
d <- data.frame(subset(svyx, svyx$condition != 0))

d$r_gender <- as.character(d$r_gender)
d$r_party  <- as.character(d$r_party)
d$r_educ   <- as.character(d$r_educ)
d$r_age    <- as.character(cut(d$r_age,breaks=quantile(d$r_age,probs=seq(0,1,.25)),
                               include.lowest = TRUE)
)


# Estimation --------------------------------------------------------------

store <- matrix(NA,0,2)
for (k in 1:length(filler)){
  
  fn <- filler[k]
  # subset to when filler is shown  
  dsub <- d[d[,fn]!=0,]
  
  # interaction form
  form1 <- as.formula(paste("pref~",
                            paste(fn,"*r_educ",sep=""),"+",
                            paste(fn,"*r_gender",sep=""),"+",
                            paste(fn,"*r_income",sep=""),"+",
                            paste(fn,"*r_age",sep=""),"+",
                            paste(fn,"*r_party",sep="")
  )
  )
  
  # additive form
  form2 <- as.formula(paste("pref~",fn,"+",
                            "r_educ+r_gender+r_income+r_age+r_party"))
  
  # fit both models 
  t1 <- try(lm(form1,data=dsub))
  t2 <- try(lm(form2,data=dsub))
  
  # if converged do wald test
  if(class(t1) != "try_error" & class(t2) != "try_error" ){
    
    wout <- try(waldtest(t1, t2))
    cat(fn,"\n") 
    cat(wout$`Pr(>F)`[2],"\n")
    
    temp <- matrix(c(wout$`Pr(>F)`[2],nobs(t2)),1,ncol=2)
    rownames(temp) <- fn
    store <- rbind(store,temp)
    
  }
}


# Package for plotting ----------------------------------------------------

store <- data.frame(store)
for(i in 1:nrow(store)){
  store$name[i] <- paste(rownames(store)[i],"\n(N=",store$X2[i],")",sep="") 
}

store <- store[order(store$X1),]
store$group <- factor(store$name,labels=store$name,levels=store$name)


# Plotting ----------------------------------------------------------------

the.color <- "dodgerblue"
m <- ggplot(store,aes(x=X1,y=group)) +
  geom_point(size=1.75,colour=the.color) + xlim(0,1) + ylab("") + 
  xlab("choice of candidates: p-value from joint test of\n pairwise interactions between filler attributes and respondent income, party, education, and gender") +
  theme_economist(dkpanel = T) + 
  theme(axis.text.x = element_text(size=10,face='bold'), axis.title.x = element_text(size=10)) +
  theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=10)) +
  theme(legend.text = element_text(size=10), legend.title = element_text(size=10))

pdf("../../results/figures/figA16.pdf",width=10,height=12)
m
dev.off()
