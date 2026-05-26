library(lmtest)
library(sandwich)
library(ggplot2)
library(ggthemes)


# Load and pre-process data -----------------------------------------------

d <- read.csv("../../data/cand_s1.csv")
filler <- c("highway","marital","relative","school","vacation")
fixed <- c("cage","chealthcare","cmarriage","cparty")

d$r_gender <- as.character(d$r_gender)
d$r_party  <- as.character(d$r_party)
d$r_educ   <- as.character(d$r_educ)
d$r_age    <- as.character(cut(d$r_age,breaks=quantile(d$r_age,probs=seq(0,1,.25)),
                               include.lowest = TRUE)
)

d$cage        <- as.factor(d$cage)
d$chealthcare <- as.factor(d$chealthcare)
d$cmarriage   <- as.factor(d$cmarriage)
d$cparty      <- as.factor(d$cparty)


# Estimation --------------------------------------------------------------

store <- matrix(NA,0,2)
for (k in 1:length(filler)){
  
  fn <- filler[k]

  # formula: predict filler based on fixed attributes, respondent characteristics, and first order interactions 
  form1 <- as.formula(paste(fn,"~",                         
                            paste(fixed,"*r_educ",collapse="+"),"+",
                            paste(fixed,"*r_gender",collapse="+"),"+",
                            paste(fixed,"*r_income",collapse="+"),"+",
                            paste(fixed,"*r_age",collapse="+"),"+",
                            paste(fixed,"*r_party",collapse="+"))
  )
  
  
  # formula: predict filler based on fixed attributes, respondent characteristics, and no interactions
  form2 <- as.formula(paste(fn,"~",paste(fixed,collapse="+"),"+",
                            "r_educ+r_gender+r_income+r_age+r_party"))
  
  # fit both models
  t1 <- try(lm(form1,data=d))
  t2 <- try(lm(form2,data=d))
  
  # wald test for interactions jointly zero (if both models fit)
  if(class(t1) != "try_error" & class(t2) != "try_error" ){
    
    wout <- try(waldtest(t1, t2))
    cat(fn,"\n") 
    cat(wout$`Pr(>F)`[2],"\n")
    
    # store p-value and N
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
p <- ggplot(store,aes(x=X1,y=group)) +
  geom_point(size=1.75,colour=the.color) + xlim(0,1) + ylab("") + 
  xlab("choice of filler attributes: p-value from joint test of\n pairwise interactions between fixed attributes and respondent income, party, education, and gender") +
  theme_economist(dkpanel = T) + 
  theme(axis.text.x = element_text(size=10,face='bold'), axis.title.x = element_text(size=10)) +
  theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=10)) +
  theme(legend.text = element_text(size=10), legend.title = element_text(size=10))

pdf("../../results/figures/figA15.pdf",width=10,height=8)
p
dev.off()
