library(lmtest)
library(ggplot2)
library(ggthemes)


# Load and pre-process data -----------------------------------------------

svyx <- read.csv("../../data/cand_s2_SSI.csv")
source("../helper_scripts/cand_s2_datapreprocess_SSI.R")
fixed <- c("cpartyp","cmarriagep","chealthcarep","cagep")
filler <- c("cmaritalm","cvacationm","crelativem","chighwaym",
            "cschoolm","cdogm","ctrainm","cicecreamm","celectionm",
            "cvotem","ctripm","cstonem","cclassm","cdinosaurm","creportm",
            "cdaybornm","cfavcolorm","cyearbornm","cnamem","cfoodm",
            "cbirthdaym","cclassroomm","cbaseballm","cbeveragem",
            "cseasonm","ctreem","ccomposerm","caddressm","cdoorm",
            "ccarcolorm","cteamm","cemailm","cshopm","cwarm","cgrandm")

# reduce to partisans and exclude second stage runs without any fillers (condition is 0)
d <- data.frame(subset(svyx,svyx$partisan == 1 & svyx$condition != 0))

d$r_gender <- as.character((d$r_gender))
d$r_party  <- as.character((d$r_party))
d$r_educ   <- as.character((d$r_educ))
d$r_age    <- as.character(cut(d$r_age,breaks=quantile(d$r_age,probs=seq(0,1,.25)),
                           include.lowest = TRUE)
                           )


# Estimation --------------------------------------------------------------

store <- matrix(NA,0,2)
for (k in 1:length(filler)){
  
  fn <- filler[k]
  # subset to profiles where specific filler is shown  
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
  form2 <- as.formula(paste("pref~",
                            fn,"+","r_educ+r_gender+r_income+r_age+r_party"
                            )
                      )
            
  # run both models
  t1 <- try(lm(form1,data=dsub))
  t2 <- try(lm(form2,data=dsub))

  # fit boths fit do wald test
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

store       <- store[order(store$X1),]
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

pdf("../../results/figures/figA19.pdf",width=10,height=12)
m
dev.off()
