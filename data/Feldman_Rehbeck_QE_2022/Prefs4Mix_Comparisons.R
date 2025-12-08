# Replication File for Feldman and Rehbeck (2021) "Revealing a Preference for Mixtures: An Experimental Study of Risk"
# This file includes all tests for consistency with a "preference for randomization" which strengthens a "preference for mixtures".
# Generates "heat maps" (Figure 8) in section 4.2
# This version:September 21,2021
###############
# R.version
# _                           
# platform       x86_64-apple-darwin17.0     
# arch           x86_64                      
# os             darwin17.0                  
# system         x86_64, darwin17.0          
# status                                     
# major          4                           
# minor          1.1                         
# year           2021                        
# month          08                          
# day            10                          
# svn rev        80725                       
# language       R                           
# version.string R version 4.1.1 (2021-08-10)
# nickname       Kick Things  
################

install.packages("ggplot2") # Can comment out if already installed to speed up
# Please see https://www.datacamp.com/community/tutorials/r-packages-guide if you are having issues installing packages in R
library(ggplot2)
# Import data
getwd() # Should return folder containing data file and "Prefs4Mix_Comparisons.r", else open .r file directly or set directory manually to "~/R_Output"
DT=read.csv("RP_r_clean.csv")
## Variables (numbers should match matlab matrix files column identifiers)
# %(1) id=per session
# %(2) ee= unique subject identifier
# %(4) alpha=weight on option (B)
# %(5) r.task=real task number
# %(6) n.task=order tasks were in
# %(7) ProbC1= probability of choice (A) assigned to $2
# %(8) ProbB1= probability of choice (A) assigned to $10
# %(9) ProbA1= probability of choice (A) assigned to $30
# %(10) ProbC2= probability of choice (B) assigned to $2
# %(11) ProbB2= probability of choice (B) assigned to $10
# %(12) ProbA2= probability of choice (B) assigned to $30

# Note: data is presorted by r.task and ee, so ordering agrees with numbering of 
# budgets from right to left in the standard MM triangle (plow,phigh), starting 
# on the bottom right corner (1,0), and not the order in which tasks appeared

# setwd("~/Dropbox/your_dir") # Use this to output graphs to a different location

# non-constant == randomizing, interior == mixing

######################
# Set the budgets #
######################
# Budgets sets were hardcoded and copied directly from our otree app to prevent inconsistencies

#Verbatim snipet from otree's model file
###################################################################################################################################################################
# class Constants(BaseConstants):
    # name_in_url = 'risk_preferences'
    # players_per_group = None


# #option A
    # probA1 = [5,10,15,20,25,30,35,40,45,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,
    # 90,95,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,35,40,45,50,55,60,65,70,75,80,85,
    # 90,95,50,55,60,65,70,75,80,85,90,95,65,70,75,80,85,90,95,80,85,90,95,95]
    # probC1 = [95,90,85,80,75,70,65,60,55,95,90,85,80,75,70,65,60,55,50,45,40,35,30,25,20,15,
    # 10,5,80,75,70,65,60,55,50,45,40,35,30,25,20,15,10,5,65,60,55,50,45,40,35,30,25,20,15,
    # 10,5,50,45,40,35,30,25,20,15,10,5,35,30,25,20,15,10,5,20,15,10,5,5]
# #option B
    # probA2 = [  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    # 0,  0,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,25,25,25,25,25,25,25,25,25,25,25,
    # 25,25,40,40,40,40,40,40,40,40,40,40,55,55,55,55,55,55,55,70,70,70,70,85]
    # probC2 = [ 50, 50, 50, 50, 50, 50, 50, 50, 50,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    # 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    # 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0]


    # prizeA = c(30)
    # prizeB = c(10)
    # prizeC = c(2)

    # probB1=[100 - x - y for x, y in zip(probA1, probC1)]
    # probB2=[100 - x - y for x, y in zip(probA2, probC2)]

# #Part II
# # option A Risky
    # prob2A1 = [35,35,35,5,5,5]
    # prob2C1 = [65,65,65,95,95,95]
 # # option B Safe
    # prob2A2 = [10,10,10,0,0,0]
    # prob2C2 = [0,0,0,50,50,50]

    # prob2B1 = [100 - x - y for x, y in zip(prob2A1, prob2C1)]
    # prob2B2 = [100 - x - y for x, y in zip(prob2A2, prob2C2)]

###################################################################################################################################################################

## Convex Budgets ##
#Option A: The extreme lottery
  probA1 <- c(.05, .10, .15, .20, .25, .30, .35, .40, .45, .05, .10, .15, .20, .25, .30, .35, .40, .45, .50, .55, .60, .65, .70, .75, 		.80, .85, .90, .95, .20, .25, .30, .35, .40, .45, .50, .55, .60, .65, .70, .75, .80, .85, .90, .95, .35, .40, .45, .50, .55, .60, .65, 		.70, .75, .80, .85, .90, .95, .50, .55, .60, .65, .70, .75, .80, .85, .90, .95, .65, .70, .75, .80, .85, .90, .95, .80, .85, .90, 
              .95, .95)
  
  probC1 <- c( .95, .90, .85, .80, .75, .70, .65, .60, .55, .95, .90, .85, .80, .75, .70, .65, .60, .55, .50, .45, .40, .35, .30, .25, 
               .20, .15, .10, .05, .80, .75, .70, .65, .60, .55, .50, .45, .40, .35, .30, .25, .20, .15, .10, .05, .65, .60, .55, .50, .45, .40, 
               .35, .30, .25, .20, .15, .10, .05, .50, .45, .40, .35, .30, .25, .20, .15, .10, .05, .35, .30, .25, .20, .15, .10, .05, .20, .15, 
               .10, .05, .05)
               
  probB1 <- 1-probA1-probC1
  
  #Option B: The numeraire lottery
  probA2 <- c( 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 0,  0, .10, .10, 
               .10, .10, .10, .10, .10, .10, .10, .10, .10, .10, .10, .10, .10, .10, .25, .25, .25, .25, .25, .25, .25, .25, .25, .25, .25, .25, 
               .25, .40, .40, .40, .40, .40, .40, .40, .40, .40, .40, .55, .55, .55, .55, .55, .55, .55, .70, .70, .70, .70, .85)
  
  probC2 <- c(.5, .5, .5, .5, .5, .5, .5, .5, .5,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 0,  0,  0,  0,  0,
              0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  
              0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0)
              
  probB2 <- 1-probA2-probC2

## Discrete Budgets ##
d2 <- 1
d1 <- 32
d <- c(d1,d2)
n <- max(DT$u.id) #number of subjects

# Preallocate
cC<-matrix(data = 0, nrow = n, ncol = length(d))
dC<-matrix(data = 0, nrow = n, ncol = length(d) )
con <- vector(mode = "numeric", length = length(d))
corr <- vector(mode = "numeric", length = length(d))
Cor<-matrix(data = 0, nrow = n, ncol = 2*length(d) )

# Double-check Discrete choice tasks match Verbatim snippet
# Note, p=pH,pM,pL
D1.s<-c(probA2[d1],probB2[d1],probC2[d1])
D1.r<-c(probA1[d1],probB1[d1],probC1[d1])
D1<-c(D1.s,D1.r)
D2.s<-c(probA2[d2],probB2[d2],probC2[d2])
D2.r<-c(probA1[d2],probB1[d2],probC1[d2])
D2<-c(D2.s,D2.r)

print(D1)
print(D2)

########################################################################################
# Extract the Convex tasks and match them to the implied mixtures from the repeated discrete tasks
########################################################################################

for (k in 1:length(d) ){
  for (i in 1:n){
    it=85*(i-1) # Each subjects choices
    kt=it+d[k] # Get budgets 1 and 32 for every subject
    dt=it+79+3*(k-1) # 79 choices are convex, next 3x2 are discrete
    cC[i,k]=DT$alpha[kt] # Convex choice alpha
    dC[i,k]=(DT$alpha[dt+1]+DT$alpha[dt+2]+DT$alpha[dt+3])/3 # Discrete choice alpha
  }
}

###################################################################################
#Figure 8: Convex Mixtures Against Implied Discrete Mixtures#
###################################################################################

#Panel A: Steeper Task (D1)
b1<- dC[,1] #Discrete
c1<- cC[,1] #Convex
near<- vector(mode = "numeric", length = length(b1))

# Loop for discretizing distances
for (k in 1:length(b1)){
	if ( min( abs(c1[k]-0), abs(c1[k]-33.3333), abs(c1[k]-66.6666), abs(c1[k]-100) ) == abs(c1[k]-0) ){
		near[k]=round(abs(0-b1[k])/33.33333,digits=0)
	}		
	else if ( min( abs(c1[k]-0), abs(c1[k]-33.3333), abs(c1[k]-66.6666), abs(c1[k]-100) ) == abs(c1[k]-33.3333) ){
		near[k]=round(abs(33.3333-b1[k])/33.33333,digits=0)
	}
	else if ( min( abs(c1[k]-0), abs(c1[k]-33.3333), abs(c1[k]-66.6666), abs(c1[k]-100) ) == abs(c1[k]-66.6666) ){
		near[k]=round(abs(66.6666-b1[k])/33.33333,digits=0)
	}
	else{
		near[k]=round(abs(100-b1[k])/33.33333,digits=0)
	}
}

# Color Graph
df <-as.data.frame(cbind(b1, c1))
pdf("corr_B_C_D1.pdf")
ggplot(df, aes(x=b1, y=c1, color=near)) +scale_color_gradient(low="green", high="red") + geom_point() + geom_count() + scale_radius(breaks=c(1,10,30,50,90), range = c(1,5) ) + ylab("Chosen Mixtures on Convex-Choice Tasks") + xlab("Implied Mixtures on Discrete-Choice Tasks") + geom_abline(intercept = 0, slope = 1) 
dev.off()

# B&W Graph
df <-as.data.frame(cbind(b1, c1, near))
pdf("corr_B_C_D1_greyscale.pdf")
ggplot(df, aes(x=b1, y=c1, color=near)) + scale_color_gradient(low="white", high="black") + geom_point() + geom_count() + scale_radius(breaks=c(1,10,30,50,90), range = c(1,5) ) + ylab("Chosen Mixtures on Convex-Choice Tasks") + xlab("Implied Mixtures on Discrete-Choice Tasks") + geom_abline(intercept = 0, slope = 1) 
dev.off()

#Panel B: Flatter Task (D2)
bb1<- dC[,2] #Discrete
cc1<- cC[,2] #Convex
near<- vector(mode = "numeric", length = length(bb1))

# Loop for discretizing distances
for (k in 1:length(b1)){
	if ( min( abs(cc1[k]-0), abs(cc1[k]-33.3333), abs(cc1[k]-66.6666), abs(cc1[k]-100) ) == abs(cc1[k]-0) ){
		near[k]=round(abs(0-bb1[k])/33.33333,digits=0)
	}		
	else if ( min( abs(cc1[k]-0), abs(cc1[k]-33.3333), abs(cc1[k]-66.6666), abs(cc1[k]-100) ) == abs(cc1[k]-33.3333) ){
		near[k]=round(abs(33.3333-bb1[k])/33.33333,digits=0)
	}
	else if ( min( abs(c1[k]-0), abs(cc1[k]-33.3333), abs(cc1[k]-66.6666), abs(cc1[k]-100) ) == abs(cc1[k]-66.6666) ){
		near[k]=round(abs(66.6666-bb1[k])/33.33333,digits=0)
	}
	else{
		near[k]=round(abs(100-bb1[k])/33.33333,digits=0)
	}
}

# Color Graph
df2 <-as.data.frame(cbind(bb1, cc1))
pdf("corr_B_C_D2.pdf")
ggplot(df2, aes(x=bb1, y=cc1, color=near)) + scale_color_gradient(low="green", high="red") + geom_point() + geom_count() + scale_radius(breaks=c(1,10,30,50,90), range = c(1,9) ) + ylab("Chosen Mixtures on Convex-Choice Tasks") + xlab("Implied Mixtures on Discrete-Choice Tasks") + geom_abline(intercept = 0, slope = 1) 
dev.off()

# B&W Graph
df2 <-as.data.frame(cbind(bb1, cc1, near))
pdf("corr_B_C_D2_greyscale.pdf")
ggplot(df2, aes(x=bb1, y=cc1, color=near)) + scale_color_gradient(low="white", high="black") + geom_point()+ geom_count() + scale_radius(breaks=c(1,10,30,50,90), range = c(1,9) )  + ylab("Chosen Mixtures on Convex-Choice Tasks") + xlab("Implied Mixtures on Discrete-Choice Tasks") + geom_abline(intercept = 0, slope = 1) 
dev.off()
  
