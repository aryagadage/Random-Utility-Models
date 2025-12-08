# Replication File for Feldman and Rehbeck (2021) "Revealing a Preference for Mixtures: An Experimental Study of Risk"
# This file includes all tests for consistency with a "preference for randomization" which strengthens a "preference for mixtures".
# Generates Choice Triangles: Figure 12, 20, and 21
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

#Specify output location for Figure 12 triangles
a->"~/Dropbox/R_Output/"
#Specify output location for Figure 20 triangles
b->"~/Dropbox/R_Output/"
#Specify output location for Figure 21 triangles
c->"~/Dropbox/R_Output/All_Choice_Triangles"

# Import data
getwd() # Should return folder containing data file and "Prefs4Mix_Comparisons.r", if it open .r file directly or set directory manually to "~/R_Output"
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


# Number of Subjects in Experiment:
n <- max(DT$u.id) 
# Total Number of CONVEX tasks.
m <- 79
# Total Number of Tasks:
m2 <- 85 

# Individuals for types
i.a <- 123 #which.min(iCV$EU) #Returns EU Guy
i.b <- 20 # Middle Thresholding
i.c <- 143 # Combination
i.d <- 11 # Price Responsive
i.e <- 132 # Low Thresholding
i.f <- 62 #Returns Random Guy

ind<- c(i.a,i.b,i.c, i.d, i.e, i.f)

# Denotes the breaks for the distinct endowments:
breaks <- c(0,9,28,44,57,67,74,78,79)
# Budget Lines Considered: E1, E2 and E5.
lb=breaks[1]+1 #First budget line considered
mlb=breaks[3]
mub=breaks[5]+1
ub=breaks[6] #Last budget line considered
# Generates the subset:
c<-seq(lb,mlb, by=1)
c<-c(c,seq(mub,ub, by=1))

###################################################################################################
# Generates Choice Triangles for types for Endowments 1,2 and 5.
# Figure 12
setwd(a)#Select output folder

for (i in 1:length(ind)) {# Number of Sujects
	pdf(paste("ind_choice2_E125_",ind[i],".pdf",sep=""))
	# Plot the diagonal line
	p10= seq(from=0, to=1, by=.01) #Changed labeling of pi's to make it consistent with xi's.
	p30= 1-p10
	p0= p10*0 #For the sides of the triangle.

	#Creating the triangle
	plot(p10, p30, type="l", xlim=c(0,1), lty="solid", lwd=2.5, xlab="P$2", ylab="P$30")
	lines(p30, p0, lwd=2.5) #Generates edges of triangle.
	lines(p0, p10, lwd=2.5) #Generates edges of triangle.
	
		for(j in 1:length(c)){
			x=c(probC1[c[j]],probC2[c[j]])
			y=c(probA1[c[j]],probA2[c[j]])
			lines(x, y)
			xx=c(DT$alpha[85*(ind[i]-1)+c[j]]/100*probC2[c[j]] + (1-DT$alpha[85*(ind[i]-1)+c[j]]/100)*probC1[c[j]])
			yy=c(DT$alpha[85*(ind[i]-1)+c[j]]/100*probA2[c[j]] + (1-DT$alpha[85*(ind[i]-1)+c[j]]/100)*probA1[c[j]])
			points(xx,yy, pch=19, lwd=2.5)
			} 
	dev.off() 
	}

###################################################################################################
# Generates Choice Triangles for types for Endowments 1-7.	
# Figure 20
setwd(b)#Select output folder

for (i in 1:length(ind)) {# Number of Sujects
	pdf(paste("ind_choice2_Eall_",ind[i],".pdf",sep=""))
	# Plot the diagonal line
	p10= seq(from=0, to=1, by=.01) #Changed labeling of pi's to make it consistent with xi's.
	p30= 1-p10
	p0= p10*0 #For the sides of the triangle.

	#Creating the triangle
	plot(p10, p30, type="l", xlim=c(0,1), lty="solid", lwd=2.5, xlab="P$2", ylab="P$30")
	lines(p30, p0, lwd=2.5) #Generates edges of triangle.
	lines(p0, p10, lwd=2.5) #Generates edges of triangle.
	
	for(j in 1:m){
			x=c(probC1[j],probC2[j])
			y=c(probA1[j],probA2[j])
			lines(x, y)
			xx=c(DT$alpha[85*(ind[i]-1)+j]/100*probC2[j] + (1-DT$alpha[85*(ind[i]-1)+j]/100)*probC1[j])
			yy=c(DT$alpha[85*(ind[i]-1)+j]/100*probA2[j] + (1-DT$alpha[85*(ind[i]-1)+j]/100)*probA1[j])
			points(xx,yy, pch=19, lwd=2.5)
			} 
	dev.off() 
	}

###################################################################################################
# Generates Choice Triangles for All Subjects for Endowments 1-7.
# Figure 21

#Set the folder correctly to package the output neatly
setwd(c)
for (i in 1:n) {# Number of Sujects
	pdf(paste("ind_choice2_Eall_",i,".pdf",sep=""))
	# Plot the diagonal line
	p10= seq(from=0, to=1, by=.01) #Changed labeling of pi's to make it consistent with xi's.
	p30= 1-p10
	p0= p10*0 #For the sides of the triangle.

	#Creating the triangle
	plot(p10, p30, type="l", xlim=c(0,1), lty="solid", lwd=2.5, xlab="P$2", ylab="P$30")
	lines(p30, p0, lwd=2.5) #Generates edges of triangle.
	lines(p0, p10, lwd=2.5) #Generates edges of triangle.
	
	for(j in 1:m){
			x=c(probC1[j],probC2[j])
			y=c(probA1[j],probA2[j])
			lines(x, y)
			xx=c(DT$alpha[85*(ind[i]-1)+j]/100*probC2[j] + (1-DT$alpha[85*(ind[i]-1)+j]/100)*probC1[j])
			yy=c(DT$alpha[85*(ind[i]-1)+j]/100*probA2[j] + (1-DT$alpha[85*(ind[i]-1)+j]/100)*probA1[j])
			points(xx,yy, pch=19, lwd=2.5)
			} 
	dev.off() 
	}
  