# Convert all conjoint attributes to factors ------------------------------

mark1 <- which(colnames(svyx) == "cfloor")
mark2 <- which(colnames(svyx) == "cinternet")
mark3 <- which(colnames(svyx) == "cpillowsm")
mark4 <- length(colnames(svyx))

for (i in mark1:mark2){
  svyx[,i] <- as.factor(svyx[,i])
}

for (i in mark3:mark4){
  svyx[,i] <- as.factor(svyx[,i])
}

#Set reference levels for core attributes
svyx$cview <- relevel(svyx$cview, ref = "Mountain view")
svyx$cfurniture <- relevel(svyx$cfurniture, ref = "1 queen bed and 1 large couch")
svyx$cinternet <- relevel(svyx$cinternet, ref = "Free standard bandwidth wireless")
svyx$cfloor <- relevel(svyx$cfloor, ref = "Gym and spa floor (5th)")

rm(mark1,mark2,mark3,mark4)