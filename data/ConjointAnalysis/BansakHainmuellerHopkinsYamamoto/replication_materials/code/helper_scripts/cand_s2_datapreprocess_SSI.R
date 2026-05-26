
# Convert all conjoint attributes (original and recoded) to factors -----

mark1 <- which(colnames(svyx) == "cage")
mark2 <- which(colnames(svyx) == "cmarriage")
mark3 <- which(colnames(svyx) == "cpartyp")
mark4 <- length(colnames(svyx))

for (i in mark1:mark2){
  svyx[,i] <- as.factor(svyx[,i])
}

for (i in mark3:mark4){
  svyx[,i] <- as.factor(svyx[,i])
}

rm(mark1,mark2,mark3,mark4)