#9595437
rm(list=ls())
library(dplyr)
library(pbapply)
data=read.csv("c:/Users/hgcha/OneDrive/Documents/clicks_train.csv")

#number of unique ad id: 478950
length(unique(data$ad_id))

#scenarios: 16874593
length(unique(data$display_id))

#counts data
counts=data %>%  group_by(display_id) %>% count()
max(counts[,2])

###choice set with size 12
large_set=counts[which(counts[,2]==12),]
temp=as.numeric(large_set$display_id)

###subsets of the size-12 sets
n_distinct_subset=read.csv("number_of_distinct_subsets.csv")
hist(n_distinct_subset[,3])
which.max(n_distinct_subset[,3])
n_distinct_subset=n_distinct_subset[order(n_distinct_subset[,3],decreasing=TRUE),]
n_distinct_subset[which.max(n_distinct_subset[,3]),]


