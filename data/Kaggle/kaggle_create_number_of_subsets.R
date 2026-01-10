library(dplyr)
args <- commandArgs(trailingOnly = TRUE)
this_index=as.integer(args[1])

#load data
data=read.csv("clicks_train.csv")

#counts data
counts=data %>%  group_by(display_id) %>% count()

###largest choice set size
max(counts[,2])
###choice set with size 12
large_set=counts[which(counts[,2]==12),]
large_set_ind=as.numeric(large_set$display_id)

####################################################
##################Index#############################
####################################################
start_index=(this_index-1)*20+1
end_index = min(this_index*20,length(large_set_ind))


###################################################
###create number of contained choice sets##########
####################################################
output=c()
for (i in start_index:end_index){
  print(i)
  target_ads=data[which(data$display_id==large_set_ind[i]),'ad_id']
  contained_ids_proper <- data %>%
    group_by(display_id) %>%
    summarise(n = n(), n_in = sum(ad_id %in% target_ads), .groups="drop") %>%
    filter(n == n_in, n < length(target_ads)) %>%
    pull(display_id)
  
  output=rbind(output,c(large_set_ind[i],length(contained_ids_proper)))

  }

file_name=paste0('/home/hc654/palmer_scratch/kaggle_summary/',start_index,'_',end_index,'.csv')
write.csv(output,file_name)
