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
  
  target_ads <- data[data$display_id == large_set_ind[i], "ad_id"]
  target_ads <- unique(target_ads)
  target_len <- length(target_ads)
  
  # 1) filter contained display_ids WITHOUT constructing set_id
  contained_ids <- data %>%
    group_by(display_id) %>%
    summarise(
      n    = n(),
      n_in = sum(ad_id %in% target_ads),
      .groups = "drop"
    ) %>%
    filter(n == n_in, n < target_len) %>%
    pull(display_id)
  
  # 2) construct the choice-set signature ONLY for these contained ids
  contained_distinct_patterns <- data %>%
    filter(display_id %in% contained_ids) %>%
    group_by(display_id) %>%
    summarise(
      set_id = paste(sort(unique(ad_id)), collapse = "_"),
      .groups = "drop"
    ) %>%
    distinct(set_id) %>%
    nrow()
  
  output <- rbind(output, c(large_set_ind[i], contained_distinct_patterns))
}

file_name=paste0('/home/hc654/palmer_scratch/kaggle_summary/','distinct_',start_index,'_',end_index,'.csv')
write.csv(output,file_name)
