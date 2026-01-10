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

############################################################################
############################################################################
############################################################################

find_contained_subsets=function(target_ads){
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
  
  
  contained_distinct_patterns <- data %>%
    filter(display_id %in% contained_ids) %>%
    group_by(display_id) %>%
    summarise(
      set_id = paste(sort(unique(ad_id)), collapse = "_"),
      .groups = "drop"
    )%>%
    distinct(set_id) 
  
  return(contained_distinct_patterns)
}

############################################################################
############################################################################
############################################################################
alt_13405649_15976803=unique(data[which(data$display_id %in% c(13405649,15976803)),2])
pattern_13405649_15976803=find_contained_subsets(alt_13405649_15976803)



############################################################################
############################################################################
############################################################################
alt_13405649_15976803_1684329=unique(data[which(data$display_id %in% c(13405649,15976803,1684329)),2])
pattern_13405649_15976803_1684329=find_contained_subsets(alt_13405649_15976803_1684329)

############################################################################
############################################################################
############################################################################
alt_13405649_15976803_1716222=unique(data[which(data$display_id %in% c(13405649,15976803,1716222)),2])
pattern_13405649_15976803_1716222=find_contained_subsets(alt_13405649_15976803_1716222)

############################################################################
############################################################################
############################################################################
alt_13405649_15976803_10660495=unique(data[which(data$display_id %in% c(13405649,15976803,10660495)),2])
pattern_13405649_15976803_10660495=find_contained_subsets(alt_13405649_15976803_10660495)