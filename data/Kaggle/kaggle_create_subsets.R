library(dplyr)
args <- commandArgs(trailingOnly = TRUE)
this_index=as.integer(args[1])

#load data
data=read.csv("/Users/haoge/Desktop/clicks_train.csv")

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
  
  # 1) filter contained display_ids
  contained_subsets <- data %>%
    group_by(display_id) %>%
    summarise(
      n    = n(),
      n_in = sum(ad_id %in% target_ads),
      .groups = "drop"
    ) %>%
    filter(n == n_in, n < target_len) %>%
    pull(display_id)
  
  # 2) subset data based on the filtered id
  data_subset <- data %>%
    filter(display_id %in% contained_subsets)
  
  # 3) map ad_ids to index 
  ad_map <- data_subset %>%
    distinct(ad_id) %>%
    arrange(ad_id) %>%
    mutate(ad_idx = row_number())
  
  data_subset <- data_subset %>%
    left_join(ad_map, by = "ad_id")
  
  # 4) distinct choice set patterns
  set_id_by_display <- data_subset %>%
    filter(display_id %in% contained_subsets) %>%
    group_by(display_id) %>%
    summarise(
      set_id = paste(sort(unique(ad_idx)), collapse = "_"),
      .groups = "drop"
    )
  
  # 5) Merge data with choice set patterns
  data_subset <- data_subset %>%
    left_join(set_id_by_display,by='display_id')
  
  # 6) choice probabilities
  choice_set_probabilities <- data_subset %>%
    filter(!is.na(set_id)) %>%
    group_by(set_id, ad_idx) %>%
    summarise(
      freq_rows   = n(),                        # total observations (rows) for (set_id, ad_idx)
      avg_clicked = mean(clicked, na.rm = TRUE),# optional: example average
      .groups = "drop"
    )
  
  return(list(data = choice_set_probabilities, map = ad_map,choice_set=set_id_by_display))
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