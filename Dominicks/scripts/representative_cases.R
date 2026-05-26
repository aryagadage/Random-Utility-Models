#set working directory and read data
path="/Users/haoge/Dropbox/Research/Random-Utility-Models"
setwd(path)

#all categories data 
all_categories_data=read.csv("./Dominicks/store_summary_all_categories.csv")

#create quantiles in terms of number of UPCs and number of distinct menus
num_upcs=quantile(all_categories_data$n_upcs,probs=seq(0,1,by=0.1))
num_menus=quantile(all_categories_data$n_menus,probs=seq(0,1,by=0.1))


# 11*11 categories
possible_combinations=expand.grid(num_upcs,num_menus)
possible_combinations=cbind(1:121,possible_combinations)
#possible_combinations

#attribute each store/category to a combination
#  bucket = smallest (upc_q, menu_q) rectangle that contains the row.
#  iterate from largest bucket down so the smallest fitting one wins.
all_categories_data$upc_menu_complexity_group <- NA_integer_
for (i in nrow(possible_combinations):1) {
  ind <- all_categories_data$n_upcs  <= possible_combinations[i, 2] &
         all_categories_data$n_menus <= possible_combinations[i, 3]
  all_categories_data$upc_menu_complexity_group[ind] <- possible_combinations[i, 1]
}

write.csv(all_categories_data,"./Dominicks/store_summary_all_categories_with_complexity_group.csv",row.names=FALSE)


#calculate depth
all_categories_data=read.csv('./Dominicks/store_summary_all_categories_min_cs.csv')
all_categories_data$depth=all_categories_data$n_upcs-all_categories_data$min_menu_size
num_depth=quantile(all_categories_data$depth,probs=seq(0,1,by=0.001))
all_categories_data_shallow=all_categories_data[all_categories_data$depth<=10,]
