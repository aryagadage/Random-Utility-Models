library(dplyr)

path='/Users/haoge/Dropbox/Research/RUM_Testing/Dataset/FeldmanRehbeck_QE_2022/'
setwd(path)

#import dataaset
raw_data=read.csv('RP_r_clean.csv')

#lottery list
lottery_list=rbind(as.matrix(raw_data[1:79,16:18]),as.matrix(raw_data[1:79,19:21]))
lottery_list<-cbind(1:27, unique(lottery_list) )#27 unique choice set

#recode the data so that the discete choice tasks are easy to aggregate
#raw_data[which(raw_data$r.task==81),'r.task']=80
#raw_data[which(raw_data$r.task==82),'r.task']=80
#raw_data[which(raw_data$r.task==83),'r.task']=81
#raw_data[which(raw_data$r.task==84),'r.task']=81
#raw_data[which(raw_data$r.task==85),'r.task']=81

#only include convex choice task
raw_data=raw_data[which(raw_data$r.task<=79),]

#sanity check: 144 units and 79 choice tasks = 11376 observations
length(unique(raw_data$u.id.otree))

#combine chioce data
choice_data = raw_data %>% group_by(r.task) %>% summarise(avg=mean(alpha)/100)

#alternaative
choice_set=cbind(raw_data[1:79,c('r.task')],raw_data[1:79,16:21])

#combine data
choice_data=cbind(choice_data,choice_set)
choice_data[,'set_alt1']=NA
choice_data[,'set_alt2']=NA
choice_data[,'alt']=NA


#label choice set and alternatives
for (i in 1:nrow(choice_data)){
  
    for (j in 1:nrow(lottery_list)){
      
      if (all(lottery_list[j,2:4]==choice_data[i,4:6])){
        
        choice_data[i,'set_alt1']=lottery_list[j,1] #if match: update index
        choice_data[i,'alt']=lottery_list[j,1] #if match: update index
        
      }
      
      if (all(lottery_list[j,2:4]==choice_data[i,7:9])){
        
        choice_data[i,'set_alt2']=lottery_list[j,1]
        
      }      
    }


}

#create final choice data
final_choice_data=matrix(NA,nrow=2*nrow(choice_data),ncol=4)
for (i in 1:nrow(choice_data)){
  
  final_choice_data[2*i-1,1]=choice_data[i,2]
  final_choice_data[2*i-1,2]=choice_data[i,'set_alt1']
  final_choice_data[2*i-1,3]=choice_data[i,'set_alt2']
  final_choice_data[2*i-1,4]=choice_data[i,'alt']
  
  
  final_choice_data[2*i,1]=1-choice_data[i,2]
  final_choice_data[2*i,2]=choice_data[i,'set_alt1']
  final_choice_data[2*i,3]=choice_data[i,'set_alt2']
  final_choice_data[2*i,4]=choice_data[i,'set_alt2']
  
  
}

colnames(final_choice_data)=c('choice_probability','set_alt1','set_al2','alt')
write.csv(final_choice_data,'cleaned_choice_frequency.csv')
