#load packages
library(tidyverse)

#Set working directory to location of data
setwd("G:/My Drive/CodeChallenge2025/CodeChallenge2025/")

#Load data into memory
IDs<-readLines("IDs.txt")
load("CodeChallenge2024.RData")


#############################################################
# Data cleaning
#############################################################

#Get earliest consent date for each participant
consent_date<-consent_date %>%
  group_by(ID) %>%
  summarize(earliest_consent_date=min(c_across(starts_with("reg_")),na.rm=T))
  
#Look to ensure all IDs are in id_map
sum(HAM_sleep$ID %in% id_map$new_id)==nrow(HAM_sleep)
sum(HAM_protect$ID %in% id_map$old_id)==nrow(HAM_protect)
sum(consent_date$ID %in% id_map$new_id)==nrow(consent_date)

#Filter id_map to only include wanted IDs
id_map<-filter(id_map,old_id %in% IDs)

#IDs need to be converted to new_id & filtered
HAM_protect<-inner_join(HAM_protect,id_map,by=join_by(ID==old_id)) %>%
  select(-ID) %>% select(new_id,everything())
HAM_sleep<-rename(HAM_sleep,new_id=ID)

#Look at data to determine whether any of the non-numeric columns have string data that needs to be preserved
str(HAM_sleep)
str(HAM_protect)
why_char<-select(HAM_protect,where(is.character))
for (c in names(why_char[-1:-2])){
  print(c)
  print(unique(why_char[[c]]))
}
rm(why_char)

#String data can be converted to numeric
HAM_protect<-mutate(HAM_protect,across(c("new_id",starts_with("ham")),as.numeric))

#Check to make sure all column names are the same
sum(names(HAM_protect)==names(HAM_sleep))==ncol(HAM_protect)

#Merge data frames
HAM_merged<-full_join(HAM_protect,HAM_sleep) %>%
  left_join(consent_date,by=join_by(new_id==ID))%>%
  #Check merged data to ensure merge was successful
  {print(paste0("equal rows? ",nrow(.)==nrow(HAM_sleep)+nrow(HAM_protect)));.} %>%
  {print(paste0("duplicates? ",sum(duplicated(.))));.} %>%
  #Find rows with no data, and with dates where HAM was administered
  mutate(all_NA=rowSums(across(c(bq_date:ham_17_weight),~!is.na(.x)))==0,
                   not_NA_HAM=rowSums(across(starts_with("ham"),~!is.na(.x)))) %>%   
  #Remove rows with no data
  filter(!all_NA) %>%
  #Calculate score if administered
  mutate(total_ham=ifelse(not_NA_HAM>0,
                          rowSums(across(matches("ham_[0-9]+_[a-z]+")),na.rm=T),
                          NA),
         #Combine date fields into one
         date_admin=as.Date(ifelse(is.na(bq_date),fug_date,bq_date))) %>%  
  group_by(new_id) %>%
  #Find relevant dates for each participant
  mutate(latest_ham_date=max(date_admin[which(not_NA_HAM>0)],na.rm=T), #Only want latest date HAM was administered
         diffoneyear=difftime(date_admin,earliest_consent_date+years(1),units="days")) %>%
  #Remove extra fields
  select(-all_NA,-bq_date,-fug_date,-starts_with("ham"))

#Get latest score
latest_HAMs<-filter(HAM_merged,date_admin==latest_ham_date& not_NA_HAM>0)%>% 
  rename(latest_ham=total_ham) %>% select(new_id,latest_ham) %>% unique()

#Get score closest to 1yr followup (that isn't baseline)
oneyear_HAMs<-filter(HAM_merged,date_admin!=earliest_consent_date & not_NA_HAM>0) %>%
  slice_min(abs(diffoneyear))%>% rename(oneyear_ham=total_ham) %>% 
  #If I were a collaborator I would want to know how far off from 1 yr the follow-up is
  select(new_id,oneyear_ham,time_difference_from_oneyear=diffoneyear) 

#Put it all together into a summary table with one row per participant
final_df<- HAM_merged %>% 
  select(-not_NA_HAM,-diffoneyear,-earliest_consent_date) %>%
  group_by(new_id) %>%
  summarize(cumulative_total_ham=sum(total_ham,na.rm=T),
            mean_ham=mean(total_ham,na.rm=T)) %>%
  left_join(latest_HAMs) %>%
  left_join(oneyear_HAMs)

save(final_df,file="HAM_total_summary.RData")
write_csv(final_df,file="HAM_total_summary.csv")

#############################################################
# Data visualization
#############################################################

rmarkdown::render("Hayes_data_challenge_report.rmd")
