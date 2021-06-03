#==========================================================================
# Stratified Sampling
# to mantain class balance (equal event:nonevent ratio), sampling needs to
# be run for different diseases (cvd, afib, stroke, ect.)
# 
# For outerloop split of training-validing data:
# 5 folds (80 training : 20 validing ratio) x 5 times
# Output: returns the IDs for each training-validing split 
#==========================================================================
rm(list=ls()) #Clear all
cat("\014")

list.of.packages <- c('dplyr', 'tibble')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)


source_dir <- 'U:/Hieu/CARDIA_project/CARDIA_project'
source(paste0(source_dir,'/Git/code/snippet/stratified_event_split.R'))


# set spliting parameters:
nfold = 5 # 5-fold cross-validation
num_split_times = 5 # 5 times of 5-fold cross-validation

work_dir = 'U:/Hieu/CARDIA_longi_project'

setwd(work_dir)


# load the dataset
loading_dir = paste0(work_dir, '/csv_files')

data_longi_long_for_analysis <- read.csv(paste0(work_dir,'/csv_files/data_longi_long_format_ascvd_risk_factors.csv'))

#exclude instances with events or censored before exam year 15 
data_longi_long_y15 <- data_longi_long_for_analysis %>% filter(time_te_in_yrs >15) 

# only include medical history from y0 to y15:
data_longi_long_up_to_y15 <- data_longi_long_y15 %>% filter(exam_year <=15)

# baseline data:
data_at_baseline <- data_longi_long_up_to_y15 %>% filter(!duplicated(ID, fromLast=FALSE)) 

# most recent data at landmark time (y15):
data_most_recent_by_y15 <- data_longi_long_up_to_y15 %>% filter(!duplicated(ID, fromLast=TRUE))



# Stratified sampling: ######################################################################



saving.dir = paste0(work_dir,'/rdata_files')

# training-testing split:
ID.split = stratified_event_split(data_at_baseline,time_var = 'time_to_te_in_yrs',event_var = 'status', id_var = 'ID'
                                  ,nfold = 5 # 5-fold cross-validation
                                  ,num_split_times = 5 )

train.ID = ID.split$train.ID
valid.ID = ID.split$valid.ID
test.ID = ID.split$test.ID


write.csv(train.ID, paste0(work_dir, '/csv_files','/all_training_set_ID.csv')
          ,row.names = F)
write.csv(valid.ID, paste0(work_dir, '/csv_files','/all_validation_set_ID.csv')
          ,row.names = F)
write.csv(test.ID, paste0(work_dir, '/csv_files','/all_testing_set_ID.csv')
          ,row.names = F)
