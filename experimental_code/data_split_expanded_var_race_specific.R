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

work_dir = 'U:/Hieu/CARDIA_longi_project'
setwd(work_dir)

# source snippet functions:
source_dir <- paste0(work_dir, '/code/git_code/snippet')
source(paste0(source_dir,'/stratified_event_split.R'))


# set spliting parameters:
nfold = 5 # 5-fold cross-validation
num_split_times = 5 # 5 times of 5-fold cross-validation


# load the dataset
loading_dir = paste0(work_dir, '/csv_files')

data_longi_long_for_analysis <- read.csv(paste0(work_dir,'/csv_files/data_longi_long_format_expanded_variables_removed_missing_data_2.csv'))
#'/csv_files/data_longi_long_format_ascvd_risk_factors_with_missing_data.csv'
#
subjects_in_cohort <- read.csv(paste0(work_dir,'/csv_files/subjects_in_final_analysis_cohort.csv'))

data_longi_long_up_to_y15 <- data_longi_long_for_analysis %>% filter(exam_year <=15)
data_longi_analysis_cohort <- data_longi_long_up_to_y15 %>% filter(ID %in% subjects_in_cohort[[1]])

# baseline data:
data_at_baseline <- data_longi_long_for_analysis %>% filter(!duplicated(ID, fromLast=FALSE)) 
# data_at_baseline <- data_longi_analysis_cohort %>% filter(ID %in% subjects_in_cohort[[1]]) %>% filter(exam_year == 0)
# most recent data at landmark time (y15):
# data_y15 <- data_longi_analysis_cohort %>% filter(ID %in% subjects_in_cohort[[1]]) %>% filter(exam_year == 15)
data_y15 <- data_longi_long_for_analysis %>% filter(!duplicated(ID, fromLast=TRUE)) 

# truncate time to make start time at y15 (to avoid 15 years of immortal time):
data_y15_truncated_tte <- data_y15 %>% 
  mutate(time_te_in_yrs = time_te_in_yrs -15) %>% 
  dplyr::select(-time) %>% filter(time_te_in_yrs >0) %>%
  dplyr::rename(event = status) %>% dplyr::rename(time = time_te_in_yrs) 


# Dividing data by race: ####################################################################
data_white = data_y15_truncated_tte %>% dplyr::filter(RACEBLACK == 0)
data_black = data_y15_truncated_tte %>% dplyr::filter(RACEBLACK == 1)


# Stratified sampling: ######################################################################




saving.dir = paste0(work_dir,'/rdata_files')

separate_by_variable_split <- function(data, data_name){
    
  # training-validation-testing split:
  
  # stratify by time to event, by 4 blocks (0-> 5, 5->10, 10->15, 15+), then by event status within each block:
  
  data_block_1 <- data %>% filter(time <=5)
  data_block_2 <- data %>% filter(time >5) %>% filter (time <=10)
  data_block_3 <- data %>% filter(time >10) %>% filter (time <=15)
  data_block_4 <- data %>% filter(time >15)
  
  ID.split_block_1 = stratified_event_split(data_block_1, time_var = 'time',event_var = 'event', id_var = 'ID'
                                    ,nfold = 5 # 5-fold cross-validation
                                    ,num_split_times = 5 )
  ID.split_block_2 = stratified_event_split(data_block_2, time_var = 'time',event_var = 'event', id_var = 'ID'
                                            ,nfold = 5 # 5-fold cross-validation
                                            ,num_split_times = 5 )
  ID.split_block_3 = stratified_event_split(data_block_3, time_var = 'time',event_var = 'event', id_var = 'ID'
                                            ,nfold = 5 # 5-fold cross-validation
                                            ,num_split_times = 5 )
  ID.split_block_4 = stratified_event_split(data_block_4, time_var = 'time',event_var = 'event', id_var = 'ID'
                                            ,nfold = 5 # 5-fold cross-validation
                                            ,num_split_times = 5 )
  
  
  train.ID = rbind(ID.split_block_1$train.ID, ID.split_block_2$train.ID, ID.split_block_3$train.ID, ID.split_block_4$train.ID)
  valid.ID = rbind(ID.split_block_1$valid.ID, ID.split_block_2$valid.ID, ID.split_block_3$valid.ID, ID.split_block_4$valid.ID)
  test.ID = rbind(ID.split_block_1$test.ID, ID.split_block_2$test.ID, ID.split_block_3$test.ID, ID.split_block_4$test.ID)
  
  
  
  write.csv(train.ID, paste0(work_dir, '/csv_files','/all_training_set_ID_',data_name,'.csv')
            ,row.names = F)
  write.csv(valid.ID, paste0(work_dir, '/csv_files','/all_validation_set_ID_',data_name,'.csv')
            ,row.names = F)
  write.csv(test.ID, paste0(work_dir, '/csv_files','/all_testing_set_ID_',data_name,'.csv')
            ,row.names = F)
  
}


separate_by_variable_split(data_white, 'white_only')
separate_by_variable_split(data_black, 'black_only')

