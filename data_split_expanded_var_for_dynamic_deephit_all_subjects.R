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

data_longi_long_for_analysis <- read.csv(paste0(work_dir,'/csv_files/data_longi_long_expanded_variables_between_y0_y15_all_subjects.csv'))
#'/csv_files/data_longi_long_format_ascvd_risk_factors_with_missing_data.csv'
#

data_longi_long_up_to_y15 <- data_longi_long_for_analysis %>% filter(exam_year <=15)

# baseline data:
data_at_baseline <- data_longi_long_up_to_y15 %>% filter(!duplicated(ID, fromLast=FALSE)) 
# data_at_baseline <- data_longi_analysis_cohort %>% filter(ID %in% subjects_in_cohort[[1]]) %>% filter(exam_year == 0)
# most recent data at landmark time (y15):
# data_y15 <- data_longi_analysis_cohort %>% filter(ID %in% subjects_in_cohort[[1]]) %>% filter(exam_year == 15)

data_y0 <- data_at_baseline %>% 
  #mutate(time_te_in_yrs = time_te_in_yrs -15) %>% 
  dplyr::select(-time) %>% dplyr::filter(time_te_in_yrs >0) %>%
  dplyr::rename(event = status) %>% dplyr::rename(time = time_te_in_yrs) 


# Stratified sampling: ######################################################################



saving.dir = paste0(work_dir,'/rdata_files')

# training-testing split:

# stratify by time to event, by 5 blocks (0-> 15 from Y0, 15-> 20, 20 -> 25, 25->30, 30+), then by event status within each block:

data_y0_block_neg_2 <- data_y0 %>% filter(time <=5)
data_y0_block_neg_1 <- data_y0 %>% filter(time >5) %>% filter(time <=10)
data_y0_block_0 <- data_y0 %>% filter(time >10) %>% filter(time <=15)
#data_y0_block_0 <- data_y0 %>% filter(time <=15)
data_y0_block_1 <- data_y0 %>% filter(time >15) %>% filter(time <=15+5)
data_y0_block_2 <- data_y0 %>% filter(time >15+5) %>% filter (time <=15+10)
data_y0_block_3 <- data_y0 %>% filter(time >15+10) %>% filter (time <=15+15)
data_y0_block_4 <- data_y0 %>% filter(time >15+15)


ID.split_block_neg_2 = stratified_event_split(data_y0_block_neg_2, time_var = 'time',event_var = 'event', id_var = 'ID'
                                          ,nfold = 5 # 5-fold cross-validation
                                          ,num_split_times = 5 )
ID.split_block_neg_1 = stratified_event_split(data_y0_block_neg_1, time_var = 'time',event_var = 'event', id_var = 'ID'
                                          ,nfold = 5 # 5-fold cross-validation
                                          ,num_split_times = 5 )
ID.split_block_0 = stratified_event_split(data_y0_block_0, time_var = 'time',event_var = 'event', id_var = 'ID'
                                          ,nfold = 5 # 5-fold cross-validation
                                          ,num_split_times = 5 )
ID.split_block_1 = stratified_event_split(data_y0_block_1, time_var = 'time',event_var = 'event', id_var = 'ID'
                                  ,nfold = 5 # 5-fold cross-validation
                                  ,num_split_times = 5 )
ID.split_block_2 = stratified_event_split(data_y0_block_2, time_var = 'time',event_var = 'event', id_var = 'ID'
                                          ,nfold = 5 # 5-fold cross-validation
                                          ,num_split_times = 5 )
ID.split_block_3 = stratified_event_split(data_y0_block_3, time_var = 'time',event_var = 'event', id_var = 'ID'
                                          ,nfold = 5 # 5-fold cross-validation
                                          ,num_split_times = 5 )
ID.split_block_4 = stratified_event_split(data_y0_block_4, time_var = 'time',event_var = 'event', id_var = 'ID'
                                          ,nfold = 5 # 5-fold cross-validation
                                          ,num_split_times = 5 )


train.ID = rbind(ID.split_block_0$train.ID, ID.split_block_1$train.ID, ID.split_block_2$train.ID, ID.split_block_3$train.ID, ID.split_block_4$train.ID)
valid.ID = rbind(ID.split_block_0$valid.ID, ID.split_block_1$valid.ID, ID.split_block_2$valid.ID, ID.split_block_3$valid.ID, ID.split_block_4$valid.ID)
test.ID = rbind(ID.split_block_0$test.ID, ID.split_block_1$test.ID, ID.split_block_2$test.ID, ID.split_block_3$test.ID, ID.split_block_4$test.ID)


write.csv(train.ID, paste0(work_dir, '/csv_files','/all_training_set_ID_dynamic_deephit_all_subjects_2.csv')
          ,row.names = F)
write.csv(valid.ID, paste0(work_dir, '/csv_files','/all_validation_set_ID_dynamic_deephit_all_subjects_2.csv')
          ,row.names = F)
write.csv(test.ID, paste0(work_dir, '/csv_files','/all_testing_set_ID_dynamic_deephit_all_subjects_2.csv')
          ,row.names = F)
