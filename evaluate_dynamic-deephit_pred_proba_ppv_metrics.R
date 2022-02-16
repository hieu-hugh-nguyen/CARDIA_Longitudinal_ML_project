# outer loop 

rm(list=ls()) #Clear all
cat("\014")

# set working directory: 
work_dir = 'U:/Hieu/CARDIA_longi_project'
setwd(work_dir)

# load libraries:
list.of.packages <- c("mlbench",'ggplot2','caret', 'dplyr', 'tibble', 'ROCR','parallelMap'
                      ,'riskRegression', 'survival','randomForestSRC', 'survivalROC'
                      , 'pec', 'risksetROC','timeROC')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)

ncores <- parallel::detectCores(all.tests = FALSE, logical = TRUE)

parallelMap::parallelStartSocket(ncores-1)

# source snippet functions:
source_dir <- 'U:/Hieu/CARDIA_longi_project/code/git_code/snippet'
source(paste0(source_dir,'/running_rsf.R'))
source(paste0(source_dir,'/running_algo.R'))
source(paste0(source_dir,'/createDir.R'))
source(paste0(source_dir,'/subsetDataTopnVar.R'))
source(paste0(source_dir,'/classif_task.R'))
source(paste0(source_dir,'/predictSurvProb.R'))
source(paste0(source_dir,'/eval_performance.R'))
source(paste0(source_dir,'/eval_performance_using_different_auc_package.R'))




# load the dataset
loading_dir = paste0(work_dir, '/csv_files')

data_longi_long_for_analysis <- read.csv(paste0(work_dir,'/csv_files/data_longi_long_format_ascvd_risk_factors_removed_missing_data.csv'))
#'/csv_files/data_longi_long_format_ascvd_risk_factors_with_missing_data.csv'
#
subjects_in_cohort <- read.csv(paste0(work_dir,'/csv_files/subjects_in_final_analysis_cohort.csv'))

data_longi_long_up_to_y15 <- data_longi_long_for_analysis %>% filter(exam_year <=15)
data_longi_analysis_cohort <- data_longi_long_up_to_y15 %>% filter(ID %in% subjects_in_cohort[[1]])

# baseline data:
# data_at_baseline <- data_longi_long_for_analysis %>% filter(!duplicated(ID, fromLast=FALSE)) 
data_at_baseline <- data_longi_analysis_cohort %>% filter(ID %in% subjects_in_cohort[[1]]) %>% filter(exam_year == 0)
# most recent data at landmark time (y15):
data_y15 <- data_longi_analysis_cohort %>% filter(ID %in% subjects_in_cohort[[1]]) %>% filter(exam_year == 15)

# truncate time to make start time at y15 (to avoid 15 years of immortal time):
data_y15_truncated_tte <- data_y15 %>% 
  mutate(time_te_in_yrs = time_te_in_yrs -15) %>% 
  dplyr::select(-time) %>% filter(time_te_in_yrs >0) %>%
  rename(event = status) %>% rename(time = time_te_in_yrs) %>%
  dplyr::select(-exam_year)


# update age variable to be at landmark time:
data_y15_truncated_tte <- data_y15_truncated_tte  %>% mutate(AGE_Y15 = AGE_Y0 +15) %>% dplyr::select(-AGE_Y0)



# load the ascvd calculated risk:
ascvd_risk_df <- read.csv(paste0(work_dir,'/csv_files/ascvd_calc_with_id_y15.csv'))

data <- data_y15_truncated_tte[,c('ID', 'time','event')] %>% left_join(ascvd_risk_df[,c('ID','ascvd')], by = 'ID') 

# load training IDs:
trainingid_all <- read.csv(paste0(work_dir,'/csv_files/all_training_set_ID.csv'))
validationid_all <- read.csv(paste0(work_dir,'/csv_files/all_validation_set_ID.csv'))
testingid_all <- read.csv(paste0(work_dir,'/csv_files/all_testing_set_ID.csv'))




### PERFORMANCE TEST SET DYNAMIC-DEEPHIT: ######################################################




seed <- 4495
set.seed(seed)
nfolds <- 10

endpt <- 17; # after Year 15
eval_times <- seq(1, endpt, by = 1)



for (fold in 1:nfolds){
  # fold = 1
  trainingid <- na.omit(c(trainingid_all[,fold], validationid_all[,fold]))
  train_data <- data %>% filter(ID %in% trainingid)
  test_data <- data %>% filter((ID %in% testingid_all[,fold])) 
  train_id <- train_data$ID
  test_id <- test_data$ID

  gc()
  main.dir = paste0(work_dir, '/rdata_files')
  sub.dir = paste0('dynamic_deephit_ascvd_var_y15_fold_',fold)
  if(!dir.exists(file.path(main.dir, sub.dir))){
    createDir(main.dir, sub.dir)
  }
  saving.dir = file.path(main.dir, sub.dir)
  
  
  

  filename = paste0(work_dir, '/rdata_files/dynamic_deephit_ascvd_var_y15_fold_', fold,'/prob_risk_test.csv')
  prob_risk_test_with_ID = read.csv(paste0(filename))
    
  prob_risk_test = prob_risk_test_with_ID %>% dplyr::select(-one_of('ID')) %>% as.matrix()
  prob_risk_test[is.na(prob_risk_test)] = 0
  loading.dir = saving.dir
  
  
  performance_testset_3 =  eval_performance3(prob.risk.test.set = prob_risk_test
                                             , test.data = test_data
                                             , trained.data = train_data
                                             , eval.times = eval_times
  )
  
  performance_testset_4 = eval_performance4(prob.risk.test.set = prob_risk_test
                                          , test.data = test_data
                                          , trained.data = train_data
                                          , eval.times = eval_times
  )
  
  
  save(performance_testset_4
       , file = paste0(saving.dir, '/performance_testset_updated_F1.RData'))

  # roc_df_curr_time <- performance_testset$roc[[17]][['ROC']]$plotframe
  # roc_df_curr_time$youden_index <-  roc_df_curr_time$TPR + 1- roc_df_curr_time$FPR - 1
  # roc_df_curr_time_na_rm <- 
  # best_cutoff <- roc_df_curr_time$risk[which(roc_df_curr_time$youden_index == max(na.omit(roc_df_curr_time$youden_index)))][1]
  
  
}  



