# outer loop 

rm(list=ls()) #Clear all
cat("\014")

# set working directory: 
work_dir = 'U:/Hieu/CARDIA_longi_project'
setwd(work_dir)

# load libraries:
list.of.packages <- c("mlbench",'ggplot2','caret', 'dplyr', 'tibble', 'ROCR','parallelMap'
                      ,'riskRegression', 'survival','randomForestSRC', 'survivalROC'
                      , 'pec', 'risksetROC')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)

ncores <- parallel::detectCores(all.tests = FALSE, logical = TRUE)

parallelMap::parallelStartSocket(ncores-1)

# source snippet functions:
source_dir <- 'U:/Hieu/CARDIA_project/CARDIA_project/Git'
source(paste0(source_dir,'/code/snippet/running_rsf.R'))
source(paste0(source_dir,'/code/snippet/running_algo.R'))
source(paste0(source_dir,'/code/snippet/createDir.R'))
source(paste0(source_dir,'/code/snippet/subsetDataTopnVar.R'))
source(paste0(source_dir,'/code/snippet/classif_task.R'))
source(paste0(source_dir,'/code/snippet/predictSurvProb.R'))
source(paste0(source_dir,'/code/snippet/eval_performance.R'))
source(paste0(source_dir,'/code/snippet/eval_performance_using_different_auc_package.R'))




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

# No truncate time 
data_baseline_no_truncate_tte <- data_at_baseline %>% 
  #  mutate(time_te_in_yrs = time_te_in_yrs -15) %>% 
  dplyr::select(-time) %>% filter(time_te_in_yrs >0) %>%
  rename(event = status) %>% rename(time = time_te_in_yrs) %>%
  dplyr::select(-exam_year)




data <- data_baseline_no_truncate_tte


# load the ascvd calculated risk:
ascvd_risk_df <- read.csv(paste0(work_dir,'/csv_files/ascvd_calc_with_id_y15.csv'))

data <- data_baseline_no_truncate_tte[,c('ID', 'time','event')] %>% left_join(ascvd_risk_df[,c('ID','ascvd')], by = 'ID') 

# load training IDs:
trainingid_all <- read.csv(paste0(work_dir,'/csv_files/all_training_set_ID.csv'))
validationid_all <- read.csv(paste0(work_dir,'/csv_files/all_validation_set_ID.csv'))
testingid_all <- read.csv(paste0(work_dir,'/csv_files/all_testing_set_ID.csv'))




### ASCVD risk score model: ######################################################




seed <- 4495
set.seed(seed)
nfolds <- 10

endpt <- 17; # after Year 15
eval_times <- 15+seq(1, endpt, by = 1)



for (fold in 1:nfolds){
  # fold = 1
  trainingid <- na.omit(c(trainingid_all[,fold], validationid_all[,fold]))
  train_data <- data %>% filter(ID %in% trainingid)
  test_data <- data %>% filter((ID %in% testingid_all[,fold])) 
  train_id <- train_data$ID
  test_id <- test_data$ID

  gc()
  main.dir = paste0(work_dir, '/rdata_files')
  sub.dir = paste0('ascvd_risk_score_y0_no_truncate_fold_',fold)
  if(!dir.exists(file.path(main.dir, sub.dir))){
    createDir(main.dir, sub.dir)
  }
  saving.dir = file.path(main.dir, sub.dir)
  
  
  
  prob_risk_test_with_ID = test_data %>% dplyr::select('ID')
  for(time_point in eval_times){
    prob_risk_test_with_ID[[paste0('risk_time_',time_point)]] = test_data$ascvd
  }
  save(prob_risk_test_with_ID
       , file = paste0(saving.dir, '/prob_risk_test_set_with_ID.RData'))
  
  
  prob_risk_test = prob_risk_test_with_ID %>% dplyr::select(-one_of('ID')) %>% as.matrix()
  prob_risk_test[is.na(prob_risk_test)] = 0
  loading.dir = saving.dir
  performance_testset = eval_performance2(prob.risk.test.set = prob_risk_test
                                          , test.data = test_data
                                          , trained.data = train_data
                                          , eval.times = eval_times
                                          )
  save(performance_testset
       , file = paste0(saving.dir, '/performance_testset.RData'))
  
  
  prob_risk_train_with_ID = train_data %>% dplyr::select('ID')
  for(time_point in eval_times){
    prob_risk_train_with_ID[[paste0('risk_time_',time_point)]] = train_data$ascvd
  }
  save(prob_risk_train_with_ID
       , file = paste0(saving.dir, '/prob_risk_train_set_with_ID.RData'))
  


  
  
  c_ascvd = sapply(eval_times, function(t){
    return(survConcordance(Surv(time, event)~ascvd,
                           data = within(test_data, {
                             index1 = time < t
                             index2 = event == 1
                             event[index1 & index2] = 1
                             event[!index1 | !index2] = 0
                           }))$concordance)
  })
  print(c_ascvd)
  save(c_ascvd
       , file = paste0(saving.dir, '/c_ascvd_testset.RData'))

    
}  




