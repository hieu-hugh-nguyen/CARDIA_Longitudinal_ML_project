# outer loop 

rm(list=ls()) #Clear all
cat("\014")

# set working directory: 
work_dir = 'U:/Hieu/CARDIA_longi_project'
work_dir = '/Volumes/MR-Research$/Hieu/CARDIA_longi_project'
setwd(work_dir)

# load libraries:
list.of.packages <- c("mlbench",'ggplot2','caret', 'dplyr', 'tibble', 'ROCR','parallelMap'
                      ,'riskRegression', 'survival','randomForestSRC', 'survivalROC'
                      , 'pec', 'risksetROC')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)

ncores <- parallel::detectCores(all.tests = FALSE, logical = TRUE)

# parallelMap::parallelStartSocket(ncores-1)

# source snippet functions:
source_dir <- paste0(work_dir, '/code/git_code/snippet')
source(paste0(source_dir, '/running_rsf.R'))
source(paste0(source_dir, '/running_algo.R'))
source(paste0(source_dir, '/createDir.R'))
source(paste0(source_dir, '/subsetDataTopnVar.R'))
source(paste0(source_dir, '/classif_task.R'))
source(paste0(source_dir, '/predictSurvProb.R'))
source(paste0(source_dir, '/eval_performance.R'))
source(paste0(source_dir, '/eval_performance_using_different_auc_package.R'))




# load the dataset
loading_dir = paste0(work_dir, '/csv_files')

data_longi_long_for_analysis <- read.csv(paste0(work_dir,'/csv_files/data_longi_long_expanded_variables_between_y0_y15_all_subjects.csv'))


data_longi_long_up_to_y15 <- data_longi_long_for_analysis %>% filter(exam_year <=15)

# baseline data:
data_at_baseline <- data_longi_long_up_to_y15 %>% filter(!duplicated(ID, fromLast=FALSE)) 

data_y0 <- data_at_baseline %>% 
  #mutate(time_te_in_yrs = time_te_in_yrs -15) %>% 
  dplyr::select(-time) %>% dplyr::filter(time_te_in_yrs >0) %>%
  dplyr::rename(event = status) %>% dplyr::rename(time = time_te_in_yrs) 


# load training IDs:
trainingid_all <- read.csv(paste0(work_dir,'/csv_files/all_training_set_ID_dynamic_deephit_all_subjects_2.csv'))
validationid_all <- read.csv(paste0(work_dir,'/csv_files/all_validation_set_ID_dynamic_deephit_all_subjects_2.csv'))
testingid_all <- read.csv(paste0(work_dir,'/csv_files/all_testing_set_ID_dynamic_deephit_all_subjects_2.csv'))




### PERFORMANCE TEST SET DYNAMIC-DEEPHIT: ######################################################




seed <- 4495
set.seed(seed)
nfolds <- 10

endpt <- 15+17 # after Y0 exam
eval_times <- seq(1, endpt, by = 1)

data <- data_y0

for (fold in 1:nfolds){
  # fold = 1
  trainingid <- na.omit(c(trainingid_all[,fold], validationid_all[,fold]))
  train_data <- data %>% filter(ID %in% trainingid)
  test_data <- data %>% filter((ID %in% testingid_all[,fold])) 
  train_id <- train_data$ID
  test_id <- test_data$ID

  model_name = 'dynamic_deephit_expanded_var_all_subjects_2'
  gc()
  main.dir = paste0(work_dir, '/rdata_files')
  sub.dir = paste0(model_name, '_fold_',fold)
  if(!dir.exists(file.path(main.dir, sub.dir))){
    createDir(main.dir, sub.dir)
  }
  saving.dir = file.path(main.dir, sub.dir)
  
  
  


  # filename = paste0(work_dir, '/rdata_files/dynamic_deephit_expanded_var_y15_2_1_fold_', fold,'/prob_risk_test.csv')
  filename = paste0(work_dir, '/rdata_files/',model_name, '_fold_', fold,'/prob_risk_test.csv')
  
  
  prob_risk_test_with_ID = read.csv(paste0(filename))
  
#  prob_risk_test_with_ID_filtered <- prob_risk_test_with_ID[prob_risk_test_with_ID$ID %in% test_id,]  
  prob_risk_test = prob_risk_test_with_ID %>% dplyr::select(-one_of('ID')) %>% as.matrix()
  prob_risk_test[is.na(prob_risk_test)] = 0
  loading.dir = saving.dir
  performance_testset = eval_performance3(prob.risk.test.set = prob_risk_test
                                          , test.data = test_data
                                          , trained.data = train_data
                                          , eval.times = eval_times
                                          )
  save(performance_testset
       , file = paste0(saving.dir, '/performance_testset.RData'))

}  



