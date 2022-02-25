# outer loop 

rm(list=ls()) #Clear all
cat("\014")

# set working directory: 
work_dir = 'U:/Hieu/CARDIA_longi_project'
setwd(work_dir)

# load libraries:
list.of.packages <- c("mlbench",'ggplot2','caret', 'dplyr', 'tibble', 'ROCR','parallelMap'
                      ,'riskRegression', 'survival','randomForestSRC', 'survivalROC'
                      , 'pec', 'risksetROC','tsfeatures','survAUC')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)

ncores <- parallel::detectCores(all.tests = FALSE, logical = TRUE)

parallelMap::parallelStartSocket(ncores-1)


source_dir <- paste0(work_dir, '/code/git_code/snippet')
source(paste0(source_dir, '/running_rsf.R'))
source(paste0(source_dir, '/running_algo.R'))
source(paste0(source_dir, '/createDir.R'))
source(paste0(source_dir, '/subsetDataTopnVar.R'))
source(paste0(source_dir, '/classif_task.R'))
source(paste0(source_dir, '/predictSurvProb.R'))
source(paste0(source_dir, '/eval_performance.R'))
source(paste0(source_dir, '/eval_performance_using_different_auc_package.R'))
source(paste0(source_dir,'/normalize_var_importance.R'))


# load extracted timeseries features:
loading_dir = paste0(work_dir, '/csv_files')
# ts_features <- read.csv(paste0(work_dir,'/csv_files/tsfresh_features_extract_relevant_drop_na_rm_corr_expanded_var.csv'))
ts_features <- read.csv(paste0(work_dir,'/csv_files/tsfresh_features_extract_relevant_drop_na_rm_corr_expanded_var_2.csv'))

# ts_features <- read.csv(paste0(work_dir,'/csv_files/tsfresh_features_drop_nonunique_dropna_rm_corr_expanded_var.csv'))
# ts_features <- read.csv(paste0(work_dir,'/csv_files/tsfresh_features_extract_relevant_drop_na_expanded_var.csv'))
# ts_features <- read.csv(paste0(work_dir,'/csv_files/tsfresh_features_drop_nonunique_dropna_drop_correlated_training_rm_nonrelevant.csv'))
# ts_features <- read.csv(paste0(work_dir,'/csv_files/tsfresh_features_drop_nonunique_drop_na_drop_correlated_expanded_var_crunhcr.csv'))

ts_features <- ts_features %>% dplyr::rename(ID = X)


# load the dataset
loading_dir = paste0(work_dir, '/csv_files')

data_longi_long_for_analysis <- read.csv(paste0(work_dir,'/csv_files/data_longi_long_format_expanded_variables_removed_missing_data.csv'))
data_longi_long_for_analysis$ID <- data_longi_long_for_analysis$ID %>% as.character()

#'/csv_files/data_longi_long_format_ascvd_risk_factors_with_missing_data.csv'
#
subjects_in_cohort <- read.csv(paste0(work_dir,'/csv_files/subjects_in_final_analysis_cohort.csv'))

# data_longi_expanded_var_for_dynamic_deephit <- read.csv(paste0(work_dir, '/csv_files/data_longi_expanded_var_for_dynamic_deephit.csv'))
# data_longi_expanded_var_for_dynamic_deephit$ID <- data_longi_expanded_var_for_dynamic_deephit$ID %>% as.character()
# Does not have this ID: 204337013307





data_longi_long_up_to_y15 <- data_longi_long_for_analysis %>% filter(exam_year <=15)
data_longi_analysis_cohort <- data_longi_long_up_to_y15 %>% filter(ID %in% subjects_in_cohort[[1]])

# baseline data:
data_at_baseline <- data_longi_long_for_analysis %>% filter(!duplicated(ID, fromLast=FALSE)) 
# data_at_baseline <- data_longi_analysis_cohort %>% filter(ID %in% subjects_in_cohort[[1]]) %>% filter(exam_year == 0)
# most recent data at landmark time (y15):
data_y15 <- data_longi_analysis_cohort %>% filter(ID %in% subjects_in_cohort[[1]]) %>% 
  # filter(exam_year == 15)
  dplyr::filter(!duplicated(ID, fromLast=TRUE))

# truncate time to make start time at y15 (to avoid 15 years of immortal time):
data_y15_truncated_tte <- data_y15 %>% 
  mutate(time_te_in_yrs = time_te_in_yrs -15) %>% 
  dplyr::select(-time) %>% filter(time_te_in_yrs >0) %>%
  dplyr::rename(event = status) %>% dplyr::rename(time = time_te_in_yrs) %>%
  dplyr::select(-exam_year)



data <- data_y15_truncated_tte
# update age variable to be at landmark time:
data <- data %>% mutate(AGE_Y15 = AGE_Y0 +15) %>% dplyr::select(-AGE_Y0)





# load training IDs:
trainingid_all <- read.csv(paste0(work_dir,'/csv_files/all_training_set_ID.csv'))
validationid_all <- read.csv(paste0(work_dir,'/csv_files/all_validation_set_ID.csv'))
testingid_all <- read.csv(paste0(work_dir,'/csv_files/all_testing_set_ID.csv'))




### START BUILDING MODELS: #################################################################




seed <- 4495
set.seed(seed)
nfolds <- 10

endpt <- 17; # after Year 15
eval_times <- seq(1, endpt, by = 1)


# merge y15 data with ts derived features:

data$ID <- data$ID %>% as.character()
ts_features$ID <- as.character(ts_features$ID)

data_tsfeatures <- data %>% dplyr::select('ID','event','time','AGE_Y15','MALE','RACEBLACK') %>%
  inner_join(ts_features, by = 'ID')

# data_tsfeatures_disjoint <- data %>% dplyr::select('ID','event','time','AGE_Y15','MALE','RACEBLACK') %>%
#   anti_join(ts_features, by = 'ID')
# data_tsfeatures_disjoint %>% nrow()

# check if nrow of data is the same as cohort population


data_tsfeatures_plus_most_recent_by_y15 <- data_y15_truncated_tte %>% 
  mutate(AGE_Y15 = AGE_Y0 +15) %>% dplyr::select(-AGE_Y0) %>% 
  left_join(ts_features, by = 'ID') %>%
  dplyr::select('ID','event','time',everything())

data <- data_tsfeatures_plus_most_recent_by_y15[complete.cases(data_tsfeatures_plus_most_recent_by_y15),]
# check nrow(data)



### cFOREST ###################################
for (fold in 1:nfolds){
  # Training and fitting model:
  ## fold =1 
  trainingid <- na.omit(c(trainingid_all[,fold], validationid_all[,fold]))
  train_data <- data %>% dplyr::filter(ID %in% trainingid)
  test_data <- data %>% dplyr::filter((ID %in% testingid_all[,fold])) 
  train_id <- train_data$ID
  test_id <- test_data$ID
  train_data$ID <- NULL
  test_data$ID <- NULL
  
  
  model_name <- 'cForest_expanded_var_tsfeatures_plus_data_y15_rm_correlation'
  gc()
  main_dir <- paste0(work_dir, '/rdata_files')
  sub_dir <- paste0(model_name, '_fold_',fold)
  
  if(!dir.exists(file.path(main_dir, sub_dir))){
    createDir(main_dir, sub_dir)
  }
  set.seed(seed)
  model = running_cForest(train_data)
  saving_dir = file.path(main_dir, sub_dir)
  save(model, file = paste0(saving_dir,'/', model_name, '.RData'))
  
  
  
  
  # Test set performance: ###################################################################
  loading.dir <- paste0(work_dir, '/rdata_files/', model_name, '_fold_', fold)
  saving.dir <- loading.dir
  trained_data <- train_data
  trained_model <- model
  
  
  tryCatch({
    
    # probability of having had the disease:
    prob_risk_test = predictRisk.cForest(trained_model
                                         #  , traindata = trained_data
                                         , newdata = test_data
                                         , times = eval_times
    )
    
    # probability of having had the disease:
    
    prob_risk_test_with_ID <- cbind(test_id, prob_risk_test)
    save(prob_risk_test_with_ID
         , file = paste0(saving.dir, '/prob_risk_test_set_with_ID.RData'))
    
    
    
    prob_risk_test[is.na(prob_risk_test)] = 0
    performance_testset = eval_performance3(prob.risk.test.set = prob_risk_test
                                            , test.data = test_data
                                            , trained.data = trained_data
                                            , eval.times = eval_times
    )
    save(performance_testset
         , file = paste0(saving.dir, '/performance_testset.RData'))
    
    
    
    prob_risk_train <- predictRisk.cForest(trained_model
                                       , newdata = trained_data
                                       , times = eval_times
    )
    prob_risk_train_with_ID <- cbind(train_id, prob_risk_train)
    save(prob_risk_train_with_ID
         , file = paste0(saving.dir, '/prob_risk_train_set_with_ID.RData'))
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  
}










### LASSO-COX ###################################
for (fold in 1:nfolds){
  # Training and fitting model:
  trainingid <- na.omit(c(trainingid_all[,fold], validationid_all[,fold]))
  train_data <- data %>% dplyr::filter(ID %in% trainingid)
  test_data <- data %>% dplyr::filter((ID %in% testingid_all[,fold])) 
  train_id <- train_data$ID
  test_id <- test_data$ID
  train_data$ID <- NULL
  test_data$ID <- NULL
  
  
  model_name <- 'lasso_expanded_var_tsfeatures_plus_data_y15_rm_correlation'
  gc()
  main_dir <- paste0(work_dir, '/rdata_files')
  sub_dir <- paste0(model_name, '_fold_',fold)
  
  if(!dir.exists(file.path(main_dir, sub_dir))){
    createDir(main_dir, sub_dir)
  }
  set.seed(seed)
  model = running_lasso(train_data)
  saving_dir = file.path(main_dir, sub_dir)
  save(model, file = paste0(saving_dir,'/', model_name, '.RData'))
  
  
  
  
  # Test set performance: ###################################################################
  loading.dir <- paste0(work_dir, '/rdata_files/', model_name, '_fold_', fold)
  saving.dir <- loading.dir
  trained_data <- train_data
  trained_model <- model
  
  tryCatch({
    
    # probability of having had the disease:
    prob_risk_test = predictRisk.cox(trained_model
                                     #  , traindata = trained_data
                                     , newdata = test_data
                                     , times = eval_times
    )
    
    # probability of having had the disease:
    
    prob_risk_test_with_ID <- cbind(test_id, prob_risk_test)
    save(prob_risk_test_with_ID
         , file = paste0(saving.dir, '/prob_risk_test_set_with_ID.RData'))
    
    
    
    prob_risk_test[is.na(prob_risk_test)] = 0
    performance_testset = eval_performance3(prob.risk.test.set = prob_risk_test
                                            , test.data = test_data
                                            , trained.data = trained_data
                                            , eval.times = eval_times
    )
    save(performance_testset
         , file = paste0(saving.dir, '/performance_testset.RData'))
    
    
    
    prob_risk_train <- predictRisk.cox(trained_model
                                       , newdata = trained_data
                                       , times = eval_times
    )
    prob_risk_train_with_ID <- cbind(train_id, prob_risk_train)
    save(prob_risk_train_with_ID
         , file = paste0(saving.dir, '/prob_risk_train_set_with_ID.RData'))
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  
}




### XGBOOST ###################################

pacman::p_load_gh("IyarLin/survXgboost")
pacman::p_load("survival")
pacman::p_load("xgboost")

data_for_xgb <- data

data_for_xgb$xgb_label <- ifelse(data_for_xgb$event == 1, data_for_xgb$time, -data_for_xgb$time)

for (fold in 1:nfolds){
  # Training and fitting model:
  ## fold = 1
  trainingid <- na.omit(c(trainingid_all[,fold], validationid_all[,fold]))
  train_data <- data %>% dplyr::filter(ID %in% trainingid)
  test_data <- data %>% dplyr::filter((ID %in% testingid_all[,fold])) 
  train_id <- train_data$ID
  test_id <- test_data$ID
  train_data$ID <- NULL
  test_data$ID <- NULL
  
  x_train <- as.matrix(train_data %>% dplyr::select(-one_of(c("time", "event"))))
  x_label <- data_for_xgb$xgb_label[data_for_xgb$ID %in% trainingid]
  
  x_val <- xgb.DMatrix(as.matrix(test_data %>% dplyr::select(-one_of(c("time", "event"))))
                       , label = data_for_xgb$xgb_label[data_for_xgb$ID %in% testingid_all[,fold]])
  x_test <- as.matrix(test_data %>% dplyr::select(-one_of(c("time", "event"))))

  
  
  model_name <- 'xgboost_expanded_var_tsfeatures_plus_data_y15_rm_correlation'
  gc()
  main_dir <- paste0(work_dir, '/rdata_files')
  sub_dir <- paste0(model_name, '_fold_',fold)
  
  if(!dir.exists(file.path(main_dir, sub_dir))){
    createDir(main_dir, sub_dir)
  }
  

  set.seed(seed)
  # train surv_xgboost
  
  model <- xgb.train.surv(
    params = list(
      objective = "survival:cox",
      eval_metric = "cox-nloglik",
      eta = 0.001 # larger eta leads to algorithm not converging, resulting in NaN predictions
      , alpha = 0.01
    )
    , data = x_train, label = x_label,
    watchlist = list(val2 = x_val),
    nrounds = 25000, early_stopping_rounds = 250
  )
  
  
  loading.dir <- paste0(work_dir, '/rdata_files/', model_name, '_fold_', fold)
  saving.dir <- loading.dir
  trained_data <- train_data
  trained_model <- model
  
  
  survival_curves <- predict(object = model, newdata = x_test, type = "surv", times = eval_times)
  
  prob_risk_test <- 1-survival_curves
  prob_risk_test_with_ID <- cbind(test_id, prob_risk_test)
  save(prob_risk_test_with_ID
       , file = paste0(saving.dir, '/prob_risk_test_set_with_ID.RData'))
  
  
  
  prob_risk_test[is.na(prob_risk_test)] = 0
  performance_testset = eval_performance3(prob.risk.test.set = prob_risk_test
                                          , test.data = test_data
                                          , trained.data = trained_data
                                          , eval.times = eval_times
  )
  save(performance_testset
       , file = paste0(saving.dir, '/performance_testset.RData'))
  
  
}  






### RSF with top 10 vars ###################################
n.top = 20

for (fold in 1:nfolds){
  # Training and fitting model:
  ## fold =1 
  
  # Load the var ranking:
  loading_dir = paste0(work_dir,'/rdata_files/rsf_expanded_var_tsfeatures_plus_data_y15_fold_', fold)
  var_order_df = read.csv(file = paste0(loading_dir,
                                        '/depth_rank.csv'))
  
  var_order = apply(var_order_df, 2, as.character)[,1]
  
  data <- data %>% dplyr::select(one_of(c('ID', 'time', 'event',
                   var_order[1:n.top])))
  
  trainingid <- na.omit(c(trainingid_all[,fold], validationid_all[,fold]))
  train_data <- data %>% dplyr::filter(ID %in% trainingid)
  test_data <- data %>% dplyr::filter((ID %in% testingid_all[,fold])) 
  train_id <- train_data$ID
  test_id <- test_data$ID
  train_data$ID <- NULL
  test_data$ID <- NULL
  
  
  model_name <- 'rsf_expanded_var_tsfeatures_plus_data_y15_top_20_var'
  gc()
  main_dir <- paste0(work_dir, '/rdata_files')
  sub_dir <- paste0(model_name, '_fold_',fold)
  
  if(!dir.exists(file.path(main_dir, sub_dir))){
    createDir(main_dir, sub_dir)
  }
  set.seed(seed)
  model = running_rsf(train_data)
  saving_dir = file.path(main_dir, sub_dir)
  save(model, file = paste0(saving_dir,'/', model_name, '.RData'))
  
  
  
  
  # Test set performance: ###################################################################
  loading.dir <- paste0(work_dir, '/rdata_files/', model_name, '_fold_', fold)
  saving.dir <- loading.dir
  trained_data <- train_data
  trained_model <- model
  
  
  tryCatch({
    
    # probability of having had the disease:
    prob_risk_test = predictRisk.rsf(trained_model
                                         #  , traindata = trained_data
                                         , newdata = test_data
                                         , times = eval_times
    )
    
    # probability of having had the disease:
    
    prob_risk_test_with_ID <- cbind(test_id, prob_risk_test)
    save(prob_risk_test_with_ID
         , file = paste0(saving.dir, '/prob_risk_test_set_with_ID.RData'))
    
    
    
    prob_risk_test[is.na(prob_risk_test)] = 0
    performance_testset = eval_performance3(prob.risk.test.set = prob_risk_test
                                            , test.data = test_data
                                            , trained.data = trained_data
                                            , eval.times = eval_times
    )
    save(performance_testset
         , file = paste0(saving.dir, '/performance_testset.RData'))
    
    
    
    prob_risk_train <- predictRisk.cForest(trained_model
                                           , newdata = trained_data
                                           , times = eval_times
    )
    prob_risk_train_with_ID <- cbind(train_id, prob_risk_train)
    save(prob_risk_train_with_ID
         , file = paste0(saving.dir, '/prob_risk_train_set_with_ID.RData'))
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  
}
