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
ts_features <- read.csv(paste0(work_dir,'/csv_files/tsfresh_features_extract_relevant_drop_na_rm_corr_expanded_var_2.csv'))
# ts_features <- read.csv(paste0(work_dir,'/csv_files/tsfresh_features_extract_relevant_drop_na_2.csv'))


 
 
 # ts_features <- read.csv(paste0(work_dir,'/csv_files/tsfresh_features_extract_relevant_drop_na_rm_corr_expanded_var.csv'))
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


data <- data_tsfeatures[complete.cases(data_tsfeatures),]
# check if nrow of data is the same as cohort population


# for (fold in 1:nfolds){
#   # Training and fitting model:
#   # fold = 1
#   trainingid <- na.omit(c(trainingid_all[,fold], validationid_all[,fold]))
#   train_data <- data %>% filter(ID %in% trainingid)
#   test_data <- data %>% filter((ID %in% testingid_all[,fold]))
#   train_id <- train_data$ID
#   test_id <- test_data$ID
#   train_data$ID <- NULL
#   test_data$ID <- NULL
# 
# 
#   model_name <- 'rsf_expanded_var_tsfeatures_corr_rm'
#   gc()
#   main_dir <- paste0(work_dir, '/rdata_files')
#   sub_dir <- paste0(model_name, '_fold_',fold)
# 
#   if(!dir.exists(file.path(main_dir, sub_dir))){
#     createDir(main_dir, sub_dir)
#   }
#   #set.seed(seed)
#   model <- running_rsf(train_data)
#   saving_dir <- file.path(main_dir, sub_dir)
#   save(model, file = paste0(saving_dir,'/', model_name, '.RData'))
# 
# 
# 
#   # Test set performance: ###################################################################
#   loading.dir <- paste0(work_dir, '/rdata_files/', model_name, '_fold_', fold)
#   saving.dir <- loading.dir
#   trained_data <- train_data
#   trained_model <- model
# 
#   tryCatch({
# 
#     # probability of having had the disease:
#     prob_risk_test <- predictRisk.rsf(trained_model
#                                       , newdata = test_data
#                                       , times = eval_times
#     )
#     prob_risk_test_with_ID <- cbind(test_id, prob_risk_test)
#     save(prob_risk_test_with_ID
#          , file = paste0(saving.dir, '/prob_risk_test_set_with_ID.RData'))
# 
# 
# 
#     prob_risk_test[is.na(prob_risk_test)] = 0
#     performance_testset = eval_performance3(prob.risk.test.set = prob_risk_test
#                                             , test.data = test_data
#                                             , trained.data = trained_data
#                                             , eval.times = eval_times
#     )
#     save(performance_testset
#          , file = paste0(saving.dir, '/performance_testset.RData'))
# 
# 
# 
#     prob_risk_train <- predictRisk.rsf(trained_model
#                                        , newdata = trained_data
#                                        , times = eval_times
#     )
#     prob_risk_train_with_ID <- cbind(train_id, prob_risk_train)
#     save(prob_risk_train_with_ID
#          , file = paste0(saving.dir, '/prob_risk_train_set_with_ID.RData'))
# 
#   }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
# 
# 
#   ## VIMP:
#   max.subtree = max.subtree(model, conservative = F)
#   #save(max.subtree, file = paste(saving.dir, '/RF_maxtree.Rdata', sep = ''))
# 
#   # Get minimal depth of maximal subtree in terms of variable name, ascending order:
#   allvardepth = sort(max.subtree$order[, 1])
#   allvardepth.df = data.frame(Variable=names(allvardepth),MinDepthMaxSubtree=allvardepth,row.names = NULL)
# 
#   allvardepth.df$normalized_depth = normalize_var_imp(allvardepth.df$MinDepthMaxSubtree)
# 
#   write.csv(allvardepth.df, file = paste(saving_dir, '/depth_rank.csv', sep = ''),row.names=F)
# 
# }












data_tsfeatures_plus_most_recent_by_y15 <- data_y15_truncated_tte %>% 
  mutate(AGE_Y15 = AGE_Y0 +15) %>% dplyr::select(-AGE_Y0) %>% 
  left_join(ts_features, by = 'ID') %>%
  dplyr::select('ID','event','time',everything())

data <- data_tsfeatures_plus_most_recent_by_y15[complete.cases(data_tsfeatures_plus_most_recent_by_y15),]
# check nrow(data)

for (fold in 1:nfolds){
  # Training and fitting model:
  trainingid <- na.omit(c(trainingid_all[,fold], validationid_all[,fold]))
  train_data <- data %>% filter(ID %in% trainingid)
  test_data <- data %>% filter((ID %in% testingid_all[,fold])) 
  train_id <- train_data$ID
  test_id <- test_data$ID
  train_data$ID <- NULL
  test_data$ID <- NULL
  
  
  model_name <- 'rsf_expanded_var_tsfeatures_plus_data_y15'
  gc()
  main_dir <- paste0(work_dir, '/rdata_files')
  sub_dir <- paste0(model_name, '_fold_',fold)
  
  if(!dir.exists(file.path(main_dir, sub_dir))){
    createDir(main_dir, sub_dir)
  }
  #set.seed(seed)
  model <- running_rsf(train_data)
  saving_dir <- file.path(main_dir, sub_dir)
  save(model, file = paste0(saving_dir,'/', model_name, '.RData'))
  
  
  
  # Test set performance: ###################################################################
  loading.dir <- paste0(work_dir, '/rdata_files/', model_name, '_fold_', fold)
  saving.dir <- loading.dir
  trained_data <- train_data
  trained_model <- model
  
  tryCatch({
    
    # probability of having had the disease:
    prob_risk_test <- predictRisk.rsf(trained_model
                                      , newdata = test_data
                                      , times = eval_times
    )
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
    
    
    
    prob_risk_train <- predictRisk.rsf(trained_model
                                       , newdata = trained_data
                                       , times = eval_times
    )
    prob_risk_train_with_ID <- cbind(train_id, prob_risk_train)
    save(prob_risk_train_with_ID
         , file = paste0(saving.dir, '/prob_risk_train_set_with_ID.RData'))
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  
  # VIMP:
  
  
  max.subtree = max.subtree(model, conservative = F)
  #save(max.subtree, file = paste(saving.dir, '/RF_maxtree.Rdata', sep = ''))
  
  # Get minimal depth of maximal subtree in terms of variable name, ascending order:
  allvardepth = sort(max.subtree$order[, 1])
  allvardepth.df = data.frame(Variable=names(allvardepth),MinDepthMaxSubtree=allvardepth,row.names = NULL)
  
  allvardepth.df$normalized_depth = normalize_var_imp(allvardepth.df$MinDepthMaxSubtree)
  
  write.csv(allvardepth.df, file = paste(saving_dir, '/depth_rank.csv', sep = ''),row.names=F)
  
}





## MERGE WITH OLD ASCVD VAR EXTRACTED FEATURES: ############

new_data <- data





# load extracted timeseries features:
loading_dir = paste0(work_dir, '/csv_files')
ts_features <- read.csv(paste0(work_dir,'/csv_files/tsfresh_features_drop_nonunique_drop_na_relevant_df_drop_correlated.csv'))
ts_features <- ts_features %>% dplyr::rename(ID = X)



# load the dataset
loading_dir = paste0(work_dir, '/csv_files')

data_longi_long_for_analysis <- read.csv(paste0(work_dir,'/csv_files/data_longi_long_format_ascvd_risk_factors_removed_missing_data.csv'))
#'/csv_files/data_longi_long_format_ascvd_risk_factors_with_missing_data.csv'
#
subjects_in_cohort <- read.csv(paste0(work_dir,'/csv_files/subjects_in_final_analysis_cohort.csv'))

data_longi_long_up_to_y15 <- data_longi_long_for_analysis %>% dplyr::filter(exam_year <=15)
data_longi_analysis_cohort <- data_longi_long_up_to_y15 %>% dplyr::filter(ID %in% subjects_in_cohort[[1]])

# baseline data:
# data_at_baseline <- data_longi_long_for_analysis %>% dplyr::filter(!duplicated(ID, fromLast=FALSE)) 
data_at_baseline <- data_longi_analysis_cohort %>% dplyr::filter(ID %in% subjects_in_cohort[[1]]) %>% dplyr::filter(exam_year == 0)
# most recent data at landmark time (y15):
data_y15 <- data_longi_analysis_cohort %>% dplyr::filter(ID %in% subjects_in_cohort[[1]]) %>% dplyr::filter(exam_year == 15)

# truncate time to make start time at y15 (to avoid 15 years of immortal time):
data_y15_truncated_tte <- data_y15 %>% 
  mutate(time_te_in_yrs = time_te_in_yrs -15) %>% 
  dplyr::select(-time) %>% dplyr::filter(time_te_in_yrs >0) %>%
  dplyr::rename(event = status) %>% dplyr::rename(time = time_te_in_yrs) %>%
  dplyr::select(-exam_year)



data <- data_y15_truncated_tte
# update age variable to be at landmark time:
data <- data %>% mutate(AGE_Y15 = AGE_Y0 +15) %>% dplyr::select(-AGE_Y0)


# data with ts_features alone:

old_data <- data %>% dplyr::select('ID','event','time','AGE_Y15','MALE','RACEBLACK') %>%
  left_join(ts_features, by = 'ID')

# data_ts_ascvd_var <- read.csv(paste0(work_dir,'/csv_files/data_for_training_tsfeatures_models.csv'))
data_ts_ascvd_var <- old_data

data_ts_ascvd_var$ID <- as.character(data_ts_ascvd_var$ID)

`%notin%` <- Negate(`%in%`)
data_ts_ascvd_var_non_overlap <- data_ts_ascvd_var[,names(data_ts_ascvd_var)[names(data_ts_ascvd_var) %notin% names(new_data)]]
data_ts_ascvd_var_non_overlap_with_ID <- cbind (ID = data_ts_ascvd_var$ID, data_ts_ascvd_var_non_overlap)

merged_data <- new_data %>% inner_join(data_ts_ascvd_var_non_overlap_with_ID, by = 'ID')

# nrow(merged_data)

data <- merged_data
for (fold in 1:nfolds){
  # Training and fitting model:
  trainingid <- na.omit(c(trainingid_all[,fold], validationid_all[,fold]))
  train_data <- data %>% filter(ID %in% trainingid)
  test_data <- data %>% filter((ID %in% testingid_all[,fold])) 
  train_id <- train_data$ID
  test_id <- test_data$ID
  train_data$ID <- NULL
  test_data$ID <- NULL
  
  
  model_name <- 'rsf_expanded_var_and_ascvd_var_tsfeatures_plus_data_y15'
  gc()
  main_dir <- paste0(work_dir, '/rdata_files')
  sub_dir <- paste0(model_name, '_fold_',fold)
  
  if(!dir.exists(file.path(main_dir, sub_dir))){
    createDir(main_dir, sub_dir)
  }
  #set.seed(seed)
  model <- running_rsf(train_data)
  saving_dir <- file.path(main_dir, sub_dir)
  save(model, file = paste0(saving_dir,'/', model_name, '.RData'))
  
  
  
  # Test set performance: ###################################################################
  loading.dir <- paste0(work_dir, '/rdata_files/', model_name, '_fold_', fold)
  saving.dir <- loading.dir
  trained_data <- train_data
  trained_model <- model
  
  tryCatch({
    
    # probability of having had the disease:
    prob_risk_test <- predictRisk.rsf(trained_model
                                      , newdata = test_data
                                      , times = eval_times
    )
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
    
    
    
    prob_risk_train <- predictRisk.rsf(trained_model
                                       , newdata = trained_data
                                       , times = eval_times
    )
    prob_risk_train_with_ID <- cbind(train_id, prob_risk_train)
    save(prob_risk_train_with_ID
         , file = paste0(saving.dir, '/prob_risk_train_set_with_ID.RData'))
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  
  # VIMP:
  
  
  max.subtree = max.subtree(model, conservative = F)
  #save(max.subtree, file = paste(saving.dir, '/RF_maxtree.Rdata', sep = ''))
  
  # Get minimal depth of maximal subtree in terms of variable name, ascending order:
  allvardepth = sort(max.subtree$order[, 1])
  allvardepth.df = data.frame(Variable=names(allvardepth),MinDepthMaxSubtree=allvardepth,row.names = NULL)
  
  allvardepth.df$normalized_depth = normalize_var_imp(allvardepth.df$MinDepthMaxSubtree)
  
  write.csv(allvardepth.df, file = paste(saving_dir, '/depth_rank.csv', sep = ''),row.names=F)
  
}

