# outer loop 

rm(list=ls()) #Clear all
cat("\014")

# set working directory: 
work_dir = 'U:/Hieu/CARDIA_longi_project'
# work_dir = '/Volumes/MR-Research$/Hieu/CARDIA_longi_project'

setwd(work_dir)

# if a package is not available, try:
# require(devtools)
# install_version("randomForestSRC", version = "3.0.1", repos = "http://cran.us.r-project.org")
library('randomForestSRC')
# load libraries:
list.of.packages <- c("mlbench",'ggplot2','caret', 'dplyr', 'tibble', 'ROCR','parallelMap'
                      ,'riskRegression', 'survival','randomForestSRC', 'survivalROC'
                      , 'pec', 'risksetROC','survAUC')
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




compute_performance_testset <- function(trained_model_, model_name_, test_data_, eval_times_, trained_data_){
  prob_risk_test <- predictRisk.rsf(trained_model_
                                    , newdata = test_data_
                                    , times = eval_times_
  )
  prob_risk_test_with_ID <- cbind(test_id, prob_risk_test)
  save(prob_risk_test_with_ID
       , file = paste0(saving.dir, '/prob_risk_test_set_with_ID_for_', model_name_, '.RData'))
  
  
  prob_risk_test[is.na(prob_risk_test)] = 0
  performance_testset = eval_performance3_5(prob.risk.test.set = prob_risk_test
                                          , test.data = test_data_
                                          , trained.data = trained_data_
                                          , eval.times = eval_times_
  )
  save(performance_testset
       , file = paste0(saving.dir, '/performance_testset_for_', model_name_, '.RData'))
  
  return(performance_testset)
}





# load extracted timeseries features:
loading_dir = paste0(work_dir, '/csv_files')

ts_features <- read.csv(paste0(work_dir,'/csv_files/tsfresh_features_extracted_features_drop_nonunique_dropna_rm_nonrelevant_drop_correlated_dropna_2.csv'))

ts_features <- ts_features %>% dplyr::rename(ID = X)


# load the dataset
loading_dir = paste0(work_dir, '/csv_files')

data_longi_long_for_analysis <- read.csv(paste0(work_dir,'/csv_files/data_longi_long_format_expanded_variables_removed_missing_data_2.csv'))
data_longi_long_for_analysis$ID <- data_longi_long_for_analysis$ID %>% as.character()

subjects_in_cohort <- read.csv(paste0(work_dir,'/csv_files/subjects_in_final_analysis_cohort.csv'))






data_longi_long_up_to_y15 <- data_longi_long_for_analysis %>% filter(exam_year <=15)
data_longi_analysis_cohort <- data_longi_long_up_to_y15 %>% filter(ID %in% subjects_in_cohort[[1]])

# baseline data:
data_at_baseline <- data_longi_long_for_analysis %>% filter(!duplicated(ID, fromLast=FALSE)) 

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









### START BUILDING MODELS: #################################################################




seed <- 4495
set.seed(seed)
nfolds <- 10

endpt <- 17; # after Year 15
eval_times <- seq(1, endpt, by = 1)

data$ID <- data$ID %>% as.character()
ts_features$ID <- as.character(ts_features$ID)






data_tsfeatures_plus_most_recent_by_y15 <- data_y15_truncated_tte %>% 
  mutate(AGE_Y15 = AGE_Y0 +15) %>% dplyr::select(-AGE_Y0) %>% 
  left_join(ts_features, by = 'ID') %>%
  dplyr::select('ID','event','time',everything())

data <- data_tsfeatures_plus_most_recent_by_y15[complete.cases(data_tsfeatures_plus_most_recent_by_y15),]



## MERGE WITH OLD ASCVD VAR EXTRACTED FEATURES: ############

new_data <- data
# load extracted timeseries features:
loading_dir = paste0(work_dir, '/csv_files')
old_ts_features <- read.csv(paste0(work_dir,'/csv_files/tsfresh_features_drop_nonunique_drop_na_relevant_df_drop_correlated.csv'))
old_ts_features <- old_ts_features %>% dplyr::rename(ID = X)



data_ts_ascvd_var <- old_ts_features

data_ts_ascvd_var$ID <- as.character(data_ts_ascvd_var$ID)

`%notin%` <- Negate(`%in%`)
data_ts_ascvd_var_non_overlap <- data_ts_ascvd_var[,names(data_ts_ascvd_var)[names(data_ts_ascvd_var) %notin% names(new_data)]]
data_ts_ascvd_var_non_overlap_with_ID <- cbind (ID = data_ts_ascvd_var$ID, data_ts_ascvd_var_non_overlap)

merged_data <- new_data %>% inner_join(data_ts_ascvd_var_non_overlap_with_ID, by = 'ID')

# nrow(merged_data)

data <- merged_data



data_white = data %>% dplyr::filter(RACEBLACK == 0) %>% dplyr::select(-RACEBLACK)
data_black = data %>% dplyr::filter(RACEBLACK == 1) %>% dplyr::select(-RACEBLACK)




running_tsfeatures_models <- function(data_, data_name){
  
  
  # data_ = data_white
  # data_name = 'white_only'
  
  # load training IDs:
  trainingid_all <- read.csv(paste0(work_dir,'/csv_files/all_training_set_ID_',data_name,'.csv'))
  validationid_all <- read.csv(paste0(work_dir,'/csv_files/all_validation_set_ID_',data_name,'.csv'))
  testingid_all <- read.csv(paste0(work_dir,'/csv_files/all_testing_set_ID_',data_name,'.csv'))
  
  for (fold in 1:nfolds){
  
    ## fold = 1
    trainingid <- na.omit(c(trainingid_all[,fold], validationid_all[,fold]))
    train_data <- data_ %>% filter(ID %in% trainingid)
    test_data <- data_ %>% filter((ID %in% testingid_all[,fold]))
    train_id <- train_data$ID
    test_id <- test_data$ID
    train_data$ID <- NULL
    test_data$ID <- NULL

  
    model_name <- paste0('rsf_expanded_var_and_ascvd_var_tsfeatures_plus_data_y15_', data_name)
    gc()
    main_dir <- paste0(work_dir, '/rdata_files')
    sub_dir <- paste0(model_name, '_fold_',fold)
  
    if(!dir.exists(file.path(main_dir, sub_dir))){
      createDir(main_dir, sub_dir)
    }
    
    set.seed(seed)
    # model <- running_rsf(train_data)
    model <- rfsrc(Surv(time,event)~., data = train_data 
              , ntree = 1001
              , splitrule = 'logrank' #there is also logrankrandom, logrankscore, and conserve splitting  
    )
    
    saving_dir <- file.path(main_dir, sub_dir)
    save(model, file = paste0(saving_dir,'/','unfiltered_untuned.RData'))
    
  
  
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
      performance_testset = eval_performance3_5(prob.risk.test.set = prob_risk_test
                                              , test.data = test_data
                                              , trained.data = trained_data
                                              , eval.times = eval_times
      )
      print(performance_testset$iauc.uno)
      save(performance_testset
           , file = paste0(saving.dir, '/performance_testset.RData'))
  
  
  
     
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
    
    
  }

}



  
running_tsfeatures_models(data_white, 'white_only')
running_tsfeatures_models(data_black, 'black_only')