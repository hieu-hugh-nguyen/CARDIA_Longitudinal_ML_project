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
source_dir <- paste0(work_dir, '/code/git_code/snippet')
source(paste0(source_dir, '/running_rsf.R'))
source(paste0(source_dir, '/running_algo.R'))
source(paste0(source_dir, '/createDir.R'))
source(paste0(source_dir, '/subsetDataTopnVar.R'))
source(paste0(source_dir, '/classif_task.R'))
source(paste0(source_dir, '/predictSurvProb.R'))
source(paste0(source_dir, '/eval_performance.R'))
source(paste0(source_dir, '/eval_performance_using_different_auc_package.R'))
source(paste0(source_dir, '/normalize_var_importance.R'))



# load the dataset
loading_dir = paste0(work_dir, '/csv_files')

data_longi_long_for_analysis <- read.csv(paste0(work_dir,'/csv_files/data_longi_long_format_expanded_variables_removed_missing_data_2.csv'))

subjects_in_cohort <- read.csv(paste0(work_dir,'/csv_files/subjects_in_final_analysis_cohort.csv'))

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


#Check if there is any character column, then delete them to make sure all data is numeric:
nums <- unlist(lapply(data, is.character))  
data[,nums]<-NULL
# write.csv(data, file = paste0(work_dir,'/csv_files/data_for_y15_models.csv'), row.names = FALSE)


# load training IDs:
trainingid_all <- read.csv(paste0(work_dir,'/csv_files/all_training_set_ID_2.csv'))
validationid_all <- read.csv(paste0(work_dir,'/csv_files/all_validation_set_ID_2.csv'))
testingid_all <- read.csv(paste0(work_dir,'/csv_files/all_testing_set_ID_2.csv'))




### START BUILDING MODELS: #################################################################




seed <- 4495
set.seed(seed)
nfolds <- 10

endpt <- 17; # after Year 15
eval_times <- seq(1, endpt, by = 1)





### COX-PH MODEL ###################################

for (fold in 1:nfolds){
  # Training and fitting model:
  # fold = 11
  trainingid <- na.omit(c(trainingid_all[,fold], validationid_all[,fold]))
  train_data <- data %>% filter(ID %in% trainingid)
  test_data <- data %>% filter((ID %in% testingid_all[,fold])) 
  train_id <- train_data$ID
  test_id <- test_data$ID
  train_data$ID <- NULL
  test_data$ID <- NULL
  
  
  model_name <- 'cox_expanded_var_y15_2'
  gc()
  main_dir <- paste0(work_dir, '/rdata_files')
  sub_dir <- paste0(model_name, '_fold_',fold)
  
  if(!dir.exists(file.path(main_dir, sub_dir))){
    createDir(main_dir, sub_dir)
  }
  set.seed(seed)
  # model <- running_coxph(train_data)
  model <- coxph(Surv(time,event) ~. # MALE+RACEBLACK+CHOL+DIAB+HBM+HDL+SBP+SMKNW+AGE_Y15
                 , data = train_data, singular.ok = T, x=TRUE)
  saving_dir <- file.path(main_dir, sub_dir)
  save(model, file = paste0(saving_dir,'/', model_name, '.RData'))
  
  
  
  # Test set performance: ###################################################################
  loading.dir <- paste0(work_dir, '/rdata_files/', model_name, '_fold_', fold)
  saving.dir <- loading.dir
  trained_data <- train_data
  trained_model <- model
  
  tryCatch({
    
    # probability of having had the disease:
    prob_risk_test <- predictRisk.cox(trained_model
                                      , newdata = test_data
                                      , times = eval_times
    )  
    # prob.risk = riskRegression::predictRisk(trained.model, newdata = newdata
    #                                            , times = times)
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






### LASSO-COX MODEL ###################################

for (fold in 1:nfolds){
  # Training and fitting model:
  # fold = 1
  trainingid <- na.omit(c(trainingid_all[,fold], validationid_all[,fold]))
  train_data <- data %>% filter(ID %in% trainingid)
  test_data <- data %>% filter((ID %in% testingid_all[,fold])) 
  train_id <- train_data$ID
  test_id <- test_data$ID
  train_data$ID <- NULL
  test_data$ID <- NULL
  
  
  model_name <- 'lasso_expanded_var_y15_2_1'
  gc()
  main_dir <- paste0(work_dir, '/rdata_files')
  sub_dir <- paste0(model_name, '_fold_',fold)
  
  if(!dir.exists(file.path(main_dir, sub_dir))){
    createDir(main_dir, sub_dir)
  }
  set.seed(seed)
  model <- running_lasso(train_data)
  # model <- coxph(Surv(time,event) ~. # MALE+RACEBLACK+CHOL+DIAB+HBM+HDL+SBP+SMKNW+AGE_Y15
  #                , data = train_data, singular.ok = T, x=TRUE)
  saving_dir <- file.path(main_dir, sub_dir)
  save(model, file = paste0(saving_dir,'/', model_name, '.RData'))
  
  
  
  # Test set performance: ###################################################################
  loading.dir <- paste0(work_dir, '/rdata_files/', model_name, '_fold_', fold)
  saving.dir <- loading.dir
  trained_data <- train_data
  trained_model <- model
  
  tryCatch({
    
    # probability of having had the disease:
    prob_risk_test <- predictRisk.cox(trained_model
                                      , newdata = test_data
                                      , times = eval_times
    )  
    # prob.risk = riskRegression::predictRisk(trained.model, newdata = newdata
    #                                            , times = times)
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






### RSF MODEL ###################################
for (fold in 1:nfolds){
  # Training and fitting model:
  trainingid <- na.omit(c(trainingid_all[,fold], validationid_all[,fold]))
  train_data <- data %>% filter(ID %in% trainingid)
  test_data <- data %>% filter((ID %in% testingid_all[,fold])) 
  train_id <- train_data$ID
  test_id <- test_data$ID
  train_data$ID <- NULL
  test_data$ID <- NULL
  
  
#  model_name <- 'rsf_expanded_var_y15_2_1'
  model_name <- 'rsf_expanded_var_y15_2_2'
  gc()
  main_dir <- paste0(work_dir, '/rdata_files')
  sub_dir <- paste0(model_name, '_fold_',fold)
  
  if(!dir.exists(file.path(main_dir, sub_dir))){
    createDir(main_dir, sub_dir)
  }
#  set.seed(seed)
  set.seed(seed-1)
  
  # model <- running_rsf(train_data)
  model= rfsrc(Surv(time,event)~., data = train_data 
            , ntree = 101
            , splitrule = 'logrank' #there is also logrankrandom, logrankscore, and conserve splitting  
  )
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
    print(performance_testset$iauc.uno)
    
    
    # prob_risk_train <- predictRisk.rsf(trained_model
    #                                   , newdata = trained_data
    #                                   , times = eval_times
    # )
    # prob_risk_train_with_ID <- cbind(train_id, prob_risk_train)
    # save(prob_risk_train_with_ID
    #      , file = paste0(saving.dir, '/prob_risk_train_set_with_ID.RData'))
    
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









### cFOREST MODEL ###################################
for (fold in 1:nfolds){
  # Training and fitting model:
  trainingid <- na.omit(c(trainingid_all[,fold], validationid_all[,fold]))
  train_data <- data %>% filter(ID %in% trainingid)
  test_data <- data %>% filter((ID %in% testingid_all[,fold])) 
  train_id <- train_data$ID
  test_id <- test_data$ID
  train_data$ID <- NULL
  test_data$ID <- NULL
  
  
  model_name <- 'cForest_expanded_var_y15_2'
  gc()
  main_dir <- paste0(work_dir, '/rdata_files')
  sub_dir <- paste0(model_name, '_fold_',fold)
  
  if(!dir.exists(file.path(main_dir, sub_dir))){
    createDir(main_dir, sub_dir)
  }
  set.seed(seed)
  model <- running_cForest(train_data)
  saving_dir <- file.path(main_dir, sub_dir)
  save(model, file = paste0(saving_dir,'/', model_name, '.RData'))
  
  
  
  # Test set performance: ###################################################################
  loading.dir <- paste0(work_dir, '/rdata_files/', model_name, '_fold_', fold)
  saving.dir <- loading.dir
  trained_data <- train_data
  trained_model <- model
  
  tryCatch({
    
    # probability of having had the disease:
    prob_risk_test <- predictRisk.cForest(trained_model
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
    
    
    
    prob_risk_train <- predictRisk.cForest(trained_model
                                       , newdata = trained_data
                                       , times = eval_times
    )
    prob_risk_train_with_ID <- cbind(train_id, prob_risk_train)
    save(prob_risk_train_with_ID
         , file = paste0(saving.dir, '/prob_risk_train_set_with_ID.RData'))
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  
}








