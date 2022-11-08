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




longi_var_df_wide <- reshape(data_longi_analysis_cohort %>% dplyr::select(-one_of('status', 'time', 'time_te_in_yrs', 'AGE_Y0', 'MALE', 'RACEBLACK'))
                     , idvar = "ID", timevar = "exam_year", direction = "wide")

longi_var_df_wide_2 <- tidyr::pivot_wider(data_longi_analysis_cohort %>% dplyr::select(-one_of('status', 'time', 'time_te_in_yrs', 'AGE_Y0', 'MALE', 'RACEBLACK'))
                             , id_cols = "ID"
                             , names_from = 'exam_year'
                             , values_from = names(data_longi_analysis_cohort %>% dplyr::select(-one_of('ID', 'exam_year', 'status', 'time', 'time_te_in_yrs', 'AGE_Y0', 'MALE', 'RACEBLACK')))
                             , names_sep = "."
                             )

data_wide_locf <- setNames(data.frame(t(longi_var_df_wide[,-1])), longi_var_df_wide[,1]) %>% 
  tidyr::fill(names(.), .direction = 'down') %>% tidyr::fill(names(.), .direction = 'up') %>% t() %>%
  data.frame() %>% tibble::rownames_to_column("ID")

data_wide_locf_2 <- setNames(data.frame(t(longi_var_df_wide_2[,-1])), longi_var_df_wide_2[,1] %>% unlist() %>% as.character()) %>% 
  tidyr::fill(names(.), .direction = 'down') %>% tidyr::fill(names(.), .direction = 'up') %>% t() %>%
  data.frame() %>% tibble::rownames_to_column("ID")



data_y15 <- data_longi_analysis_cohort %>% filter(ID %in% subjects_in_cohort[[1]]) %>% 
  # filter(exam_year == 15)
  dplyr::filter(!duplicated(ID, fromLast=TRUE))
                
# truncate time to make start time at y15 (to avoid 15 years of immortal time):
data_y15_truncated_tte <- data_y15 %>% 
  mutate(time_te_in_yrs = time_te_in_yrs -15) %>% 
  dplyr::select(-time) %>% filter(time_te_in_yrs >0) %>%
  dplyr::rename(event = status) %>% dplyr::rename(time = time_te_in_yrs) %>%
  dplyr::select(-exam_year)




data_y15_truncated_tte$ID <- as.character(data_y15_truncated_tte$ID)
data_wide_locf$ID <- as.character(data_wide_locf$ID)

data_wide_locf_2$ID <- as.character(data_wide_locf_2$ID)


data <- data_y15_truncated_tte %>% dplyr::select(one_of(c('ID', 'event', 'time', 'AGE_Y0', 'MALE', 'RACEBLACK'))) %>%
  inner_join(data_wide_locf, by = 'ID')


# update age variable to be at landmark time:
data <- data %>% mutate(AGE_Y15 = AGE_Y0 +15) %>% dplyr::select(-AGE_Y0)

 
# plot corr matrix:
data_for_corr <- data_y15_truncated_tte %>% dplyr::select(one_of(c('ID', 'event', 'time', 'AGE_Y0', 'MALE', 'RACEBLACK'))) %>%
  inner_join(data_wide_locf_2, by = 'ID') %>% 
  dplyr::select(-one_of(c('ID', 'event', 'time'))) %>%
  dplyr::select(one_of(c('SBP.0','SBP.2','SBP.5','SBP.7','SBP.10','SBP.15'
                  ,'DBP.0','DBP.2','DBP.5','DBP.7','DBP.10','DBP.15'
                  ,'GLU.0','GLU.2','GLU.5','GLU.7','GLU.10','GLU.15'
                  ,'WST.0','WST.2','WST.5','WST.7','WST.10','WST.15'
                  ,'CGTDY.0','CGTDY.2','CGTDY.5','CGTDY.7','CGTDY.10','CGTDY.15'
                  
                  )))

corr_df <- cor(data_for_corr) %>% round(2)

# install.packages("corrplot")
library(corrplot)
corrplot(corr_df, method="color")
# corrplot(corr_df[1:50, 1:50], method="color")
# corrplot(corr_df[(ncol(corr_df)-50):ncol(corr_df), (ncol(corr_df)-50):ncol(corr_df)], method="color")








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
  
  
  model_name <- 'cox_expanded_var_concat'
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
  
  
  model_name <- 'lasso_expanded_var_concat'
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
  
  
  model_name <- 'rsf_expanded_var_concat'
  gc()
  main_dir <- paste0(work_dir, '/rdata_files')
  sub_dir <- paste0(model_name, '_fold_',fold)
  
  if(!dir.exists(file.path(main_dir, sub_dir))){
    createDir(main_dir, sub_dir)
  }
  set.seed(seed)
  # model <- running_rsf(train_data)
  model= rfsrc(Surv(time,event)~., data = train_data 
               , ntree = 1001
               # , importance = 'permute'
               , splitrule = 'logrank' #there is also logrankrandom, logrankscore, and conserve splitting  
  )
  saving_dir <- file.path(main_dir, sub_dir)
  save(model, file = paste0(saving_dir,'/', model_name, '.RData'))
  
  
  # Permutation importance:
  vimp_obj <- vimp(model, importance = 'permute')
  permute_importance = vimp_obj$importance
  permute_importance.df = data.frame(var.name = names(permute_importance), importance = permute_importance)
  permute_importance2.df = permute_importance.df[order(permute_importance.df[,2], decreasing = T), ]
  write.csv(permute_importance2.df, file = paste0(saving_dir,'/permut_vimp.csv'), row.names = F)
  
  
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
  
  
  model_name <- 'rsf_expanded_var_concat_ntree_100'
  gc()
  main_dir <- paste0(work_dir, '/rdata_files')
  sub_dir <- paste0(model_name, '_fold_',fold)
  
  if(!dir.exists(file.path(main_dir, sub_dir))){
    createDir(main_dir, sub_dir)
  }
  set.seed(seed)
  # model <- running_rsf(train_data)
  model= rfsrc(Surv(time,event)~., data = train_data 
               , ntree = 101
               # , importance = 'permute'
               , splitrule = 'logrank' #there is also logrankrandom, logrankscore, and conserve splitting  
  )
  saving_dir <- file.path(main_dir, sub_dir)
  save(model, file = paste0(saving_dir,'/', model_name, '.RData'))
  
  
  # Permutation importance:
  vimp_obj <- vimp(model, importance = 'permute')
  permute_importance = vimp_obj$importance
  permute_importance.df = data.frame(var.name = names(permute_importance), importance = permute_importance)
  permute_importance2.df = permute_importance.df[order(permute_importance.df[,2], decreasing = T), ]
  write.csv(permute_importance2.df, file = paste0(saving_dir,'/permut_vimp.csv'), row.names = F)
  
  
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


