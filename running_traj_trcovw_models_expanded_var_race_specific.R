rm(list=ls()) #Clear all
cat("\014")



work_dir = 'U:/Hieu/CARDIA_longi_project'


setwd(work_dir)

list.of.packages <- c("mlbench",'ggplot2','caret', 'dplyr', 'tibble', 'ROCR','parallelMap'
                      ,'riskRegression', 'survival','randomForestSRC', 'survivalROC'
                      , 'pec', 'risksetROC','survAUC', 'traj')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)

ncores <- parallel::detectCores(all.tests = FALSE, logical = TRUE)

# parallelMap::parallelStartSocket(ncores-1)


source_dir <- paste0(work_dir, '/code/git_code/snippet')
source(paste0(source_dir, '/running_rsf.R'))
source(paste0(source_dir, '/running_algo.R'))
source(paste0(source_dir, '/createDir.R'))
source(paste0(source_dir, '/subsetDataTopnVar.R'))
source(paste0(source_dir, '/predictSurvProb.R'))
source(paste0(source_dir, '/eval_performance.R'))
source(paste0(source_dir, '/eval_performance_using_different_auc_package.R'))
source(paste0(source_dir,'/normalize_var_importance.R'))
source(paste0(source_dir,'/step1measures2.R'))




##### merge with data Y15 ###########################################################

# load the dataset
loading_dir = paste0(work_dir, '/csv_files')

cluster_cont_var_df <- read.csv(paste0(work_dir, '/csv_files/proc_traj_cluster_assignments_trcovw_4.csv'))
cluster_binary_var_df <- read.csv(paste0(work_dir, '/csv_files/proc_traj_cluster_assignments_binary_var_trcovw.csv'))

cluster_all_df <- cluster_binary_var_df %>% 
  dplyr::filter(!duplicated(ID, fromLast=TRUE)) %>%
  inner_join(cluster_cont_var_df %>% dplyr::select(-one_of(c('event', 'time', 'RACEBLACK', 'MALE', 'AGE_Y0'))) %>% 
                                                      dplyr::filter(!duplicated(ID, fromLast=TRUE))
                                                    , by = 'ID')


data_longi_long_for_analysis <- read.csv(paste0(work_dir,'/csv_files/data_longi_long_format_expanded_variables_removed_missing_data_2.csv'))

subjects_in_cohort <- read.csv(paste0(work_dir,'/csv_files/subjects_in_final_analysis_cohort.csv'))

data_longi_long_up_to_y15 <- data_longi_long_for_analysis %>% filter(exam_year <=15)
data_longi_analysis_cohort <- data_longi_long_up_to_y15 %>% filter(ID %in% subjects_in_cohort[[1]])

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




# merge Y15 data and cluster assignments:
data$ID <- data$ID %>% as.character()
cluster_all_df$ID <- cluster_all_df$ID %>% as.character()

data_and_cluster_df <- data %>% 
  inner_join(cluster_all_df %>% dplyr::select(-one_of(c('event', 'time', 'RACEBLACK', 'MALE', 'AGE_Y0'))) %>% 
               dplyr::filter(!duplicated(ID, fromLast=TRUE))
             , by = 'ID')
# nrow(data_and_cluster_df)
# write.csv(data_and_cluster_df, file = paste0(work_dir, '/csv_files','/data_for_traj_models.csv'), row.names = F)



data <- cluster_all_df %>%
  dplyr::filter(!duplicated(ID, fromLast=TRUE)) %>%
  dplyr::mutate(AGE_Y15 = AGE_Y0 +15) %>% dplyr::select(-AGE_Y0) %>%
  dplyr::mutate(time = time -15) %>%
  inner_join(data_y15_truncated_tte %>%
               dplyr::select(one_of(c('ID', 'CHNOW', 'PATCK', 'LIVER'))) %>%
               dplyr::mutate(ID = as.character(ID))
             , by = 'ID') %>%
  dplyr::select(-one_of(c('PATCK_cluster', 'LIVER_cluster'))) %>%
  dplyr::rename(PATCK_cluster = PATCK) %>%
  dplyr::rename(LIVER_cluster = LIVER) %>%
  dplyr::rename(CHNOW_cluster = CHNOW)

# write.csv(data, file = paste0(work_dir, '/csv_files','/data_for_traj_trcovw_6_model.csv'), row.names = F)


### START BUILDING MODELS: #################################################################



seed <- 4495
set.seed(seed)
nfolds <- 10

endpt <- 17; # after Year 15
eval_times <- seq(1, endpt, by = 1)



data_white = data %>% dplyr::filter(RACEBLACK == 0) %>% dplyr::select(-RACEBLACK)
data_black = data %>% dplyr::filter(RACEBLACK == 1) %>% dplyr::select(-RACEBLACK)



running_traj_models <- function(data_, data_name){
  
  
  # data_ = data_white
  # data_name = 'white_only'
  
  # load training IDs:
  trainingid_all <- read.csv(paste0(work_dir,'/csv_files/all_training_set_ID_',data_name,'.csv'))
  validationid_all <- read.csv(paste0(work_dir,'/csv_files/all_validation_set_ID_',data_name,'.csv'))
  testingid_all <- read.csv(paste0(work_dir,'/csv_files/all_testing_set_ID_',data_name,'.csv'))
  
  


# 
# 
#   ### COX-PH MODEL ###################################
#   
#   for (fold in 1:nfolds){
#     # Training and fitting model:
#     # fold = 1
#     trainingid <- na.omit(c(trainingid_all[,fold], validationid_all[,fold]))
#     train_data <- data_ %>% filter(ID %in% trainingid)
#     test_data <- data_ %>% filter((ID %in% testingid_all[,fold])) 
#     train_id <- train_data$ID
#     test_id <- test_data$ID
#     train_data$ID <- NULL
#     test_data$ID <- NULL
#     
#   
#     model_name <- paste0('cox_expanded_var_traj_cont_n_binary_var_only_trcovw_', data_name)
#     gc()
#     main_dir <- paste0(work_dir, '/rdata_files')
#     sub_dir <- paste0(model_name, '_fold_',fold)
#     
#     if(!dir.exists(file.path(main_dir, sub_dir))){
#       createDir(main_dir, sub_dir)
#     }
#     set.seed(seed)
#     # model <- running_coxph(train_data)
#     model <- coxph(Surv(time,event) ~. # MALE+RACEBLACK+CHOL+DIAB+HBM+HDL+SBP+SMKNW+AGE_Y15
#                    , data = train_data, singular.ok = T, x=TRUE)
#     saving_dir <- file.path(main_dir, sub_dir)
#     save(model, file = paste0(saving_dir,'/', model_name, '.RData'))
#     
#     
#     
#     # Test set performance: ###################################################################
#     loading.dir <- paste0(work_dir, '/rdata_files/', model_name, '_fold_', fold)
#     saving.dir <- loading.dir
#     trained_data <- train_data
#     trained_model <- model
#     
#     tryCatch({
#       
#       # probability of having had the disease:
#       prob_risk_test <- predictRisk.cox(trained_model
#                                         , newdata = test_data
#                                         , times = eval_times
#       )  
#       # prob.risk = riskRegression::predictRisk(trained.model, newdata = newdata
#       #                                            , times = times)
#       prob_risk_test_with_ID <- cbind(test_id, prob_risk_test)
#       save(prob_risk_test_with_ID
#            , file = paste0(saving.dir, '/prob_risk_test_set_with_ID.RData'))
#       
#       
#       
#       prob_risk_test[is.na(prob_risk_test)] = 0
#       performance_testset = eval_performance3_5(prob.risk.test.set = prob_risk_test
#                                               , test.data = test_data
#                                               , trained.data = trained_data
#                                               , eval.times = eval_times
#       )
#       save(performance_testset
#            , file = paste0(saving.dir, '/performance_testset.RData'))
#       
#        
#     }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#     
#     
#   }
#   
# 
# 
# 
# 
# 
#   ### LASSO-COX MODEL ###################################
#   
#   for (fold in 1:nfolds){
#     # Training and fitting model:
#     # fold = 1
#     trainingid <- na.omit(c(trainingid_all[,fold], validationid_all[,fold]))
#     train_data <- data_ %>% filter(ID %in% trainingid)
#     test_data <- data_ %>% filter((ID %in% testingid_all[,fold])) 
#     train_id <- train_data$ID
#     test_id <- test_data$ID
#     train_data$ID <- NULL
#     test_data$ID <- NULL
#   
#   
#     model_name <- paste0('lasso_expanded_var_traj_cont_n_binary_var_only_trcovw_', data_name)
#     gc()
#     main_dir <- paste0(work_dir, '/rdata_files')
#     sub_dir <- paste0(model_name, '_fold_',fold)
#     
#     if(!dir.exists(file.path(main_dir, sub_dir))){
#       createDir(main_dir, sub_dir)
#     }
#     set.seed(seed)
#     model <- running_lasso(train_data)
#     # model <- coxph(Surv(time,event) ~. # MALE+RACEBLACK+CHOL+DIAB+HBM+HDL+SBP+SMKNW+AGE_Y15
#     #                , data = train_data, singular.ok = T, x=TRUE)
#     saving_dir <- file.path(main_dir, sub_dir)
#     save(model, file = paste0(saving_dir,'/', model_name, '.RData'))
#     
#     
#     
#     # Test set performance: ###################################################################
#     loading.dir <- paste0(work_dir, '/rdata_files/', model_name, '_fold_', fold)
#     saving.dir <- loading.dir
#     trained_data <- train_data
#     trained_model <- model
#     
#     tryCatch({
#       
#       # probability of having had the disease:
#       prob_risk_test <- predictRisk.cox(trained_model
#                                         , newdata = test_data
#                                         , times = eval_times
#       )  
#       # prob.risk = riskRegression::predictRisk(trained.model, newdata = newdata
#       #                                            , times = times)
#       prob_risk_test_with_ID <- cbind(test_id, prob_risk_test)
#       save(prob_risk_test_with_ID
#            , file = paste0(saving.dir, '/prob_risk_test_set_with_ID.RData'))
#       
#       
#       
#       prob_risk_test[is.na(prob_risk_test)] = 0
#       performance_testset = eval_performance3_5(prob.risk.test.set = prob_risk_test
#                                               , test.data = test_data
#                                               , trained.data = trained_data
#                                               , eval.times = eval_times
#       )
#       save(performance_testset
#            , file = paste0(saving.dir, '/performance_testset.RData'))
#     
#       
#     
#     }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#     
#     
#   }





  
  ### RSF MODEL ###################################
  for (fold in 6:nfolds){
    # Training and fitting model:
    trainingid <- na.omit(c(trainingid_all[,fold], validationid_all[,fold]))
    train_data <- data_ %>% filter(ID %in% trainingid)
    test_data <- data_ %>% filter((ID %in% testingid_all[,fold])) 
    train_id <- train_data$ID
    test_id <- test_data$ID
    train_data$ID <- NULL
    test_data$ID <- NULL
  
  
    model_name <- paste0('rsf_expanded_var_traj_cont_n_binary_var_only_trcovw_', data_name)
    gc()
    main_dir <- paste0(work_dir, '/rdata_files')
    sub_dir <- paste0(model_name, '_fold_',fold)
    
    if(!dir.exists(file.path(main_dir, sub_dir))){
      createDir(main_dir, sub_dir)
    }
    set.seed(seed)
    # model <- running_rsf(train_data)
    model= rfsrc(Surv(time,event)~., data = train_data 
                 , ntree = 10001
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
      performance_testset = eval_performance3_5(prob.risk.test.set = prob_risk_test
                                              , test.data = test_data
                                              , trained.data = trained_data
                                              , eval.times = eval_times
      )
      save(performance_testset
           , file = paste0(saving.dir, '/performance_testset.RData'))
      print(performance_testset$iauc.uno)
      
    

  
    
    # VIMP:


    max.subtree = max.subtree(model, conservative = F)
    #save(max.subtree, file = paste(saving.dir, '/RF_maxtree.Rdata', sep = ''))

    # Get minimal depth of maximal subtree in terms of variable name, ascending order:
    allvardepth = sort(max.subtree$order[, 1])
    allvardepth.df = data.frame(Variable=names(allvardepth),MinDepthMaxSubtree=allvardepth,row.names = NULL)

    allvardepth.df$normalized_depth = normalize_var_imp(allvardepth.df$MinDepthMaxSubtree)

    write.csv(allvardepth.df, file = paste(saving_dir, '/depth_rank.csv', sep = ''),row.names=F)
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
  }



}




running_traj_models(data_white, 'white_only')
running_traj_models(data_black, 'black_only')
