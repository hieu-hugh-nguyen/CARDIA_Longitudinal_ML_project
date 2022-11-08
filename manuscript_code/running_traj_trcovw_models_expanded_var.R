rm(list=ls()) #Clear all
cat("\014")

# head(example.data$data)
# 
# head(example.data$time)
# s1 = step1measures(example.data$data, example.data$time, ID = TRUE)
# s2=step2factors(s1)
# s3=step3clusters(s2,nclusters=4)
# s3$clust.distr
# plot(s3)
# plotMeanTraj(s3)
# plotMedTraj(s3)
# plotCombTraj(s3)



work_dir = 'U:/Hieu/CARDIA_longi_project'
# work_dir = '/Volumes/MR-Research$/Hieu/CARDIA_longi_project'


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




### perform clustering ###############################################


longi_data <- read.csv(paste0(work_dir,'/csv_files/data_longi_expanded_var_for_dynamic_deephit_and_ts_extraction_2.csv'))

# check range of each variable to check for outliers:
range_df <- t(sapply(longi_data, range)) %>% data.frame() %>% dplyr::mutate(across(where(is.numeric), ~round(.,0)))

# replace inplausible ARMCI values (0) with locf and then NOCB: 
longi_data_replace_ARMCI <- longi_data %>% dplyr::mutate(ARMCI = ifelse(ARMCI == 0, NA, ARMCI)) %>%
  group_by(ID) %>% tidyr::fill(ARMCI, .direction = 'down') %>% tidyr::fill(ARMCI, .direction = 'up') %>% ungroup()


traj_data_time <- data.frame(ID = longi_data$ID %>% unique() %>% as.character()
                         , time.0 = 0
                         , time.2 = 2
                         , time.5 = 5
                         , time.7 = 7
                         , time.15 = 10
                         , time.10 = 15
                    )

fixed_var = c('RACEBLACK','MALE','AGE_Y0')
long_data_ts <- longi_data %>% dplyr::select(-one_of(c('event','time',fixed_var)))


cluster_all_df <- longi_data %>% dplyr::select(one_of(c('ID','event','time',fixed_var))) %>%
  dplyr::filter(!duplicated(ID, fromLast=TRUE))
  
cluster_all_df$ID <- cluster_all_df$ID %>% as.character()

s3_all <- vector("list", 1)

'%notin%' <- Negate('%in%')
clustered_var <- c("ARMCI", "BMI","CHOL","DFPAY"
                   ,"ED","GLU","HDL","LDL","NTRIG"
                   ,"PSTYR","PULSE","WGT","WST","DBP","SBP")
var_to_be_clustered <- names(long_data_ts)[names(long_data_ts) %notin% clustered_var]


for (longi_var_idx in 3:length(var_to_be_clustered)){
  # longi_var_idx <-4 # BEER
  longi_var <- var_to_be_clustered[longi_var_idx]
  print(paste0('Variable number: ', longi_var_idx -3+1, ' -- Variable name: ', longi_var))
        
#  longi_var <- 'GLU'
  longi_var_df <- long_data_ts %>% dplyr::select(c('ID', 'exam_year', longi_var))
  longi_var_df$exam_year = longi_var_df$exam_year %>% as.numeric()

  longi_var_df_wide <- stats::reshape(longi_var_df, idvar = 'ID', timevar = 'exam_year', direction = 'wide')
 

  if (longi_var == 'ARMCI'){
    longi_var_df_wide[longi_var_df_wide == 0] <- NA
  }
  
  longi_var_df_wide_locf <- setNames(data.frame(t(longi_var_df_wide[,-1])), longi_var_df_wide[,1]) %>% 
    tidyr::fill(names(.), .direction = 'down') %>% tidyr::fill(names(.), .direction = 'up') %>% t() %>%
    data.frame() %>% tibble::rownames_to_column("ID")
  
  
  s1 <- NULL  
  tryCatch({
    s1 <- step1measures2(longi_var_df_wide_locf, traj_data_time, ID = TRUE)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
  
  
  
  
  if(!is.null(s1)){
    s2 <- step2factors(s1)
    #s3 <- step3clusters(s2)
    
    s3 <- step3clusters2(s2
                         , max_no_clust = 5
                         , criteria= 'trcovw')
    #s3_all_criteria <- step3clusters(s2, criteria= 'all')
    
    # library(NbClust)
    # diss_matrix<- dist(s2$factors, method = "euclidean", diag=FALSE)
    # 
    # s3_trcovw <- NbClust(data = s2$factors %>% as.matrix(), diss = diss_matrix, distance = NULL
    #                   , index= 'trcovw', method = 'kmeans', min.nc=2, max.nc=6)
    # s3_trcovw <- NbClust(data, method = "kmeans", index = criteria, max.nc =6)
        # NbClust(data, diss=diss_matrix, distance = NULL
    #         method = "ward.D2", index = "all") 
    
    s3$clust.distr
    s3_all[[paste0(longi_var, '_s3')]] <- s3
    
    # extract cluster assignments for this longi variable then merge with other data:
    cluster_assign_df <- s3$clusters 
    cluster_assign_df[paste0(longi_var, '_cluster')] <- cluster_assign_df$cluster
    cluster_assign_df$ID <- cluster_assign_df$ID %>% as.character()
    cluster_all_df <- cluster_all_df %>% inner_join(cluster_assign_df %>% dplyr::select(c('ID', paste0(longi_var, '_cluster'))), by = 'ID')
    
  }
  else{
    s3_all[[paste0(longi_var, '_s3')]] <- NULL
  }

  
}

additional_var_clustered <- names(cluster_all_df %>% dplyr::select(-one_of(c('ID', 'event', 'time', fixed_var))))

write.csv(cluster_all_df, file = paste0(work_dir, '/csv_files/proc_traj_cluster_assignments_binary_var_trcovw.csv'), row.names = FALSE)

save(s3_all, file = paste0(work_dir, '/rdata_files/proc_traj_s3_object_binary_var_trcovw.RData'))


# frequency table of cluster assignments:
library(tidyverse)
cluster_assign_freq_list <- cluster_all_df %>% dplyr::select(-one_of(c('event', 'time', 'RACEBLACK', 'MALE', 'AGE_Y0'))) %>% 
  dplyr::filter(!duplicated(ID, fromLast=TRUE)) %>% 
  map( table )
cluster_assign_freq_table <- apply(cluster_all_df %>% dplyr::select(-one_of(c('event', 'time', 'RACEBLACK', 'MALE', 'AGE_Y0'))) %>% 
                                     dplyr::filter(!duplicated(ID, fromLast=TRUE))
                                   , 2, function(x) length(unique(x)))















# s3_load <- get(load(paste0(work_dir, '/rdata_files/proc_traj_s3_object_gap_3.RData')))
# plotCombTraj(s3_load$SBP_s3)
# s3_with_NA_load <- get(load(paste0(work_dir, '/rdata_files/proc_traj_s3_object_with_NA_gap_3.RData')))
# plotCombTraj(s3_with_NA_load$SBP_s3)


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



# data <- data_and_cluster_df


# data <- cluster_all_df %>%
#   dplyr::filter(!duplicated(ID, fromLast=TRUE)) %>%
#   dplyr::mutate(AGE_Y15 = AGE_Y0 +15) %>% dplyr::select(-AGE_Y0) %>%
#   dplyr::mutate(time = time -15)

# data <- cluster_all_df %>%
#   dplyr::filter(!duplicated(ID, fromLast=TRUE)) %>%
#   dplyr::mutate(AGE_Y15 = AGE_Y0 +15) %>% dplyr::select(-AGE_Y0) %>%
#   dplyr::mutate(time = time -15) %>%
#   inner_join(data_y15_truncated_tte %>%
#                dplyr::select(one_of(c('ID', 'CHNOW', 'ASMA', 'PATCK', 'KIDNY', 'LIVER', 'MENTL'))) %>%
#                dplyr::mutate(ID = as.character(ID))
#               , by = 'ID') %>%
#   dplyr::select(-one_of(c('ASMA_cluster', 'PATCK_cluster', 'KIDNY_cluster', 'LIVER_cluster', 'MENTL_cluster'))) %>%
#   dplyr::rename(ASMA_cluster = ASMA) %>% dplyr::rename(PATCK_cluster = PATCK) %>%
#   dplyr::rename(KIDNY_cluster = KIDNY) %>% dplyr::rename(LIVER_cluster = LIVER) %>%
#   dplyr::rename(MENTL_cluster = MENTL) %>%
#   dplyr::rename(CHNOW_cluster = CHNOW)

# for version 6:
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

write.csv(data, file = paste0(work_dir, '/csv_files','/data_for_traj_trcovw_6_model.csv'), row.names = F)


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
  
  
  model_name <- 'cox_expanded_var_traj_cont_n_binary_var_only_trcovw'
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
  
  
  model_name <- 'lasso_expanded_var_traj_cont_n_binary_var_only_trcovw'
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
  
  
#  model_name <- 'rsf_expanded_var_traj_cont_n_binary_var_only_trcovw_5'
  model_name <- 'rsf_expanded_var_traj_cont_n_binary_var_only_trcovw_6'
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





