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
work_dir = '/Volumes/MR-Research$/Hieu/CARDIA_longi_project'
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



### Get AIC and BIC plots for each longi variable: ############################# 
for (longi_var_idx in 3:length(names(long_data_ts))){
  # longi_var_idx <- 32 # SBP
  longi_var <- names(long_data_ts)[longi_var_idx]
  print(paste0('Variable number: ', longi_var_idx -3+1, ' -- Variable name: ', longi_var))
        
  
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
    s1 <- step1measures(longi_var_df_wide_locf, traj_data_time, ID = TRUE)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
  if(!is.null(s1)){
    s2 <- step2factors(s1)
    #s3 <- step3clusters(s2)
    
    
    kmeansAIC <- function(fit){
      
      m = ncol(fit$centers) 
      k = nrow(fit$centers)
      D = fit$tot.withinss
      return(D + 2*m*k)
      
    } 
    # source: https://bgstieber.github.io/post/an-introduction-to-the-kmeans-algorithm/ 
    kmeansBIC <- function(fit){
      m = ncol(fit$centers) 
      n = length(fit$cluster)
      k = nrow(fit$centers)
      D = fit$tot.withinss
      return(D + log(n) * m * k) # using log(n) instead of 2, penalize model complexity
    }
    
    
    kmeans_aic_bic <- function(data, center_range, iter.max, nstart, plot = TRUE){
      
      #fit kmeans for each center
      all_kmeans <- lapply(center_range, 
                           FUN = function(k) 
                             kmeans(data, center = k, iter.max = iter.max, nstart = nstart))
      
      #extract AIC from each
      all_aic <- sapply(all_kmeans, kmeansAIC)
      #extract BIC from each
      all_bic <- sapply(all_kmeans, kmeansBIC)
      #extract tot.withinss
      all_wss <- sapply(all_kmeans, FUN = function(fit) fit$tot.withinss)
      #extract between ss
      btwn_ss <- sapply(all_kmeans, FUN = function(fit) fit$betweenss)
      #extract totall sum of squares
      tot_ss <- all_kmeans[[1]]$totss
      #put in data.frame
      clust_res <- 
        data.frame('Clusters' = center_range, 
                   'AIC' = all_aic, 
                   'BIC' = all_bic, 
                   'WSS' = all_wss,
                   'BSS' = btwn_ss,
                   'TSS' = tot_ss)
      #plot or no plot?
      if(plot){
        pdf(paste0(work_dir,"/figures/", longi_var, "_AIC_BIC_plot.pdf"),         # File name
            width = 8, height = 7, # Width and height in inches
            bg = "white",          # Background color
            colormodel = "cmyk",    # Color model (cmyk is required for most publications)
            paper = "A4")
        
        par(mfrow = c(2,2))
        with(clust_res,{
          plot(Clusters, AIC)
          plot(Clusters, BIC)
          plot(Clusters, WSS, ylab = 'Within Cluster SSE')
          plot(Clusters, BSS / TSS, ylab = 'Prop of Var. Explained')
          
        dev.off() 
        
        })
      }
      
      return(clust_res)
    }
    
    var_oi_clust <-kmeans_aic_bic(data = s2$factors[-1], center_range = 2:20, iter.max = 20, nstart = 25)
  }}





### Examine the AIC-BIC plots for each longi marker and decide on the number of clusters ##########


aic_bic_n_clust_list <- list() 
aic_bic_n_clust_list[['SBP']] <- 7
aic_bic_n_clust_list[['PSTYR']] <- 5
aic_bic_n_clust_list[['PSTYR']] <- 5
aic_bic_n_clust_list[['DBP']] <- 7
aic_bic_n_clust_list[['WST']] <- 8
aic_bic_n_clust_list[['WGT']] <- 6
aic_bic_n_clust_list[['PULSE']] <- 10
aic_bic_n_clust_list[['NTRIG']] <- 6
aic_bic_n_clust_list[['LDL']] <- 8
aic_bic_n_clust_list[['HDL']] <- 8
aic_bic_n_clust_list[['GLU']] <- 8
aic_bic_n_clust_list[['ED']] <- 6
aic_bic_n_clust_list[['DFPAY']] <- 7
aic_bic_n_clust_list[['CHOL']] <- 8
aic_bic_n_clust_list[['BMI']] <- 6
aic_bic_n_clust_list[['ARMCI']] <- 7




# cluster and assign memberships with the optimal number of clusters:

cluster_all_df <- longi_data %>% dplyr::select(one_of(c('ID','event','time',fixed_var))) %>% 
  dplyr::filter(!duplicated(ID, fromLast=TRUE)) %>% 
  cluster_all_df$ID <- cluster_all_df$ID %>% as.character()


for (longi_var_idx in 3:length(names(long_data_ts))){
  # longi_var_idx <- 32 # SBP
  longi_var <- names(long_data_ts)[longi_var_idx]
  print(paste0('Variable number: ', longi_var_idx -3+1, ' -- Variable name: ', longi_var))
  
  
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
    s1 <- step1measures(longi_var_df_wide_locf, traj_data_time, ID = TRUE)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  if(!is.null(s1)){
    s2 <- step2factors(s1)

    var_oi_opt_kmeans <- kmeans(s2$factors[-1], centers = aic_bic_n_clust_list[[longi_var]], nstart = 50, iter.max = 50)
    cluster_assign <- clue::cl_predict(var_oi_opt_kmeans, s2$factors[-1])
    cluster_assign_df <- data.frame('ID' = s2$factors$ID
                                    , 'cluster' = cluster_assign %>% as.numeric())
    
    # extract cluster assignments for this longi variable then merge with other data:
    cluster_assign_df[paste0(longi_var, '_cluster')] <- cluster_assign_df$cluster
    cluster_assign_df$ID <- cluster_assign_df$ID %>% as.character()
    cluster_all_df <- cluster_all_df %>% inner_join(cluster_assign_df %>% dplyr::select(c('ID', paste0(longi_var, '_cluster'))), by = 'ID')
  }
  
}

var_clustered <- names(cluster_all_df %>% dplyr::select(-one_of(c('ID', 'event', 'time', fixed_var))))

write.csv(cluster_all_df, file = paste0(work_dir, '/csv_files/proc_traj_cluster_assignments_aic_bic.csv'), row.names = FALSE)


# frequency table of cluster assignments:
library(tidyverse)
cluster_assign_freq_list <- cluster_all_df %>% dplyr::select(-one_of(c('event', 'time', 'RACEBLACK', 'MALE', 'AGE_Y0'))) %>% 
  dplyr::filter(!duplicated(ID, fromLast=TRUE)) %>% 
  map( table )
cluster_assign_freq_table <- apply(cluster_all_df %>% dplyr::select(-one_of(c('event', 'time', 'RACEBLACK', 'MALE', 'AGE_Y0'))) %>% 
                                     dplyr::filter(!duplicated(ID, fromLast=TRUE))
                                   , 2, function(x) length(unique(x)))













##### merge with data Y15 ###########################################################

# load the dataset
loading_dir = paste0(work_dir, '/csv_files')

cluster_all_df <- read.csv(paste0(work_dir, '/csv_files/proc_traj_cluster_assignments_aic_bic.csv'))

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


#Check if there is any character column, then delete them to make sure all data is numeric:
nums <- unlist(lapply(data, is.character))  
data[,nums]<-NULL


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

data <- cluster_all_df %>% 
  dplyr::filter(!duplicated(ID, fromLast=TRUE)) %>%
  dplyr::mutate(AGE_Y15 = AGE_Y0 +15) %>% dplyr::select(-AGE_Y0) %>%
  dplyr::mutate(time = time -15)


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
  
  
  model_name <- 'cox_expanded_var_traj_only_aic_bic'
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
  
  
  model_name <- 'lasso_expanded_var_traj_only_aic_bic'
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
  
  
  model_name <- 'rsf_expanded_var_traj_only_aic_bic'
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

