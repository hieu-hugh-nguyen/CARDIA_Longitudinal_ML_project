# outer loop 

rm(list=ls()) #Clear all
cat("\014")

# set working directory: 
work_dir = 'U:/Hieu/CARDIA_longi_project'
work_dir = '/home/idies/workspace/Storage/hnguye78/persistent/CARDIA_longi_project'

setwd(work_dir)

# load libraries:
list.of.packages <- c("mlbench",'ggplot2','caret', 'dplyr', 'tibble', 'ROCR','parallelMap'
                      ,'riskRegression', 'survival','randomForestSRC', 'survivalROC'
                      , 'pec', 'risksetROC','JMbayes2')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)

ncores <- parallel::detectCores(all.tests = FALSE, logical = TRUE)

parallelMap::parallelStartSocket(ncores-1)

# source snippet functions:
source_dir <- 'U:/Hieu/CARDIA_project/CARDIA_project/Git'
source_dir <- '/home/idies/workspace/Storage/hnguye78/persistent/CARDIA_project/cvd_outcome_rerun/'

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



# NO truncation of time since this is joint modeling:
data <- data_longi_analysis_cohort %>% 
  # mutate(time_te_in_yrs = time_te_in_yrs -15) %>% 
  dplyr::select(-time) %>% filter(time_te_in_yrs >0) %>%
  rename(event = status) %>% rename(time = time_te_in_yrs)
#dplyr::select(-exam_year)
data_at_baseline_formatted <- data_at_baseline %>% 
  # mutate(time_te_in_yrs = time_te_in_yrs -15) %>% 
  dplyr::select(-time) %>% filter(time_te_in_yrs >0) %>%
  rename(event = status) %>% rename(time = time_te_in_yrs)
#dplyr::select(-exam_year)



#Check if there is any character column, then delete them to make sure all data is numeric:
nums <- unlist(lapply(data, is.character))  
data[,nums]<-NULL



# load training IDs:
trainingid_all <- read.csv(paste0(work_dir,'/csv_files/all_training_set_ID.csv'))
validationid_all <- read.csv(paste0(work_dir,'/csv_files/all_validation_set_ID.csv'))
testingid_all <- read.csv(paste0(work_dir,'/csv_files/all_testing_set_ID.csv'))




### START BUILDING MODELS: #################################################################




seed <- 4495
set.seed(seed)
nfolds <- 1

endpt <- 33; # after Year 15
eval_times <- seq(1, endpt, by = 1)


for (fold in c(5,6,8,9,10,1,2,3,7)){
  # fold = 1
  # Training and fitting model:
  trainingid <- na.omit(c(trainingid_all[,fold], validationid_all[,fold]))
  train_data <- data %>% filter(ID %in% trainingid)
  test_data <- data %>% filter((ID %in% testingid_all[,fold])) 
  train_id <- train_data$ID
  test_id <- test_data$ID
  #train_data$ID <- NULL
  #test_data$ID <- NULL
  
  
  model_name <- 'jmbayes2_ascvd_var_y15'
  gc()
  main_dir <- paste0(work_dir, '/rdata_files')
  sub_dir <- paste0(model_name, '_fold_',fold)
  
  if(!dir.exists(file.path(main_dir, sub_dir))){
    createDir(main_dir, sub_dir)
  }

  tryCatch({
  
  start_time <- Sys.time()
  
  df_ = 2 
  
  chol_lme <- nlme::lme(CHOL ~ exam_year * MALE * RACEBLACK   + AGE_Y0
                        , data = train_data
                        , random = ~ exam_year | ID
                        , control = lmeControl(opt = 'optim'))
  
  hdl_lme <- nlme::lme(HDL ~ exam_year * MALE * RACEBLACK  + AGE_Y0
                        , data = train_data
                        , random =~ exam_year | ID 
                        , control = lmeControl(opt = 'optim'))

  # hdl_lme <- nlme::lme(HDL ~ ns(exam_year,df_) * MALE * RACEBLACK # + AGE_Y0
  #                      , data = train_data
  #                      , random =~ ns(exam_year,df_) | ID 
  #                      , control = lmeControl(opt = 'optim'))
  
  
  sbp_lme <- nlme::lme(SBP ~ exam_year * MALE * RACEBLACK  + AGE_Y0
                        , data = train_data
                        , random =~ exam_year | ID
                        , control = lmeControl(opt = 'optim'))

  # sbp_lme <- nlme::lme(SBP ~ ns(exam_year,df_) * MALE * RACEBLACK # + AGE_Y0
  #                      , data = train_data
  #                      , random =~ ns(exam_year,df_) | ID
  #                      , control = lmeControl(opt = 'optim'))
  
  smknw_mixed <- GLMMadaptive::mixed_model(SMKNW ~ exam_year*MALE*RACEBLACK #+ AGE_Y0
                                     ,data = train_data
                                     ,random =~ exam_year | ID
                                     ,family = binomial())
  # print('smknw_mixed done!')
  # diab_mixed <- GLMMadaptive::mixed_model(DIAB ~ exam_year*MALE*RACEBLACK #+ AGE_Y0
  #                                          ,data = train_data
  #                                          ,random =~ exam_year | ID
  #                                          ,family = binomial())
  # print('diab_mixed done!')
  # hbm_mixed <- GLMMadaptive::mixed_model(HBM ~ exam_year*MALE*RACEBLACK #+ AGE_Y0
  #                                         ,data = train_data
  #                                         ,random =~ exam_year | ID
  #                                        ,family = binomial())
  # print('hbm_mixed done!')
  
  
  data_cox = data_at_baseline %>% filter(ID %in% train_id) %>%
    dplyr::select(-time) %>% filter(time_te_in_yrs >0) %>%
    rename(event = status) %>% rename(time = time_te_in_yrs)
  
  CoxFit <- coxph(Surv(time, event) ~ AGE_Y0+MALE+RACEBLACK+HDL+SBP+SMKNW #+CHOL+DIAB+HBM
                  , data = data_cox
                  , model = TRUE)
  
  jointFit <-JMbayes2::jm(CoxFit, list(chol_lme, hdl_lme, sbp_lme
                              #,smknw_mixed, diab_mixed, hbm_mixed
                              )
                 ,time_var = "exam_year"
                 ,id_var = 'ID'
                 #,
                 # ,n_iter = 12000L
                 # ,n_burnin = 2000L
                 # ,n_thin = 5L
                 ) 
  run_time <- Sys.time() - start_time
  print(paste0('Model Training Time: ', run_time))

  #summary(jointFit)
  
  model <- jointFit
  saving_dir <- file.path(main_dir, sub_dir)
  save(model, file = paste0(saving_dir,'/', model_name, '.RData'))
  
  
  ### AUC Computation ##################
  roc <- tvROC(model, newdata = test_data, Tstart = 15, Thoriz = 16)
  
  auc <- tvAUC(roc)
  
  
  eval_times <- seq(16,33,1)
  auc_over_time <- rep(NA, base::length(eval_times))
  
  for(eval_time in 1: base::length(eval_times)){
    roc <- tvROC(jointFit, newdata = test_data, Tstart = 15, Thoriz = eval_times[eval_time])
    auc <- tvAUC(roc)
    auc_over_time[eval_time] = auc$auc

  }
  print(auc_over_time)  

  save(auc_over_time
       , file = paste0(saving_dir, '/auc_over_time_testset.RData'))
  
  
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}  
  








  # individual prediction:
  predLong1 <- predict(model, newdata = test_data %>% filter(ID %in% c('100230401835'  ,'100161529872')), return_newdata = TRUE)
  plot(predLong1)
  predLong2 <- predict(jointFit, newdata = test_data %>% filter(ID %in% c('100230401835','100161529872'))
                       , return_newdata = TRUE
                       ,  times = seq(15, 35, 1))
  plot(predLong2,  subject ='100230401835')
  
  
  
  predSurv <- predict(jointFit, newdata = test_data %>% filter(ID %in% c('100023004268'))#  ,'100894332119'))
                      , process = "event"
                      ,times = seq(16, 35, 1),
                      ,type = "subject_specific"
                      ,return_newdata = TRUE
  )
  
  
  # 
  # Error in FUN(X[[i]], ...) : 
  #   length of 'derivs' is larger than length of 'x'
  # In addition: Warning message:
  #   In max(event[who2]) : no non-missing arguments to max; returning -Inf
  # >   predLong1 <- predict(model, newdata = test_data %>% filter(ID %in% c('100230401835'  ,'100161529872')), return_newdata = TRUE)
  # Error in FUN(X[[i]], ...) : 
  #   length of 'derivs' is larger than length of 'x'
  # In addition: Warning message:
  #   In max(event[who2]) : no non-missing arguments to max; returning -Inf
  # >   plot(predLong1)
  # Error in plot(predLong1) : object 'predLong1' not found
  
  
  data_for_pred <- test_data # %>% filter(ID %in% c('100023004268','100894332119'))
  
  predSurv <- predict(jointFit, newdata = test_data %>% filter(ID %in% c('100023004268','100894332119')), process = "event",
                      times = seq(16, 32, 1),
                      return_newdata = TRUE
  )

  
  plot(predSurv)
  

  
 
# # Output:  auc_over_time:
#   [1] 0.4073791 0.5526942 0.5540027 0.5711683 0.5723289 0.5577807 0.5628452 0.5591864 0.5452364 0.5286486 0.5252897 0.5263556 0.5167756 0.5193594 0.5369369 0.5410898 0.6995242
#   [18]        NA
  
  
  
  
  
  
  
  
  
  
  
  
    
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
#                                      , newdata = test_data
#                                      , times = eval_times
#                                      )
#     prob_risk_test_with_ID <- cbind(test_id, test_data)
#     save(prob_risk_test_with_ID
#          , file = paste0(saving.dir, '/prob_risk_test_set_with_ID.RData'))
#     
#     
#     
#     prob_risk_test[is.na(prob_risk_test)] = 0
#     performance_testset = eval_performance2(prob.risk.test.set = prob_risk_test
#                                             , test.data = test_data
#                                             , trained.data = trained_data
#                                             , eval.times = eval_times
#                                             )
#     save(performance_testset
#          , file = paste0(saving.dir, '/performance_testset.RData'))
#     
#     
#     
#     prob_risk_train <- predictRisk.rsf(trained_model
#                                       , newdata = trained_data
#                                       , times = eval_times
#     )
#     prob_risk_train_with_ID <- cbind(train_id, trained_data)
#     save(prob_risk_train_with_ID
#          , file = paste0(saving.dir, '/prob_risk_train_set_with_ID.RData'))
#     
#   }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#   
#   
# }


