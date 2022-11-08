# outer loop 

rm(list=ls()) #Clear all
cat("\014")

# set working directory: 
work_dir = 'U:/Hieu/CARDIA_longi_project'

setwd(work_dir)

# load libraries:
list.of.packages <- c("mlbench",'ggplot2','caret', 'dplyr', 'tibble', 'ROCR','parallelMap'
                      ,'riskRegression', 'mlr','mlrMBO')#,'irace')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)

ncores <- parallel::detectCores(all.tests = FALSE, logical = TRUE)
parallelMap::parallelStartSocket(ncores-1)

source(paste0(work_dir,'/code/git_code/snippet/running_rsf.R'))
source(paste0(work_dir,'/code/git_code/snippet/createDir.R'))
source(paste0(work_dir,'/code/git_code/snippet/subsetDataTopnVar.R'))






### load the dataset ###########################################################

# load extracted timeseries features:
loading_dir = paste0(work_dir, '/csv_files')

ts_features <- read.csv(paste0(work_dir,'/csv_files/tsfresh_features_extracted_features_drop_nonunique_dropna_rm_nonrelevant_drop_correlated_dropna_2.csv'))
# 148 features
# 2_4 version

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




data$ID <- data$ID %>% as.character()
ts_features$ID <- as.character(ts_features$ID)

data_tsfeatures_plus_most_recent_by_y15 <- data_y15_truncated_tte %>% 
  mutate(AGE_Y15 = AGE_Y0 +15) %>% dplyr::select(-AGE_Y0) %>% 
  left_join(ts_features, by = 'ID') %>%
  dplyr::select('ID','event','time',everything())

data <- data_tsfeatures_plus_most_recent_by_y15[complete.cases(data_tsfeatures_plus_most_recent_by_y15),]
# check nrow(data)




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




# START THE OUTER LOOP:

start_time <- Sys.time()
seed = 4495
set.seed(seed)
#for (fold in 1:ncol(trainingid.all)){
#for (fold in 1:25){
  fold = 1 
  
  
  trainingid <- na.omit(c(trainingid_all[,fold], validationid_all[,fold]))
  train_data <- data %>% filter(ID %in% trainingid)
  test_data <- data %>% filter((ID %in% testingid_all[,fold]))
  train_id <- train_data$ID
  test_id <- test_data$ID
  train_data$ID <- NULL
  test_data$ID <- NULL
  
  
  model_name <- 'rsf_expanded_var_and_ascvd_var_tsfeatures_plus_data_y15_2_4_5'
  gc()
  main_dir <- paste0(work_dir, '/rdata_files')
  sub_dir <- paste0(model_name, '_fold_',fold)
  
  if(!dir.exists(file.path(main_dir, sub_dir))){
    createDir(main_dir, sub_dir)
  }
  set.seed(seed)



  saving.dir = file.path(main_dir, sub_dir)
  
  #mlr.mbo:
  train.dat = train_data
  surv.task = makeSurvTask(data = train.dat, target = c("time", "event"))
  lrn = mlr::makeLearner("surv.randomForestSRC")
  
  ps = makeParamSet(
      makeIntegerParam(id = "ntree", default = 1000L, lower = 1L, upper = 6000),
      makeIntegerParam(id = "mtry", lower = 1L, upper = ceiling(ncol(train_data)/3)), #60
      makeIntegerParam(id = "nodesize", lower = 1L, default = 3L, upper = 5) #5
    #  makeIntegerLearnerParam(id = "nodedepth", default = -1L, ),
      #makeDiscreteLearnerParam(id = "splitrule", default = "logrank",
       #                        values = c("logrank", "logrankscore", "random")),
      #makeIntegerLearnerParam(id = "nsplit", lower = 0L, default = 0L,
       #                       requires = quote(splitrule != "random")) # nsplit is ignored and internally set to 1 for splitrule = "random"
  )
  


  task = surv.task
  mbo.ctrl = mlrMBO::makeMBOControl(save.on.disk.at = c(1,seq(5,100,5),101)
                            ,save.file.path = file.path(saving.dir, "optimization/mbo_rfsrc2.RData"))
  #mbo.ctrl = makeMBOControl(save.on.disk.at.time = 60*seq(1,10))
  mbo.ctrl = setMBOControlTermination(mbo.ctrl, iters = 100)
  surrogate.lrn = mlr::makeLearner("regr.km", predict.type = "se")
  ctrl.mbo = mlr:::makeTuneControlMBO(learner = surrogate.lrn, mbo.control = mbo.ctrl)
  ctrl.irace  = mlr::makeTuneControlIrace(maxExperiments = 200L)
  rdesc = makeResampleDesc(method = "RepCV", folds=5, reps=2)
  
  #r = resample(learner = lrn, task = surv.task, resampling = rdesc)
  
  # rdesc.10fold.1time = makeResampleDesc(method = 'CV', iters = 10)
  # resample.10fold = resample(learner = surv.lrn, task = surv.task
  #                            , resampling = rdesc.10fold.1time
  #                            , model = T
  #                            , show.info = T
  # )

  
  # start tuning:

  # irace:
  res.irace = mlr::tuneParams(lrn, surv.task, rdesc, par.set = ps, control = ctrl.irace,
                              show.info = TRUE
                              #, measures = iauc.uno
                              , measures = cindex.uno
                              )
  # #ctrl = makeTuneControlIrace(maxExperiments = 5, nbIterations = 1, minNbSurvival = 1)
  # 
  # #saving.dir = "U:/Hieu/CardiacArrestPrediction/SupervisedLearning/RData_files/glmnet_tuning"
  # saving.dir = 'U:/Hieu/CARDIA_project/CARDIA_project/rdata_files/optimization'
  #
  save(res.irace, file = paste0(saving.dir, '/irace_run.RData'))
  
  
  
  
  res.mbo = mlr::tuneParams(lrn, surv.task, rdesc, par.set = ps, control = ctrl.mbo, 
                            show.info = TRUE
                            #, measures = iauc.uno
                            , measures = cindex.uno
                            )
  
  save(res.mbo, file = paste0(saving.dir, '/res_mbo_rfsrc_expanded_var_iauc_uno.RData'))
  print(res.mbo)
  print('Run_time:')
  print((Sys.time() - start_time)/60)
  
    

#}  
  
