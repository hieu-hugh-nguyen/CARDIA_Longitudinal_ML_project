# outer loop 

rm(list=ls()) #Clear all
cat("\014")

# set working directory: 
work_dir = 'U:/Hieu/CARDIA_longi_project'
setwd(work_dir)

# load libraries:
list.of.packages <- c("mlbench",'ggplot2','caret', 'dplyr', 'tibble', 'ROCR','parallelMap'
                      ,'riskRegression', 'survival','randomForestSRC', 'survivalROC'
                      , 'pec', 'risksetROC','JMbayes')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)

ncores <- parallel::detectCores(all.tests = FALSE, logical = TRUE)

#parallelMap::parallelStartSocket(ncores-1)
#parallelMap::parallelStartSocket(15)

# source snippet functions:
source_dir <- 'U:/Hieu/CARDIA_project/CARDIA_project/Git'
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

data_longi_long_up_to_y15 <- data_longi_long_for_analysis %>% dplyr::filter(exam_year <=15)
data_longi_analysis_cohort <- data_longi_long_up_to_y15 %>% dplyr::filter(ID %in% subjects_in_cohort[[1]])

# baseline data:
# data_at_baseline <- data_longi_long_for_analysis %>% dplyr::filter(!duplicated(ID, fromLast=FALSE)) 
data_at_baseline <- data_longi_analysis_cohort %>% dplyr::filter(ID %in% subjects_in_cohort[[1]]) %>% dplyr::filter(exam_year == 0)



# NO truncation of time since this is joint modeling:
data <- data_longi_analysis_cohort %>% 
  # mutate(time_te_in_yrs = time_te_in_yrs -15) %>% 
  dplyr::select(-time) %>% dplyr::filter(time_te_in_yrs >0) %>%
  dplyr::rename(event = status) %>% dplyr::rename(time = time_te_in_yrs)
  #dplyr::select(-exam_year)
data_at_baseline_formatted <- data_at_baseline %>% 
  # mutate(time_te_in_yrs = time_te_in_yrs -15) %>% 
  dplyr::select(-time) %>% dplyr::filter(time_te_in_yrs >0) %>%
  dplyr::rename(event = status) %>% dplyr::rename(time = time_te_in_yrs)
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
nfolds <- 10

endpt <- 17; # after Year 15
eval_times <- 15+ seq(1, endpt, by = 1)


for (fold in 1:nfolds){
 # fold = 1
  # Training and fitting model:
  trainingid <- na.omit(c(trainingid_all[,fold], validationid_all[,fold]))
  train_data <- data %>% dplyr::filter(ID %in% trainingid)
  test_data <- data %>% dplyr::filter((ID %in% testingid_all[,fold])) 
  train_id <- train_data$ID
  test_id <- test_data$ID
  #train_data$ID <- NULL
  #test_data$ID <- NULL
  
  
  model_name <- 'jmbayes_ascvd_var_y15'
  gc()
  main_dir <- paste0(work_dir, '/rdata_files')
  sub_dir <- paste0(model_name, '_fold_',fold)
  
  if(!dir.exists(file.path(main_dir, sub_dir))){
    createDir(main_dir, sub_dir)
  }

  
  
  
  
  # start_time <- Sys.time()
  # MixedModelFit <-JMbayes:: mvglmer(list(CHOL ~ exam_year + MALE + RACEBLACK + (exam_year | ID)
  #                                        , HDL ~ exam_year + MALE + RACEBLACK + (exam_year | ID)
  #                                        , SBP ~ exam_year + MALE + RACEBLACK + (exam_year | ID)
  # )
  # 
  # #hepatomegaly ~ year * sex + (year | id))
  # , data = train_data 
  # , families = list(gaussian
  #                   ,gaussian
  #                   ,gaussian))#binomial))
  # mixed_model_runtime <- Sys.time()-start_time
  # 
  # 
  # 
  # 
  # data_for_surv <- data_at_baseline %>% dplyr::filter(ID %in% train_id) %>%
  #   dplyr::select(-time) %>% dplyr::filter(time_te_in_yrs >0) %>%
  #   dplyr::rename(event = status) %>% dplyr::rename(time = time_te_in_yrs)
  # CoxFit <- coxph(Surv(time, event) ~ AGE_Y0+MALE+RACEBLACK+DIAB+HBM+SMKNW ## +CHOL+HDL+SBP # don't need longitudinal vars in Cox formula
  #                 , data = data_for_surv
  #                 #dplyr::select(-exam_year)
  #                 , model = TRUE)
  # 
  # 
  # 
  # 
  # start_time <- Sys.time()
  # JMFit <- JMbayes::mvJointModelBayes(MixedModelFit, CoxFit, timeVar = "exam_year")
  # jm_model_runtime <- Sys.time()-start_time
  # 
  # print(paste0('Model training elapsed time =',jm_model_runtime))
  # 
  # 
  # 
  # 
  # 
  # model<- JMFit
  # 
  # #set.seed(seed)
  # saving_dir <- file.path(main_dir, sub_dir)
  # save(model, file = paste0(saving_dir,'/', model_name, '.RData'))
  
  
  
  # Test set performance: ###################################################################
  loading.dir <- paste0(work_dir, '/rdata_files/', model_name, '_fold_', fold)
  saving.dir <- loading.dir
  trained_data <- train_data
  # trained_model <- model
  trained_model <- get(load(paste0(saving.dir,'/', model_name, '.RData')))
  
 tryCatch({
    
    
    # roc <- rocJM(model, newdata = test_data, Tstart = 15, Dt = 1, idVar = 'ID')
    # 
    # plot(roc)
    
    # auc_over_time <- rep(NA, base::length(eval_times))
    # 
    # for(eval_time in 1: base::length(eval_times)){
    #   auc <- aucJM(model, newdata = test_data, Tstart = 15, Thoriz = eval_times[eval_time], idVar = 'ID')
    #   auc_over_time[eval_time] = auc$auc
    #   print(auc_over_time)
    # }
    # # auc_over_time
    # save(auc_over_time
    #      , file = paste0(saving.dir, '/auc_over_time_testset.RData'))
    
    
    
    
    
    
    # probability of having had the disease:
    
    predictRisk.JMbayes <- function(trained_model
                                   , newdata = test_data
                                   , times = eval_times){
      
      prob_surv_test_sprobs <- JMbayes::survfitJM(trained_model, test_data, idVar = 'ID'
                                                  ,survTimes = eval_times)
      
      
      # convert to the prob_risk format: row = ID, col = prob at each eval time
      for (i in 1:base::length(prob_surv_test_sprobs$summaries)){
        #i = 3
        participant_id <- names(prob_surv_test_sprobs$summaries)[i]
        df_pred_oi <- prob_surv_test_sprobs$summaries[[i]] %>% as_tibble() %>% dplyr::filter(times %in% eval_times) %>%
          dplyr::select(c('times', 'Median'))
        names(df_pred_oi)[2] = participant_id
        
        if (i == 1){
          prob_surv_test_df <- df_pred_oi
        }
        else{
          prob_surv_test_df <- prob_surv_test_df %>% dplyr::full_join(df_pred_oi, by = 'times')
        }
        
        
      }

      prob_surv_test <- prob_surv_test_df %>% t()
      colnames(prob_surv_test) <- prob_surv_test[1,]
      prob_surv_test <- prob_surv_test[2: base::nrow(prob_surv_test),]
      prob_risk_test <- 1-prob_surv_test
      prob_risk_test <- cbind(test_id = rownames(prob_risk_test), as_tibble(prob_risk_test))

      return(prob_risk_test)
    }
      
    
    
    
    
    prob_risk_test_with_ID <- predictRisk.JMbayes(trained_model, newdata = test_data, times = eval_times)
    save(prob_risk_test_with_ID
         , file = paste0(saving.dir, '/prob_risk_test_set_with_ID.RData'))
    
    
#    prob_risk_test_with_ID <- prob_risk_test
    prob_risk_test <- prob_risk_test_with_ID %>% dplyr::select(-test_id) %>% as.matrix()

    prob_risk_test[is.na(prob_risk_test)] = 0
    performance_testset = eval_performance2(prob.risk.test.set = prob_risk_test
                                            , test.data = test_data %>% dplyr::filter(!duplicated(ID))
                                            , trained.data = trained_data %>% dplyr::filter(!duplicated(ID))
                                            , eval.times = eval_times
                                            )
    save(performance_testset
         , file = paste0(saving.dir, '/performance_testset.RData'))
    


    # prob_risk_train_with_ID <- predictRisk.JMbayes(JMFit, newdata = trained_data, times = eval_times)
    # save(prob_risk_train_with_ID
    #      , file = paste0(saving.dir, '/prob_risk_train_set_with_ID.RData'))
    

    
    
 }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  
  
}

