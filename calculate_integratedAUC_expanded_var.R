# outer loop 

rm(list=ls()) #Clear all
cat("\014")

# set working directory: 
work_dir = 'U:/Hieu/CARDIA_longi_project'
setwd(work_dir)


### Load the time-varying AUC from each model:

require(dplyr)
require(ggplot2)
require(reshape2)
require(matrixStats)

source(paste0(work_dir,'/code/git_code/snippet/bootstrap_ci.R'))
source(paste0(work_dir,'/code/git_code/snippet/print_ci_values.R'))


nfolds = 10

eval_time_points = 17


auc_df_from_performance_testset <- function(model_name, nfolds =10){
  

  for (fold in 1:nfolds){
    
    filename = paste0(work_dir, '/rdata_files/',model_name,'_fold_', fold,'/performance_testset.RData')
    #  print(paste0('Fold ', fold))
    
    if(file.exists(filename)){
      if (fold ==1){
        auc_df <-  data.frame(fold_1 = get(load(filename))$auc)
      }
      else{
        auc_curr_fold <-as.numeric(get(load(filename))$auc)
        auc_curr_fold[is.nan(auc_curr_fold)] <- as.numeric(NA)
        if(length(auc_curr_fold)>eval_time_points){
          auc_curr_fold = auc_curr_fold[(length(auc_curr_fold)-eval_time_points+1) : length(auc_curr_fold)]
        }
        auc_df[[paste0('fold_',fold)]] <- auc_curr_fold
      }
    }
  }
  auc_df[is.na(auc_df)] <- as.numeric(NA)
  auc_df$mean = rowMeans(auc_df, na.rm = T)
  auc_df$median = apply(auc_df[,1:(length(auc_df)-1)], 1, median, na.rm=T)
  return(auc_df)  
}



### Compute integrated AUC: ##############

get_iAUC <- function(AUC_df, St_, eval_times_for_iauc_, nfolds = 10
){
  iAUC <- rep(NA, length = nfolds)
  for(fold in 1:nfolds){
    # fold = 1
    auc_curr_fold <- AUC_df[[paste0('fold_',fold)]] 
    if (!is.null(auc_curr_fold)){
      iAUC[fold] <- survAUC::IntAUC(auc_curr_fold[eval_times_for_iauc_]
                                    ,times = eval_times_for_iauc_
                                    ,S = St_[eval_times_for_iauc_]
                                    ,tmax = endpt
                                    ,auc.type = 'cumulative')
    }
  }
  return(iAUC)
}





### Compute St, the vector of survival probabilities for each eval time: 

loading_dir = paste0(work_dir, '/csv_files')

data_longi_long_for_analysis <- read.csv(paste0(work_dir,'/csv_files/data_longi_long_format_expanded_variables_removed_missing_data.csv'))
#'/csv_files/data_longi_long_format_ascvd_risk_factors_with_missing_data.csv'
#
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




endpt <- 17; # after Year 15
eval_times <- seq(1, endpt, by = 1)

surv.object <-survfit(Surv(data$time,data$event)~1) # first find the estimated survival probabilities at unique failure times
pos <- prodlim::sindex(jump.times=surv.object$time,eval.times=eval_times) # step function to locate the position of survival time matching with an eval_time
St <- surv.object$surv[pos]








eval_times_for_iauc =seq(5,endpt,1)


#model_name = 'rsf_ascvd_var_tsfeatures_plus_data_y15_rm_correlation'

model_list = c(
  ## model_name = 
  'rsf_expanded_var_and_ascvd_var_tsfeatures_plus_data_y15'
  # 'rsf_expanded_var_tsfeatures_plus_data_y15'
#  'dynamic_deephit_expanded_var_y15'
#  'dynamic_deephit_expanded_var_y15_4_RNN_layers'
  #  'rsf_expanded_var_tsfeatures_corr_rm'
  # 'lasso_expanded_var_tsfeatures_plus_data_y15_rm_correlation'
  # , 'cForest_expanded_var_tsfeatures_plus_data_y15_rm_correlation'
# 'rsf_expanded_var_tsfeatures_plus_data_y15_corr_rm'
#  'rsf_expanded_var_tsfeatures_plus_data_y15_crunchr'
# 'rsf_expanded_var_tsfeatures_plus_data_y15_40_features'
#  'rsf_expanded_var_tsfeatures_plus_data_y15'
 # 'rsf_expanded_var_tsfeatures_plus_data_y15_rm_correlation'
  
  
  #   'rsf_expanded_var_y15' 
   # , 'cForest_expanded_var_y15'
   # , 'cox_expanded_var_y15'
  # , 'lasso_expanded_var_y15'
  
  # 'rsf_expanded_var_baseline_no_truncate'
  # , 'cox_expanded_var_baseline_no_truncate'
  # , 'lasso_expanded_var_baseline_no_truncate'
)

for(n in 1:length(model_list)){
  print(paste0('IntAUC for model ', model_list[n], ':'))
  print(print_ci_values(bootstrap_ci(na.omit(get_iAUC(auc_df_from_performance_testset(model_list[n])
                                                , St_ = St, nfolds = 10
                                                , eval_times_for_iauc_ = eval_times_for_iauc))), digits = 3))
  cat('\n')
}




