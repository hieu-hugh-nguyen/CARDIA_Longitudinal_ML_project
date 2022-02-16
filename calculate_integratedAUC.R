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

data_longi_long_up_to_y15 <- data_longi_long_for_analysis %>% filter(exam_year <=15)
data_longi_analysis_cohort <- data_longi_long_up_to_y15 %>% filter(ID %in% subjects_in_cohort[[1]])

# baseline data:
# data_at_baseline <- data_longi_long_for_analysis %>% filter(!duplicated(ID, fromLast=FALSE)) 
data_at_baseline <- data_longi_analysis_cohort %>% filter(ID %in% subjects_in_cohort[[1]]) %>% filter(exam_year == 0)
# most recent data at landmark time (y15):
data_y15 <- data_longi_analysis_cohort %>% filter(ID %in% subjects_in_cohort[[1]]) %>% filter(exam_year == 15)

# truncate time to make start time at y15 (to avoid 15 years of immortal time):
data_y15_truncated_tte <- data_y15 %>% 
  mutate(time_te_in_yrs = time_te_in_yrs -15) %>% 
  dplyr::select(-time) %>% filter(time_te_in_yrs >0) %>%
  dplyr::rename(event = status) %>% dplyr::rename(time = time_te_in_yrs) %>%
  dplyr::select(-exam_year)



data <- data_y15_truncated_tte

endpt <- 17; # after Year 15
eval_times <- seq(1, endpt, by = 1)





### Compute the vector of survival probabilities for each eval time: 

surv.object <-survfit(Surv(data$time,data$event)~1) # first find the estimated survival probabilities at unique failure times
pos <- prodlim::sindex(jump.times=surv.object$time,eval.times=eval_times) # step function to locate the position of survival time matching with an eval_time
St <- surv.object$surv[pos]


### Load the time-varying AUC from each model:

require(dplyr)
require(ggplot2)
require(reshape2)
require(matrixStats)


nfolds = 10

# extract mean c-index values:
loading.dir = paste0(work_dir,'/rdata_files')

eval_time_points = 17


auc_df_from_performance_testset <- function(model_name, nfolds =10){
  fold =1
  filename = paste0(work_dir, '/rdata_files/',model_name,'_fold_', fold,'/performance_testset.RData')
  auc_df <-  data.frame(fold_1 = get(load(filename))$auc)
  
  for (fold in 2:nfolds){
    filename = paste0(work_dir, '/rdata_files/',model_name,'_fold_', fold,'/performance_testset.RData')
    #  print(paste0('Fold ', fold))
    if(file.exists(filename)){
      auc_curr_fold <-as.numeric(get(load(filename))$auc)
      auc_curr_fold[is.nan(auc_curr_fold)] <- as.numeric(NA)
      if(length(auc_curr_fold)>eval_time_points){
        auc_curr_fold = auc_curr_fold[(length(auc_curr_fold)-eval_time_points+1) : length(auc_curr_fold)]
      }
      auc_df[[paste0('fold_',fold)]] <- auc_curr_fold
    }
  }
  auc_df[is.na(auc_df)] <- as.numeric(NA)
  auc_df$mean = rowMeans(auc_df, na.rm = T)
  auc_df$median = apply(auc_df[,1:(length(auc_df)-1)], 1, median, na.rm=T)
  return(auc_df)  
}

rsf_tsfresh_plus_data_y15_auc_df <- auc_df_from_performance_testset('rsf_ascvd_var_tsfeatures_plus_data_y15')

rsf_tsfresh_plus_data_y15_auc_df_rm_corr <- auc_df_from_performance_testset('rsf_ascvd_var_tsfeatures_plus_data_y15_rm_correlation')

rsf_tsfresh_auc_df_rm_corr <- auc_df_from_performance_testset('rsf_ascvd_var_tsfeatures_plus_data_y15_rm_correlation')

lasso_tsfresh_auc_df_rm_corr <-auc_df_from_performance_testset('lasso_ascvd_var_tsfeatures_rm_correlation')

cForest_tsfresh_auc_df_rm_corr <-auc_df_from_performance_testset('cForest_ascvd_var_tsfeatures_rm_correlation')

cox_y15_auc_df <- auc_df_from_performance_testset('cox_ascvd_var_y15', nfolds = 25)

rsf_y15_auc_df <- auc_df_from_performance_testset('rsf_ascvd_var_y15')

cox_y0_auc_df <- auc_df_from_performance_testset('cox_ascvd_var_baseline')

rsf_y0_auc_df <- auc_df_from_performance_testset('rsf_ascvd_var_baseline')

cox_y0_auc_df_no_truncate <- auc_df_from_performance_testset('cox_ascvd_var_baseline_no_truncate')

rsf_y0_auc_df_no_truncate <- auc_df_from_performance_testset('rsf_ascvd_var_baseline_no_truncate')

ascvd_y15_auc_df <- auc_df_from_performance_testset('ascvd_risk_score_y15')

ascvd_y0_auc_df_no_truncate <- auc_df_from_performance_testset('ascvd_risk_score_y0_no_truncate')

dynamic_deephit_untuned_auc_df_v2 <- auc_df_from_performance_testset('dynamic_deephit_ascvd_var_y15')

jmbayes_auc_df_v2 <- auc_df_from_performance_testset('jmbayes_ascvd_var_y15')





### Compute integrated AUC: ##############

get_iAUC <- function(AUC_df, nfolds = 10
                     , eval_times_for_iauc_ = eval_times_for_iauc){
  iAUC <- rep(NA, length = nfolds)
  for(fold in 1:nfolds){
    # fold = 1
    auc_curr_fold <- AUC_df[[paste0('fold_',fold)]] 
    if (!is.null(auc_curr_fold)){
      iAUC[fold] <- survAUC::IntAUC(auc_curr_fold[eval_times_for_iauc_]
                      ,times = eval_times_for_iauc_
                      ,S = St[eval_times_for_iauc_]
                      ,tmax = endpt
                      ,auc.type = 'cumulative')
    }
  }
  return(iAUC)
}




source(paste0(work_dir,'/code/git_code/snippet/bootstrap_ci.R'))
source(paste0(work_dir,'/code/git_code/snippet/print_ci_values.R'))



eval_times_for_iauc =seq(5,endpt,1)

print_ci_values(bootstrap_ci(na.omit(get_iAUC(rsf_tsfresh_auc_df_rm_corr))), digits = 3)
print_ci_values(bootstrap_ci(na.omit(get_iAUC(cForest_tsfresh_auc_df_rm_corr))), digits = 3)
print_ci_values(bootstrap_ci(na.omit(get_iAUC(lasso_tsfresh_auc_df_rm_corr))), digits = 3)
print_ci_values(bootstrap_ci(na.omit(get_iAUC(dynamic_deephit_untuned_auc_df_v2))), digits = 3)
print_ci_values(bootstrap_ci(na.omit(get_iAUC(jmbayes_auc_df_v2))), digits = 3)


print_ci_values(bootstrap_ci(na.omit(get_iAUC(rsf_y15_auc_df))), digits = 3)
print_ci_values(bootstrap_ci(na.omit(get_iAUC(cox_y15_auc_df))), digits = 3)

print_ci_values(bootstrap_ci(na.omit(get_iAUC(rsf_y0_auc_df))), digits = 3)
print_ci_values(bootstrap_ci(na.omit(get_iAUC(cox_y0_auc_df))), digits = 3)

