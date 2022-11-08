
# Nested Random Forest model:

#==========================================================================
# Load data and library
#==========================================================================
rm(list = ls())
cat("\014")
work_dir = 'U:/Hieu/CARDIA_longi_project'
setwd(work_dir);


# load library
library(randomForestSRC)
library(pec)
library(riskRegression)
library(survival)
library(beepr)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(glmnet)
library(MASS)
library(doParallel)

ncores <- parallel::detectCores(all.tests = FALSE, logical = TRUE)

parallelMap::parallelStartSocket(ncores-1)


source_dir <- paste0(work_dir, '/code/git_code/snippet')
source(paste0(source_dir, '/running_rsf.R'))
source(paste0(source_dir, '/running_algo.R'))
source(paste0(source_dir, '/createDir.R'))
source(paste0(source_dir, '/subsetDataTopnVar.R'))
source(paste0(source_dir, '/classif_task.R'))
source(paste0(source_dir, '/predictSurvProb.R'))
source(paste0(source_dir, '/eval_performance.R'))
source(paste0(source_dir, '/eval_performance_using_different_auc_package.R'))
source(paste0(source_dir,'/normalize_var_importance.R'))


# load extracted timeseries features:
loading_dir = paste0(work_dir, '/csv_files')
# ts_features <- read.csv(paste0(work_dir,'/csv_files/tsfresh_features_extract_relevant_drop_na_rm_corr_expanded_var.csv'))
ts_features <- read.csv(paste0(work_dir,'/csv_files/tsfresh_features_extract_relevant_drop_na_rm_corr_expanded_var_2.csv'))
ts_features <- read.csv(paste0(work_dir,'/csv_files/tsfresh_features_extract_relevant_drop_na_2.csv'))

# ts_features <- read.csv(paste0(work_dir,'/csv_files/tsfresh_features_drop_nonunique_dropna_rm_corr_expanded_var.csv'))
# ts_features <- read.csv(paste0(work_dir,'/csv_files/tsfresh_features_extract_relevant_drop_na_expanded_var.csv'))
# ts_features <- read.csv(paste0(work_dir,'/csv_files/tsfresh_features_drop_nonunique_dropna_drop_correlated_training_rm_nonrelevant.csv'))
# ts_features <- read.csv(paste0(work_dir,'/csv_files/tsfresh_features_drop_nonunique_drop_na_drop_correlated_expanded_var_crunhcr.csv'))

ts_features <- ts_features %>% dplyr::rename(ID = X)


# load the dataset
loading_dir = paste0(work_dir, '/csv_files')

data_longi_long_for_analysis <- read.csv(paste0(work_dir,'/csv_files/data_longi_long_format_expanded_variables_removed_missing_data.csv'))
data_longi_long_for_analysis$ID <- data_longi_long_for_analysis$ID %>% as.character()

#'/csv_files/data_longi_long_format_ascvd_risk_factors_with_missing_data.csv'
#
subjects_in_cohort <- read.csv(paste0(work_dir,'/csv_files/subjects_in_final_analysis_cohort.csv'))

# data_longi_expanded_var_for_dynamic_deephit <- read.csv(paste0(work_dir, '/csv_files/data_longi_expanded_var_for_dynamic_deephit.csv'))
# data_longi_expanded_var_for_dynamic_deephit$ID <- data_longi_expanded_var_for_dynamic_deephit$ID %>% as.character()
# Does not have this ID: 204337013307





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





# load training IDs:
trainingid_all <- read.csv(paste0(work_dir,'/csv_files/all_training_set_ID.csv'))
validationid_all <- read.csv(paste0(work_dir,'/csv_files/all_validation_set_ID.csv'))
testingid_all <- read.csv(paste0(work_dir,'/csv_files/all_testing_set_ID.csv'))




### START BUILDING MODELS: #################################################################




seed <- 4495
set.seed(seed)
nfolds <- 10

endpt <- 17; # after Year 15
eval_times <- seq(1, endpt, by = 1)


# merge y15 data with ts derived features:

data$ID <- data$ID %>% as.character()
ts_features$ID <- as.character(ts_features$ID)

data_tsfeatures <- data %>% dplyr::select('ID','event','time','AGE_Y15','MALE','RACEBLACK') %>%
  inner_join(ts_features, by = 'ID')

# data_tsfeatures_disjoint <- data %>% dplyr::select('ID','event','time','AGE_Y15','MALE','RACEBLACK') %>%
#   anti_join(ts_features, by = 'ID')
# data_tsfeatures_disjoint %>% nrow()

# check if nrow of data is the same as cohort population


data_tsfeatures_plus_most_recent_by_y15 <- data_y15_truncated_tte %>% 
  mutate(AGE_Y15 = AGE_Y0 +15) %>% dplyr::select(-AGE_Y0) %>% 
  left_join(ts_features, by = 'ID') %>%
  dplyr::select('ID','event','time',everything())

data <- data_tsfeatures_plus_most_recent_by_y15[complete.cases(data_tsfeatures_plus_most_recent_by_y15),]
# check nrow(data)





# Load the var ranking:
loading_dir = paste0(work_dir,'/rdata_files/rsf_expanded_var_tsfeatures_plus_data_y15_corr_rm_fold_1')
var_order_df = read.csv(file = paste0(loading_dir,
                                                 '/depth_rank.csv'))

var_order = apply(var_order_df, 2, as.character)[,1]



## START THE NESTED RSF PROCESS: ###############################################

var.order = var_order

fold =1 
# trainingid <- na.omit(c(trainingid_all[,fold], validationid_all[,fold]))

trainingid <- na.omit(trainingid_all[,fold])

train_data <- data %>% dplyr::filter(ID %in% trainingid)
val_data <- data %>% dplyr::filter((ID %in% validationid_all[,fold])) 
test_data <- data %>% dplyr::filter((ID %in% testingid_all[,fold])) 

train_id <- train_data$ID
val_id <- val_data$ID
test_id <- test_data$ID

train_data$ID <- NULL
val_data$ID <- NULL
test_data$ID <- NULL


  

seed = 4995
set.seed(seed)

all.events=c("CVD")


  

# Function to calculate prediction error and c-index at end time of RSF for a specific number of variables: 

RSF.performance = function(numVar){
  print(paste('number of variables  =', numVar))
  
  n.top = numVar
  train_data_top_var = train_data[, c('time', 'event',
                             var.order[1:n.top])]
  
  # RF objects
  set.seed(seed)
  rf.obj = rfsrc(Surv(time, event)~.,
                 data = train_data_top_var,
                 ntree = 1001,
                 splitrule = 'logrank')
  
  # External
  
  # Prediction error and c-index at end time
  rf.pe = pec::pec(list(rf.obj),
                   formula = Hist(time, event)~1,
                   data = val_data,
                   times = min(endpt, 
                               max(val_data$time)), 
                   exact = T,
                   splitMethod = 'none',
                   cens.model = 'marginal')
  
  
  rf.c = cindex(list(rf.obj),
                formula = Hist(time, event)~1,
                data = val_data,
                eval.times = min(endpt, 
                                 max(val_data$time)),
                splitMethod = 'none',
                cens.model = 'marginal')
  
  pe.external = rf.pe$AppErr$rfsrc[
    which(rf.pe$time == min(endpt, 
                            max(val_data$time)))][1]
  c.external = rf.c$AppCindex
  
  return(c(n.top, 
           pe.external,
           c.external))
}



ptm = proc.time()
numVariables = length(var.order)


pe.c = lapply(c(1:19, seq(20, 100, by = 5), seq(110, numVariables, by = 10), numVariables),
       RSF.performance)
  #lapply(seq(1,2,1), 
   #                  RSF.performance)

              # Could use mclapply if not using Windows
                       
pe.c = data.frame(do.call(rbind, pe.c))

names(pe.c) = c('n.top', 'pe.external', 'c.external')

saving.dir = paste0(work_dir,'/rdata_files')
save(pe.c, file = paste(saving.dir, '/PEC_nestedRF_tsfresh_features_extract_relevant_drop_na_2.Rdata', sep = ''))

print(proc.time() - ptm)




