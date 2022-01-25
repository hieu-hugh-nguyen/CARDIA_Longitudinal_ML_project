rm(list=ls()) #Clear all
cat("\014")

# set working directory: 
work_dir = 'U:/Hieu/CARDIA_longi_project'
setwd(work_dir)

# load libraries:
list.of.packages <- c("mlbench",'ggplot2','caret', 'dplyr', 'tibble', 'parallelMap', 'boot','table1')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)

ncores <- parallel::detectCores(all.tests = FALSE, logical = TRUE)

parallelMap::parallelStartSocket(ncores-1)

## source snippet functions:
# source_dir <- 'U:/Hieu/CARDIA_project/CARDIA_project/Git'
# source(paste0(source_dir,'/code/snippet/running_rsf.R'))
# source(paste0(source_dir,'/code/snippet/running_algo.R'))
# source(paste0(source_dir,'/code/snippet/createDir.R'))
# source(paste0(source_dir,'/code/snippet/subsetDataTopnVar.R'))
# source(paste0(source_dir,'/code/snippet/classif_task.R'))
# source(paste0(source_dir,'/code/snippet/predictSurvProb.R'))
# source(paste0(source_dir,'/code/snippet/eval_performance.R'))
# source(paste0(source_dir,'/code/snippet/eval_performance_using_different_auc_package.R'))




# load the dataset
loading_dir = paste0(work_dir, '/csv_files')

data_longi_long_for_analysis <- read.csv(paste0(work_dir,'/csv_files/data_longi_long_format_ascvd_risk_factors.csv'))


#exclude instances with events or censored before exam year 15 
data_longi_long_y15 <- data_longi_long_for_analysis %>% filter(time_te_in_yrs >15) 

# only include longitudinal data from y0 to y15:
data_longi_long_up_to_y15 <- data_longi_long_y15 %>% filter(exam_year <=15)

# baseline data:
data_at_baseline <- data_longi_long_up_to_y15 %>% filter(!duplicated(ID, fromLast=FALSE)) 

# most recent data at landmark time (y15):
data_most_recent_by_y15 <- data_longi_long_up_to_y15 %>% filter(!duplicated(ID, fromLast=TRUE))


data_at_baseline_full <- data_longi_long_for_analysis %>% filter(!duplicated(ID, fromLast=FALSE)) 


table1(~factor(MALE)+factor(RACEBLACK)+., data = data_at_baseline_full)

#' #'C08DIAB' 'HBP05'
#' var = 'C08KIDNY'
#' table(data_y10[[var]])

