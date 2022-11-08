# glucose:


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



longi_var_df <- long_data_ts %>% dplyr::select(c('ID', 'exam_year', longi_var))
longi_var_df$exam_year = longi_var_df$exam_year %>% as.numeric()

longi_var_df_wide <- stats::reshape(longi_var_df, idvar = 'ID', timevar = 'exam_year', direction = 'wide')


longi_var <- 'GLU'

longi_var_df_wide %>% dim()

var_oi <- 'GLU'
longi_data_mean <- longi_var_df %>% 
  dplyr::group_by(exam_year) %>% 
  dplyr::summarise(
    median_ = median(get(paste0(var_oi)))
    , mean_ = mean(get(paste0(var_oi)))
    , ql = quantile(get(paste0(var_oi)), 0.025)
    , qh = quantile(get(paste0(var_oi)), 0.975)
    , sd = sd(get(paste0(var_oi)))
    
  ) 

cluster.plot = ggplot(data = longi_data_mean)

plot_final <- cluster.plot +
  geom_line(aes(exam_year, GLU, color = 'lightgrey', group = ID), size = 1, alpha = 0.1)+
  
  plot_final


plot_final <- cluster.plot +
  # geom_line(aes(exam_year, SBP, color = cluster_assignment, group = ID), size = 1, alpha = 0.1)+
  geom_line(data = longi_data_mean, aes(exam_year, mean_), alpha = 1) +
  geom_point(data = longi_data_mean, aes( exam_year, mean_), alpha = 1) +
  geom_ribbon(data = longi_data_mean, aes(x= exam_year, y = mean_, ymin = ql, ymax = qh), alpha = 0.2, colour = NA) +
  xlab('Exam Year')+
  ylab('GLU')
  
  # geom_line(data = longi_data_mean, aes(exam_year, ql, color = cluster_assignment, group = cluster_assignment), alpha = 1, size = 1, linetype = 'dashed') +
  #   geom_line(data = longi_data_mean, aes(exam_year, qh, color = cluster_assignment, group = cluster_assignment), alpha = 1, size = 1, linetype = 'dashed') +
  
  # geom_smooth(data = longi_data_mean, aes( exam_year, mean_, color = cluster_assignment, group = cluster_assignment), size = 1.3, se = FALSE, method = 'loess', span = 3)+ #usual lowess span = 1
  # geom_smooth(data = longi_data_mean, aes( exam_year, mean_, color = cluster_assignment, group = cluster_assignment), size = 1.3, se = FALSE, method = lm, formula = y ~ x, alpha = 0.2)+
  
  
plot_final
