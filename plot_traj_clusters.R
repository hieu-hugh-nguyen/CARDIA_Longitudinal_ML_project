rm(list=ls()) #Clear all
cat("\014")

work_dir = 'U:/Hieu/CARDIA_longi_project'

setwd(work_dir)

list.of.packages <- c('dplyr', 'ggplot2', 'dplyr', 'tibble')

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)


source_dir <- paste0(work_dir, '/code/git_code/snippet')
source(paste0(source_dir, '/createDir.R'))
source(paste0(source_dir, '/subsetDataTopnVar.R'))


# load the dataset
loading_dir = paste0(work_dir, '/csv_files')

# cluster_all_df <- read.csv(paste0(work_dir, '/csv_files/proc_traj_cluster_assignments_gap_3.csv'))
cluster_all_df <- read.csv(paste0(work_dir, '/csv_files/proc_traj_cluster_assignments_gap.csv'))
cluster_all_df <- cluster_all_df %>% dplyr::select(-one_of(c('event', 'time', 'RACEBLACK', 'MALE', 'AGE_Y0'))) %>% 
  dplyr::filter(!duplicated(ID, fromLast=TRUE))


cluster_assign_freq_table <- apply(cluster_all_df %>% dplyr::select(-one_of(c('event', 'time', 'RACEBLACK', 'MALE', 'AGE_Y0'))) %>% 
                                     dplyr::filter(!duplicated(ID, fromLast=TRUE))
                                   , 2, function(x) length(unique(x)))

longi_data <- read.csv(paste0(work_dir,'/csv_files/data_longi_expanded_var_for_dynamic_deephit_and_ts_extraction_2.csv'))





### Plot traj for each variable: ############################################
var_with_clusters <- names(cluster_all_df)[names(cluster_all_df) != 'ID']

var_names <- sapply(var_with_clusters, function(x) {gsub("\\_cluster","", x)})

color_scheme <- c(
  "black","firebrick3",  "dodgerblue3",  "forestgreen", "darkmagenta", "hotpink3", "steelblue4", "mediumpurple3","thistle4",
  "indianred4", "yellow4", "mediumseagreen", "goldenrod3"
)





plot_traj <- function(var_oi = 'SBP'){
  longi_data_with_assignment <- longi_data %>% dplyr::select(one_of(c('ID', 'exam_year', var_oi))) %>%
    inner_join(cluster_all_df %>% dplyr::select(one_of(c('ID', paste0(var_oi, '_cluster')))), by = 'ID')
  
  longi_data_median <- longi_data_with_assignment %>% 
    group_by(exam_year, get(paste0(var_oi, '_cluster'))) %>% 
    summarise(
      median_ = median(get(paste0(var_oi)))
      , mean_ = mean(get(paste0(var_oi)))
      , ql = quantile(get(paste0(var_oi)), 0.25)
      , qh = quantile(get(paste0(var_oi)), 0.75)
      , sd = sd(get(paste0(var_oi)))
      
    ) 
  # %>% 
  #   dplyr::mutate(lowerbound = median_-sd) %>%
  #   dplyr::mutate(upperbound = median_+sd)
  
  names(longi_data_median)[2] <- 'cluster_assignment'
  longi_data_median$cluster_assignment <- as.factor(longi_data_median$cluster_assignment)
  
  
  cluster.plot = ggplot(data = longi_data_median)
  
  plot_final <- cluster.plot +
    # geom_line(aes(exam_year, SBP, color = cluster_assignment, group = ID), size = 1, alpha = 0.1)+
    geom_line(data = longi_data_median, aes(exam_year, median_, color = cluster_assignment, group = cluster_assignment), alpha = 1, size = 1.2) +
    geom_point(data = longi_data_median, aes( exam_year, median_, color = cluster_assignment, group = cluster_assignment, shape=cluster_assignment), size = 2, alpha = 1) +
    
    geom_ribbon(data = longi_data_median, aes(x= exam_year, y = median_, ymin = ql, ymax = qh, fill = cluster_assignment, color = cluster_assignment, group = cluster_assignment), alpha = 0.2, colour = NA) +
    
    # geom_line(data = longi_data_median, aes(exam_year, ql, color = cluster_assignment, group = cluster_assignment), alpha = 1, size = 1, linetype = 'dashed') +
    #   geom_line(data = longi_data_median, aes(exam_year, qh, color = cluster_assignment, group = cluster_assignment), alpha = 1, size = 1, linetype = 'dashed') +
    
    # geom_smooth(data = longi_data_median, aes( exam_year, median_, color = cluster_assignment, group = cluster_assignment), size = 1.3, se = FALSE, method = 'loess', span = 1)+ #usual lowess span = 1
    # geom_smooth(data = longi_data_median, aes( exam_year, median_, color = cluster_assignment, group = cluster_assignment), size = 1.3, se = FALSE, method = lm, formula = y ~ x, alpha = 0.2)+
    
    # geom_smooth(aes( time, quant, color = Model, group = Model), size = 1.3, se = FALSE, method = lm, formula = y ~ splines::bs(x, 1))+
    scale_color_manual(values = color_scheme[1:length(unique(longi_data_median$cluster_assignment))]) +
    scale_fill_manual(values = color_scheme[1:length(unique(longi_data_median$cluster_assignment))]) +
    xlab("Exam Year") +
    # ylab(var_oi) +
    ggtitle(var_oi) + 
    # scale_x_continuous(breaks=seq(5,17,2))+
    # ylim(min(longi_data_median$median_)*0.9, max(longi_data_median$median_)*1.1) + 
    theme_minimal() + 
    theme(axis.text=element_text(size=15),
          plot.title = element_text(hjust = 0.5),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.title=element_text(size=15,face="italic"),
          # legend.title = element_text(color = "black", size = 18),
          # legend.text = element_text(color = "black", size = 18),
          legend.position = "none") 
  # geom_vline(xintercept=10)
  
  return(plot_final)
}





plot_traj <- function(var_oi = 'SBP'){
  longi_data_with_assignment <- longi_data %>% dplyr::select(one_of(c('ID', 'exam_year', var_oi))) %>%
    inner_join(cluster_all_df %>% dplyr::select(one_of(c('ID', paste0(var_oi, '_cluster')))), by = 'ID')
  
  longi_data_median <- longi_data_with_assignment %>% 
    group_by(exam_year, get(paste0(var_oi, '_cluster'))) %>% 
    summarise(
      median_ = median(get(paste0(var_oi)))
      , mean_ = mean(get(paste0(var_oi)))
      , ql = quantile(get(paste0(var_oi)), 0.25)
      , qh = quantile(get(paste0(var_oi)), 0.75)
      , sd = sd(get(paste0(var_oi)))
      
    ) 
  # %>% 
  #   dplyr::mutate(lowerbound = median_-sd) %>%
  #   dplyr::mutate(upperbound = median_+sd)
  
  names(longi_data_median)[2] <- 'cluster_assignment'
  longi_data_median$cluster_assignment <- as.factor(longi_data_median$cluster_assignment)
  
  
  cluster.plot = ggplot(data = longi_data_median)
  
  plot_final <- cluster.plot +
    # geom_line(aes(exam_year, SBP, color = cluster_assignment, group = ID), size = 1, alpha = 0.1)+
    geom_line(data = longi_data_median, aes(exam_year, mean_, color = cluster_assignment, group = cluster_assignment), alpha = 1, size = 1.2) +
    geom_point(data = longi_data_median, aes( exam_year, mean_, color = cluster_assignment, group = cluster_assignment, shape=cluster_assignment), size = 2, alpha = 1) +
    
    geom_ribbon(data = longi_data_median, aes(x= exam_year, y = mean_, ymin = ql, ymax = qh, fill = cluster_assignment, color = cluster_assignment, group = cluster_assignment), alpha = 0.2, colour = NA) +
    
    # geom_line(data = longi_data_median, aes(exam_year, ql, color = cluster_assignment, group = cluster_assignment), alpha = 1, size = 1, linetype = 'dashed') +
    #   geom_line(data = longi_data_median, aes(exam_year, qh, color = cluster_assignment, group = cluster_assignment), alpha = 1, size = 1, linetype = 'dashed') +
    
    # geom_smooth(data = longi_data_median, aes( exam_year, mean_, color = cluster_assignment, group = cluster_assignment), size = 1.3, se = FALSE, method = 'loess', span = 1)+ #usual lowess span = 1
    # geom_smooth(data = longi_data_median, aes( exam_year, mean_, color = cluster_assignment, group = cluster_assignment), size = 1.3, se = FALSE, method = lm, formula = y ~ x, alpha = 0.2)+
    
    # geom_smooth(aes( time, quant, color = Model, group = Model), size = 1.3, se = FALSE, method = lm, formula = y ~ splines::bs(x, 1))+
    scale_color_manual(values = color_scheme[1:length(unique(longi_data_median$cluster_assignment))]) +
    scale_fill_manual(values = color_scheme[1:length(unique(longi_data_median$cluster_assignment))]) +
    xlab("Exam Year") +
    # ylab(var_oi) +
    ggtitle(var_oi) + 
    # scale_x_continuous(breaks=seq(5,17,2))+
    # ylim(min(longi_data_median$mean_)*0.9, max(longi_data_median$mean_)*1.1) + 
    theme_minimal() + 
    theme(axis.text=element_text(size=15),
          plot.title = element_text(hjust = 0.5),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.title=element_text(size=15,face="italic"),
          # legend.title = element_text(color = "black", size = 18),
          # legend.text = element_text(color = "black", size = 18),
          legend.position = "none") 
  # geom_vline(xintercept=10)
  
  return(plot_final)
}





library(patchwork)

ranked_var_list <- c('DBP', 'PULSE', 'LDL', 'WST', 'SBP', 'DFPAY', 'HDL', 'GLU', 'BMI', 'NTRIG', 'WGT', 'ED', 'PSTYR', 'CHOL')


# DBP_traj <- plot_traj(var_oi = 'DBP')  
# DBP_traj <- plot_traj(var_oi = 'DBP') 
# 
# SBP_traj <- plot_traj(var_oi = 'SBP') 
# BMI_traj <- plot_traj(var_oi = 'BMI') 
# WST_traj <- plot_traj(var_oi = 'WST') 


plot_all_obj <- plot_traj(ranked_var_list[1])
for (var_idx in 2:length(ranked_var_list)){
  plot_all_obj <- plot_all_obj +
    plot_traj(ranked_var_list[var_idx]) 
    
}
  
plot_all_obj 

