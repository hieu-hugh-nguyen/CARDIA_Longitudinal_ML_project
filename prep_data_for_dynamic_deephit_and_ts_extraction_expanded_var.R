# outer loop 

rm(list=ls()) #Clear all
cat("\014")

# set working directory: 
work_dir = 'U:/Hieu/CARDIA_longi_project'
setwd(work_dir)

# load libraries:
list.of.packages <- c('dplyr', 'tibble')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)




# load the dataset
loading_dir = paste0(work_dir, '/csv_files')

data_longi_long_for_analysis <- read.csv(paste0(work_dir,'/csv_files/data_longi_long_format_expanded_variables_removed_missing_data_2.csv'))

data_longi_long_for_analysis_all <- read.csv(paste0(work_dir, '/csv_files/data_longi_long_expanded_variables_final_all_subjects_with_missing_data_up_to_y15_rm_hratk.csv'))

#
subjects_in_cohort <- read.csv(paste0(work_dir,'/csv_files/subjects_in_final_analysis_cohort.csv'))

data_longi_long_up_to_y15 <- data_longi_long_for_analysis %>% filter(exam_year <=15)
data_longi_analysis_cohort <- data_longi_long_up_to_y15 %>% filter(ID %in% subjects_in_cohort[[1]])

data_longi_analysis_cohort$ID <- data_longi_analysis_cohort$ID %>% as.character()

data <- data_longi_analysis_cohort %>% 
  dplyr::select(-time) %>% dplyr::filter(time_te_in_yrs >0) %>%
  dplyr::rename(event = status) %>% dplyr::rename(time = time_te_in_yrs)

write.csv(data, file= paste0(work_dir,'/csv_files/data_longi_expanded_var_for_dynamic_deephit_and_ts_extraction_2.csv'), row.names = F)