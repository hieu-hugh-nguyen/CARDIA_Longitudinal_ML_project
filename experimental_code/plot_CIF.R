rm(list=ls()) #Clear all
cat("\014")


# load libraries:
list.of.packages <- c('survminer', 'cmprsk', "mlbench",'ggplot2','caret', 'dplyr', 'tibble', 'ROCR','parallelMap'
                      ,'riskRegression', 'survival', 'survivalROC'
                      , 'pec', 'risksetROC')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)


work_dir = 'U:/Hieu/CARDIA_longi_project'
setwd(work_dir)



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
data_full <- data_y15 %>% 
  mutate(time_te_in_yrs = time_te_in_yrs -15) %>% 
  dplyr::select(-time) %>% filter(time_te_in_yrs >0) %>%
  rename(event = status) %>% rename(time = time_te_in_yrs) %>%
  dplyr::select(-exam_year)



ci_fit <- 
  cuminc(
    ftime = data_full$time, 
    fstatus = data_full$event, 
    #cencode = 2
  )


cif_plot <- ggcompetingrisks(
  fit = ci_fit, 
  multiple_panels = FALSE,
  xlab = "Years After Exam Y15",
  ylab = "Cumulative Incidence",
  main = NULL,
  ylim = c(0, 0.20),
  ggtheme = theme_gray(base_size = 14),
  legend = "none",
  
) + geom_line(size =1.5) 
cif_plot + theme_minimal()


# Add table:
mel_fit <- survfit(
  Surv(time, event) ~ 1, 
  data = data_full
)
num <- ggsurvplot(
  fit = mel_fit, 
  risk.table = TRUE, 
  risk.table.y.text = FALSE,
  ylab = "CIF",
  xlab = 'Years After Exam Y15',
  risk.table.fontsize = 4.5,
  tables.theme = theme_survminer(font.main = 14)
) 

cowplot::plot_grid(
  cif_plot, 
  num$table, 
  nrow = 2, 
  rel_heights = c(4, 1), 
  align = "v", 
  axis = "b"
)+theme_minimal()