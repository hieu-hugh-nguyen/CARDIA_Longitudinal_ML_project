# Get subjectid those who showed up to each exam:


rm(list=ls()) #Clear all
cat("\014")


# Input Exam Year:
#exam_year = 'Y0'

work_dir <- 'U:/Hieu/CARDIA_longi_project'
# exam_year = 'Y5'
# 
# 
# if (exam_year == 'Y0'){
#   work_dir <- 'U:/CARDIA Other/CARDIACCdata/Y0/Y0/DATA'
# }
# if (exam_year == 'Y2'){
#   work_dir <- 'U:/CARDIA Other/CARDIACCdata/Y2/Y2/DATA'
# }
# if (exam_year == 'Y5'){
#   work_dir <- 'U:/CARDIA Other/CARDIACCdata/Y5/DATA'
# }
# if (exam_year == 'Y7'){
#   work_dir <- 'U:/CARDIA Other/CARDIACCdata/Y7/Y7/DATA'
# }
# if (exam_year == 'Y10'){
#   work_dir <- 'U:/CARDIA Other/CARDIACCdata/Y10/Y10/DATA/SAS'
# }
# if (exam_year == 'Y15'){
#   work_dir <- 'U:/CARDIA Other/CARDIACCdata/Y15/Y15/DATA'
# }
# if (exam_year == 'Y20'){
#   work_dir <- 'U:/CARDIA Other/CARDIACCdata/Y20/Y20/CORE/DATA'
# }
# if (exam_year == 'Y25'){
#   work_dir <- 'U:/CARDIA Other/CARDIACCdata/Y25 7.26.17/DATA'
# }
# if (exam_year == 'Y30'){
#   work_dir <- 'U:/CARDIA Other/CARDIACCdata/Y30/Y30data_v13'
# }

setwd(work_dir)




##### Load libraries:##################################################################
list.of.packages <- c('haven', 'tibble','Hmisc','labelled','DataCombine', 'dplyr')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)






exam_years <- c(0, 2, 5, 7, 10, 15, 20, 25, 30)
exam_age_var <-c('EXAMAGE', 'EX2_AGE','EX3_AGE','EX4_AGE','EX5_AGE','EX6_AGE','EX7_AGE','EX8_AGE','EX9_AGE')

for (i in 1:length(exam_years)){
  
  featurespace <-read.csv(paste0(work_dir,'/csv_files/Y', exam_years[i], '/Y', exam_years[i],'_unimputed_featurespace.csv'))
  cohort_id_oi <- data.frame('ID'= featurespace[!is.na(featurespace[[exam_age_var[i]]]),'ID'])
  print(nrow(cohort_id_oi))
  write.csv(cohort_id_oi
           ,file = paste0(work_dir,'/csv_files','/subjects_showed_up_to_Y', exam_years[i],'.csv'), row.names = F)
  
}
