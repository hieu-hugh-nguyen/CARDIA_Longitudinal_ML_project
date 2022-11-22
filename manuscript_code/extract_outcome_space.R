rm(list=ls()) #Clear all
cat("\014")

require(haven)
require(dplyr)
# library(Hmisc)
# library('foreign')


#Read SAS outcome files:
loading.dir <- "U:/CARDIA Other/CARDIACCdata/M & M/MM20/data";
#outcome_data = haven::read_sas(paste0(loading.dir,'/outcomes18.sas7bdat'))
outcome_data <- haven::read_sas(paste0(loading.dir,'/outcomes20.sas7bdat'))

saving.dir <- "U:/Hieu/CARDIA_longi_project/csv_files"
write.csv(outcome_data, file = paste0(saving.dir,"/outcomes_up_to_2020.csv"),row.names = F)

# extract mortality and cvd labels since y0: 
death.df <- outcome_data %>% dplyr::select('ID','DEAD20','DEAD20att')
names(death.df) <- c('ID','status','time')
write.csv(death.df, file = paste0(saving.dir,"/mortality_outcome_up_to_2020.csv"),row.names = F)


cvd.df = outcome_data %>% dplyr::select('ID','cvda','cvdafnfatt')
names(cvd.df) <- c('ID','status','time')
write.csv(cvd.df, file = paste0(saving.dir,"/cvd_outcome_up_to_2020.csv"),row.names = F)


