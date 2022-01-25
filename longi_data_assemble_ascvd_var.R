# Input: longitudinal data dictionary, actual data sheets from each year
# Output: longitudinal data sheet, in long format, for all variables in the input data dictionary

cat("\014")
rm(list=ls()) #Clear all
# set working directory: 
work_dir = 'U:/Hieu/CARDIA_longi_project'
setwd(work_dir)

# load libraries:
list.of.packages <- c('ggplot2', 'dplyr', 'tibble', 'parallelMap','parallel')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)

parallel::detectCores()
parallelMap::parallelStartSocket(5)

# source snippet functions:
source_dir <- 'U:/Hieu/CARDIA_project/CARDIA_project'
source(paste0(source_dir,'/Git/code/snippet/createDir.R'))
source(paste0(source_dir,'/Git/code/snippet/subsetDataTopnVar.R'))


var_dict <- read.csv(paste0(work_dir,'/csv_files/','longi_data_avalability_dictionary_ascvd_risk_factors2.csv'))





exam_years <- c(0, 2, 5, 7, 10, 15, 20, 25, 30)


for (i in 1:length(exam_years)){
  
  featurespace <-read.csv(paste0(work_dir,'/csv_files/Y', exam_years[i], '/Y', exam_years[i],'_unimputed_featurespace.csv'))
  vars_oi <- var_dict %>% filter(exam_year == exam_years[i]) %>% dplyr::pull(Variable.Name)
  featurespace <- featurespace[!is.na(featurespace$SHORT_ID),]
  
  featurespace_vars_oi <- featurespace %>% dplyr::select(one_of(c('ID',vars_oi)))

  if(i == 1){
    featurespace_longi <- featurespace_vars_oi
  }
  
  else{
    class(featurespace_longi$ID) <- 'character'
    class(featurespace_vars_oi$ID) <- 'character'
    featurespace_longi <- featurespace_longi %>% full_join(featurespace_vars_oi, by ='ID')
  }
}

# change varname variables in the dictionary: add exam year to the name in the varying variables:
var_dict <- var_dict %>% mutate(varname_longi_exam_year = case_when(varname_longi %in% c('RACE1','SEX','AGE1') ~ varname_longi
                                                                    ,TRUE ~ paste0(varname_longi,'_',exam_year))) 

# change varname variables in the data sheet:
names_featurespace_longi <- names(featurespace_longi)
for (i in 1:length(names_featurespace_longi)){
  if (names_featurespace_longi[i] %in% var_dict$Variable.Name){
    names_featurespace_longi[i] <- var_dict$varname_longi_exam_year[var_dict$Variable.Name == names_featurespace_longi[i]]
  }
}

featurespace_longi_change_varname <- featurespace_longi
names(featurespace_longi_change_varname) <- names_featurespace_longi







# merge feature space with the outcome:
outcome_data <- read.csv(paste0(work_dir,"/csv_files/cvd_outcome_up_to_2020.csv"))
class(outcome_data$ID) <- 'character'

data_longi_wide <- outcome_data %>% left_join(featurespace_longi_change_varname, by = 'ID') %>%
                              mutate(MALE = case_when(SEX == 1 ~ 1
                                                      ,TRUE ~0)) %>% 
                              mutate(RACEBLACK = case_when(RACE1 == 4 ~ 1
                                                           ,RACE1 == 3 ~ 1
                                                           ,TRUE ~ 0)) %>% dplyr::select(-c('SEX','RACE1'))








# convert data from wide to long format:

time_independent_var <- c('ID', 'status','time', 'RACEBLACK', 'MALE','AGE1')
varying_var <- names(data_longi_wide)[!(names(data_longi_wide) %in% time_independent_var)]

data_longi_long = reshape(data_longi_wide, direction = 'long', idvar = 'ID', sep = "_"
                                  , varying= varying_var 
                                  , timevar = 'exam_year') %>% 
  arrange(ID, exam_year) %>% rename(AGE_Y0 = AGE1) %>%
  # mutate(exam_year_in_days = exam_year*365.25) %>%
  mutate(time_te_in_yrs = time/365.25) %>%
  dplyr::select(c('ID','status','time','exam_year','time_te_in_yrs',everything())) 




# Deal with missing data: ########################################################################################################


missing_table <- (5115-colSums(is.na(data_longi_wide))) %>% as.data.frame()

# recode some variables:
# for variable <currently taking high-blood pressure meds, 1=NO, 2=YES, 8= note sure/treated as NO, missing = NO
data_longi_long <- data_longi_long %>% mutate(HBM = case_when(HBM  == 1 ~ 0
                                                             ,HBM == 2 ~ 1
                                                             ,HBM == 8 ~ 0
                                                             )) %>% 
                                      mutate(DIAB = case_when(DIAB  == 1 ~ 0
                                                             ,DIAB == 2 ~ 1
                                                             ,DIAB == 8 ~ 0
                                                             #,is.na(DIAB) ~ 0
                                                             )) %>% 
                                      mutate(SMKNW = case_when(SMKNW  == 1 ~ 0
                                                              ,SMKNW == 2 ~ 1
                                                              ,SMKNW == 8 ~ 0
                                               

# Remove completely missing years (when all varying measurements in one exam year are missing):
data_longi_long_varying_var <- data_longi_long %>% dplyr::select(-one_of(c(time_independent_var, 'exam_year', 'time_te_in_yrs','AGE_Y0')))
completely_missing_rows <- which(rowSums(is.na(data_longi_long_varying_var)) == ncol(data_longi_long_varying_var))
data_longi_long_na_rm <- data_longi_long %>% slice(-completely_missing_rows)


# treat missing HBM status as not taking HBM, because participant can only see the question for HBM if they indicates they have hypertension
data_longi_long_na_rm$HBM[data_longi_long_na_rm$HBM %>% is.na()] <- 0 

# assume that missing SMKNW status means the person is not smoking regularly (need to check again)
data_longi_long_na_rm$SMKNW[data_longi_long_na_rm$SMKNW %>% is.na()] <- 0 
               #,is.na(DIAB) ~ 0
                                      )) 
data_longi_long_complete_cases <- data_longi_long_na_rm %>% na.omit() 
# 5109/5114 have completed cases

data_longi_long_for_analysis <- data_longi_long_complete_cases %>% filter(exam_year<time_te_in_yrs)

write.csv(data_longi_long_for_analysis, file = paste0(work_dir,'/csv_files/data_longi_long_format_ascvd_risk_factors.csv'),row.names = F)




