# Input: longitudinal data dictionary, actual data sheets from each year
# Output: longitudinal data sheet, in long format, for all variables in the input data dictionary

cat("\014")
rm(list=ls()) #Clear all
# set working directory: 
work_dir = 'U:/Hieu/CARDIA_longi_project'
setwd(work_dir)

# load libraries:
list.of.packages <- c('ggplot2', 'dplyr', 'tibble', 'parallelMap','parallel', 'readxl', 'tidyr', 'zoo')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)

parallel::detectCores()
# parallelMap::parallelStartSocket(5)

# source snippet functions:
source_dir <- paste0(work_dir, '/code/git_code/snippet')
source(paste0(source_dir,'/createDir.R'))
source(paste0(source_dir,'/subsetDataTopnVar.R'))




# load var dictionary:

var_dict_bubble_format <- read_excel(paste0(work_dir,'/csv_files/','longi_data_avalability_dup_rm_bubble_format3_corrected.xlsx'))

longi_var_to_be_used <- var_dict_bubble_format %>% dplyr::filter(Include == 1) %>% dplyr::select(varname_longi) %>% unlist() %>% as.character() 

var_dict_all_var <- read.csv(paste0(work_dir,'/csv_files/','longi_data_avalability_dictionary.csv'))

var_dict_all_var[nrow(var_dict_all_var) + 1,] <- c(0, 'HBNOW', NA, 'A08BPMED', 'EVER TAKEN HIGH BP MEDICATION', NA, 0, 'a4f08')

# var_dict_ascvd <- read.csv(paste0(work_dir,'/csv_files/','longi_data_avalability_dictionary_ascvd_risk_factors2.csv'))


var_to_be_rm <- c('A21ASMA', 'D21ASMA','E53BEER', 'E53WGT', 'C35CHOL', 'D06CHOL', 'DV6CHOL','E05DIAB','E5ADIAB'
                  , 'C36LIFE', 'E36LIFE', 'F36LIFE', 'E53WINE'
                  ,'SEX', 'A13SEX', 'B12SEX', 'C12SEX', 'C13SEX', 'E12SEX', 'E53SEX')

`%notin%` <- Negate(`%in%`)

var_dict_to_be_used <- var_dict_all_var %>% dplyr::filter(varname_longi %in% longi_var_to_be_used) %>% 
  mutate(exam_year = as.numeric(exam_year)) %>% 
  dplyr::filter(exam_year <= 15) %>% 
  dplyr::filter(Variable.Name %notin% var_to_be_rm) %>% # remove unwanted vars (same longi name (shortened) but meant differently for each exam)
  dplyr::filter(!duplicated(Variable.Name, fromLast=FALSE)) %>%
  rbind(var_dict_all_var %>% dplyr::filter(Variable.Name %in%(c('A02DBP', 'A02SBP')))) %>%
  mutate(varname_longi = ifelse(varname_longi == 'AVGDI', 'DBP', varname_longi)) %>%
  mutate(varname_longi = ifelse(varname_longi == 'AVGSY', 'SBP', varname_longi))







exam_years <- c(0, 2, 5, 7, 10, 15)


for (i in 1:length(exam_years)){
  # i = 1
  
  vars_oi <- var_dict_to_be_used %>% filter(exam_year == exam_years[i]) %>% dplyr::pull(Variable.Name)
  if(i == 1){
    vars_oi <- c(vars_oi,'A08BPMED')
  }
  
  featurespace <-read.csv(paste0(work_dir,'/csv_files/Y', exam_years[i], '/Y', exam_years[i],'_unimputed_featurespace.csv'))
  
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
var_dict_add_exam_year_to_var_name <- var_dict_to_be_used %>% mutate(varname_longi_exam_year = case_when(varname_longi %in% c('RACE1','SEX','AGE1') ~ varname_longi
                                                                    ,TRUE ~ paste0(varname_longi,'_',exam_year))) 

# change varname variables in the data sheet:
names_featurespace_longi <- names(featurespace_longi)
for (i in 1:length(names_featurespace_longi)){
  if (names_featurespace_longi[i] %in% var_dict_add_exam_year_to_var_name$Variable.Name){
    names_featurespace_longi[i] <- var_dict_add_exam_year_to_var_name$varname_longi_exam_year[var_dict_add_exam_year_to_var_name$Variable.Name == names_featurespace_longi[i]]
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


# Missing: GLU_2, GLU_5, LIVER_5, MENTL_5, DFPAY_5, ED_15, MATCK_2, MATCK_7, MATCK_15, FATCK_2, FATCK_7, FATCK_15, CHNOW_0, CHNOW_2

data_longi_wide_fill_in_missing_exam_features <- data_longi_wide %>%
  mutate(GLU_2 = GLU_0) %>% mutate(GLU_5 = GLU_7) %>%
  mutate(LIVER_5 = LIVER_2) %>%
  mutate(MENTL_5 = MENTL_2) %>%
  mutate(DFPAY_5 = DFPAY_2) %>%
  mutate(ED_15 = ED_10) %>% 
  mutate(MATCK_2 = MATCK_0) %>% mutate(MATCK_7 = MATCK_5) %>% mutate(MATCK_15 = MATCK_10) %>%  
  mutate(FATCK_2 = FATCK_0) %>% mutate(FATCK_7 = FATCK_5) %>% mutate(FATCK_15 = FATCK_10) %>%
  mutate(CHNOW_2 = CHNOW_5) %>% mutate(CHNOW_0 = CHNOW_5)

  
time_independent_var <- c('ID', 'status','time', 'RACEBLACK', 'MALE','AGE1')
varying_var <- names(data_longi_wide_fill_in_missing_exam_features)[!(names(data_longi_wide_fill_in_missing_exam_features) %in% time_independent_var)]


data_longi_long = reshape(data_longi_wide_fill_in_missing_exam_features, direction = 'long', idvar = 'ID', sep = "_"
                                  , varying= varying_var 
                                  , timevar = 'exam_year') %>% 
  arrange(ID, exam_year) %>% dplyr::rename(AGE_Y0 = AGE1) %>% dplyr::rename(HBM = HBNOW) %>% dplyr::rename(WST = WST1) %>% 
  # mutate(exam_year_in_days = exam_year*365.25) %>%
  mutate(time_te_in_yrs = time/365.25) %>%
  dplyr::select(c('ID','status','time','exam_year','time_te_in_yrs',everything())) 




# Recode categorical variables: ########################################################################################################


# missing_table <- (5115-colSums(is.na(data_longi_wide))) %>% as.data.frame()

# recode some categorical variables:
# for example, cancer, 1=NO, 2=YES, 8= note sure/treated as NO, missing = NO will be changed to (0 = NO, 1= YES)

## var_to_be_recoded <- var_dict_bubble_format$varname_longi[var_dict_bubble_format$Fill_NA_with == 0] %>% na.omit() %>% as.character()

var_to_be_recoded <- c("ASMA",  "CANCR", "DIAB",  "GALL",  "HEART", "KIDNY", "LIVER", "HRTAK", "SMKNW", "MENTL", "HBM", "CHNOW", "MATCK", "FATCK")

categorical_recode_func <- function(x, na.rm = FALSE){
  x = ifelse(x == 1, 0, x)
  x = ifelse(x == 8, 0, x)
  x = ifelse(x == 2, 1, x)
  return (x)
}

data_longi_long_recode <- data_longi_long %>% mutate_at(var_to_be_recoded, categorical_recode_func)

## the above function takes care of the manual recoding below:
# data_longi_long_recode <- data_longi_long %>% mutate(HBM = case_when(HBM  == 1 ~ 0
#                                                               ,HBM == 2 ~ 1
#                                                              ,HBM == 8 ~ 0
#                                                              )) %>% 
#                                       mutate(DIAB = case_when(DIAB  == 1 ~ 0
#                                                              ,DIAB == 2 ~ 1
#                                                              ,DIAB == 8 ~ 0
#                                                              #,is.na(DIAB) ~ 0
#                                                              )) %>% 
#                                       mutate(SMKNW = case_when(SMKNW  == 1 ~ 0
#                                                               ,SMKNW == 2 ~ 1
#                                                               ,SMKNW == 8 ~ 0
#                                                              ))


# recode 8 and 9 in DFPAY to be NA:
data_longi_long_recode$DFPAY <- ifelse(data_longi_long_recode$DFPAY %in% c(8,9), NA, data_longi_long_recode$DFPAY)

# combine MATCK and FATCK to PATCK (parent ever had heart attack):
data_longi_long_recode$PATCK <- ifelse((data_longi_long_recode$MATCK == 1 | data_longi_long_recode$FATCK == 1), 1, 0)


# Deal with missing data - First pass: #########################################################################


# Remove completely missing years (when all varying measurements in one exam year are missing):
data_longi_long_varying_var <- data_longi_long_recode %>% dplyr::select(-one_of(c(time_independent_var, 'exam_year', 'time_te_in_yrs','AGE_Y0')))
completely_missing_rows <- which(rowSums(is.na(data_longi_long_varying_var)) == ncol(data_longi_long_varying_var))
data_longi_long_na_rm_complete_missing <- data_longi_long_recode %>% dplyr::slice(-completely_missing_rows)

# fill in missing data for some variables (when missing implies never had a certain medical condition or never taken a certain activity):
var_to_be_NA_filled <- var_dict_bubble_format %>% dplyr::filter(Include == 1) %>% 
  dplyr::filter(Fill_NA_with == 0) %>% dplyr::select(varname_longi) %>% unlist() %>% na.omit() %>% as.character() %>% c('HBM', 'PATCK')


data_longi_long_na_fill <- data_longi_long_na_rm_complete_missing %>%
  mutate_at(var_to_be_NA_filled, ~tidyr::replace_na(., 0)) %>% 
  dplyr::select(-one_of('MATCK', 'FATCK')) 






#### Cohort Filtering: ########################################################################################


### only include subjects in the ASCVD-only cohort:

count_N_subjects_and_instances <- function(df){
  print(paste0('N unique subjects: ',df$ID %>% unique() %>% length()))
  print(paste0('N instances: ',df$ID %>% length()))
}


subjects_in_ascvd_cohort <- read.csv(paste0(work_dir,'/csv_files/subjects_in_final_analysis_cohort.csv'))
data_longi_long_expanded_variables_ascvd_cohort <- data_longi_long_na_fill %>% 
  filter(ID %in% subjects_in_ascvd_cohort$x) # %>% filter(HRTAK!= 1)
count_N_subjects_and_instances(data_longi_long_expanded_variables_ascvd_cohort)
# count_N_subjects_and_instances(data_longi_long_expanded_variables_ascvd_cohort_1)

# data_longi_long_na_fill %>% filter(ID %in% subjects_in_ascvd_cohort$x) %>% filter(HRTAK== 1) %>% View()




# Remove the whole exam data with missing values in the most important variables (ASCVD risk factors) as the subjects without
# these variables at an exam likely meant they missed their exam visit:

var_must_be_non_missing_ascvd <- c('SBP', 'HDL', 'CHOL')
data_longi_long_expanded_variables_ascvd_cohort_complete_ascvd_vars <- data_longi_long_expanded_variables_ascvd_cohort %>% 
  drop_na(all_of(var_must_be_non_missing_ascvd))
count_N_subjects_and_instances(data_longi_long_expanded_variables_ascvd_cohort_complete_ascvd_vars)

var_must_be_non_missing_expanded_1 <- c('DBP', 'BMI')
data_longi_long_expanded_variables_ascvd_cohort_complete_essential_vars_1 <- data_longi_long_expanded_variables_ascvd_cohort_complete_ascvd_vars %>% 
  drop_na(all_of(var_must_be_non_missing_expanded_1))
count_N_subjects_and_instances(data_longi_long_expanded_variables_ascvd_cohort_complete_essential_vars_1)




### examine the missing patterns:
# # data frame with at least na in one of the columns:
show_na_in_any_row <- function(df){
  return(df[rowSums(is.na(df)) > 0, ])               # Missings in any row
}
count_na_per_row <- function(x) sum(is.na(x))
# data_longi_long_expanded_variables_ascvd_cohort_complete_essential_vars_1_show_na <- show_na_in_any_row(data_longi_long_expanded_variables_ascvd_cohort_complete_essential_vars_1)
# View(data_longi_long_expanded_variables_ascvd_cohort_complete_essential_vars_1_show_na %>%
#   mutate(count_na = apply(., 1, count_na_per_row)))
# # looks good as maximum missing variables is 4 per each exam visit per subject





### Use LOCF to fill in missing data: 
na.locf2 <- function(x) zoo::na.locf(x, na.rm = FALSE)
data_longi_long_expanded_variables_ascvd_cohort_complete_essential_vars_1_locf <- 
  data_longi_long_expanded_variables_ascvd_cohort_complete_essential_vars_1 %>% group_by(ID) %>%
  do(na.locf2(.)) %>% ungroup()
# View(show_na_in_any_row(data_longi_long_expanded_variables_ascvd_cohort_complete_essential_vars_1_locf))
# there are still early year missing (exam_year == 0) in ARMCI, GLU so will use NOCB

# Use NOCB to fill missing data in early exam years if they had data in following exam years:
data_longi_long_expanded_variables_ascvd_cohort_complete_essential_vars_1_locf_nocb <- 
   data_longi_long_expanded_variables_ascvd_cohort_complete_essential_vars_1_locf %>% 
  tidyr::fill(names(.)
               , .direction = 'up')
# View(show_na_in_any_row(data_longi_long_expanded_variables_ascvd_cohort_complete_essential_vars_1_locf_nocb))
# no more missing data


## if decide to remove entire exam datapoint if missing one of the following variables:
# var_must_be_non_missing_expanded_2 <- c('NTRIG', 'LDL', 'PULSE', 'WST1', 'PSTYR', 'ARMCI')
# data_longi_long_expanded_variables_ascvd_cohort_complete_essential_vars_1_2 <- data_longi_long_expanded_variables_ascvd_cohort_complete_essential_vars_1 %>% 
#   drop_na(all_of(var_must_be_non_missing_expanded_2))
# count_N_subjects_and_instances(data_longi_long_expanded_variables_ascvd_cohort_complete_essential_vars_1_2)
# 
# 
# var_must_be_non_missing_expanded_2 <- c('NTRIG', 'LDL', 'PULSE', 'WST1', 'PSTYR', 'ARMCI', 'ED')
# data_longi_long_expanded_variables_ascvd_cohort_complete_essential_vars_1_2 <- data_longi_long_expanded_variables_ascvd_cohort_complete_essential_vars_1 %>% 
#   drop_na(all_of(var_must_be_non_missing_expanded_2))
# count_N_subjects_and_instances(data_longi_long_expanded_variables_ascvd_cohort_complete_essential_vars_1_2)
# 
# 
# var_must_be_non_missing_expanded_3 <- c('GLU', 'DFPAY')
# data_longi_long_expanded_variables_ascvd_cohort_complete_essential_vars_1_2_3 <- data_longi_long_expanded_variables_ascvd_cohort_complete_essential_vars_1_2 %>% 
#   drop_na(all_of(var_must_be_non_missing_expanded_3))
# count_N_subjects_and_instances(data_longi_long_expanded_variables_ascvd_cohort_complete_essential_vars_1_2_3)




# data_at_least_one_NA <- 
#   data_longi_long_expanded_variables_ascvd_cohort_complete_essential_vars[!complete.cases(data_longi_long_expanded_variables_ascvd_cohort_complete_essential_vars), ]
# # data_at_least_one_NA$ID %>% unique() %>% length()

# na_count_df <-data.frame(na_count = sapply(data_longi_long_expanded_variables_ascvd_cohort_complete_essential_vars_1_2, function(y) sum(length(which(is.na(y))))))
# na_count_df$var_name <- rownames(na_count_df)

na_count_df <-data.frame(na_count = sapply(data_longi_long_expanded_variables_ascvd_cohort_complete_essential_vars_1_locf_nocb, function(y) sum(length(which(is.na(y))))))
na_count_df$var_name <- rownames(na_count_df)





### remove subjects with prior heart attack:
subjects_with_hrtak <- data_longi_long_expanded_variables_ascvd_cohort_complete_essential_vars_1_locf_nocb %>% 
  filter(HRTAK== 1) %>% dplyr::select(ID) %>% unlist() %>% as.character() %>% unique()
# 12 unique subjects with HRTAK
# data_longi_long_expanded_variables_ascvd_cohort_complete_essential_vars_1_locf_nocb %>% filter(HEART== 1) %>% dplyr::select(ID) %>% unique() %>% nrow()
data_longi_long_expanded_variables_ascvd_cohort_complete_essential_vars_1_locf_nocb_rm_hrtak <- 
  data_longi_long_expanded_variables_ascvd_cohort_complete_essential_vars_1_locf_nocb %>% 
  filter(ID %notin% subjects_with_hrtak)
count_N_subjects_and_instances(data_longi_long_expanded_variables_ascvd_cohort_complete_essential_vars_1_locf_nocb_rm_hrtak)

  
### finalized cohort:
data_longi_long_expanded_variables_final <- data_longi_long_expanded_variables_ascvd_cohort_complete_essential_vars_1_locf_nocb_rm_hrtak %>%
  dplyr::select(-one_of(c('HEART', 'HRTAK'))) 
count_N_subjects_and_instances(data_longi_long_expanded_variables_final)

write.csv(data_longi_long_expanded_variables_final, file = paste0(work_dir,'/csv_files/data_longi_long_format_expanded_variables_removed_missing_data_2.csv'),row.names = F)


# same cohort but with missing data:
data_longi_long_expanded_variables_final_with_missing_data <- data_longi_long_expanded_variables_ascvd_cohort %>%
  dplyr::select(-one_of(c('HEART', 'HRTAK'))) %>% 
  filter(ID %notin% subjects_with_hrtak)
count_N_subjects_and_instances(data_longi_long_expanded_variables_final_with_missing_data)

write.csv(data_longi_long_expanded_variables_final_with_missing_data, file = paste0(work_dir,'/csv_files/data_longi_long_format_expanded_variables_with_missing_data_2.csv'),row.names = F)


# cohort with all subjects:

count_N_subjects_and_instances(data_longi_long_na_fill)
write.csv(data_longi_long_na_fill %>% dplyr::select(-one_of(c('HEART', 'HRTAK'))), file = paste0(work_dir,'/csv_files/data_longi_long_expanded_variables_final_all_subjects_with_missing_data_up_to_y15.csv'),row.names = F)


data_longi_long_expanded_variables_final_all_subjects_with_missing_data_up_to_y15_rm_hrtak <- data_longi_long_na_fill %>%
  dplyr::select(-one_of(c('HEART', 'HRTAK'))) %>% 
  filter(ID %notin% subjects_with_hrtak)
count_N_subjects_and_instances(data_longi_long_expanded_variables_final_all_subjects_with_missing_data_up_to_y15)
write.csv(data_longi_long_expanded_variables_final_all_subjects_with_missing_data_up_to_y15_rm_hrtak, file = paste0(work_dir,'/csv_files/data_longi_long_expanded_variables_final_all_subjects_with_missing_data_up_to_y15_rm_hratk.csv'),row.names = F)

