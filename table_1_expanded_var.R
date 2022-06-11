rm(list=ls()) #Clear all
cat("\014")

# set working directory: 
work_dir = 'U:/Hieu/CARDIA_longi_project'
setwd(work_dir)

# devtools::install_github("benjaminrich/table1")
# load libraries:
list.of.packages <- c("mlbench",'ggplot2','caret', 'dplyr', 'tibble', 'parallelMap', 'boot','table1', 'xfun')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)

ncores <- parallel::detectCores(all.tests = FALSE, logical = TRUE)

# parallelMap::parallelStartSocket(ncores-1)



# load the dataset
loading_dir = paste0(work_dir, '/csv_files')

data_longi_long_for_analysis_cohort <- read.csv(paste0(work_dir,'/csv_files/data_longi_long_format_expanded_variables_removed_missing_data_2.csv'))

# data_longi_long_for_analysis_no_missing_data <- read.csv(paste0(work_dir, '/csv_files/data_longi_long_format_expanded_variables_with_missing_data_2.csv'))
data_longi_long_for_analysis <- read.csv(paste0(work_dir,'/csv_files/data_longi_long_expanded_variables_final_all_subjects_with_missing_data_up_to_y15.csv'))


## Filtering process: ########################################
#exclude subjects who didn't show up to exam year 15: 
subjects_y15 <- read.csv(paste0(work_dir,'/csv_files/subjects_showed_up_to_Y15.csv'))
subjects_y0 <- read.csv(paste0(work_dir,'/csv_files/subjects_showed_up_to_Y0.csv'))

#exclude instances with events or censored before exam year 15 
# only include longitudinal data from y0 to y15:
data_longi_long_up_to_y15 <- data_longi_long_for_analysis_cohort %>% 
  filter(ID %in% subjects_y15$ID) %>% 
  filter(ID %in% subjects_y0$ID) %>% 
  filter(time_te_in_yrs >15) %>%
  filter(exam_year <=15)

# subjects with event or censored prior to Y15 exam:
data_longi_long_for_analysis_cohort %>% 
  filter(ID %in% subjects_y15$ID) %>% 
  filter(ID %in% subjects_y0$ID) %>% 
  filter(time_te_in_yrs <= 15) %>% nrow() %>% print()
# 0 subjects


# baseline data for everyone:
# data_at_baseline_everyone <- data_longi_long_for_analysis %>% filter(!duplicated(ID, fromLast = FALSE))
data_at_baseline_everyone <- data_longi_long_for_analysis %>% filter(exam_year == 0)
everyone_id <- data_at_baseline_everyone$ID

#baseline data of the cohort: 
data_at_baseline_cohort <- data_longi_long_up_to_y15 %>% filter(!duplicated(ID, fromLast=FALSE)) 
# data_at_baseline <- data_longi_long_up_to_y15 %>% filter(exam_year == 0)
cohort_baseline_id <- data_at_baseline_cohort$ID
print(length(cohort_baseline_id))

# most recent data at landmark time (y15):
data_most_recent_by_y15_cohort <- data_longi_long_up_to_y15 %>% filter(!duplicated(ID, fromLast=TRUE))
cohort_y15_id <- data_most_recent_by_y15_cohort$ID
print(length(cohort_y15_id))
# cohort_baseline and cohort_y15 have the same participants













# # cohort ids that has no missing data:
# cohort_baseline_complete_cases <- data_at_baseline %>% na.omit() 
# print(paste0('number of subjects with completed cases at baseline: ',cohort_baseline_complete_cases$ID %>% unique() %>% length()))
# # 3614/3639 unique patients have completed cases
# # closer look at the number of cases removed: 
# data_rm <- data_at_baseline %>% anti_join(cohort_baseline_complete_cases, by = 'ID')
# # 25 subjects did not have any recordings of CHOL and HDL
# 
# cohort_y15_complete_cases <- data_most_recent_by_y15 %>% na.omit()
# print(paste0('number of subjects with completed cases at y15: ',cohort_y15_complete_cases$ID %>% unique() %>% length()))
# # 3575/3639 unique patients have completed cases
# data_rm_y15 <- data_most_recent_by_y15 %>% anti_join(cohort_y15_complete_cases, by = 'ID')
# # 64 subjects either missing CHOL/HDL or SBP or both
# 
# # intersect: 
# cohort_baseline_and_y15_complete_cases_id <- cohort_baseline_complete_cases$ID %>% intersect(cohort_y15_complete_cases$ID)
# # 3551 subjects
# print(paste0('number of subjects with completed cases at baseline and y15: ',cohort_baseline_and_y15_complete_cases_id %>% unique() %>% length()))
# 
# data_at_baseline_complete_cases <- data_at_baseline %>% filter(ID %in% cohort_baseline_and_y15_complete_cases_id)
# data_most_recent_y15_complete_cases <- data_most_recent_by_y15 %>% filter(ID %in% cohort_baseline_and_y15_complete_cases_id)

# write.csv(ID = cohort_baseline_and_y15_complete_cases_id, file = paste0(work_dir,'/csv_files/subjects_in_final_analysis_cohort.csv'), row.names = F)



# #exclude instances with events or censored before exam year 15 
# # only include longitudinal data from y0 to y15:
# data_longi_long_up_to_y15_no_missing_data <- data_longi_long_for_analysis_no_missing_data %>% 
#   filter(ID %in% subjects_y15$ID) %>% 
#   filter(time_te_in_yrs >15) %>%
#   filter(exam_year <=15)
# 
# # baseline data for everyone:
# data_at_baseline_everyone_no_missing_data <- data_longi_long_for_analysis_no_missing_data %>% filter(exam_year == 0)
# everyone_id_no_missing_data <- data_at_baseline_everyone_no_missing_data$ID
# 
# #baseline data of the cohort: 
# data_at_baseline_no_missing_data <- data_longi_long_up_to_y15_no_missing_data %>% filter(exam_year == 0)
# cohort_baseline_id_no_missing_id <- data_at_baseline_no_missing_data$ID
# # 3614/3639 unique patients have completed cases
# 
# # most recent data at landmark time (y15):
# data_most_recent_by_y15_no_missing_data <- data_longi_long_up_to_y15_no_missing_data %>% filter(!duplicated(ID, fromLast=TRUE)) %>% na.omit()
# # cohort_baseline and cohort_y15 have the same participants







## Table 1: ###########################################


recode_var <- function(df, var, factor_type =TRUE, level_change =c(1,0)
                       , label_change = c("1","0"), varname_change = NA){
  
  df_recoded <- df
  if(factor_type){
  df_recoded[[var]] <- 
    factor(df_recoded[[var]], levels=level_change,
           labels=label_change)}
  if(!is.na(varname_change)){
    label(df_recoded[[var]])   = varname_change
  }
  return(df_recoded)
}

my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=2), c("",
                                                           "Mean (SD)"=sprintf("%s (%s)", MEAN, SD)))
}
my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,
                                                  sprintf("%d (%0.0f%%)", FREQ, PCT))))
}
#' as.data.frame.table1 function from Github to convert table1 object to data frame:
as.data.frame.table1 <- function(x, ...) {
  obj <- attr(x, "obj")
  with(obj, {
    rlh <- if (is.null(rowlabelhead) || rowlabelhead=="") "\U{00A0}" else rowlabelhead
    z <- lapply(contents, function(y) {
      y <- as.data.frame(y, stringsAsFactors=F)
      y2 <- data.frame(x=paste0(c("", rep("\U{00A0}\U{00A0}", nrow(y) - 1)), rownames(y)), stringsAsFactors=F)
      y <- cbind(setNames(y2, rlh), y)
      y
    })
    df <- do.call(rbind, z)
    df <- rbind(c("", ifelse(is.na(headings[2,]), "", sprintf("(N=%s)", headings[2,]))), df)
    colnames(df) <- c(rlh, headings[1,])
    rownames(df) <- NULL
    noquote(df)
  })
}



# load and merge mortality outcome:
mort_outcome <- read.csv(paste0(work_dir,'/csv_files/mortality_outcome_up_to_2020.csv'))
mort_outcome$mort_status <- mort_outcome$status

data_at_baseline_everyone_with_mort <- data_at_baseline_everyone %>% left_join(mort_outcome[,c('ID', 'mort_status')], by = 'ID')


# table 1 for year 0:
table_1_data_Y0 <- data_at_baseline_everyone_with_mort

table_1_data_Y0$cohort_inclusion_status <- ifelse(table_1_data_Y0$ID %in% cohort_baseline_id, 1, 0)

recode_table_1_func <- function(table_1_data_){
  table_1_data_recoded <- recode_var(table_1_data_, var="MALE", factor_type = TRUE
                                     , label_change = c("Male","Female")
                                     , varname_change = "Sex")
  table_1_data_recoded <- recode_var(table_1_data_recoded, var="RACEBLACK", factor_type = TRUE
                                     , label_change = c("Black","White")
                                     , varname_change = "Race")
  table_1_data_recoded <- recode_var(table_1_data_recoded, var="DIAB", factor_type = TRUE
                                     , label_change = c("Diabetic", "Non-diabetic")
                                     , varname_change = "Diabetes")
  table_1_data_recoded <- recode_var(table_1_data_recoded, var="HBM", factor_type = TRUE
                                     , label_change = c("Yes", "No")
                                     , varname_change = "Use of hypertensive medication")
  table_1_data_recoded <- recode_var(table_1_data_recoded, var="SMKNW", factor_type = TRUE
                                     , label_change = c("Yes", "No")
                                     , varname_change = "Smoking now")
  table_1_data_recoded <- recode_var(table_1_data_recoded, var="CHOL", factor_type = FALSE
                                     , varname_change = "Total cholesterol (mg/dL)")
  table_1_data_recoded <- recode_var(table_1_data_recoded, var="status", factor_type = TRUE
                                     , label_change = c("Yes", "No")
                                     , varname_change = "CVD event by end of follow up")
  table_1_data_recoded <- recode_var(table_1_data_recoded, var="ARMCI", factor_type = FALSE
                                     , varname_change = "Arm circumference (cm)")
  table_1_data_recoded <- recode_var(table_1_data_recoded, var="ASMA", factor_type = TRUE
                                     , label_change = c("Yes", "No")
                                     , varname_change = "Asthma")
  table_1_data_recoded <- recode_var(table_1_data_recoded, var="BEER", factor_type = FALSE
                                     , varname_change = "# of drinks of beer/week (12 oz/drink)")
  table_1_data_recoded <- recode_var(table_1_data_recoded, var="CANCR", factor_type = TRUE
                                     , label_change = c("Yes", "No")
                                     , varname_change = "Cancer")
  table_1_data_recoded <- recode_var(table_1_data_recoded, var="CGTDY", factor_type = FALSE
                                     , varname_change = "Cigarettes smoked/day")
  table_1_data_recoded <- recode_var(table_1_data_recoded, var="DFPAY", factor_type = TRUE
                                     , level_change =c(1,2,3,4)
                                     , label_change = c("Very hard", "Hard", "Somewhat hard", "Not very hard")
                                     , varname_change = "Ability to pay for the very basics")
  table_1_data_recoded <- recode_var(table_1_data_recoded, var="ED", factor_type = FALSE
                                     , varname_change = "Education level (grades)")
  table_1_data_recoded <- recode_var(table_1_data_recoded, var="GALL", factor_type = TRUE
                                     , label_change = c("Yes", "No")
                                     , varname_change = "Gall bladder problem")
  table_1_data_recoded <- recode_var(table_1_data_recoded, var="GLU", factor_type = FALSE
                                     , varname_change = "Fasting glucose (mg/100 ml)")
  table_1_data_recoded <- recode_var(table_1_data_recoded, var="KIDNY", factor_type = TRUE
                                     , label_change = c("Yes", "No")
                                     , varname_change = "Kidney problem")
  table_1_data_recoded <- recode_var(table_1_data_recoded, var="LIFE", factor_type = FALSE
                                     , varname_change = "Times used marijuana in life")
  table_1_data_recoded <- recode_var(table_1_data_recoded, var="LIQR", factor_type = FALSE
                                     , varname_change = "# of shots of hard liquor/week")
  table_1_data_recoded <- recode_var(table_1_data_recoded, var="LIVER", factor_type = TRUE
                                     , label_change = c("Yes", "No")
                                     , varname_change = "Liver problem")
  table_1_data_recoded <- recode_var(table_1_data_recoded, var="MENTL", factor_type = TRUE
                                     , label_change = c("Yes", "No")
                                     , varname_change = "Nervous, emotional or mental disorder")
  table_1_data_recoded <- recode_var(table_1_data_recoded, var="NPREG", factor_type = FALSE
                                     , varname_change = "# of times pregnant in life")
  table_1_data_recoded <- recode_var(table_1_data_recoded, var="NTRIG", factor_type = FALSE
                                     , varname_change = "Triglycerides (mg/dl)")
  table_1_data_recoded <- recode_var(table_1_data_recoded, var="PSTYR", factor_type = TRUE
                                     , level_change = c(1,2,3,4,5)
                                     , label_change = c("1: Inactive", "2", "3: Moderately active", "4", "5: very active")
                                     , varname_change = "Physical activity level (self-assessed)")
  table_1_data_recoded <- recode_var(table_1_data_recoded, var="PULSE", factor_type = FALSE
                                     , varname_change = "Pulse beats (in 30s)")
  table_1_data_recoded <- recode_var(table_1_data_recoded, var="WGT", factor_type = FALSE
                                     , varname_change = "Weight (lbs)")
  table_1_data_recoded <- recode_var(table_1_data_recoded, var="WINE", factor_type = FALSE
                                     , varname_change = "# of drinks of wine/week (5 oz/drink)")
  table_1_data_recoded <- recode_var(table_1_data_recoded, var="WST", factor_type = FALSE
                                     , varname_change = "Waist girth (cm)")
  table_1_data_recoded <- recode_var(table_1_data_recoded, var="CHNOW", factor_type = TRUE
                                     , label_change = c("Yes", "No")
                                     , varname_change = "Use of cholesterol-lowering medication")
  table_1_data_recoded <- recode_var(table_1_data_recoded, var="DBP", factor_type = FALSE
                                     , varname_change = "Diastolic blood pressure (DBP)")
  table_1_data_recoded <- recode_var(table_1_data_recoded, var="SBP", factor_type = FALSE
                                     , varname_change = "Systolic blood pressure (SBP)")
  table_1_data_recoded <- recode_var(table_1_data_recoded, var="BMI", factor_type = FALSE
                                     , varname_change = "Body Mass Index (BMI)")
  table_1_data_recoded <- recode_var(table_1_data_recoded, var="AGE_Y0", factor_type = FALSE
                                     , varname_change = "Age")
  table_1_data_recoded <- recode_var(table_1_data_recoded, var="HDL", factor_type = FALSE
                                     , varname_change = "Total HDL cholesterol (mg/dl)")
  table_1_data_recoded <- recode_var(table_1_data_recoded, var="LDL", factor_type = FALSE
                                     , varname_change = "Total LDL cholesterol (mg/dl)")
  table_1_data_recoded <- recode_var(table_1_data_recoded, var="PATCK", factor_type = TRUE
                                     , label_change = c("Yes", "No")
                                     , varname_change = "Parent history of heart attack")
  table_1_data_recoded <- recode_var(table_1_data_recoded, var="mort_status", factor_type = TRUE
                                     , label_change = c("Yes", "No")
                                     , varname_change = "All-cause death by end of follow-up")
  
  if("cohort_inclusion_status" %in% colnames(table_1_data_recoded)){
    table_1_data_recoded <- recode_var(table_1_data_recoded, var="cohort_inclusion_status", factor_type = TRUE
                                       , label_change = c("Exam Y0", "Excluded"))
  }

  return(table_1_data_recoded)
}



table_1_data_Y0_recoded <- recode_table_1_func(table_1_data_Y0)

table_1_output_Y0 <- table1(~. | cohort_inclusion_status, data= table_1_data_Y0_recoded %>%
         dplyr::select(-one_of(c('ID','time', 'exam_year', 'time_te_in_yrs'))) %>% 
         # dplyr::select(-one_of(c('status')), 'mort_status','status')
         dplyr::select(one_of("AGE_Y0", "MALE", "RACEBLACK"
                              , "SBP", "DBP", "HBM", "PULSE" 
                              , "BMI", "WST", "WGT", "ARMCI"
                              , "ED", "DFPAY"
                              , "CHOL", "CHNOW", "HDL", "LDL", "NTRIG"
                              , "GLU",  "NPREG"
                              , "ASMA", "CANCR", "DIAB",  "GALL", "KIDNY", "MENTL", "LIVER", "PATCK"
                              , "SMKNW", "CGTDY"
                              , "BEER", "LIQR", "WINE"
                              , "LIFE"
                              , "PSTYR" 
                              , "mort_status", "status", "cohort_inclusion_status"))
       , overall="Exam Y0 (all subjects)"
       #, render.continuous=my.render.cont
       , render.categorical=my.render.cat, render.missing=NULL
       , topclass="Rtable1-grid"
      )
table_1_output_Y0
table_1_df_Y0 <- as.data.frame.table1(table_1_output_Y0)








# table 1 for year 15:

data_most_recent_by_y15_cohort_with_mort <- data_most_recent_by_y15_cohort %>% left_join(mort_outcome[,c('ID', 'mort_status')], by = 'ID')

table_1_data_Y15_recoded <- recode_table_1_func(data_most_recent_by_y15_cohort_with_mort %>% 
                                                  mutate(AGE_Y0 = AGE_Y0+15))

table_1_y15 <- table1(~., data=table_1_data_Y15_recoded %>% 
                    dplyr::select(-one_of(c('ID','time', 'exam_year', 'time_te_in_yrs'))) %>% 
                  #  dplyr::select(-one_of(c('status')), 'status')
                    dplyr::select(one_of("AGE_Y0", "MALE", "RACEBLACK"
                                         , "SBP", "DBP", "HBM", "PULSE" 
                                         , "BMI", "WST", "WGT", "ARMCI"
                                         , "ED", "DFPAY"
                                         , "CHOL", "CHNOW", "HDL", "LDL", "NTRIG"
                                         , "GLU",  "NPREG"
                                         , "ASMA", "CANCR", "DIAB",  "GALL", "KIDNY", "MENTL", "LIVER", "PATCK"
                                         , "SMKNW", "CGTDY"
                                         , "BEER", "LIQR", "WINE"
                                         , "LIFE"
                                         , "PSTYR" 
                                         , "mort_status", "status"))
                  , overall="Exam Y15"
                  #, render.continuous=my.render.cont
                  , render.categorical=my.render.cat, render.missing=NULL
                  , topclass="Rtable1-grid"
)
table_1_y15
table_1_df_Y15 <- as.data.frame.table1(table_1_y15)








# Combine Y0 and Y15 tables:
table_1_combined <- cbind(table_1_df_Y0[,c(1,4,2)], 'Exam Y15' = table_1_df_Y15$`Exam Y15`)

# copy and paste to excel:
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

write.excel(table_1_combined)

#' #'C08DIAB' 'HBP05'
#' var = 'C08KIDNY'
#' table(data_y10[[var]])

