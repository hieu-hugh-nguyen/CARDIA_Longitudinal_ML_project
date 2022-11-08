rm(list=ls()) #Clear all
cat("\014")

# set working directory: 
work_dir = 'U:/Hieu/CARDIA_longi_project'
setwd(work_dir)

# devtools::install_github("benjaminrich/table1")
# load libraries:
list.of.packages <- c("mlbench",'ggplot2','caret', 'dplyr', 'tibble', 'parallelMap', 'boot','table1')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)

ncores <- parallel::detectCores(all.tests = FALSE, logical = TRUE)

parallelMap::parallelStartSocket(ncores-1)



# load the dataset
loading_dir = paste0(work_dir, '/csv_files')

data_longi_long_for_analysis <- read.csv(paste0(work_dir,'/csv_files/data_longi_long_format_ascvd_risk_factors_with_missing_data.csv'))

data_longi_long_for_analysis_no_missing_data <- read.csv(paste0(work_dir, '/csv_files/data_longi_long_format_ascvd_risk_factors_removed_missing_data.csv'))


## Filtering process: ########################################
#exclude subjects who didn't show up to exam year 15: 
subjects_y15 <- read.csv(paste0(work_dir,'/csv_files/subjects_showed_up_to_Y15.csv'))
subjects_y0 <- read.csv(paste0(work_dir,'/csv_files/subjects_showed_up_to_Y0.csv'))

#exclude instances with events or censored before exam year 15 
# only include longitudinal data from y0 to y15:
data_longi_long_up_to_y15 <- data_longi_long_for_analysis %>% 
  filter(ID %in% subjects_y15$ID) %>% 
  filter(ID %in% subjects_y0$ID) %>% 
  filter(time_te_in_yrs >15) %>%
  filter(exam_year <=15)

# subjects with event or censored prior to Y15 exam:
data_longi_long_for_analysis %>% 
  filter(ID %in% subjects_y15$ID) %>% 
  filter(ID %in% subjects_y0$ID) %>% 
  filter(time_te_in_yrs <= 15) %>% nrow() %>% print()
# 148 subjects


# baseline data for everyone:
# data_at_baseline_everyone <- data_longi_long_for_analysis %>% filter(!duplicated(ID, fromLast = FALSE))
data_at_baseline_everyone <- data_longi_long_for_analysis %>% filter(exam_year == 0)
everyone_id <- data_at_baseline_everyone$ID

#baseline data of the cohort: 
# data_at_baseline <- data_longi_long_up_to_y15 %>% filter(!duplicated(ID, fromLast=FALSE)) 
data_at_baseline <- data_longi_long_up_to_y15 %>% filter(exam_year == 0)
cohort_baseline_id <- data_at_baseline$ID
print(length(cohort_baseline_id))

# most recent data at landmark time (y15):
data_most_recent_by_y15 <- data_longi_long_up_to_y15 %>% filter(!duplicated(ID, fromLast=TRUE))
cohort_y15_id <- data_most_recent_by_y15$ID
print(length(cohort_y15_id))
# cohort_baseline and cohort_y15 have the same participants



# cohort ids that has no missing data:
cohort_baseline_complete_cases <- data_at_baseline %>% na.omit() 
print(paste0('number of subjects with completed cases at baseline: ',cohort_baseline_complete_cases$ID %>% unique() %>% length()))
# 3614/3639 unique patients have completed cases
# closer look at the number of cases removed: 
data_rm <- data_at_baseline %>% anti_join(cohort_baseline_complete_cases, by = 'ID')
# 25 subjects did not have any recordings of CHOL and HDL

cohort_y15_complete_cases <- data_most_recent_by_y15 %>% na.omit()
print(paste0('number of subjects with completed cases at y15: ',cohort_y15_complete_cases$ID %>% unique() %>% length()))
# 3575/3639 unique patients have completed cases
data_rm_y15 <- data_most_recent_by_y15 %>% anti_join(cohort_y15_complete_cases, by = 'ID')
# 64 subjects either missing CHOL/HDL or SBP or both

# intersect: 
cohort_baseline_and_y15_complete_cases_id <- cohort_baseline_complete_cases$ID %>% intersect(cohort_y15_complete_cases$ID)
# 3551 subjects
print(paste0('number of subjects with completed cases at baseline and y15: ',cohort_baseline_and_y15_complete_cases_id %>% unique() %>% length()))

data_at_baseline_complete_cases <- data_at_baseline %>% filter(ID %in% cohort_baseline_and_y15_complete_cases_id)
data_most_recent_y15_complete_cases <- data_most_recent_by_y15 %>% filter(ID %in% cohort_baseline_and_y15_complete_cases_id)

write.csv(ID = cohort_baseline_and_y15_complete_cases_id, file = paste0(work_dir,'/csv_files/subjects_in_final_analysis_cohort.csv'), row.names = F)



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
    label(df_recoded[[var]])       <- varname_change
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



table_1_data <- data_at_baseline_everyone
table_1_data$cohort_inclusion_status <- ifelse(table_1_data$ID %in% cohort_baseline_id, 1, 0)

table_1_data_recoded <- recode_var(table_1_data, var="MALE", factor_type = TRUE
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
table_1_data_recoded <- recode_var(table_1_data_recoded, var="cohort_inclusion_status", factor_type = TRUE
                                   , label_change = c("Exam Y0", "Excluded"))




table_1 <- table1(~. | cohort_inclusion_status, data=table_1_data_recoded %>% 
         dplyr::select(-one_of(c('ID','time', 'exam_year', 'time_te_in_yrs'))) %>% 
         dplyr::select(-one_of(c('status')), 'status')
       , overall="Exam Y0 (all subjects)"
       , render.continuous=my.render.cont, render.categorical=my.render.cat, render.missing=NULL
       , topclass="Rtable1-grid"
      )

table_1_df <- as.data.frame.table1(table_1)





table_1_data <- data_most_recent_by_y15


table_1_data_recoded <- recode_var(table_1_data, var="MALE", factor_type = TRUE
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




table_1_y15 <- table1(~., data=table_1_data_recoded %>% 
                    dplyr::select(-one_of(c('ID','time', 'exam_year', 'time_te_in_yrs'))) %>% 
                    dplyr::select(-one_of(c('status')), 'status')
                  , overall="Exam Y15"
                  , render.continuous=my.render.cont, render.categorical=my.render.cat, render.missing=NULL
                  , topclass="Rtable1-grid"
)

table_1_y15_df <- as.data.frame.table1(table_1_y15)


table_1_combined <- cbind(table_1_df[,c(1,4,2)], 'Exam Y15' = table_1_y15_df$`Exam Y15`)

# copy and paste to excel:
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

write.excel(table_1_combined)

#' #'C08DIAB' 'HBP05'
#' var = 'C08KIDNY'
#' table(data_y10[[var]])

