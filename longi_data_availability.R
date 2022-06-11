# Take input from variable dictionary from each year, combine them all together and arrange to make a dictionary of longitudinal variables, 
# one for all variables, one for all non-questionnaire variables, and one for ascvd risk factors


cat("\014")
rm(list=ls()) #Clear all
# set working directory: 
work_dir = 'U:/Hieu/CARDIA_longi_project'
setwd(work_dir)

# load libraries:
list.of.packages <- c('ggplot2','caret', 'dplyr', 'tibble', 'ROCR','parallelMap'
                      ,'riskRegression', 'survival','survivalROC'
                      , 'pec', 'risksetROC', 'parallel')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)

parallel::detectCores()
# parallelMap::parallelStartSocket(5)

# source snippet functions:
source_dir <- 'U:/Hieu/CARDIA_longi_project'
source(paste0(source_dir,'/code/git_code/snippet/createDir.R'))
source(paste0(source_dir,'/code/git_code/snippet/subsetDataTopnVar.R'))

# load var dicts:
exam_years <- c(0, 2, 5, 7, 10, 15, 20, 25, 30)
exam_char_code <- c('A', 'B', 'D', 'E', 'F', 'G', 'H', 'I')


longi_data <- tibble()
for (i in 1:length(exam_years)){
  var_dict <- read.csv(paste0(work_dir,'/csv_files/Y', exam_years[i], '/Y', exam_years[i],'_all_vars_dictionary.csv'))
  names(var_dict)[1:4] <- c('Variable.Name', 'Variable.Label', 'Num.non.missing.participants', 'categorical_or_continuous')
  
  # calculate percentage of non-missing for each variable:
  var_dict <- var_dict %>% mutate(non_missing_per = Num.non.missing.participants/max(Num.non.missing.participants %>% na.omit))
  
  # # varname conversion: make consistent varname to combine across exam years
  # If varname contains a letter and two number in the first three letters, delete the first three letters:
  var_dict <- var_dict %>% mutate(varname_longi = ifelse(grepl('[0-9]', var_dict$Variable.Name %>% substr(2,3))  
                                                         , substr(var_dict$Variable.Name,4, length(var_dict$Variable.Name))
                         , var_dict$Variable.Name)) %>%
    mutate(exam_year = exam_years[i]) %>%
    dplyr::select(exam_year, varname_longi, non_missing_per, Variable.Name, everything())
  
  longi_data <- rbind(longi_data, var_dict[,1:ncol(var_dict)])                          
  assign( paste0('var_dict_Y',as.character(exam_years[i])),var_dict)
                            
}


longi_data <- longi_data %>% mutate(varname_longi = ifelse(grepl('_',substr(longi_data$varname_longi, 1, 1))
                                                           , substr(longi_data$varname_longi, 2, length(longi_data$varname_longi))
                                                           , longi_data$varname_longi)) %>%
  arrange(varname_longi) 


saving.dir = file.path(work_dir,'csv_files')
# # write.csv(longi_data, file = paste0(saving.dir,'/longi_data_avalability_dictionary.csv'), row.names = F)


# remove questionnaire variables, retain lab-based and reading center variables:

lab_datadoc <- c('a5chem', 'a4cot', 'a4ins', 'a4lip','a4hcy','a4hcy'
            ,'b2lip','b2hemo','b2leptin'
            ,'c1fibr','c1lip','c1lpa','c1apob','c1gmp','c1hemo'
            ,'d1apoe','d1crp','d3glu','d1ins','d1lip','d1fibr','d1hcy','d1hemo','d1leptin','d1proins'
            ,'e2chem','e3glu','e1ins','e1lip','e1micro','e1apob','e1catech','e1leptin','e1renald'
            ,'f2chem','f1ins','f1isop','f1lip','f1micro','f2crp','f2glu','f2catech','f2hcy','f2hrv','f2ige','f2igfil','f2ucort','f3scort'
            ,'g3lip','g4chem','g3crp','g3fibr','g5glu','g3il6','g3ins','g3micro' 
            ,'h3chem','h3crp','h3glu','h4ins','h3lip','h3micro','h3hba1c'
            ,'i2chem','i3glu','i3ins','i2lip','i2micro','i3hba1c')
img_datadoc <- c('c1echo','c1cartda'
                 ,'d1dexa'
                 ,'e1ebct','e1echy05','e1echy10'
                 ,'f1ebctadj','f2ebct'
                 ,'g3cartd','g3ebct'
                 ,'h4ebct','h10echo', 'h7echo','h4mri'
                 ,'i5echo','i4mri'
                 )
# dexa = DUAL ENERGY X-RAY ABSORPTIOMETRY
# cartda = carotid ultrasound


bp_datadoc <- c('a4f02','b2f02','c1f02','d1f02','e1f02','f1f02','e1f02','g3f02','h3f02v2','i1f02')
anthro_datadoc <-c ('a4f20','b2f20','c1f20','d1f20a','e1f20','f2f20','g3f20','h3f20', 'i1f20') #anthropometry
medical_his_datadoc <- c('a4f08','a4f09med','b2f08','b2f09mhb','c1f08','d1f08a','e1f08','f1f08','g3f08','h3f08','i1f08')
smoking_datadoc <- c('a4f09tob','b2f09tob','c1f09tob','d1f9toba','e1f09tob','f1f09tob','g3f09tob','h3f09tob','i1f09tob')
race_age_edu_datadoc <- c('a4f01') # c('b3ref')
sex_verified_datadoc <- c('a4f01') # c('b3ref') # the datadoc from Y0 (a4f09gen) only contains 4008/5115 non-missing values, hence use datadoc from Y2

# Others: # socioeconomic status: a4f01 
# a4f08 a4f09 # medical history


longi_useful_data <- longi_data %>% filter(Datadoc %in% c(race_age_edu_datadoc, sex_verified_datadoc
                                                          ,lab_datadoc, img_datadoc, bp_datadoc
                                                          ,anthro_datadoc, medical_his_datadoc
                                                          ,smoking_datadoc)) %>%
  mutate(var_group = case_when(Datadoc %in% lab_datadoc ~ "lab"
                               ,Datadoc %in% img_datadoc ~ "imaging"
                               ,Datadoc %in% bp_datadoc ~ "blood_pressure"
                               ,Datadoc %in% anthro_datadoc ~ "anthropometry"
                               ,Datadoc %in% medical_his_datadoc ~ "medical_history"
                               ,Datadoc %in% smoking_datadoc ~ "smoking"
                               ,Datadoc %in% race_age_edu_datadoc ~ "demographics"
                               ,Datadoc %in% sex_verified_datadoc ~ "demographics"
                               )) %>%
  arrange(var_group,varname_longi,exam_year)


saving.dir = file.path(work_dir,'csv_files')
# write.csv(longi_useful_data, file = paste0(saving.dir,'/longi_data_avalability_dictionary_no_questionnaire2.csv'), row.names = F)




# filter ascvd risk factors only:
bp_variables <- c('A02SBP','B02AVGSY','C02AVGSY','D02AVGSY','E02AVGSY','F02AVGSY','G02SAVG','H02SAVG','I02SAVG')

longi_ascvd_var_dict <- longi_useful_data %>% filter(varname_longi %in% c('RACE1','SEX','AGE1'
                                                                          ,'DIAB','SMKNW'
                                                                          ,'CHOL','HDL')) %>% 
                                              rbind(longi_useful_data %>% filter(Variable.Name %in% bp_variables)
                                                    %>% mutate(varname_longi = "SBP")) %>%
                                              rbind(longi_useful_data %>% filter(varname_longi %in% c('MDNOW','HBNOW'))
                                                    %>% mutate(varname_longi = "HBM")) %>%
                                              arrange(varname_longi,exam_year)


saving.dir = file.path(work_dir,'csv_files')
# write.csv(longi_ascvd_var_dict, file = paste0(saving.dir,'/longi_data_avalability_dictionary_ascvd_risk_factors2.csv'), row.names = F)


#A09MDNOW
#demographic 
# b3ref %>% select(RACE,EXAMAGE)

# 
#a4f01 %>% select(A01SMNOW)
#a4f08 %>% select(A08DIAB)
#a4f08 %>% select(A08BPMED)




# dataset$sex = ifelse(dataset$SEX == 1, "Male", "Female")
# dataset$race = ifelse(dataset$RACE == 5, "White", "Black")
# dataset$age = dataset$CALCULATED.AGE.AT.EXAM.3
# 
# dataset$currentsmoker = ifelse(dataset$A01SMNOW == 2, 1, 0)
# dataset$hbp.medication = ifelse(dataset$CURRENTLY.TAKING.HBP.MEDICATION >= 1.5, 1, 0)
# dataset$diabetes.status = ifelse(dataset$DIABETES >= 1.5, 1, 0)
# 


