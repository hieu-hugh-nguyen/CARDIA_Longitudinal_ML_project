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
parallelMap::parallelStartSocket(5)

# source snippet functions:
source_dir <- 'U:/Hieu/CARDIA_project/CARDIA_project'
source(paste0(source_dir,'/Git/code/snippet/createDir.R'))
source(paste0(source_dir,'/Git/code/snippet/subsetDataTopnVar.R'))

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
write.csv(longi_data, file = paste0(saving.dir,'/longi_data_avalability_dictionary.csv'), row.names = F)


# remove questionnaire variables, retain lab-based and reading center variables:

longi_data %>% filter(exam_year == '0') %>% dplyr::select('Datadoc') %>% unique()
View(longi_data %>% filter(exam_year == '0') %>% arrange(Datadoc)) 
lab_y0 <- c('a5chem', 'a4cot', 'a4ins', 'a4lip','a4hcy','a4hcy'
            ,'b2lip','b2hemo','b2leptin'
            ,'c1echo','c1fibr','c1lip','c1lpa','c1apob','c1cartda','c1gmp','c1hemo'
            ,'d1apoe','d1crp','d1glu','d1ins','d1lip','d1dexa','d1fibr','d1hcy','d1hemo','d1leptin','d1proins'
            ,'e1chem','e1glu','e1ins','e1lip','e1micro','e1apob','e1catech','e1ebct','e1echy05','e1echy10','e1leptin','e1renald'
            ,'f1chem','f1ebctadj','f1ins','f1isop','f1lip','f1micro','f2crp','f2ebct','f2glu','f2catech','f2hcy','f2hrv','f2ige','f2igfil','f2ucort','f3scort'
            ,'g3adipoq','g3cartd','g3chem','g3crp','g3ebct','g3fibr','g3glu','g3il6','g3ins','g3lip','g3micro','g3pcsk9','g3snps','g3cfssnp2','g3cfssnp3','g3cfssnp4','g3cfssnp5','g3cfssnp','g4actigraph','g4dxa'
            ,'h2chem','h2crp','h2ebct_abdomen','h2echo','h2glu','h2ins','h2lip','h2micro','2ebct_chest','h3hba1c','h4mri'
            ,'i3hba1c')



bp_datadoc <- c('a4f02','b2f02','c1f02','d1f02','e1f02','f1f02','e1f02','g3f02','h3f02v2','i1f02')
anthro_datadoc <-c ('a4f20') #anthropometry


y0_lab_data <- longi_data %>% filter(Datadoc %in% dataset_considered_y0)


# plus traditional risk factors, bp, and anthropometries
 #bp

# Others: # socioeconomic status: a4f01 
# a4f08 a4f09 # medical history


# ascvd risk factors:

#demographic 
a4ref %>% select(RACE,EXAMAGE)

#a4f01 %>% select(A01SMNOW)
#a4f08 %>% select(A08DIAB)
#a4f08 %>% select(A08BPMED)




dataset$sex = ifelse(dataset$SEX == 1, "Male", "Female")
dataset$race = ifelse(dataset$RACE == 5, "White", "Black")
dataset$age = dataset$CALCULATED.AGE.AT.EXAM.3

dataset$currentsmoker = ifelse(dataset$A01SMNOW == 2, 1, 0)
dataset$hbp.medication = ifelse(dataset$CURRENTLY.TAKING.HBP.MEDICATION >= 1.5, 1, 0)
dataset$diabetes.status = ifelse(dataset$DIABETES >= 1.5, 1, 0)


dataset$sbp = dataset$SYSTOLIC.BLOOD.PRESSURE

bp_variables <- c('A02SBP B02AVGSY C02AVGSY D02AVGSY E02AVGSY F02AVGSY G02SAVG H02SAVG I02SAVG')



dataset$total.choles = dataset$TOTAL.CHOLESTEROL..MG.DL.
dataset$hdl.choles = dataset$TOTAL.HDL.CHOLESTEROL..MG.DL.


