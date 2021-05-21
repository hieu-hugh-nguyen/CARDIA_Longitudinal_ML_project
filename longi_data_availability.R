cat("\014")

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
  
  longi_data <- rbind(longi_data, var_dict[,1:7])                          
  assign( paste0('var_dict_Y',as.character(exam_years[i])),var_dict)
                            
}


longi_data <- longi_data %>% mutate(varname_longi = ifelse(grepl('_',substr(longi_data$varname_longi, 1, 1))
                                                           , substr(longi_data$varname_longi, 2, length(longi_data$varname_longi))
                                                           , longi_data$varname_longi)) %>%
  arrange(varname_longi) 


saving.dir = file.path(work_dir,'csv_files')
write.csv(longi_data, file = paste0(saving.dir,'/longi_data_avalability_dictionary2.csv'), row.names = F)
