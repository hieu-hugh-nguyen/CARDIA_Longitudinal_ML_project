# load libraries:
list.of.packages <- c('ggplot2', 'dplyr', 'tibble', 'parallelMap','parallel','JMbayes')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)
parallel::detectCores()
parallelMap::parallelStartSocket(5)

work_dir = 'U:/Hieu/CARDIA_longi_project'


data_longi_long_for_analysis <- read.csv(paste0(work_dir,'/csv_files/data_longi_long_format_ascvd_risk_factors.csv'))

data_longi_long_y15 <- data_longi_long_for_analysis %>% filter(time_te_in_yrs >15) #exclude instances with events or censored before exam year 15


start_time <- Sys.time()
MixedModelFit <-JMbayes:: mvglmer(list(CHOL ~ exam_year + MALE + RACEBLACK + (exam_year | ID)
                                       , HDL ~ exam_year + MALE + RACEBLACK + (exam_year | ID)
                                       , SBP ~ exam_year + MALE + RACEBLACK + (exam_year | ID)
                                       )
                                       
                                        #hepatomegaly ~ year * sex + (year | id))
                                       , data = data_longi_long_y15 
                                       , families = list(gaussian
                                                       ,gaussian
                                                       ,gaussian))#binomial))
mixed_model_runtime <- Sys.time()-start_time

data_exam_0 <- data_longi_long_for_analysis %>% filter(!duplicated(ID))
data_exam_0 <- data_longi_long_y15 %>% filter(!duplicated(ID))
                         
CoxFit <- coxph(Surv(time_te_in_yrs, status) ~ AGE_Y0+MALE+RACEBLACK+CHOL+DIAB+HBM+HDL+SBP+SMKNW, data = data_exam_0, model = TRUE)





start_time <- Sys.time()
JMFit <- JMbayes::mvJointModelBayes(MixedModelFit, CoxFit, timeVar = "exam_year")
jm_model_runtime <- Sys.time()-start_time

data_for_pred <- data_longi_long_y15 %>% filter(ID == '100894332119')

sprobs <- survfitJM(JMFit, data_for_pred, idVar = 'ID')

sprobs
plot(sprobs)


                         