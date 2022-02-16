
rm(list=ls()) #Clear all
cat("\014")

# set working directory: 
work_dir = 'U:/Hieu/CARDIA_longi_project'
setwd(work_dir)

# load data containing coefficients of ASCVD Pooled Equation
load_dir= paste0(work_dir,'/ASCVDRiskScoreCalculatorInR/GoldStandardRiskScoreCalculation')
dat<-read.table(paste0(load_dir, "/table.dat"), row.names=1)




# load the dataset
loading_dir = paste0(work_dir, '/csv_files')

data_longi_long_for_analysis <- read.csv(paste0(work_dir,'/csv_files/data_longi_long_format_ascvd_risk_factors_removed_missing_data.csv'))
#'/csv_files/data_longi_long_format_ascvd_risk_factors_with_missing_data.csv'
#
subjects_in_cohort <- read.csv(paste0(work_dir,'/csv_files/subjects_in_final_analysis_cohort.csv'))

data_longi_long_up_to_y15 <- data_longi_long_for_analysis %>% filter(exam_year <=15)
data_longi_analysis_cohort <- data_longi_long_up_to_y15 %>% filter(ID %in% subjects_in_cohort[[1]])

# baseline data:
# data_at_baseline <- data_longi_long_for_analysis %>% filter(!duplicated(ID, fromLast=FALSE)) 
data_at_baseline <- data_longi_analysis_cohort %>% filter(ID %in% subjects_in_cohort[[1]]) %>% filter(exam_year == 0)
# most recent data at landmark time (y15):
data_y15 <- data_longi_analysis_cohort %>% filter(ID %in% subjects_in_cohort[[1]]) %>% filter(exam_year == 15)


# No truncate time 
data_baseline_no_truncate_tte <- data_at_baseline %>% 
#  mutate(time_te_in_yrs = time_te_in_yrs -15) %>% 
  dplyr::select(-time) %>% filter(time_te_in_yrs >0) %>%
  rename(event = status) %>% rename(time = time_te_in_yrs) %>%
  dplyr::select(-exam_year)




data <- data_baseline_no_truncate_tte


#Check if there is any character column, then delete them to make sure all data is numeric:
nums <- unlist(lapply(data, is.character))  
data[,nums]<-NULL



# loading.dir = "C:/Users/HIEU/Desktop/CARDIA project/Y5/ComprehensiveExcelfilesWithLABELS"
# dataset <- read.csv(file = paste0(loading.dir, "/Fast_Impute_Y5_BigDataSheet.csv"))

library(dplyr)

dataset <- data %>% mutate(sex = ifelse(MALE == 1, "Male", "Female")) %>% dplyr::select(-one_of('MALE')) %>%
  mutate(race = ifelse(RACEBLACK, "Black", "White")) %>% dplyr::select(-one_of('RACEBLACK')) %>%
  rename(currentsmoker = SMKNW ) %>%
  rename(hbp.medication = HBM) %>%
  rename(sbp = SBP) %>%
  rename(total.choles = CHOL) %>%
  rename(hdl.choles = HDL) %>%
  rename(diabetes.status = DIAB) %>%
  rename(age = AGE_Y0)






ascvd.sheet = NULL 

for( i in 1:nrow(dataset)){
datarow <- dataset[i,]

     if(datarow[,'sex'] =="Male"){
       ifelse(datarow[,'race'] =="White", category <- 3, category <-4)
     }else{
       ifelse(datarow[,'race'] =="White", category <- 1, category <-2)}

  
  # smokc<-reactive({if("Current smoker," %in% input$checkGroup) const()$smoker else const()$nonsmoker})
  # smokcov<-reactive({if("Current smoker," %in% input$checkGroup) 1 else 0})
  # BPc<-reactive({if("Treated systolic blood pressure," %in% input$checkGroup) const()$ln_treated_BP else const()$ln_untreated_BP})
  # BPcov<-reactive({if("Treated systolic blood pressure," %in% input$checkGroup) const()$ln_age_BP else const()$ln_age_ln_untreated_BP})
  # diab<-reactive({if("Diabetes," %in% input$checkGroup) const()$diabetes else const()$nondiabetes})
  # meancoef<-reactive({const()$meancoef  })
  
  ifelse(datarow[,'hbp.medication'] == 1, coef.bp <- 'ln_treated_BP', coef.bp <- 'ln_untreated_BP')
  ifelse(datarow[,'hbp.medication'] == 1, coef.bpage <- 'ln_age_BP', coef.bpage <- 'ln_age_ln_untreated_BP')
  ifelse(datarow[,'currentsmoker'] == 1, coef.smoke <- 'smoker', coef.smoke <- 'nonsmoker')
  ifelse(datarow[,'diabetes.status'] == 1, coef.dia <- 'diabetes', coef.dia <- 'nondiabetes')
  
  coef.df <- dat[category,]
  sum.coef.value <-  (coef.df$ln_age*log(datarow[,'age'])+
                      coef.df$ln_age_squared*log(datarow[,'age'])*log(datarow[,'age'])+
                      coef.df$ln_total_cholest*log(datarow[, 'total.choles'])+
                      coef.df$ln_age_totcholest*log(datarow[, 'total.choles'])*log(datarow[,'age'])+
                      coef.df$ln_hdlC*log(datarow[,'hdl.choles'])+
                      coef.df$ln_age_hdlC*log(datarow[,'hdl.choles'])*log(datarow[,'age'])+
                      coef.df[,coef.bp]*log(datarow[,'sbp'])+
                      coef.df[,coef.bpage]*log(datarow[,'sbp'])*log(datarow[,'age'])+              
                      coef.df[,coef.smoke]+
                      coef.df$ln_age_smoker*log(datarow[,'age'])*datarow[,'currentsmoker']+
                      coef.df[,coef.dia])
      
  # calc<-reactive({
  #     log(input$numage)*const()$ln_age+log(input$numage)*log(input$numage)*const()$ln_age_squared+
  #     +log(input$numTC)*const()$ln_total_cholest+log(input$numage)*log(input$numTC)*const()$ln_age_totcholest+
  #     +log(input$hdl)*const()$ln_hdlC+log(input$numage)*log(input$hdl)*const()$ln_age_hdlC+smokc()+
  #     +smokcov()*log(input$numage)*const()$ln_age_smoker+log(input$numBP)*BPc()+log(input$numage)*log(input$numBP)*BPcov()+
  #     +diab() })

   ascvd<- (1-(coef.df$baseline^exp(sum.coef.value-coef.df$meancoef)))
   
   
   output.df <- data.frame(ascvd = ascvd, sex = datarow$sex, race = datarow$race, age = datarow$age
                           ,cholesterol = datarow$total.choles, hdl = datarow$hdl.choles, sbp = datarow$sbp
                           ,hbp.medication = datarow$hbp.medication,smoker = datarow$currentsmoker, diabetes = datarow$diabetes.status)     
   
   ascvd.sheet = rbind(ascvd.sheet,output.df)
}

library(tibble)
ascvd.sheet.with.id = add_column(ascvd.sheet, ID = data$ID, .before = 1)

saving.dir = paste0(work_dir, '/csv_files')
write.csv(ascvd.sheet.with.id, paste0(saving.dir,"/ascvd_calc_with_id_y0.csv"), row.names = F)