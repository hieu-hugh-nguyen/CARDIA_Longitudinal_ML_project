install.packages("JMbayes2")

library(JMbayes2)
library(dplyr)
library(GLMMadaptive)
#work_dir = 'U:/Hieu/CARDIA_longi_project'
work_dir = '/home/idies/workspace/Storage/hnguye78/persistent/CARDIA_longi_project'
setwd(work_dir)

data_longi_long_for_analysis <- read.csv(paste0(work_dir,'/csv_files/data_longi_long_format_ascvd_risk_factors.csv'))

#exclude instances with events or censored before exam year 15 
data_longi_long_y15 <- data_longi_long_for_analysis %>% filter(time_te_in_yrs >15) 

# only include medical history from y0 to y15:
data_longi_long_up_to_y15 <- data_longi_long_y15 %>% filter(exam_year <=15)

# baseline data:
data_at_baseline <- data_longi_long_up_to_y15 %>% filter(!duplicated(ID, fromLast=FALSE)) 

# most recent data at landmark time (y15):
data_most_recent_by_y15 <- data_longi_long_up_to_y15 %>% filter(!duplicated(ID, fromLast=TRUE))



data_longi_long_y15 <- data_longi_long_for_analysis %>% filter(time_te_in_yrs >15) #exclude instances with events or censored before exam year 15

data_y0 <- data_longi_long_y15 %>% filter(!duplicated(ID))


chol_lme <- nlme:: lme(CHOL ~ exam_year * MALE * RACEBLACK, data = data_longi_long_y15,  random= ~ exam_year | ID)
hdl_lme <- nlme::lme(HDL ~ exam_year * MALE * RACEBLACK, data = data_longi_long_y15,  random= ~ exam_year | ID)
sbp_lme <- nlme:: lme(SBP ~ exam_year * MALE * RACEBLACK, data = data_longi_long_y15,  random= ~ exam_year | ID)

# a mixed effects logistic regression model
smknw <- GLMMadaptive::mixed_model(SMKNW ~ exam_year * MALE * RACEBLACK, data = data_longi_long_y15,
                   random = ~ exam_year | ID, family = binomial())

CoxFit <- coxph(Surv(time_te_in_yrs, status) ~ AGE_Y0+MALE+RACEBLACK+CHOL+DIAB+HBM+HDL+SBP+SMKNW, data = data_y0, model = TRUE)


# the joint model that links all sub-models
jointFit <- jm(CoxFit, list(chol_lme, hdl_lme, sbp_lme), time_var = "exam_year",
               n_iter = 12000L, n_burnin = 2000L, n_thin = 5L)
summary(jointFit)



data_for_pred <- data_longi_long_y15 %>% filter(ID == '100894332119')

predSurv <- predict(jointFit, newdata = data_for_pred, process = "event",
                    times = seq(15, 35, 1),
                    return_newdata = TRUE
                    )

plot(predSurv)


predLong1 <- predict(jointFit, newdata = data_for_pred, 
                     times= seq(15,35,1)
                     ,return_newdata = TRUE)

plot(predLong1, predSurv, outcomes = 1:3)




MixedModelFit <-JMbayes:: mvglmer(list(CHOL ~ exam_year + MALE + RACEBLACK + (exam_year | ID)
                                       , HDL ~ exam_year + MALE + RACEBLACK + (exam_year | ID)
                                       , SBP ~ exam_year + MALE + RACEBLACK + (exam_year | ID)
)

#hepatomegaly ~ year * sex + (year | id))
, data = data_longi_long_for_analysis
, families = list(gaussian
                  ,gaussian
                  ,gaussian))#binomial))


JMFit < -JMbayes::mvJointModelBayes(MixedModelFit, CoxFit, timeVar = "exam_year")





# Example from the package: ##############################################
# a linear mixed model for log serum bilirubin
fm1 <- lme(log(serBilir) ~ year * sex, data = pbc2, random = ~ year | id)

# a linear mixed model for the prothrombin time
fm2 <- lme(prothrombin ~ year * sex, data = pbc2, random = ~ year | id)

# a mixed effects logistic regression model for ascites
fm3 <- mixed_model(ascites ~ year + sex, data = pbc2,
                   random = ~ year | id, family = binomial())


pbc2.id$event <- as.numeric(pbc2.id$status != "alive")
CoxFit_sample <- coxph(Surv(years, event) ~ drug + age, data = pbc2.id)


# the joint model that links all sub-models
jointFit_sample <- jm(CoxFit_sample, list(fm1, fm2, fm3), time_var = "year",
               n_iter = 12000L, n_burnin = 2000L, n_thin = 5L)
summary(jointFit_sample)

t0 <- 5
ND <- pbc2[pbc2$id %in% c(25, 93), ]
ND <- ND[ND$year < t0, ]
ND$status2 <- 0
ND$years <- t0

predLong2 <- predict(jointFit_sample, newdata = ND,
                     times = seq(t0, 12, length.out = 51),
                     return_newdata = TRUE)

predSurv2 <- predict(jointFit_sample, newdata = ND, process = "event",
                    times = seq(t0, 12, length.out = 51),
                    return_newdata = TRUE)

plot(predLong2, predSurv2)




