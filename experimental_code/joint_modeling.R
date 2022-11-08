library(joineRML)
library(dplyr)
data(heart.valve)


work_dir = 'U:/Hieu/CARDIA_longi_project'


data_longi_long_for_analysis <- read.csv(paste0(work_dir,'/csv_files/data_longi_long_format_ascvd_risk_factors.csv'))

data_longi_long_y15 <- data_longi_long_for_analysis %>% filter(time_te_in_yrs >15) #exclude instances with events or censored before exam year 15

# split stratified base on data:



jmfit <- joineRML::mjoint(
  formLongFixed = list("CHOL" = CHOL ~ exam_year + MALE + RACEBLACK
                       #,"HDL" = HDL ~ exam_year + MALE + RACEBLACK
                       #,"SBP" = SBP ~ exam_year + MALE + RACEBLACK
                      ),
  formLongRandom = list("CHOL" = ~ exam_year | ID
                        #,"HDL" = ~ exam_year | ID
                        #,"SBP" = ~ exam_year | ID
                        ),
  formSurv = Surv(time_te_in_yrs, status) ~ AGE_Y0+MALE+RACEBLACK+CHOL+DIAB+HBM+HDL+SBP+SMKNW,
  data = list(data_longi_long_y15
              #, data_longi_long_y15
              #, data_longi_long_y15
              ),
  # inits = list("gamma" = c(0.11, 1.51, 0.80)),
  timeVar = "exam_year",
  verbose = TRUE)

patient_oi_data <- droplevels(data_longi_long_y15 %>% filter(ID == '100620205411'))


dynSurv(jmfit, patient_oi_data, u= 30)

dynSurv(fit2, hvd2, u = 7) # survival at 7-years only
out <- dynSurv(fit2, hvd2, type = "simulated")
out


hvd <- heart.valve[!is.na(heart.valve$log.grad) & !is.na(heart.valve$log.lvmi), ]
fit2 <- mjoint(
  formLongFixed = list("grad" = log.grad ~ time + sex + hs,
                       "lvmi" = log.lvmi ~ time + sex),
  formLongRandom = list("grad" = ~ 1 | num,
                        "lvmi" = ~ time | num),
  formSurv = Surv(fuyrs, status) ~ age,
  data = list(hvd, hvd),
  inits = list("gamma" = c(0.11, 1.51, 0.80)),
  timeVar = "time",
  verbose = TRUE)
hvd2 <- droplevels(hvd[hvd$num == 1, ])
dynSurv(fit2, hvd2)
dynSurv(fit2, hvd2, u = 7) # survival at 7-years only
out <- dynSurv(fit2, hvd2, type = "simulated")
out
## End(Not run)






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

