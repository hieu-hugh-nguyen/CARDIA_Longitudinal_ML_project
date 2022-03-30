# or calculate via pec package:
# prob.surv = pec::predictSurvProb(trained.model, newdata = test.data, times = eval.times)


eval_performance2 = function(prob.risk.test.set, test.data, trained.data, eval.times){
  
  prob.risk = prob.risk.test.set
  prob.surv = 1-prob.risk.test.set
  # metrics of evaluation:
  #c-index
  cind.obj = pec::cindex(object = prob.surv,
                         formula = Surv(time, event)~1,
                         data = test.data,
                         eval.times = eval.times, 
                         splitMethod = 'none',
                         cens.model = 'marginal')
  #brier score: 
  pec.obj = pec::pec(object = prob.surv,
                     formula = Surv(time, event)~1,
                     data = test.data,
                     times = eval.times[-base::length(eval.times)],
                     exact = F,
                     splitMethod = 'none',
                     cens.model = 'marginal')
  
  # c-index and Brier at a single time:
  timepoint = 26 # 26 years
  last.brier = sapply(pec.obj$AppErr,
                      function(x) x[which(pec.obj$time == min(365.25*timepoint, max(pec.obj$time)))[1]])
  last.cindex = sapply(cind.obj$AppCindex,
                       function(x) x[which(cind.obj$time == min(365.25*timepoint, max(cind.obj$time)))[1]])
  
  # cumulative dynamic AUC and roc curve:
  require(riskRegression)
  roc = list()
  auc = c()
  for (timept in 1:(base::length(eval.times))){
    # survivalROC.curve= survivalROC(Stime=test.data$time,
    #                                status=test.data$event,
    #                                marker = prob.risk[,timept],
    #                                predict.time = eval.times[timept], method="KM")
    dat = data.frame(event = test.data$event, time = test.data$time, absolute_risk = prob.risk[,timept])
    
    riskRegression.roc = riskRegression::Score(list("model"=dat$absolute_risk)
                                               ,formula=Surv(time,event)~1
                                               ,data=dat
                                               ,conf.int=FALSE
                                               ,times=eval_times[timept]
                                               ,plots = c('ROC','calibration'))
    
    auc = append(auc, riskRegression.roc$AUC$score$AUC)
    roc = append(roc, list(riskRegression.roc))
  }
  # uno c-index:
  require('survAUC')
  # lp = predict(trained.model, type = 'lp')$predicted
  # lp.new = predict(trained.model, newdata = test.data)$predicted
  Surv.rsp = Surv(trained.data$time, trained.data$event)
  Surv.rsp.new = Surv(test.data$time, test.data$event)
  uno_c = survAUC::UnoC(Surv.rsp = Surv.rsp, Surv.rsp.new = Surv.rsp.new
                        ,lpnew = prob.risk[,round(ncol(prob.risk)/2)])
  # uno AUC (similar trend to pec cindex, not exact though?)
  uno_auc = survAUC::AUC.uno(Surv.rsp = Surv.rsp, Surv.rsp.new = Surv.rsp.new
                             ,lpnew = prob.risk[,round(ncol(prob.risk)/2)]
                             ,times = eval.times)
  iauc_uno = uno_auc$iauc
  # save:
 # saving.dir = loading.dir
  performance = list(prob.risk, cind.obj, pec.obj, last.cindex, last.brier, roc, auc, uno_c, uno_auc
                     , iauc_uno)
  names(performance) = c('prob.risk.test.data', 'cind.obj', 'pec.obj', 'last.cindex', 'last.brier', 'roc', 'auc'
                         , 'uno.c', 'uno.auc', 'iauc.uno')
  return(performance)
}


# or calculate via pec package:
# prob.surv = pec::predictSurvProb(trained.model, newdata = test.data, times = eval.times)


eval_performance3_5 = function(prob.risk.test.set, test.data, trained.data, eval.times){
# corrected version of eval_performance3: changing test_data to test.data  
  require(riskRegression)
  require('survAUC')
  require('timeROC')
  
  prob.risk = prob.risk.test.set
  prob.surv = 1-prob.risk.test.set
  # metrics of evaluation:
  #c-index
  cind.obj = pec::cindex(object = prob.surv,
                         formula = Surv(time, event)~1,
                         data = test.data,
                         eval.times = eval.times, 
                         splitMethod = 'none',
                         cens.model = 'marginal')
  #brier score: 
  pec.obj = pec::pec(object = prob.surv,
                     formula = Surv(time, event)~1,
                     data = test.data,
                     times = eval.times[-base::length(eval.times)],
                     exact = F,
                     splitMethod = 'none',
                     cens.model = 'marginal')
  
  # c-index and Brier at a single time:
  timepoint = 26 # 26 years
  last.brier = sapply(pec.obj$AppErr,
                      function(x) x[which(pec.obj$time == min(365.25*timepoint, max(pec.obj$time)))[1]])
  last.cindex = sapply(cind.obj$AppCindex,
                       function(x) x[which(cind.obj$time == min(365.25*timepoint, max(cind.obj$time)))[1]])
  
  # cumulative dynamic AUC and roc curve:

  roc = list()
  auc = c()
  for (timept in 1:(base::length(eval.times))){
    # survivalROC.curve= survivalROC(Stime=test.data$time,
    #                                status=test.data$event,
    #                                marker = prob.risk[,timept],
    #                                predict.time = eval.times[timept], method="KM")
    dat = data.frame(event = test.data$event, time = test.data$time, absolute_risk = prob.risk[,timept])
    
    riskRegression.roc = riskRegression::Score(list("model"=dat$absolute_risk)
                                               ,formula=Surv(time,event)~1
                                               ,data=dat
                                               ,conf.int=FALSE
                                               ,times=eval_times[timept]
                                               ,plots = c('ROC','calibration'))
    
    auc = append(auc, riskRegression.roc$AUC$score$AUC)
    roc = append(roc, list(riskRegression.roc))
  }
  # uno c-index:
  # lp = predict(trained.model, type = 'lp')$predicted
  # lp.new = predict(trained.model, newdata = test.data)$predicted
  Surv.rsp = Surv(trained.data$time, trained.data$event)
  Surv.rsp.new = Surv(test.data$time, test.data$event)
  uno_c = survAUC::UnoC(Surv.rsp = Surv.rsp, Surv.rsp.new = Surv.rsp.new
                        ,lpnew = prob.risk[,round(ncol(prob.risk)/2)])
  # uno AUC (similar trend to pec cindex, not exact though?)
  uno_auc = survAUC::AUC.uno(Surv.rsp = Surv.rsp, Surv.rsp.new = Surv.rsp.new
                             ,lpnew = prob.risk[,round(ncol(prob.risk)/2)]
                             ,times = eval.times)
  iauc_uno = uno_auc$iauc

  
  
  ### PPV, Sens, Spec: ####################
  
  # first compute the vector of survival probabilities for each eval time: 
  
  surv.object <-survfit(Surv(test.data$time,test.data$event)~1) # first find the estimated survival probabilities at unique failure times
  pos <- prodlim::sindex(jump.times=surv.object$time,eval.times=eval_times) # step function to locate the position of survival time matching with an eval_time
  St <- surv.object$surv[pos]
  
  Sens <- rep(NA, length(eval_times))
  Spec <- rep(NA, length(eval_times))
  PPV <- rep(NA, length(eval_times))
  NPV <- rep(NA, length(eval_times))
  F1 <- rep(NA, length(eval_times))
  MCC <- rep(NA, length(eval_times))
  binary_cutpoint <- rep(0.5, length(eval_times))
  
  for (i in 1:length(eval_times)){
    roc_df_curr_time <- roc[[i]][['ROC']]$plotframe
    roc_df_curr_time$youden_index <-  roc_df_curr_time$TPR + 1- roc_df_curr_time$FPR - 1
    best_cutoff_curr_time <- roc_df_curr_time$risk[which(roc_df_curr_time$youden_index == max(roc_df_curr_time$youden_index))][1]
    
    
    # i = 10
    if(!is.na(best_cutoff_curr_time)){
      binary_cutpoint[i]= best_cutoff_curr_time
    }
    classi_metrics_obj <-timeROC::SeSpPPVNPV(cutpoint= binary_cutpoint[i]
                                    , T= test.data$time
                                    , delta=test.data$event
                                    , weighting="marginal"
                                    , marker=prob.risk.test.set[,i]
                                    , cause=1
                                    , times=eval_times[i]
                                    , iid=TRUE)
    
    Sens[i] <- classi_metrics_obj$TP[2] %>% as.numeric()
    Spec[i] <- 1-classi_metrics_obj$FP[2] %>% as.numeric()
    PPV[i] <- classi_metrics_obj$PPV[2] %>% as.numeric()
    NPV[i] <- classi_metrics_obj$NPV[2] %>% as.numeric()
    F1[i] <- 2*(PPV[i]*Sens[i])/(PPV[i]+Sens[i])
    
    TPR <- Sens[i]
    TNR <- Spec[i]
    FNR <- 1- Sens[i] # False Negative Rate
    FPR <- 1- Spec[i] # False Negative Rate
    FDR <- 1-PPV[i] # False Discovery Rate
    FOR <- 1- NPV[i] # False Omission Rate
    MCC[i] <- sqrt(PPV[i]*TPR*TNR*NPV[i]) - sqrt(FDR*FNR*FPR*FOR) #Matthews correlation coefficient
  }    
  
  

  # save:
#  saving.dir = loading.dir
  performance = list(prob.risk, cind.obj, pec.obj, last.cindex, last.brier, roc, auc, uno_c, uno_auc
                     , iauc_uno
                     , Sens, Spec, PPV, NPV, F1
                     , binary_cutpoint
                     , St
                     , Sens[length(Sens)]
                     , Spec[length(Spec)]
                     , PPV[length(PPV)]
                     , NPV[length(NPV)]
                     , F1[length(F1)]
                     , MCC[length(MCC)]
                     )
  names(performance) = c('prob.risk.test.data', 'cind.obj', 'pec.obj', 'last.cindex', 'last.brier', 'roc', 'auc'
                         , 'uno.c', 'uno.auc', 'iauc.uno'
                         , 'Sens_t', 'Spec_t', 'PPV_t', 'NPV_t', 'F1_t'
                         , 'binary_cutpoint', 'survival_prob_testset_t'
                         , 'last.sens', 'last.spec', 'last.ppv', 'last.npv'
                         , 'last.f1', 'last.mcc'
                         )
  return(performance)
}









eval_performance3 = function(prob.risk.test.set, test.data, trained.data, eval.times){

  require(riskRegression)
  require('survAUC')
  require('timeROC')
  
  prob.risk = prob.risk.test.set
  prob.surv = 1-prob.risk.test.set
  # metrics of evaluation:
  #c-index
  cind.obj = pec::cindex(object = prob.surv,
                         formula = Surv(time, event)~1,
                         data = test.data,
                         eval.times = eval.times, 
                         splitMethod = 'none',
                         cens.model = 'marginal')
  #brier score: 
  pec.obj = pec::pec(object = prob.surv,
                     formula = Surv(time, event)~1,
                     data = test.data,
                     times = eval.times[-base::length(eval.times)],
                     exact = F,
                     splitMethod = 'none',
                     cens.model = 'marginal')
  
  # c-index and Brier at a single time:
  timepoint = 26 # 26 years
  last.brier = sapply(pec.obj$AppErr,
                      function(x) x[which(pec.obj$time == min(365.25*timepoint, max(pec.obj$time)))[1]])
  last.cindex = sapply(cind.obj$AppCindex,
                       function(x) x[which(cind.obj$time == min(365.25*timepoint, max(cind.obj$time)))[1]])
  
  # cumulative dynamic AUC and roc curve:
  
  roc = list()
  auc = c()
  for (timept in 1:(base::length(eval.times))){
    # survivalROC.curve= survivalROC(Stime=test.data$time,
    #                                status=test.data$event,
    #                                marker = prob.risk[,timept],
    #                                predict.time = eval.times[timept], method="KM")
    dat = data.frame(event = test.data$event, time = test.data$time, absolute_risk = prob.risk[,timept])
    
    riskRegression.roc = riskRegression::Score(list("model"=dat$absolute_risk)
                                               ,formula=Surv(time,event)~1
                                               ,data=dat
                                               ,conf.int=FALSE
                                               ,times=eval_times[timept]
                                               ,plots = c('ROC','calibration'))
    
    auc = append(auc, riskRegression.roc$AUC$score$AUC)
    roc = append(roc, list(riskRegression.roc))
  }
  # uno c-index:
  # lp = predict(trained.model, type = 'lp')$predicted
  # lp.new = predict(trained.model, newdata = test.data)$predicted
  Surv.rsp = Surv(trained.data$time, trained.data$event)
  Surv.rsp.new = Surv(test.data$time, test.data$event)
  uno_c = survAUC::UnoC(Surv.rsp = Surv.rsp, Surv.rsp.new = Surv.rsp.new
                        ,lpnew = prob.risk[,round(ncol(prob.risk)/2)])
  # uno AUC (similar trend to pec cindex, not exact though?)
  uno_auc = survAUC::AUC.uno(Surv.rsp = Surv.rsp, Surv.rsp.new = Surv.rsp.new
                             ,lpnew = prob.risk[,round(ncol(prob.risk)/2)]
                             ,times = eval.times)
  iauc_uno = uno_auc$iauc
  
  
  
  ### PPV, Sens, Spec: ####################
  
  # first compute the vector of survival probabilities for each eval time: 
  
  surv.object <-survfit(Surv(test_data$time,test_data$event)~1) # first find the estimated survival probabilities at unique failure times
  pos <- prodlim::sindex(jump.times=surv.object$time,eval.times=eval_times) # step function to locate the position of survival time matching with an eval_time
  St <- surv.object$surv[pos]
  
  Sens <- rep(NA, length(eval_times))
  Spec <- rep(NA, length(eval_times))
  PPV <- rep(NA, length(eval_times))
  NPV <- rep(NA, length(eval_times))
  F1 <- rep(NA, length(eval_times))
  MCC <- rep(NA, length(eval_times))
  binary_cutpoint <- rep(NA, length(eval_times))
  
  for (i in 1:length(eval_times)){
    roc_df_curr_time <- roc[[i]][['ROC']]$plotframe
    roc_df_curr_time$youden_index <-  roc_df_curr_time$TPR + 1- roc_df_curr_time$FPR - 1
    best_cutoff_curr_time <- roc_df_curr_time$risk[which(roc_df_curr_time$youden_index == max(roc_df_curr_time$youden_index))][1]
    
    
    # i = 10

    classi_metrics_obj <-timeROC::SeSpPPVNPV(cutpoint= binary_cutpoint[i]
                                             , T= test_data$time
                                             , delta=test_data$event
                                             , weighting="marginal"
                                             , marker=prob.risk.test.set[,i]
                                             , cause=1
                                             , times=eval_times[i]
                                             , iid=TRUE)
    
    Sens[i] <- classi_metrics_obj$TP[2] %>% as.numeric()
    Spec[i] <- 1-classi_metrics_obj$FP[2] %>% as.numeric()
    PPV[i] <- classi_metrics_obj$PPV[2] %>% as.numeric()
    NPV[i] <- classi_metrics_obj$NPV[2] %>% as.numeric()
    F1[i] <- 2*(PPV[i]*Sens[i])/(PPV[i]+Sens[i])
    
    TPR <- Sens[i]
    TNR <- Spec[i]
    FNR <- 1- Sens[i] # False Negative Rate
    FPR <- 1- Spec[i] # False Negative Rate
    FDR <- 1-PPV[i] # False Discovery Rate
    FOR <- 1- NPV[i] # False Omission Rate
    MCC[i] <- sqrt(PPV[i]*TPR*TNR*NPV[i]) - sqrt(FDR*FNR*FPR*FOR) #Matthews correlation coefficient
  }    
  
  
  
  # save:
#  saving.dir = loading.dir
  performance = list(prob.risk, cind.obj, pec.obj, last.cindex, last.brier, roc, auc, uno_c, uno_auc
                     , iauc_uno
                     , Sens, Spec, PPV, NPV, F1
                     , binary_cutpoint
                     , St
                     , Sens[length(Sens)]
                     , Spec[length(Spec)]
                     , PPV[length(PPV)]
                     , NPV[length(NPV)]
                     , F1[length(F1)]
                     , MCC[length(MCC)]
  )
  names(performance) = c('prob.risk.test.data', 'cind.obj', 'pec.obj', 'last.cindex', 'last.brier', 'roc', 'auc'
                         , 'uno.c', 'uno.auc', 'iauc.uno'
                         , 'Sens_t', 'Spec_t', 'PPV_t', 'NPV_t', 'F1_t'
                         , 'binary_cutpoint', 'survival_prob_testset_t'
                         , 'last.sens', 'last.spec', 'last.ppv', 'last.npv'
                         , 'last.f1', 'last.mcc'
  )
  return(performance)
}










eval_performance4 = function(prob.risk.test.set, test.data, trained.data, eval.times){
  
  require(riskRegression)
  require('survAUC')
  require('timeROC')
  
  prob.risk = prob.risk.test.set
  prob.surv = 1-prob.risk.test.set
  # metrics of evaluation:
  #c-index
  cind.obj = pec::cindex(object = prob.surv,
                         formula = Surv(time, event)~1,
                         data = test.data,
                         eval.times = eval.times, 
                         splitMethod = 'none',
                         cens.model = 'marginal')
  #brier score: 
  pec.obj = pec::pec(object = prob.surv,
                     formula = Surv(time, event)~1,
                     data = test.data,
                     times = eval.times[-base::length(eval.times)],
                     exact = F,
                     splitMethod = 'none',
                     cens.model = 'marginal')
  
  # c-index and Brier at a single time:
  timepoint = 26 # 26 years
  last.brier = sapply(pec.obj$AppErr,
                      function(x) x[which(pec.obj$time == min(365.25*timepoint, max(pec.obj$time)))[1]])
  last.cindex = sapply(cind.obj$AppCindex,
                       function(x) x[which(cind.obj$time == min(365.25*timepoint, max(cind.obj$time)))[1]])
  
  # cumulative dynamic AUC and roc curve:
  
  roc = list()
  auc = c()
  for (timept in 1:(base::length(eval.times))){
    # survivalROC.curve= survivalROC(Stime=test.data$time,
    #                                status=test.data$event,
    #                                marker = prob.risk[,timept],
    #                                predict.time = eval.times[timept], method="KM")
    dat = data.frame(event = test.data$event, time = test.data$time, absolute_risk = prob.risk[,timept])
    
    riskRegression.roc = riskRegression::Score(list("model"=dat$absolute_risk)
                                               ,formula=Surv(time,event)~1
                                               ,data=dat
                                               ,conf.int=FALSE
                                               ,times=eval_times[timept]
                                               ,plots = c('ROC','calibration'))
    
    auc = append(auc, riskRegression.roc$AUC$score$AUC)
    roc = append(roc, list(riskRegression.roc))
  }
  # uno c-index:
  # lp = predict(trained.model, type = 'lp')$predicted
  # lp.new = predict(trained.model, newdata = test.data)$predicted
  Surv.rsp = Surv(trained.data$time, trained.data$event)
  Surv.rsp.new = Surv(test.data$time, test.data$event)
  uno_c = survAUC::UnoC(Surv.rsp = Surv.rsp, Surv.rsp.new = Surv.rsp.new
                        ,lpnew = prob.risk[,round(ncol(prob.risk)/2)])
  # uno AUC (similar trend to pec cindex, not exact though?)
  uno_auc = survAUC::AUC.uno(Surv.rsp = Surv.rsp, Surv.rsp.new = Surv.rsp.new
                             ,lpnew = prob.risk[,round(ncol(prob.risk)/2)]
                             ,times = eval.times)
  iauc_uno = uno_auc$iauc
  
  
  
  ### PPV, Sens, Spec: ####################
  
  # first compute the vector of survival probabilities for each eval time: 
  
  surv.object <-survfit(Surv(test_data$time,test_data$event)~1) # first find the estimated survival probabilities at unique failure times
  pos <- prodlim::sindex(jump.times=surv.object$time,eval.times=eval_times) # step function to locate the position of survival time matching with an eval_time
  St <- surv.object$surv[pos]
  
  Sens <- rep(NA, length(eval_times))
  Spec <- rep(NA, length(eval_times))
  PPV <- rep(NA, length(eval_times))
  NPV <- rep(NA, length(eval_times))
  F1 <- rep(NA, length(eval_times))
  MCC <- rep(NA, length(eval_times))
  binary_cutpoint <- rep(NA, length(eval_times))
  
  roc_with_F1 = list()
  
  for (i in 1:length(eval_times)){
    
    roc_df_curr_time <- roc[[i]][['ROC']]$plotframe
    roc_df_curr_time$youden_index <-  roc_df_curr_time$TPR + 1- roc_df_curr_time$FPR - 1
    
    F1_from_ROC <- rep(NA, nrow(roc_df_curr_time))
    MCC_from_ROC <- rep(NA, nrow(roc_df_curr_time))
    PPV_from_ROC <- rep(NA, nrow(roc_df_curr_time))
    NPV_from_ROC <- rep(NA, nrow(roc_df_curr_time))
    
    # Find F1, PPV, NPV for every point on the AUROC curve:
    
    for (cutpt_idx in 1:nrow(roc_df_curr_time)){
      binary_cutpoint_curr <- roc_df_curr_time$risk[cutpt_idx]
      classi_metrics_obj <-timeROC::SeSpPPVNPV(cutpoint= binary_cutpoint_curr
                                               , T= test_data$time
                                               , delta=test_data$event
                                               , weighting="marginal"
                                               , marker=prob.risk.test.set[,i]
                                               , cause=1
                                               , times=eval_times[i]
                                               , iid=TRUE)
      
      Sens_from_ROC <- classi_metrics_obj$TP[2] %>% as.numeric()
      Spec_from_ROC <- 1-classi_metrics_obj$FP[2] %>% as.numeric()
      PPV_from_ROC[cutpt_idx] <- classi_metrics_obj$PPV[2] %>% as.numeric()
      NPV_from_ROC[cutpt_idx] <- classi_metrics_obj$NPV[2] %>% as.numeric()
      F1_from_ROC[cutpt_idx] <- 2*(PPV_from_ROC[cutpt_idx]*Sens_from_ROC)/(PPV_from_ROC[cutpt_idx]+Sens_from_ROC)
      
      TPR <- Sens_from_ROC
      TNR <- Spec_from_ROC
      FNR <- 1- TPR # False Negative Rate
      FPR <- 1- TNR # False Negative Rate
      FDR <- 1-PPV_from_ROC[cutpt_idx] # False Discovery Rate
      FOR <- 1- NPV_from_ROC[cutpt_idx] # False Omission Rate
      MCC_from_ROC[cutpt_idx] <- sqrt(PPV_from_ROC[cutpt_idx]*TPR*TNR*NPV_from_ROC[cutpt_idx]) - sqrt(FDR*FNR*FPR*FOR) #Matthews correlation coefficient

    }
    
    roc_df_curr_time$PPV <-  PPV_from_ROC
    roc_df_curr_time$NPV <- NPV_from_ROC
    roc_df_curr_time$F1 <- F1_from_ROC
    roc_df_curr_time$MCC <- F1_from_ROC
    
    
    # return Sens, Specs, PPV, etc. corresponding to the cutoff with the highest F1-score on the AUROC curve:
    roc_df_curr_time_na_omit <- roc_df_curr_time %>% na.omit()
    best_cutoff_curr_time <- roc_df_curr_time_na_omit$risk[which(roc_df_curr_time_na_omit$F1== max(roc_df_curr_time_na_omit$F1))][1]
    
    binary_cutpoint[i]= best_cutoff_curr_time

    Sens[i] <- roc_df_curr_time_na_omit$TPR[which(roc_df_curr_time_na_omit$F1== max(roc_df_curr_time_na_omit$F1))][1] %>% as.numeric()
    Spec[i] <- 1-roc_df_curr_time_na_omit$FPR[which(roc_df_curr_time_na_omit$F1== max(roc_df_curr_time_na_omit$F1))][1] %>% as.numeric()
    PPV[i] <- roc_df_curr_time_na_omit$PPV[which(roc_df_curr_time_na_omit$F1== max(roc_df_curr_time_na_omit$F1))][1] %>% as.numeric()
    NPV[i] <- roc_df_curr_time_na_omit$NPV[which(roc_df_curr_time_na_omit$F1== max(roc_df_curr_time_na_omit$F1))][1] %>% as.numeric()
    F1[i] <- max(roc_df_curr_time_na_omit$F1)
    MCC[i] <- roc_df_curr_time_na_omit$MCC[which(roc_df_curr_time_na_omit$F1== max(roc_df_curr_time_na_omit$F1))][1] %>% as.numeric() #Matthews correlation coefficient
    
    
    roc_with_F1 <- append(roc_with_F1, list(roc_df_curr_time))    
  }    
  
  
  
  # save:
  #saving.dir = loading.dir
  performance = list(prob.risk, cind.obj, pec.obj, last.cindex, last.brier, roc, auc, uno_c, uno_auc
                     , iauc_uno
                     , Sens, Spec, PPV, NPV, F1, MCC, roc_with_F1
                     , binary_cutpoint
                     , St
                     , Sens[length(Sens)]
                     , Spec[length(Spec)]
                     , PPV[length(PPV)]
                     , NPV[length(NPV)]
                     , F1[length(F1)]
                     , MCC[length(MCC)]
  )
  names(performance) = c('prob.risk.test.data', 'cind.obj', 'pec.obj', 'last.cindex', 'last.brier', 'roc', 'auc'
                         , 'uno.c', 'uno.auc', 'iauc.uno'
                         , 'Sens_t', 'Spec_t', 'PPV_t', 'NPV_t', 'F1_t', 'MCC_t', 'roc_with_F1_t'
                         , 'binary_cutpoint_t', 'survival_prob_testset_t'
                         , 'last.sens', 'last.spec', 'last.ppv', 'last.npv'
                         , 'last.f1', 'last.mcc'
  )
  return(performance)
}