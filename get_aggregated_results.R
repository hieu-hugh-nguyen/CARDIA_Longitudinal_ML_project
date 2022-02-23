rm(list=ls()) #Clear all
cat("\014")

work_dir = 'U:/Hieu/CARDIA_longi_project'

require('matrixStats')
require(dplyr)

source(paste0(work_dir,'/code/git_code/snippet/bootstrap_ci.R'))

setwd(work_dir)
nfold = 10
endpt = 17
eval_times = seq(1,endpt,1)




model_list = c(
  ## model_name = 

  # , 'rsf_expanded_var_y15' 
  # , 'cForest_expanded_var_y15'
  # , 'cox_expanded_var_y15'
  # , 'lasso_expanded_var_y15'
  
   'rsf_expanded_var_baseline_no_truncate'
   , 'cox_expanded_var_baseline_no_truncate'
   , 'lasso_expanded_var_baseline_no_truncate'
   
  
  # 'rsf_ascvd_var_tsfeatures_plus_data_y15_rm_correlation'
  # , 'cForest_ascvd_var_tsfeatures_rm_correlation'
  # , 'lasso_ascvd_var_tsfeatures_rm_correlation'
  # , 'dynamic_deephit_ascvd_var_y15'
  # , 'jmbayes_ascvd_var_y15'
  # , 'rsf_ascvd_var_y15'
  # , 'cox_ascvd_var_y15'
  # , 'rsf_ascvd_var_baseline_no_truncate'
  # , 'cox_ascvd_var_baseline_no_truncate'
   )

agg_perf_and_ci <- function(model_name, ndigits = 3){
  for(fold in 1:nfold){
    ## fold = 1
    loading.dir = paste0(work_dir, '/rdata_files/',model_name,'_fold_', fold)
    
    if (fold ==1){
      performance.all = list()
    }  
    tryCatch({
    performance = list((get(load(paste0(loading.dir,'/performance_testset.RData')))))
    performance.all = append(performance.all, performance)
    }, error=function(e){cat("WARNING :",conditionMessage(e), "fold ", fold, "\n")})
  }
  
  # extract each metric: 
  performance.report.static = list()
  performance.report.static = within(performance.report.static,
    {last.cindex.all = c();last.brier.all = c(); uno.c.all = c()
    ; iauc.uno.all = c();last.auc.all = c();
    ; last.sens.all = c(); last.spec.all = c();
    ; last.ppv.all = c(); last.npv.all = c();
    ; last.f1.all = c(); last.mcc.all = c();
    
    }
  )
  performance.report.dynamic= list()
  performance.report.dynamic = within(performance.report.dynamic,
                                 {dynamic.auc.all = c(); dynamic.auc.uno.all = c(); 
                                 dynamic.cindex.all = c(); 
                                 brier.all = c()
                                 }
  )
  
  for(fold in 1:length(performance.all)){
    performance.report.static = within(performance.report.static, {
      last.cindex.all = append(last.cindex.all, unlist(performance.all[[fold]]$last.cindex))
      names(last.cindex.all) = NULL                          
      last.brier.all = append(last.brier.all, unlist(performance.all[[fold]]$last.brier['matrix'])) 
      names(last.brier.all) = NULL
      uno.c.all = append(uno.c.all, unlist(performance.all[[fold]]$uno.c))
      iauc.uno.all = append(iauc.uno.all, unlist(performance.all[[fold]]$iauc.uno))
      last.auc.all = append(last.auc.all, unlist(performance.all[[fold]]$auc[length(performance.all[[fold]]$auc)]))
      last.sens.all = append(last.sens.all, unlist(performance.all[[fold]]$last.sens))
      last.spec.all = append(last.spec.all, unlist(performance.all[[fold]]$last.spec))
      last.ppv.all = append(last.ppv.all, unlist(performance.all[[fold]]$last.ppv))
      last.npv.all = append(last.npv.all, unlist(performance.all[[fold]]$last.npv))
      last.f1.all = append(last.f1.all, unlist(performance.all[[fold]]$last.f1))
      last.mcc.all = append(last.mcc.all, unlist(performance.all[[fold]]$last.mcc))
      
      
    })
    performance.report.dynamic = within(performance.report.dynamic, {
      dynamic.auc.all = rbind(dynamic.auc.all, performance.all[[fold]]$auc)
      dynamic.auc.uno.all = rbind(dynamic.auc.uno.all, performance.all[[fold]]$uno.auc$auc)
      #dynamic.auc.cd.all = rbind(dynamic.auc.cd.all, performance.all[[fold]]$cd.auc$auc)
      dynamic.cindex.all = rbind(dynamic.cindex.all, performance.all[[fold]]$cind.obj$AppCindex$matrix)
      brier.all = rbind(brier.all, performance.all[[fold]]$pec.obj$AppErr$matrix)
    })  
  }
  
  # #performance.report.dynamic$dynamic.cindex.all
  # saving.dir = paste0(work_dir,'/rdata_files')
  # save(performance.report.static, performance.report.dynamic
  #      ,file = paste0(saving.dir,'/performance_',model_name,'.RData'))
  
  
  
  
  # visualization:
  loading.dir = paste0(work_dir,'/rdata_files')

  static.summary = lapply(performance.report.static, summary)
  require('matrixStats')
  dynamic.summary = lapply(performance.report.dynamic, matrixStats::colMedians)
  
  last.brier = static.summary$last.brier.all
  
  
  plot(eval_times[3:length(eval_times)], dynamic.summary$dynamic.auc.uno.all[3:length(eval_times)], type = 'l'
       ,ylim = c(0.5, 1))
  lines(eval_times[3:length(eval_times)], dynamic.summary$dynamic.cindex.all[3:length(eval_times)], col = 'red')
  lines(eval_times[3:length(eval_times)], dynamic.summary$dynamic.auc.all[3:length(eval_times)], col = 'blue')

  
  
  ### Get CI of the model performance in 4 metrics: ################### 
  ### last C-index, AUC, iAUC.uno, and Brier Score
  
  require(dplyr)
  loading.dir = paste0(work_dir,'/rdata_files')
  
  model.cindex = bootstrap_ci(na.omit(performance.report.static$last.cindex.all)) 
  model.auc = bootstrap_ci(na.omit(performance.report.static$last.auc.all))
  model.auc.uno = bootstrap_ci(na.omit(performance.report.static$uno.c.all))
  model.brier = bootstrap_ci(na.omit(performance.report.static$last.brier.all))
  model.iauc.uno = bootstrap_ci(na.omit(performance.report.static$iauc.uno.all))
  model.sens = bootstrap_ci(na.omit(performance.report.static$last.sens.all))
  model.spec = bootstrap_ci(na.omit(performance.report.static$last.spec.all))
  model.ppv = bootstrap_ci(na.omit(performance.report.static$last.ppv.all))
  model.npv = bootstrap_ci(na.omit(performance.report.static$last.npv.all))
  model.f1 = bootstrap_ci(na.omit(performance.report.static$last.f1.all))
  model.mcc = bootstrap_ci(na.omit(performance.report.static$last.mcc.all))
  

  

  print_value <- function(ci_output, digits = 2){
    return(paste0(round(ci_output[2], digits = digits),' (',
                 ci_output[1] %>% round(digits = digits),', ',
                 ci_output[3] %>% round(digits = digits),')'))
  }
  
  
  ci_df = data.frame(model.iauc.uno, model.cindex, model.auc, model.brier, model.auc.uno
                     , model.sens, model.spec, model.ppv, model.npv, model.f1, model.mcc)
  ci_written_out = c(print_value(model.iauc.uno, digits =ndigits)
                  , print_value(model.cindex, digits =ndigits)
                  , print_value(model.auc, digits =ndigits)
                  , print_value(model.brier, digits =ndigits)
                  , print_value(model.auc.uno, digits =ndigits)
                  , print_value(model.sens, digits =ndigits)
                  , print_value(model.spec, digits =ndigits)
                  , print_value(model.ppv, digits =ndigits)
                  , print_value(model.npv, digits =ndigits)
                  , print_value(model.f1, digits =ndigits)
                  , print_value(model.mcc, digits =ndigits)
                  )
  
  ci_df_written_out = rbind(ci_df, ci_written_out)
  
  print(paste0('Performance of model ', model_name,' :'))
  print(ci_df_written_out %>% dplyr::slice(4))
  cat('\n')
  
  saving.dir = paste0(work_dir,'/rdata_files')
  write.csv(ci_df_written_out
       ,file = paste0(saving.dir, '/ci_performance_', model_name,'.csv'), row.names = F)
  return(ci_df_written_out)
  
  
}




for(n in 1:length(model_list)){
  agg_perf_and_ci(model_list[n], ndigits = 3)  
}
