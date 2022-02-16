rm(list=ls()) #Clear all
cat("\014")

work_dir = 'U:/Hieu/CARDIA_longi_project'
require(dplyr)
require(ggplot2)
require(reshape2)
require(matrixStats)


nfolds = 10


# extract mean c-index values:
loading.dir = paste0(work_dir,'/rdata_files')



eval_time_points = 17

# 
# ### jmbayes:  ######
# fold =1
# filename = paste0(work_dir, '/rdata_files/jmbayes_ascvd_var_y15_fold_', fold,'/auc_over_time_testset.RData')
# jmbayes_auc_df <-  data.frame(fold_1 = get(load(filename)))
# 
# for (fold in 2:nfolds){
#   filename = paste0(work_dir, '/rdata_files/jmbayes_ascvd_var_y15_fold_', fold,'/auc_over_time_testset.RData')
#   if(file.exists(filename)){
#     auc_curr_fold <-as.numeric(get(load(filename)))
#     auc_curr_fold[is.nan(auc_curr_fold)] <- as.numeric(NA)
#     
#     jmbayes_auc_df[[paste0('fold_',fold)]] <- auc_curr_fold
#   }
# }
# jmbayes_auc_df[is.na(jmbayes_auc_df)] <- as.numeric(NA)
# jmbayes_auc_df$mean = rowMeans(jmbayes_auc_df, na.rm = T)
# jmbayes_auc_df$median = apply(jmbayes_auc_df[,1:(length(jmbayes_auc_df)-1)], 1, median, na.rm=T)
# #jmbayes_auc_df$median = matrixStats::rowMedians(jmbayes_auc_df, na.rm = T)
# 
# 
# 
# 
# 
# ### dynamic-deephit:  ######
# 
# fold =1
# filename = paste0(work_dir, '/rdata_files/dynamic_deephit_ascvd_var_y15_fold_', fold,'/c_index.csv')
# dynamic_deephit_auc_df <-  data.frame(fold_1 = read.csv((filename))[[1]])
# 
# for (fold in 2:nfolds){
#   filename = paste0(work_dir, '/rdata_files/dynamic_deephit_ascvd_var_y15_fold_', fold,'/c_index.csv')
#   if(file.exists(filename)){
#     auc_curr_fold <-as.numeric(read.csv((filename))[[1]])
#     auc_curr_fold[is.nan(auc_curr_fold)] <- as.numeric(NA)
#     if(length(auc_curr_fold)>eval_time_points){
#       auc_curr_fold = auc_curr_fold[(length(auc_curr_fold)-eval_time_points+1) : length(auc_curr_fold)]
#     }
#     dynamic_deephit_auc_df[[paste0('fold_',fold)]] <- auc_curr_fold
#   }
# }
# dynamic_deephit_auc_df[is.na(dynamic_deephit_auc_df)] <- as.numeric(NA)
# dynamic_deephit_auc_df$mean = rowMeans(dynamic_deephit_auc_df, na.rm = T)
# dynamic_deephit_auc_df$median = apply(dynamic_deephit_auc_df[,1:(length(dynamic_deephit_auc_df)-1)], 1, median, na.rm=T)
# 
# 
# 
# 
# ### rsf-tsfresh:  ######
# 
# fold =1
# filename = paste0(work_dir, '/rdata_files/rsf_ascvd_var_tsfeatures_fold_', fold,'/performance_testset.RData')
# rsf_tsfresh_auc_df <-  data.frame(fold_1 = get(load(filename))$auc)
# 
# for (fold in 2:nfolds){
#   filename = paste0(work_dir, '/rdata_files/rsf_ascvd_var_tsfeatures_fold_', fold,'/performance_testset.RData')
# #  print(paste0('Fold ', fold))
#   if(file.exists(filename)){
#     auc_curr_fold <-as.numeric(get(load(filename))$auc)
#     auc_curr_fold[is.nan(auc_curr_fold)] <- as.numeric(NA)
#     if(length(auc_curr_fold)>eval_time_points){
#       auc_curr_fold = auc_curr_fold[(length(auc_curr_fold)-eval_time_points+1) : length(auc_curr_fold)]
#     }
#     rsf_tsfresh_auc_df[[paste0('fold_',fold)]] <- auc_curr_fold
#   }
# }
# rsf_tsfresh_auc_df[is.na(rsf_tsfresh_auc_df)] <- as.numeric(NA)
# rsf_tsfresh_auc_df$mean = rowMeans(rsf_tsfresh_auc_df, na.rm = T)
# rsf_tsfresh_auc_df$median = apply(rsf_tsfresh_auc_df[,1:(length(rsf_tsfresh_auc_df)-1)], 1, median, na.rm=T)
# 
# 


### rsf-tsfresh-plus-y15-data:  ######

auc_df_from_performance_testset <- function(model_name, nfolds =10){
  fold =1
  filename = paste0(work_dir, '/rdata_files/',model_name,'_fold_', fold,'/performance_testset.RData')
  auc_df <-  data.frame(fold_1 = get(load(filename))$auc)
  
  for (fold in 2:nfolds){
    filename = paste0(work_dir, '/rdata_files/',model_name,'_fold_', fold,'/performance_testset.RData')
    #  print(paste0('Fold ', fold))
    if(file.exists(filename)){
      auc_curr_fold <-as.numeric(get(load(filename))$auc)
      auc_curr_fold[is.nan(auc_curr_fold)] <- as.numeric(NA)
      if(length(auc_curr_fold)>eval_time_points){
        auc_curr_fold = auc_curr_fold[(length(auc_curr_fold)-eval_time_points+1) : length(auc_curr_fold)]
      }
      auc_df[[paste0('fold_',fold)]] <- auc_curr_fold
    }
  }
  auc_df[is.na(auc_df)] <- as.numeric(NA)
  auc_df$mean = rowMeans(auc_df, na.rm = T)
  auc_df$median = apply(auc_df[,1:(length(auc_df)-1)], 1, median, na.rm=T)
  return(auc_df)  
}

rsf_tsfresh_plus_data_y15_auc_df <- auc_df_from_performance_testset('rsf_ascvd_var_tsfeatures_plus_data_y15')

rsf_tsfresh_plus_data_y15_auc_df_rm_corr <- auc_df_from_performance_testset('rsf_ascvd_var_tsfeatures_plus_data_y15_rm_correlation')

rsf_tsfresh_auc_df_rm_corr <- auc_df_from_performance_testset('rsf_ascvd_var_tsfeatures_plus_data_y15_rm_correlation')

lasso_tsfresh_auc_df_rm_corr <-auc_df_from_performance_testset('lasso_ascvd_var_tsfeatures_rm_correlation')

cForest_tsfresh_auc_df_rm_corr <-auc_df_from_performance_testset('cForest_ascvd_var_tsfeatures_rm_correlation')

cox_y15_auc_df <- auc_df_from_performance_testset('cox_ascvd_var_y15', nfolds = 25)

rsf_y15_auc_df <- auc_df_from_performance_testset('rsf_ascvd_var_y15')
  
cox_y0_auc_df <- auc_df_from_performance_testset('cox_ascvd_var_baseline')

rsf_y0_auc_df <- auc_df_from_performance_testset('rsf_ascvd_var_baseline')

cox_y0_auc_df_no_truncate <- auc_df_from_performance_testset('cox_ascvd_var_baseline_no_truncate')

rsf_y0_auc_df_no_truncate <- auc_df_from_performance_testset('rsf_ascvd_var_baseline_no_truncate')

ascvd_y15_auc_df <- auc_df_from_performance_testset('ascvd_risk_score_y15')

ascvd_y0_auc_df_no_truncate <- auc_df_from_performance_testset('ascvd_risk_score_y0_no_truncate')

dynamic_deephit_untuned_auc_df_v2 <- auc_df_from_performance_testset('dynamic_deephit_ascvd_var_y15')

jmbayes_auc_df_v2 <- auc_df_from_performance_testset('jmbayes_ascvd_var_y15')


### ASCVD risk score:  ######
fold =1
filename = paste0(work_dir, '/rdata_files/ascvd_risk_score_y0_no_truncate_fold_', fold,'/c_ascvd_testset.RData')
ascvd_auc_df <-  data.frame(fold_1 = get(load(filename)))

for (fold in 2:nfolds){
  filename = paste0(work_dir, '/rdata_files/ascvd_risk_score_y0_no_truncate_fold_', fold,'/c_ascvd_testset.RData')
  if(file.exists(filename)){
    auc_curr_fold <-as.numeric(get(load(filename)))
    auc_curr_fold[is.nan(auc_curr_fold)] <- as.numeric(NA)

    ascvd_auc_df[[paste0('fold_',fold)]] <- auc_curr_fold
  }
}
ascvd_auc_df[is.na(ascvd_auc_df)] <- as.numeric(NA)
ascvd_auc_df$mean = rowMeans(ascvd_auc_df, na.rm = T)
ascvd_auc_df$median = apply(ascvd_auc_df[,1:(length(ascvd_auc_df)-1)], 1, median, na.rm=T)



eval_times <- seq(1,17, 1)

df_for_plot = data.frame(eval_times
                        # ,rsf_tsfresh_auc_df_rm_corr$median
                         #,rsf_tsfresh_plus_data_y15_auc_df_rm_corr$median
                         ,cForest_tsfresh_auc_df_rm_corr$median
                         
                         #,rsf_tsfresh_auc_df$median
                         #,rsf_tsfresh_plus_data_y15_auc_df$median
                         
                         #,dynamic_deephit_untuned_auc_df_v2$median
                         
                         ,cox_y15_auc_df$mean
                         #,rsf_y15_auc_df$median
              
                         
                         
                        ,cox_y0_auc_df$median
                       #  ,rsf_y0_auc_df$median
                         #,lasso_tsfresh_auc_df_rm_corr$median
                       
                        # ,jmbayes_auc_df_v2$median                         
                        # ,ascvd_auc_df$median
                         
                         #,cox_y0_auc_df_no_truncate$median
                         #,rsf_y0_auc_df_no_truncate$median
                         # ,ascvd_y0_auc_df_no_truncate$median
                         )


 
                    
df_for_plot2 <- df_for_plot %>% filter(eval_times %in%(seq(5,17,1)))




names(df_for_plot2) = c('eval_times'
                        #,'RSF-TS'
                        #,'RSF on TS-derived features' #'RSF-TS and Static Y15 Data'
                        ,'cForest on TS-derived features' #'and Static Y15 Data'
                        #,'Dynamic-Deephit'
                        ,'Cox Y15'
                        #,'RSF Y15'
                        #,'LASSO-Cox on TS-derived features' # and Static Y15 Data'
                        #,'Joint Modeling Bayesian'
                      
                        ,'Cox Y0'
                        #,'RSF Y0'
                        #                        ,'ASCVD Risk Score Y15'
)


df_for_plot.long = melt(df_for_plot2, id = 'eval_times')

names(df_for_plot.long) = c('time','Model','quant')




auc.plot = ggplot(data = df_for_plot.long)

color_scheme <- c(
  "black", "firebrick3", "goldenrod3", "forestgreen", "dodgerblue3", "darkmagenta", "hotpink3", "steelblue4", "mediumpurple3","thistle4",
  "indianred4", "yellow4", "mediumseagreen"
)
auc.plot +
  # geom_line(aes( time, quant, color = Model, group = Model), size = 1.3)+
   geom_point(aes( time, quant, color = Model, group = Model), size = 1.3, alpha = 0.2) +
   geom_smooth(aes( time, quant, color = Model, group = Model), size = 1.3, se = FALSE, method = 'loess', span = 1)+ #usual lowess span = 1
  # geom_smooth(aes( time, quant, color = Model, group = Model), size = 1.3, se = FALSE, method = lm, formula = y ~ x)+
  
  # geom_smooth(aes( time, quant, color = Model, group = Model), size = 1.3, se = FALSE, method = lm, formula = y ~ splines::bs(x, 1))+
  scale_color_manual(values = color_scheme[1:(ncol(df_for_plot)-1)]) +
  xlab("Years After Exam 6 (Exam Year 15)") +
  ylab("Median AUC") +
  scale_x_continuous(breaks=seq(5,17,2))+
  ylim(0.65, 0.9) + 
  theme_minimal() + 
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=18,face="italic"),
        legend.title = element_text(color = "black", size = 18),
        legend.text = element_text(color = "black", size = 18)) 
 # geom_vline(xintercept=10)
