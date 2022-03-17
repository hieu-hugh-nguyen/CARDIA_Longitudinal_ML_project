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

### rsf-tsfresh-plus-y15-data:  ######

auc_df_from_performance_testset <- function(model_name, nfolds =10){
  fold =1
  filename = paste0(work_dir, '/rdata_files/',model_name,'_fold_', fold,'/performance_testset.RData')
  auc_df <-  data.frame(fold_1 = get(load(filename))$auc)
  
  for (fold in 3:nfolds){
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

# rsf_tsfresh_plus_data_y15_auc_df <- auc_df_from_performance_testset('rsf_ascvd_var_tsfeatures_plus_data_y15')
# 
# rsf_tsfresh_plus_data_y15_auc_df_rm_corr <- auc_df_from_performance_testset('rsf_ascvd_var_tsfeatures_plus_data_y15_rm_correlation')
# 
# rsf_tsfresh_auc_df_rm_corr <- auc_df_from_performance_testset('rsf_ascvd_var_tsfeatures_plus_data_y15_rm_correlation')
# 
# lasso_tsfresh_auc_df_rm_corr <-auc_df_from_performance_testset('lasso_ascvd_var_tsfeatures_rm_correlation')
# 
# cForest_tsfresh_auc_df_rm_corr <-auc_df_from_performance_testset('cForest_ascvd_var_tsfeatures_rm_correlation')
# 
# cox_y15_auc_df <- auc_df_from_performance_testset('cox_ascvd_var_y15', nfolds = 25)
# 
# rsf_y15_auc_df <- auc_df_from_performance_testset('rsf_ascvd_var_y15')
#   
# cox_y0_auc_df <- auc_df_from_performance_testset('cox_ascvd_var_baseline')
# 
# rsf_y0_auc_df <- auc_df_from_performance_testset('rsf_ascvd_var_baseline')
# 
# cox_y0_auc_df_no_truncate <- auc_df_from_performance_testset('cox_ascvd_var_baseline_no_truncate')
# 
# rsf_y0_auc_df_no_truncate <- auc_df_from_performance_testset('rsf_ascvd_var_baseline_no_truncate')
# 
# ascvd_y15_auc_df <- auc_df_from_performance_testset('ascvd_risk_score_y15')
# 
# ascvd_y0_auc_df_no_truncate <- auc_df_from_performance_testset('ascvd_risk_score_y0_no_truncate')
# 
# dynamic_deephit_untuned_auc_df_v2 <- auc_df_from_performance_testset('dynamic_deephit_ascvd_var_y15')
# 
# jmbayes_auc_df_v2 <- auc_df_from_performance_testset('jmbayes_ascvd_var_y15')
# 
# cForest_y15_auc_df <- auc_df_from_performance_testset('cForest_ascvd_var_y15')
# 
# cox_y15_auc_df_exp <- auc_df_from_performance_testset('cox_ascvd_var_y15_exp')
# 
# 
# cForest_tsfresh_expanded_var_df <- auc_df_from_performance_testset('cForest_expanded_var_tsfeatures_plus_data_y15_rm_correlation') 
# rsf_tsfresh_expanded_var_df <- auc_df_from_performance_testset('rsf_expanded_var_tsfeatures_corr_rm')
# 
# rsf_y15_expand_var_df <- auc_df_from_performance_testset('rsf_expanded_var_y15')
# cox_y15_expand_var_df <- auc_df_from_performance_testset('cox_expanded_var_y15')
# 
# cox_y0_expand_var_df <- auc_df_from_performance_testset('cox_expanded_var_baseline_no_truncate')
# 
# dynamic_deephit_tuned_expanded_var_df <- auc_df_from_performance_testset('dynamic_deephit_expanded_var_y15_4_RNN_layers')
# 
# rsf_tsfresh_expanded_var_df <- auc_df_from_performance_testset('rsf_expanded_var_and_ascvd_var_tsfeatures_plus_data_y15')



rsf_tsfresh_expanded_var_df_2 <- auc_df_from_performance_testset('rsf_expanded_var_and_ascvd_var_tsfeatures_plus_data_y15_2_4')
# rsf_tsfresh_expanded_var_df_2_1 <- auc_df_from_performance_testset('rsf_expanded_var_and_ascvd_var_tsfeatures_plus_data_y15_2_4_10000tree')

cForest_tsfresh_expanded_var_df_2 <- auc_df_from_performance_testset('cForest_expanded_var_tsfeatures_plus_data_y15_rm_correlation_2')

dynamic_deephit_tuned_expanded_var_df_2 <- auc_df_from_performance_testset('dynamic_deephit_expanded_var_y15_2_2')

cox_traj_expand_var_df <- auc_df_from_performance_testset('cox_expanded_var_traj_plus_data_y15')
rsf_traj_expand_var_df <- auc_df_from_performance_testset('rsf_expanded_var_traj_plus_data_y15')
lasso_traj_expand_var_df <- auc_df_from_performance_testset('lasso_expanded_var_traj_plus_data_y15')
cForest_traj_expand_var_df <- auc_df_from_performance_testset('cForest_expanded_var_traj_plus_data_y15')

rsf_traj_expand_var_df_gap <- auc_df_from_performance_testset('rsf_expanded_var_traj_plus_data_y15_gap')
rsf_traj_expand_var_df_gap_3 <- auc_df_from_performance_testset('rsf_expanded_var_traj_plus_data_y15_gap_3')
rsf_traj_expand_var_df_gap_2 <- auc_df_from_performance_testset('rsf_expanded_var_traj_plus_data_y15_gap_2')

rsf_traj_only_expand_var_df_gap <- auc_df_from_performance_testset('rsf_expanded_var_traj_only_gap')
rsf_traj_trcovw_df <- auc_df_from_performance_testset('rsf_expanded_var_traj_only_trcovw_4')
cox_traj_trcovw_df <- auc_df_from_performance_testset('cox_expanded_var_traj_only_trcovw_4')

rsf_traj_trcovw_plus_y15_data_df_4 <- auc_df_from_performance_testset('rsf_expanded_var_traj_trcovw_plus_some_y15_data_4')
rsf_traj_trcovw_plus_y15_data_df_5 <- auc_df_from_performance_testset('rsf_expanded_var_traj_trcovw_plus_some_y15_data_5')
rsf_traj_trcovw_plus_y15_data_df_6 <- auc_df_from_performance_testset('rsf_expanded_var_traj_trcovw_plus_some_y15_data_6')

rsf_traj_cont_n_binary_var_only_trcovw <- auc_df_from_performance_testset('rsf_expanded_var_traj_cont_n_binary_var_only_trcovw')
# rsf_traj_cont_n_binary_var_only_trcovw_2 <- auc_df_from_performance_testset('rsf_expanded_var_traj_cont_n_binary_var_only_trcovw_2')
# cox_traj_cont_n_binary_var_only_trcovw <- auc_df_from_performance_testset('cox_expanded_var_traj_cont_n_binary_var_only_trcovw')
# lasso_traj_cont_n_binary_var_only_trcovw <- auc_df_from_performance_testset('lasso_expanded_var_traj_cont_n_binary_var_only_trcovw')

# rsf_y15_expand_var_df_2 <- auc_df_from_performance_testset('rsf_expanded_var_y15_2')
rsf_y15_expand_var_df_2_1 <- auc_df_from_performance_testset('rsf_expanded_var_y15_2_1')

cox_y15_expand_var_df_2 <- auc_df_from_performance_testset('cox_expanded_var_y15_2')
cForest_y15_expand_var_df_2 <- auc_df_from_performance_testset('cForest_expanded_var_y15_2')
cox_y0_expand_var_df_2 <- auc_df_from_performance_testset('cox_expanded_var_baseline_no_truncate_2')


rsf_concat_expand_var_df <- auc_df_from_performance_testset('rsf_expanded_var_concat')
# rsf_concat_expand_var_df_ntree_100 <- auc_df_from_performance_testset('rsf_expanded_var_concat_ntree_100')



# ### ASCVD risk score:  ######
# fold =1
# filename = paste0(work_dir, '/rdata_files/ascvd_risk_score_y0_no_truncate_fold_', fold,'/c_ascvd_testset.RData')
# ascvd_auc_df <-  data.frame(fold_1 = get(load(filename)))
# 
# for (fold in 2:nfolds){
#   filename = paste0(work_dir, '/rdata_files/ascvd_risk_score_y0_no_truncate_fold_', fold,'/c_ascvd_testset.RData')
#   if(file.exists(filename)){
#     auc_curr_fold <-as.numeric(get(load(filename)))
#     auc_curr_fold[is.nan(auc_curr_fold)] <- as.numeric(NA)
# 
#     ascvd_auc_df[[paste0('fold_',fold)]] <- auc_curr_fold
#   }
# }
# ascvd_auc_df[is.na(ascvd_auc_df)] <- as.numeric(NA)
# ascvd_auc_df$mean = rowMeans(ascvd_auc_df, na.rm = T)
# ascvd_auc_df$median = apply(ascvd_auc_df[,1:(length(ascvd_auc_df)-1)], 1, median, na.rm=T)



eval_times <- seq(1,17, 1)

df_for_plot = data.frame(eval_times
                        # ,rsf_tsfresh_auc_df_rm_corr$median
                         #,rsf_tsfresh_plus_data_y15_auc_df_rm_corr$median
                        , rsf_tsfresh_expanded_var_df_2$median

                        
                      #, cForest_tsfresh_expanded_var_df_2$median

                        , dynamic_deephit_tuned_expanded_var_df_2$median
                        
                      , rsf_concat_expand_var_df$median
                      
                    #  , rsf_traj_expand_var_df_gap$median
                     # , rsf_traj_trcovw_df$median 
                     
                     # , rsf_traj_trcovw_plus_y15_data_df_6$median
                     , rsf_traj_cont_n_binary_var_only_trcovw$median
                      #, rsf_traj_only_expand_var_df_gap$median

                     # , rsf_traj_expand_var_df_gap_2$median
                      # , rsf_traj_expand_var_df_gap_3$median
                      #, cForest_traj_expand_var_df$median
                        #, lasso_traj_expand_var_df$median

                       # ,cForest_tsfresh_expanded_var_df$median 
                        #,rsf_tsfresh_expanded_var_df$median
                        #,rsf_tsfresh_expanded_var_df$median
                        
                        # ,cForest_tsfresh_auc_df_rm_corr$median
                         
                         #,rsf_tsfresh_auc_df$median
                         #,rsf_tsfresh_plus_data_y15_auc_df$median
                         
                         #,dynamic_deephit_untuned_auc_df_v2$median
                         
                       # ,cForest_y15_expand_var_df_2$median 
                        ,rsf_y15_expand_var_df_2_1$median  
                        ,cox_y15_expand_var_df_2$median
                        
                        , cox_y0_expand_var_df_2$median 
                        
                        #,cox_y15_auc_df$mean

                         # ,cox_y15_auc_df_exp$median
                         #,rsf_y15_auc_df$median
                         # ,cForest_y15_auc_df$median
                        
              
                         
                         
                        #,cox_y0_auc_df$median
                       #  ,rsf_y0_auc_df$median
                         #,lasso_tsfresh_auc_df_rm_corr$median
                       
                        # ,jmbayes_auc_df_v2$median                         
                        # ,ascvd_auc_df$median
                         
                         #,cox_y0_auc_df_no_truncate$median
                         #,rsf_y0_auc_df_no_truncate$median
                         # ,ascvd_y0_auc_df_no_truncate$median
                         )


 
                    




names(df_for_plot) = c('eval_times'
                        ,'RSF on longitudinal derived features' # \n of 35 variables' #'and Static Y15 Data'     
                       #, 'cForest'
                       , 'Dynamic-Deephit'
                       , 'RSF on concatenated data'
                      # , 'RSF on trajectory clustering data plus Y15 data'
                       , 'RSF on trajectory clustering data'
                       
                       #'RSF on clustered data gap'
                       
                       # ,'cForest on clustered data'

                        
                        #,'RSF-TS'
                        #,'RSF on TS-derived features' #'RSF-TS and Static Y15 Data'
                        #,'cForest on TS-derived features \n of 9 traditional risk factors' #'and Static Y15 Data'
                       # ,'cForest on Y15 Data'
                       ,'RSF on Y15 data'
                        ,'Cox on Y15 data'
                        
#                        ,'RSF Y15'
 #                       ,'cForest Y15'
                        #,'LASSO-Cox on TS-derived features' # and Static Y15 Data'
                        #,'Joint Modeling Bayesian'
                      
                        ,'Cox on Y0 data'
                        #,'RSF Y0'
                        #                        ,'ASCVD Risk Score Y15'
)



df_for_plot2 <- df_for_plot %>% filter(eval_times %in%(seq(5,17,1)))

df_for_plot.long = melt(df_for_plot2, id = 'eval_times')

names(df_for_plot.long) = c('time','Model','quant')



auc.plot = ggplot(data = df_for_plot.long)

color_scheme <- c(
  "black", "firebrick3", "goldenrod3", "forestgreen", "dodgerblue3", "darkmagenta", "hotpink3", "steelblue4", "mediumpurple3","thistle4",
  "indianred4", "yellow4", "mediumseagreen"
)
# color_scheme <- c(
#   #"royalblue", 
#   "blue", "dodgerblue", "deepskyblue", "forestgreen", "olivedrab", "sienna")
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
  ylim(0.68, 0.90) + 
  theme_minimal() + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=15,face="italic"),
        legend.title = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 14)) 
 # geom_vline(xintercept=10)



# Save figure in high resolution:
auc.plot +
  # geom_line(aes( time, quant, color = Model, group = Model), size = 1.3)+
  geom_point(aes( time, quant, color = Model, group = Model), size = 1.2, alpha = 0.2) +
  geom_smooth(aes( time, quant, color = Model, group = Model), size = 1.2, se = FALSE, method = 'loess', span = 1)+ #usual lowess span = 1
  # geom_smooth(aes( time, quant, color = Model, group = Model), size = 1.3, se = FALSE, method = lm, formula = y ~ x)+
  
  # geom_smooth(aes( time, quant, color = Model, group = Model), size = 1.3, se = FALSE, method = lm, formula = y ~ splines::bs(x, 1))+
  scale_color_manual(values = color_scheme[1:(ncol(df_for_plot)-1)]) +
  xlab("Years After Exam 6 (Exam Y15)") +
  ylab("Median AUC") +
  scale_x_continuous(breaks=seq(5,17,2))+
  ylim(0.68, 0.90) + 
  theme_minimal() + 
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16,face="italic"),
        legend.title = element_text(color = "black", size = 16),
        legend.text = element_text(color = "black", size = 15),
        legend.justification = c(1, 1), legend.position = c(1, 1)
  ) 
# geom_vline(xintercept=10)

ggsave(filename = 'median_AUC_over_time_figure.tif', path = paste0(work_dir, '/figures')
       , units = 'in', width = 7.5, height = 6.5
       , device = 'tiff', dpi = 300)














eval_times <- seq(1,17, 1)

df_for_plot = data.frame(eval_times
                       
                         , rsf_tsfresh_expanded_var_df_2$mean
                         , dynamic_deephit_tuned_expanded_var_df_2$mean
                         
                         , rsf_concat_expand_var_df$mean
                         
                         , rsf_traj_cont_n_binary_var_only_trcovw$mean
                          ,rsf_y15_expand_var_df_2_1$mean  
                         ,cox_y15_expand_var_df_2$mean
                         
                         , cox_y0_expand_var_df_2$mean 
                         
                       
)








names(df_for_plot) = c('eval_times'
                       ,'RSF on longitudinal derived features' # \n of 35 variables' #'and Static Y15 Data'     
                       #, 'cForest'
                       , 'Dynamic-Deephit'
                       , 'RSF on concatenated data'
                       # , 'RSF on trajectory clustering data plus Y15 data'
                       , 'RSF on trajectory clustering data'
                       
                       #'RSF on clustered data gap'
                       
                       # ,'cForest on clustered data'
                       
                       
                       #,'RSF-TS'
                       #,'RSF on TS-derived features' #'RSF-TS and Static Y15 Data'
                       #,'cForest on TS-derived features \n of 9 traditional risk factors' #'and Static Y15 Data'
                       # ,'cForest on Y15 Data'
                       ,'RSF on Y15 data'
                       ,'Cox on Y15 data'
                       
                       #                        ,'RSF Y15'
                       #                       ,'cForest Y15'
                       #,'LASSO-Cox on TS-derived features' # and Static Y15 Data'
                       #,'Joint Modeling Bayesian'
                       
                       ,'Cox on Y0 data'
                       #,'RSF Y0'
                       #                        ,'ASCVD Risk Score Y15'
)



df_for_plot2 <- df_for_plot %>% filter(eval_times %in%(seq(5,17,1)))

df_for_plot.long = melt(df_for_plot2, id = 'eval_times')

names(df_for_plot.long) = c('time','Model','quant')



auc.plot = ggplot(data = df_for_plot.long)

color_scheme <- c(
  "black", "firebrick3", "goldenrod3", "forestgreen", "dodgerblue3", "darkmagenta", "hotpink3", "steelblue4", "mediumpurple3","thistle4",
  "indianred4", "yellow4", "mediumseagreen"
)
# color_scheme <- c(
#   #"royalblue", 
#   "blue", "dodgerblue", "deepskyblue", "forestgreen", "olivedrab", "sienna")
auc.plot +
  # geom_line(aes( time, quant, color = Model, group = Model), size = 1.3)+
  geom_point(aes( time, quant, color = Model, group = Model), size = 1.3, alpha = 0.2) +
  geom_smooth(aes( time, quant, color = Model, group = Model), size = 1.3, se = FALSE, method = 'loess', span = 1)+ #usual lowess span = 1
  # geom_smooth(aes( time, quant, color = Model, group = Model), size = 1.3, se = FALSE, method = lm, formula = y ~ x)+
  
  # geom_smooth(aes( time, quant, color = Model, group = Model), size = 1.3, se = FALSE, method = lm, formula = y ~ splines::bs(x, 1))+
  scale_color_manual(values = color_scheme[1:(ncol(df_for_plot)-1)]) +
  xlab("Years After Exam 6 (Exam Year 15)") +
  ylab("mean AUC") +
  scale_x_continuous(breaks=seq(5,17,2))+
  ylim(0.68, 0.90) + 
  theme_minimal() + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=15,face="italic"),
        legend.title = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 14)) 
# geom_vline(xintercept=10)



# Save figure in high resolution:
auc.plot +
  # geom_line(aes( time, quant, color = Model, group = Model), size = 1.3)+
  geom_point(aes( time, quant, color = Model, group = Model), size = 1.2, alpha = 0.2) +
  geom_smooth(aes( time, quant, color = Model, group = Model), size = 1.2, se = FALSE, method = 'loess', span = 1)+ #usual lowess span = 1
  # geom_smooth(aes( time, quant, color = Model, group = Model), size = 1.3, se = FALSE, method = lm, formula = y ~ x)+
  
  # geom_smooth(aes( time, quant, color = Model, group = Model), size = 1.3, se = FALSE, method = lm, formula = y ~ splines::bs(x, 1))+
  scale_color_manual(values = color_scheme[1:(ncol(df_for_plot)-1)]) +
  xlab("Years After Exam 6 (Exam Y15)") +
  ylab("mean AUC") +
  scale_x_continuous(breaks=seq(5,17,2))+
  ylim(0.68, 0.90) + 
  theme_minimal() + 
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16,face="italic"),
        legend.title = element_text(color = "black", size = 16),
        legend.text = element_text(color = "black", size = 15),
        legend.justification = c(1, 1), legend.position = c(1, 1)
  ) 
# geom_vline(xintercept=10)

ggsave(filename = 'mean_AUC_over_time_figure.tif', path = paste0(work_dir, '/figures')
       , units = 'in', width = 7.5, height = 6.5
       , device = 'tiff', dpi = 300)
