rm(list=ls()) #Clear all
cat("\014")

work_dir = 'U:/Hieu/CARDIA_longi_project'
require(dplyr)
require(ggplot2)
require(reshape2)






# extract mean c-index values:
loading.dir = paste0(work_dir,'/rdata_files')

fold =1

jmbayes_auc <-  get(load(paste0(work_dir, '/rdata_files/jmbayes_ascvd_var_y15_fold_', fold,'/auc_over_time_testset.RData')))

dynamic_deephit_auc <- read.csv(paste0(work_dir, '/rdata_files/dynamic_deephit_ascvd_var_fold_', fold,'/dynamic_deephit_c_over_time.csv'))
dynamic_deephit_auc <- dynamic_deephit_auc[2:nrow(dynamic_deephit_auc),2]

cox_baseline_performance_testset <- get(load(paste0(work_dir, '/rdata_files/cox_ascvd_var_baseline_fold_', fold,'/performance_testset.RData')))
cox_baseline_auc <- cox_baseline_performance_testset$auc

rsf_baseline_performance_testset <- get(load(paste0(work_dir, '/rdata_files/rsf_ascvd_var_baseline_fold_', fold,'/performance_testset.RData')))
rsf_baseline_auc <- rsf_baseline_performance_testset$auc

rsf_tsfresh_performance_testset <- get(load(paste0(work_dir, '/rdata_files/rsf_ascvd_var_tsfeatures_fold_', fold,'/performance_testset.RData')))
rsf_tsfresh_auc <- rsf_tsfresh_performance_testset$auc

rsf_tsfresh_plus_data_y15_performance_testset <- get(load(paste0(work_dir, '/rdata_files/rsf_ascvd_var_tsfeatures_plus_data_y15_fold_', fold,'/performance_testset.RData')))
rsf_tsfresh_plus_data_y15_auc <- rsf_tsfresh_plus_data_y15_performance_testset$auc


cox_y15_performance_testset <- get(load(paste0(work_dir, '/rdata_files/cox_ascvd_var_y15_fold_', fold,'/performance_testset.RData')))
cox_y15_auc1 <- cox_y15_performance_testset$auc

rsf_y15_performance_testset <- get(load(paste0(work_dir, '/rdata_files/rsf_ascvd_var_y15_fold_', fold,'/performance_testset.RData')))
rsf_y15_auc1 <- rsf_y15_performance_testset$auc

cox_y15_performance_testset <- get(load(paste0(work_dir, '/rdata_files/cox_ascvd_var_y15_fold_2/performance_testset.RData')))
cox_y15_auc2 <- cox_y15_performance_testset$auc

rsf_y15_performance_testset <- get(load(paste0(work_dir, '/rdata_files/rsf_ascvd_var_y15_fold_2/performance_testset.RData')))
rsf_y15_auc2 <- rsf_y15_performance_testset$auc

cox_y15_performance_testset <- get(load(paste0(work_dir, '/rdata_files/cox_ascvd_var_y15_fold_3/performance_testset.RData')))
cox_y15_auc3 <- cox_y15_performance_testset$auc

rsf_y15_performance_testset <- get(load(paste0(work_dir, '/rdata_files/rsf_ascvd_var_y15_fold_3/performance_testset.RData')))
rsf_y15_auc3 <- rsf_y15_performance_testset$auc


rsf_y15_auc <- rowMeans(data.frame(rsf_y15_auc1,rsf_y15_auc2,rsf_y15_auc3))
cox_y15_auc <- rowMeans(data.frame(cox_y15_auc1,cox_y15_auc2,cox_y15_auc3))

eval_times <- seq(1,18, 1)
df_for_plot = data.frame(eval_times
                         
                         ,rsf_tsfresh_auc
                         ,rsf_tsfresh_plus_data_y15_auc
                         
                         
                         ,cox_y15_auc
                         ,rsf_y15_auc
                         ,dynamic_deephit_auc
                         
                         
                         ,cox_baseline_auc
                         ,rsf_baseline_auc
                         
                         ,jmbayes_auc
                         
                         )

# 
# names(df_for_plot) = c('eval.times'
#                        ,'Nnet-survival Top 20 Variables'
#                        ,'Nnet-survival All Variables'
#                        ,'RSF All Variables'
#                        ,'RSF Top 20 Variables'
#                        ,'cForest ASCVD Variables'
#                        ,'ASCVD Risk Score (Benchmark)')
                         
                    

df_for_plot.long = melt(df_for_plot, id = 'eval_times')

names(df_for_plot.long) = c('time','Model','quant')




auc.plot = ggplot(data = df_for_plot.long)

color_scheme <- c(
  "black", "firebrick3", "goldenrod3", "forestgreen", "dodgerblue3", "darkmagenta", "hotpink3", "steelblue4", "mediumpurple3","thistle4",
  "indianred4", "yellow4", "mediumseagreen"
)
auc.plot +
  # geom_line(aes( time, quant, color = Model, group = Model), size = 1.3)+
   geom_point(aes( time, quant, color = Model, group = Model), size = 2)+
   geom_smooth(aes( time, quant, color = Model, group = Model), size = 1.3, se = FALSE, method = 'loess', span = 0.65)+
#  geom_smooth(aes( time, quant, color = Model, group = Model), size = 1.3, se = FALSE, method = lm, formula = y ~ splines::bs(x, 5))+
  scale_color_manual(values = color_scheme[1:(ncol(df_for_plot)-1)]) +
  xlab("Years After Exam 6 (Exam Year 15)") +
  ylab("AUC") +
  xlim(3, 17) + 
  ylim(0.5, 0.95) + 
  theme_minimal() + 
  theme(axis.text=element_text(size=21+2),
        axis.title=element_text(size=21+2,face="italic"),
        legend.title = element_text(color = "black", size = 22+2),
        legend.text = element_text(color = "black", size = 22+2)) 
 # geom_vline(xintercept=10)
