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



eval_time_points = 15+17


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


dynamic_deephit_expanded_var_all_subjects_df <- auc_df_from_performance_testset('dynamic_deephit_expanded_var_all_subjects_2')



eval_times <- seq(1,15+17, 1)

df_for_plot = data.frame(eval_times
                        , dynamic_deephit_expanded_var_all_subjects_df$median
#                        , dynamic_deephit_tuned_expanded_var_df_2$median
                        )


 
                    




names(df_for_plot) = c('eval_times'
#                        ,'RSF on longitudinal derived features' # \n of 35 variables' #'and Static Y15 Data'     
                       #, 'cForest'
                       , 'Dynamic-Deephit'
                      )

df_for_plot2 <- df_for_plot %>% filter(eval_times %in%(seq(5,15+17,1)))

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
  xlab("Years After Exam Y0") +
  ylab("Median AUC") +
  scale_x_continuous(breaks=seq(5,15+17,2))+
  ylim(0.68, 0.9) + 
  theme_minimal() + 
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=18,face="italic"),
        legend.title = element_text(color = "black", size = 18),
        legend.text = element_text(color = "black", size = 18)) 
 # geom_vline(xintercept=10)
