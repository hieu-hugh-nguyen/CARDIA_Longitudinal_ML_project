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

rsf_traj_ch_df <- auc_df_from_performance_testset('rsf_expanded_var_traj_only_ch_4')
rsf_traj_kl_df <- auc_df_from_performance_testset('rsf_expanded_var_traj_only_kl_4')
rsf_traj_ccc_df <- auc_df_from_performance_testset('rsf_expanded_var_traj_only_ccc_4')
rsf_traj_hartigan_df <- auc_df_from_performance_testset('rsf_expanded_var_traj_only_hartigan_4')
rsf_traj_scott_df <- auc_df_from_performance_testset('rsf_expanded_var_traj_only_scott_4')
rsf_traj_trcovw_df <- auc_df_from_performance_testset('rsf_expanded_var_traj_only_trcovw_4')
rsf_traj_tracew_df <- auc_df_from_performance_testset('rsf_expanded_var_traj_only_tracew_4')
rsf_traj_friedman_df <- auc_df_from_performance_testset('rsf_expanded_var_traj_only_friedman_4')
rsf_traj_gap_df <- auc_df_from_performance_testset('rsf_expanded_var_traj_only_gap_4')
rsf_traj_marriot_df <- auc_df_from_performance_testset('rsf_expanded_var_traj_only_marriot_4')
rsf_traj_rubin_df <- auc_df_from_performance_testset('rsf_expanded_var_traj_only_rubin_4')
rsf_traj_cindex_df <- auc_df_from_performance_testset('rsf_expanded_var_traj_only_cindex_4')
rsf_traj_db_df <- auc_df_from_performance_testset('rsf_expanded_var_traj_only_db_4')
rsf_traj_silhouette_df <- auc_df_from_performance_testset('rsf_expanded_var_traj_only_silhouette_4')
rsf_traj_duda_df <- auc_df_from_performance_testset('rsf_expanded_var_traj_only_duda_4')


rsf_traj_ball_df <- auc_df_from_performance_testset('rsf_expanded_var_traj_only_ball_4')
rsf_traj_pseudot2_df <- auc_df_from_performance_testset('rsf_expanded_var_traj_only_pseudot2_4')
rsf_traj_beale_df <- auc_df_from_performance_testset('rsf_expanded_var_traj_only_beale_4')
rsf_traj_ptbiserial_df <- auc_df_from_performance_testset('rsf_expanded_var_traj_only_ptbiserial_4')
rsf_traj_sdindex_df <- auc_df_from_performance_testset('rsf_expanded_var_traj_only_sdindex_4')
rsf_traj_sdbw_df <- auc_df_from_performance_testset('rsf_expanded_var_traj_only_sdbw_4')

rsf_traj_aic_bic_df <- auc_df_from_performance_testset('rsf_expanded_var_traj_only_aic_bic')


eval_times <- seq(1,17, 1)

df_for_plot = data.frame(eval_times
                        ,rsf_traj_ch_df$median
                        ,rsf_traj_kl_df$median
                        ,rsf_traj_ccc_df$median
                        ,rsf_traj_hartigan_df$median
                        ,rsf_traj_scott_df$median
                        ,rsf_traj_tracew_df$median
                        ,rsf_traj_friedman_df$median
                        ,rsf_traj_gap_df$median
                        ,rsf_traj_marriot_df$median
                        ,rsf_traj_rubin_df$median
                        ,rsf_traj_cindex_df$median
                        ,rsf_traj_db_df$median
                        ,rsf_traj_silhouette_df$median
                        ,rsf_traj_duda_df$median
                        
                        
                        ,rsf_traj_ball_df$median
                        ,rsf_traj_pseudot2_df$median
                        ,rsf_traj_beale_df$median
                        ,rsf_traj_ptbiserial_df$median
                        ,rsf_traj_sdindex_df$median
                        ,rsf_traj_sdbw_df$median
                        
                        ,rsf_traj_aic_bic_df$median
                        ,rsf_traj_aic_bic_df$median
                        ,rsf_traj_trcovw_df$median
                        
                        )

 
                    




names(df_for_plot) = c('eval_times'
                      ,"ch","kl", "ccc", "hartigan", "scott"
                      , "tracew", "friedman","gap"
                      , "marriot", "rubin", "cindex", "db", "silhouette"
                      , "duda", "ball", "pseudot", "beale", "ptbiserial"
                      , "sdindex", "sdbw"
                      , "aic", "bic"
                      , "trcovw"
                      )

df_for_plot2 <- df_for_plot %>% filter(eval_times %in%(seq(5,17,1)))

df_for_plot.long = melt(df_for_plot2, id = 'eval_times')

names(df_for_plot.long) = c('time','Clustering_criteria','quant')



auc.plot = ggplot(data = df_for_plot.long)

color_scheme <- c(
  "black", "firebrick3", "goldenrod3", "forestgreen", "dodgerblue3", "darkmagenta", "hotpink3", "steelblue4", "mediumpurple3","thistle4",
  "indianred4", "yellow4", "mediumseagreen"
)


library(RColorBrewer)
n <- length(names(df_for_plot)) -1 
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

set.seed(137)
color_scheme2 <- sample(col_vector, n)


library(randomcoloR)
nColor <- 40
myColor <- randomcoloR::distinctColorPalette(k = 40)
pie(rep(1, nColor), col = myColor)
    
    
color_scheme3 <- colorRampPalette(brewer.pal(n, "Paired"))(n)

auc.plot +
  # geom_line(aes( time, quant, color = Clustering_criteria, group = Clustering_criteria), size = 1.3)+
   geom_point(aes( time, quant, color = Clustering_criteria, group = Clustering_criteria), size = 1.3, alpha = 0.2) +
   geom_smooth(aes( time, quant, color = Clustering_criteria, group = Clustering_criteria), size = 1, se = FALSE, method = 'loess', span = 1)+ #usual lowess span = 1
  # geom_smooth(aes( time, quant, color = Clustering_criteria, group = Clustering_criteria), size = 1.3, se = FALSE, method = lm, formula = y ~ x)+
  
  # geom_smooth(aes( time, quant, color = Clustering_criteria, group = Clustering_criteria), size = 1.3, se = FALSE, method = lm, formula = y ~ splines::bs(x, 1))+
 scale_color_manual(values = color_scheme3[1:(ncol(df_for_plot)-1)]) +
  xlab("Years After Exam Y15") +
  ylab("Median AUC") +
  scale_x_continuous(breaks=seq(5,17,2))+
 # ylim(0.68, 0.9) + 
  theme_minimal() + 
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16,face="italic"),
        legend.title = element_text(color = "black", size = 16),
        legend.text = element_text(color = "black", size = 15)) 
 # geom_vline(xintercept=10)





ggsave(filename = 'traj_methods_comparison.tif', path = paste0(work_dir, '/figures')
       , units = 'in', width = 7.5, height = 3.8
       , device = 'tiff', dpi = 300)