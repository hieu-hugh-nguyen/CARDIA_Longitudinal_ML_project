rm(list=ls()) #Clear all
cat("\014")

# load the library              
library("ggplot2")
library('dplyr')
work_dir = 'U:/Hieu/CARDIA_longi_project'
setwd(work_dir)


### permutation VIMP ranking: #####################################
vimp_df <- read.csv(paste0(work_dir,'/csv_files/', 'median_outerloop_permut_vimp_rsf_expanded_var_traj_cont_n_binary_var_only_trcovw_white_only.csv'))
vimp_df_for_plot <- vimp_df %>% dplyr::select(one_of(c('var.name', 'normalized_vimp_median')))

names(vimp_df_for_plot) <- c('Variable', 'VIMP')

# bar is created with the help of
# grom_bar() and ggplot() function
ggp <- ggplot(vimp_df_for_plot, aes(reorder(Variable, VIMP), VIMP)) +   
  geom_bar(stat = "identity", fill = 'deeppink4')+
  theme_minimal()+
  ylab("VIMP") +
  xlab("") +
  geom_text(aes(label=round(VIMP, 2)), position=position_dodge(width=0.9), hjust=-0.25)
  

# complete graph get flipped with the
# help of coord_flip() function
ggp +  coord_flip()



## save as high resolution tiff:
ggp <- ggplot(vimp_df_for_plot, aes(reorder(Variable, VIMP), VIMP)) +
  geom_bar(stat = "identity", fill = 'deeppink4')+
  theme_minimal()+
  ylab("VIMP") +
  xlab("") +
  geom_text(aes(label=round(VIMP, 2)), position=position_dodge(width=0.9), hjust=-0.025, size = 3)+
  theme(text=element_text(color="black"), axis.text=element_text(color="black"))


ggp +  coord_flip()

ggsave(filename = 'VIMP_traj_white_only.tif', path = paste0(work_dir, '/figures')
       , units = 'in', width = 4.2, height = 7
       , device = 'tiff', dpi = 300)


# ### global importance ranking from SHAP: ###########################
# vimp_df <- read.csv(paste0(work_dir,'/csv_files/', 'rsf_traj_SHAP/all_features/','shap_kernel_explainer_expanded_var_2_all_subjects_fold_1.csv'))
# 
# vimp_df_for_plot <- vimp_df %>% dplyr::select(one_of(c('variable_name', 'normalized_vimp')))
# 
# names(vimp_df_for_plot) <- c('Variable', 'VIMP')
# 
# # bar is created with the help of
# # grom_bar() and ggplot() function
# ggp <- ggplot(vimp_df_for_plot, aes(reorder(Variable, VIMP), VIMP)) +   
#   geom_bar(stat = "identity", fill = 'deeppink4')+
#   theme_minimal()+
#   ylab("Normalized Importance") +
#   xlab("") +
#   geom_text(aes(label=round(VIMP, 2)), position=position_dodge(width=0.9), hjust=-0.25)
# 
# 
# # complete graph get flipped with the
# # help of coord_flip() function
# ggp +  coord_flip()
