#==========================================================================
# Load data and library
#==========================================================================
rm(list = ls())

# laad current directory
# curr.dir = paste(getwd(), '/', sep = '')
work_dir = 'U:/Hieu/CARDIA_longi_project'


##### Load libraries:##################################################################
list.of.packages <- c('randomForestSRC', 'ggRandomForests','survival', 'ggplot2')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)

#==========================================================================
# Get Survival Probability From Model Prediction
#==========================================================================

# load model objects
fold = 1

model_name <- 'rsf_expanded_var_traj_cont_n_binary_var_only_trcovw_6'
loading.dir = paste0(work_dir,'/rdata_files/', model_name, '_fold_',fold)
trained.model = get(load(paste0(loading.dir, '/', model_name, '.RData')))


# Plot the marginal effect for the top variables:

marginal_object <- gg_variable(trained.model)


# get the top var:
loading_dir = paste0(work_dir,'/csv_files')


# var_order = read.csv(file = paste0(loading_dir,
#                                    '/averaged_outerloop_VIMP_short_term.csv'))
var_order = read.csv(file = paste0(loading_dir,
                                   '/median_outerloop_permut_vimp_rsf_expanded_var_traj_cont_n_binary_var_only_trcovw_6.csv'))

var_order = apply(var_order, 2, as.character)
var_order = var_order[,2]

# # n_top = length(var_order)
# n_top = 5
# var_top_n = var_order[1:n_top]




plot_lowess_cont_var <- function(var){
    # plot_final <- plot(marginal_object, xvar=var, panel=TRUE, se=TRUE
    #  , method = 'loess', span = 1, size =1, points = FALSE)
    plot_final <- ggplot(marginal_object, aes(get(var), yhat)) + 
    geom_smooth(size = 1, se = FALSE, method = 'loess', span = 1, color = 'black')+ #usual lowess span = 1
    # geom_line() +
    geom_rug(sides="b") + 
    coord_cartesian(ylim = c(0.85, 1)) +
    # coord_cartesian(ylim = c(0.6, 1)) +
    ggtitle(var)+
    labs(y = '', x = '') +
    theme_minimal()+
    theme(#axis.text=element_text(size=13),
         axis.title=element_blank()
         ,plot.title = element_text(size=8,hjust = 0.5)
  #       # legend.title = element_text(color = "black", size = 17),
  #       # legend.text = element_text(color = "black", size = 17))
         )
  return(plot_final)
}

### Have to plot categorical variables separately (ggRnadomForest doesn't support combining continuous and categorical yet)



plot_lowess_cat_var <- function(var){
  
  marginal_object_factor <- marginal_object
  marginal_object_factor[[var]] <- as.factor(marginal_object_factor[[var]])
  
  cluster_ordered_colors = c('black', 'red', 'blue', 'green')
  n_clusters <- length(unique(marginal_object_factor[[var]]))
 
  
  plot_final <- ggplot(marginal_object_factor, aes(get(var), yhat)) + 
    geom_boxplot(outlier.shape = NA) +
    geom_rug(sides="b") + 
    coord_cartesian(ylim = c(0.85, 1)) +
    labs(y = '', x = '', title = var) +
    theme_minimal() +
    theme(axis.title=element_blank()
          ,plot.title = element_text(size=8,hjust = 0.5))
          
  `%notin%` <- Negate(`%in%`)
  if (var %notin%  c('RACEBLACK', 'MALE')){
    if (n_clusters >= 4){
      cluster_ordered_colors = c('blk', 'red', 'blu', 'grn')
      
      plot_final <- plot_final +
        scale_x_discrete(labels = cluster_ordered_colors[1:n_clusters])
    }else{
    plot_final <- plot_final +
      scale_x_discrete(labels = cluster_ordered_colors[1:n_clusters])     
    }
}
  return(plot_final)
}


var_cont <- c("SBP","DBP","WST","HDL","CGTDY","LIQR","GLU","WGT","BMI"
              ,"ED","LDL","ARMCI","NTRIG","CHOL","NPREG","AGE_Y15"
              ,"BEER","WINE","PULSE")


library(patchwork)

lowess_plot_all_obj <- plot_lowess_cat_var(var_order[1])
for (var_idx in 2:length(var_order)){
  if (var_order[var_idx] %in% var_cont){
    lowess_plot_all_obj <- lowess_plot_all_obj +
      plot_lowess_cont_var(var_order[var_idx])   
  }
  
  else{
    lowess_plot_all_obj <- lowess_plot_all_obj +
      plot_lowess_cat_var(var_order[var_idx])
  }
}

lowess_plot_all_obj 

ggsave(filename = 'partial_dependence_plot.tif', path = paste0(work_dir, '/figures')
       , units = 'in', width = 7.5, height = 7
       , device = 'tiff', dpi = 600)
