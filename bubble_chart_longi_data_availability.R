# Bubble plot of non-missing longitudinal data with ggplot2 
#
#


rm(list=ls()) #Clear all
cat("\014")


# load libraries:
list.of.packages <- c('ggplot2', 'dplyr', 'tibble', 'hrbrthemes', 'viridis','vctrs')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)


work_dir <- 'U:/Hieu/CARDIA_longi_project'

#longi_data_dict <- read.csv(paste0(work_dir,'/csv_files/longi_data_avalability_dictionary_edited.csv'))
longi_data_dict <- read.csv(paste0(work_dir,'/csv_files/longi_data_avalability_dictionary2.csv'))

# # name of top 50 vars in Y5:
# load_dir <- 'U:/Hieu/CARDIA_project/CARDIA_project/cvd_outcome_rerun'
# var_ranking_y5 <- read.csv(paste0(load_dir,'/csv_files/averaged_outerloop_VIMP_y26_3rd.csv'))
# top_var_y5 <- var_ranking_y5 %>% 


top_var_longterm_y5 <- c("DIBAG","SMKYR","AVGDI","VLDL","AVGSY","NTRIG"
," MHAGE","DPINTAE","CGTDY","MMLVEXFS","LDL","ARMCI","MMLVESS","WST1","MMLVPWD","MMLAD"
,"DPVAE","WGT","ED")

longi_data_dict_top_y5_var_dup_avai <- longi_data_dict %>%  arrange(exam_year) %>% filter(varname_longi %in% top_var_longterm_y5)

longi_data_dict_top_y5_var <- longi_data_dict_top_y5_var_dup_avai %>%
  #filter(duplicated(Variable.Name) == FALSE) %>%
  # remove other duplicated values 
  filter(!(Variable.Name %in% c('E53WGT', 'H76WGT', 'H40WGT')))
saving.dir = file.path(work_dir,'csv_files')
write.csv(longi_data_dict_top_y5_var, file = paste0(saving.dir,'/longi_data_avalability_top_y5_var.csv'), row.names = F)


# Plotting:

longi_data_dict_top_y5_var <- read.csv(paste0(work_dir,'/csv_files/longi_data_avalability_top_y5_var_edited.csv'))

color_scheme <- c(
  "black", "firebrick3", "goldenrod3", "darkmagenta", "hotpink3", "steelblue4", "mediumpurple3", "forestgreen", "dodgerblue3", "thistle4",
  "indianred4", "yellow4", "mediumseagreen"
)


# Bubble chart of data availability:
longi_data_dict_top_y5_var %>%
  dplyr::arrange((row_number())) %>%
  #mutate(var = factor(var)) %>%
  mutate(exam_year = factor(exam_year)) %>%
  ggplot(aes(x=exam_year, y=varname_longi, size=non_missing_per)) +
  geom_point(alpha=0.5) +
  scale_size_area(max_size = 15, name="Complete data") +
  #scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  #theme_ipsum() +
  theme(legend.position="right") +
#  guides(fill = guide_legend(override.aes = list(size = 5))) +
  xlab("Exam year") +
  ylab("Variable") 
 # labs(fill = 'Var group') +
  #scale_fill_manual(values = color_scheme[])





bubble_chart <- longi_data_dict_top_y5_var_dup_avai %>%   
#  filter(duplicated(Variable.Name) == FALSE) %>%
  dplyr::arrange((row_number())) %>%
  mutate(exam_year = factor(exam_year)) %>%
  ggplot(aes(x=exam_year, y=varname_longi, size=non_missing_per)) +
  geom_point(alpha=0.5, shape = 21, color = 'darkblue') +
  scale_size_area(max_size = 10, name="Complete data") +
  theme(legend.position="right") +
  xlab("Exam year") +
  ylab("Variable") +
# labs(fill = 'Var group') +
 scale_fill_manual(values = color_scheme[2])

bubble_chart



# organize the data dict just like bubble chart:

# remove duplicated variables (usually from a variable in the data of a certain exam but collected from a previous exam)
longi_data_dict_dup_rm <- longi_data_dict %>% arrange(exam_year) %>% dplyr::filter(duplicated(Variable.Name) == FALSE)


library(tidyr)

bubble_longi_data_dict <- longi_data_dict %>% arrange(exam_year) %>% dplyr::select(exam_year, varname_longi, non_missing_per) %>% tidyr::pivot_wider(names_from = exam_year, values_from = non_missing_per)
saving.dir = file.path(work_dir,'csv_files')
write.csv(bubble_longi_data_dict %>% apply(2,as.character), file = paste0(saving.dir,'/longi_data_avalability_bubble_format2.csv'), row.names = F)


bubble_longi_data_dict_dup_rm <- longi_data_dict_dup_rm %>% arrange(exam_year) %>% dplyr::select(exam_year, varname_longi, non_missing_per) %>% tidyr::pivot_wider(names_from = exam_year, values_from = non_missing_per)
write.csv(bubble_longi_data_dict_dup_rm %>% apply(2,as.character), file = paste0(saving.dir,'/longi_data_avalability_dup_rm_bubble_format2.csv'), row.names = F)

# add var labels
