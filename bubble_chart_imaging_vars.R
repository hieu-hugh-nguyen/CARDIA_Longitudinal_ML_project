# Bubble plot of non-missing longitudinal data with ggplot2 
# Input: longitudinal variable dictionary
# Output: bubble plot and a dataframe mimics the bubble plot format to easier track missing data availability over the years


rm(list=ls()) #Clear all
cat("\014")


# load libraries:
list.of.packages <- c('ggplot2', 'dplyr', 'tibble', 'hrbrthemes', 'viridis','vctrs')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)


work_dir <- 'U:/Hieu/CARDIA_longi_project'

#longi_data_dict <- read.csv(paste0(work_dir,'/csv_files/longi_data_avalability_dictionary_edited.csv'))
longi_data_dict <- read.csv(paste0(work_dir,'/csv_files/longi_data_availability_dictionary_imaging.csv'))
longi_data_dict$varname_longi <- longi_data_dict$varname_longi %>% recode('PREG.x' = 'PREG')

####### Organize the data dict just like bubble chart:

# remove duplicated variables (usually from a variable in the data of a certain exam but collected from a previous exam)
longi_data_dict_dup_rm <- longi_data_dict %>% arrange(exam_year) %>% dplyr::filter(duplicated(Variable.Name) == FALSE)


library(tidyr)

bubble_longi_data_dict_dup_rm <- longi_data_dict_dup_rm %>% arrange(exam_year) %>% dplyr::select(exam_year, varname_longi, non_missing_per) %>% 
  tidyr::pivot_wider(names_from = exam_year, values_from = non_missing_per) %>%
  left_join(longi_data_dict_dup_rm %>% select(var_group, Datadoc, varname_longi,Variable.Label) %>% dplyr::filter(duplicated(varname_longi) == FALSE),by = 'varname_longi') %>%
  dplyr::select(var_group, Datadoc, varname_longi,Variable.Label,everything())

write.csv(bubble_longi_data_dict_dup_rm %>% apply(2,as.character), file = paste0(work_dir,'/csv_files/longi_data_avalability_dup_rm_bubble_format_imaging_vars.csv'), row.names = F)

# add var labels


######### Plotting:


color_scheme <- c(
  "black", "firebrick3", "goldenrod3", "darkmagenta", "hotpink3", "steelblue4", "mediumpurple3", "forestgreen", "dodgerblue3", "thistle4",
  "indianred4", "yellow4", "mediumseagreen"
)


# Bubble chart of data availability:
longi_data_dict_dup_rm %>%
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


