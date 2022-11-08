#rm(list=ls()) #Clear all
cat("\024")

# set working directory: 
work_dir = 'U:/Hieu/CARDIA_longi_project'
source(paste0(source_dir,'/normalize_var_importance.R'))
source(paste0(source_dir,'/createDir.R'))

setwd(work_dir)

require(dplyr)
require(ggplot2)


model_name <- 'rsf_expanded_var_concat'
vimp_df <- read.csv(paste0(work_dir, '/csv_files' ,'/median_outerloop_permut_vimp_', model_name,'.csv'))

vimp_df$Median_vimp_neg_as_zero <- ifelse(vimp_df$Median_vimp>0, vimp_df$Median_vimp, 0)
vimp_df$normalized_vimp_exclude_neg <- normalize_var_imp(vimp_df$Median_vimp_neg_as_zero)

n_exams = 6

demo_vars <- c('AGE_Y15', 'RACEBLACK', 'MALE')
exams <- c(0, 2, 5, 7, 10, 15)

vimp_df_demo <- vimp_df

for (demo_var in demo_vars){
  for (exam in exams){
    demo_var_row <- vimp_df[vimp_df$var.name == demo_var,]
    demo_var_row[,2] <- paste0(demo_var_row[,2], '.', exam)
    vimp_df_demo[nrow(vimp_df_demo)+1,] <- demo_var_row
    
  }
}


# long to wide format:
# vimp_df_ts_var <- vimp_df %>% dplyr::filter(!var.name %in% c('AGE_Y15', 'RACEBLACK', 'MALE'))
vimp_df_ts_var <- vimp_df_demo %>% dplyr::filter(!var.name %in% c('AGE_Y15', 'RACEBLACK', 'MALE'))

vimp_df_ts_var$var_wo_time <- sapply(vimp_df_ts_var$var.name, function(x) {gsub("[.][\\s\\S]*$", "", x, perl=T)}) 
vimp_df_ts_var$exam_year <- sapply(vimp_df_ts_var$var.name, function(x) {sub('.*\\.', '', x)}) %>% as.numeric()


heatmap_df <- vimp_df_ts_var %>% dplyr::select(one_of(c('var_wo_time', 'exam_year', 'normalized_vimp_exclude_neg'))) %>%
  dplyr::arrange(exam_year) %>% 
  dplyr::rename(vimp = normalized_vimp_exclude_neg) %>%
  dplyr::rename(var = var_wo_time) %>%
  reshape(idvar = 'var', timevar = 'exam_year', direction = 'wide')



heatmap_df$sum_vimp <- rowSums(heatmap_df %>% dplyr::select(-'var'))

heatmap_df_ranked <- heatmap_df %>% dplyr::arrange(desc(sum_vimp)) %>% dplyr::select(-sum_vimp)

heatmap_df_ranked_matrix <-  data.frame(heatmap_df_ranked[,-1], row.names = heatmap_df_ranked[,1]) %>% as.matrix()


# library(RColorBrewer)
# heatmap(heatmap_df_ranked_matrix, Colv = NA, Rowv = NA
#         , scale="none"
#         , cexRow=1.1
#         , labCol=c('Y0', 'Y2', 'Y5', 'Y7', 'Y10', 'Y15')
#         , col= colorRampPalette(brewer.pal(9, "Blues"))(500))

dev.off()
gplots::heatmap.2(heatmap_df_ranked_matrix, Colv = NA, Rowv = NA
        , scale="none"
        , cexRow=1.7, cexCol = 1.7
        , labCol=c('Y0', 'Y2', 'Y5', 'Y7', 'Y10', 'Y15')
        , col= colorRampPalette(brewer.pal(9, "Blues"))(500)
        , dendrogram = 'none'
        , key = FALSE
        , trace = 'none'
        #, cellnote = round(heatmap_df_ranked_matrix,2)
        , notecol="black"
        , notecex=1.2
        ,  lhei= c(0.5, 5)
        , lwid = c(0.5, 4, 2)
        , lmat = rbind(c(5,2,3),c(6,1,4))
        , srtCol=30

      # , margins=c(3,0)
        )


legend(x="right", legend=c("0", "0.25", "0.5", "0.75","1")
       , title="Normalized \nImportance"
       ,fill=colorRampPalette(brewer.pal(9, "Blues"))(5)
       ,cex=1.4, pt.cex = 1.7
       , box.col="white"
       )


## look into cor matrix to see if repeated measures could be considered as independent var:
# corr(vimp_df_ts_var)

# plot:

heatmap_df_ranked_long <- heatmap_df_ranked %>% tidyr::pivot_longer(cols = var, names_to = 'vimp')

