# read all feature ranking .csv files, get Variable Importance (VIMP) (in this case is min tree depth) of each feature and output the averaged min tree depth
#rm(list=ls()) #Clear all
cat("\024")

# set working directory: 
work_dir = 'U:/Hieu/CARDIA_longi_project'
source(paste0(source_dir,'/normalize_var_importance.R'))
source(paste0(source_dir,'/createDir.R'))

setwd(work_dir)


## average permutation VIMP:

model_name = 'rsf_expanded_var_concat'
nfold = 10

fold = 1
loading.dir = paste0(work_dir,'/rdata_files/', model_name, '_fold_',fold)
feature.ranking = read.csv(paste0(loading.dir,'/permut_vimp.csv'))
colnames(feature.ranking)[2] = paste0('vimp_fold_', fold)
feature.ranking$normalized_depth = NULL
#colnames(feature.ranking)[1] = ''
for (fold in 2:nfold){
  loading.dir = paste0(work_dir,'/rdata_files/', model_name, '_fold_',fold)
  feature.ranking.foldn = read.csv(paste0(loading.dir,'/permut_vimp.csv'))
  colnames(feature.ranking.foldn)[2] = paste0('vimp_fold_', fold)
  feature.ranking = merge(feature.ranking, feature.ranking.foldn[,c(1,2)], .by = 'Variable')
}
feature.ranking$'Avg_vimp' = rowMeans(feature.ranking[,seq(2,nfold+1,1)])
feature.ranking = feature.ranking %>% dplyr::arrange(desc(Avg_vimp))
feature.ranking$normalized_vimp = normalize_var_imp(feature.ranking$Avg_vimp)

feature.ranking.median = feature.ranking
feature.ranking.median$'Median_vimp' = matrixStats::rowMedians(feature.ranking[,seq(2,nfold+1,1)] %>% as.matrix())
feature.ranking.median = feature.ranking.median %>% dplyr::arrange(desc(Median_vimp))
feature.ranking.median$normalized_vimp_median = normalize_var_imp(feature.ranking.median$Median_vimp)


saving.dir = createDir(work_dir, 'csv_files')
feature.ranking.median$'index' = seq(1,nrow(feature.ranking),1)
write.csv(feature.ranking.median[,c('index','var.name','Median_vimp','normalized_vimp_median')]
          ,file = paste0(saving.dir,'/median_outerloop_permut_vimp_', model_name,'.csv'), row.names = F)



## average min depth:

model_name = 'rsf_expanded_var_concat'
nfold = 10

fold = 1
loading.dir = paste0(work_dir,'/rdata_files/', model_name, '_fold_',fold)
feature.ranking = read.csv(paste0(loading.dir,'/depth_rank.csv'))
colnames(feature.ranking)[2] = paste0('MinDepth_fold_', fold)
feature.ranking$normalized_depth = NULL
#colnames(feature.ranking)[1] = ''
for (fold in 2:nfold){
  loading.dir = paste0(work_dir,'/rdata_files/', model_name, '_fold_',fold)
  feature.ranking.foldn = read.csv(paste0(loading.dir,'/depth_rank.csv'))
  colnames(feature.ranking.foldn)[2] = paste0('MinDepth_fold_', fold)
  feature.ranking = merge(feature.ranking, feature.ranking.foldn[,c(1,2)], .by = 'Variable')
}
feature.ranking$'Avg_min_depth' = rowMeans(feature.ranking[,seq(2,nfold+1,1)])
feature.ranking = within(feature.ranking, rm(Var.2))
feature.ranking = feature.ranking[order(feature.ranking$Avg_min_depth),]
feature.ranking$normalized_depth = normalize_var_imp(feature.ranking$Avg_min_depth)

saving.dir = createDir(work_dir, 'csv_files')
feature.ranking$'index' = seq(1,nrow(feature.ranking),1)
write.csv(feature.ranking[,c('index','Variable','Avg_min_depth','normalized_depth')]
          ,file = paste0(saving.dir,'/averaged_outerloop_MinDepth_', model_name,'.csv'), row.names = F)


