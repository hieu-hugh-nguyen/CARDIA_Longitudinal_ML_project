rm(list=ls()) #Clear all
cat("\014")


# Input Exam Year:
#exam_year = 'Y0'

 exam_year = 'Y5'



 


if (exam_year == 'Y0'){
  work_dir <- 'U:/CARDIA Other/CARDIACCdata/Y0/Y0/DATA'
}
if (exam_year == 'Y2'){
  work_dir <- 'U:/CARDIA Other/CARDIACCdata/Y2/Y2/DATA'
}
if (exam_year == 'Y5'){
  work_dir <- 'U:/CARDIA Other/CARDIACCdata/Y5/DATA'
}
if (exam_year == 'Y7'){
  work_dir <- 'U:/CARDIA Other/CARDIACCdata/Y7/Y7/DATA'
}
if (exam_year == 'Y10'){
  work_dir <- 'U:/CARDIA Other/CARDIACCdata/Y10/Y10/DATA/SAS'
}
if (exam_year == 'Y15'){
  work_dir <- 'U:/CARDIA Other/CARDIACCdata/Y15/Y15/DATA'
}
if (exam_year == 'Y20'){
  work_dir <- 'U:/CARDIA Other/CARDIACCdata/Y20/Y20/CORE/DATA'
}
if (exam_year == 'Y25'){
  work_dir <- 'U:/CARDIA Other/CARDIACCdata/Y25 7.26.17/DATA'
}
if (exam_year == 'Y30'){
  work_dir <- 'U:/CARDIA Other/CARDIACCdata/Y30/Y30data_v13'
}

setwd(work_dir)




##### Load libraries:##################################################################
list.of.packages <- c('haven', 'tibble','Hmisc','labelled','DataCombine', 'dplyr')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)


source_dir <- 'U:/Hieu/CARDIA_project/CARDIA_project'
source(paste0(source_dir,'/Git/code/snippet/remove_hms_time_type.R'))

##### Initialization:##################################################################


#Extract file names from the SAS folder:
filenames=list.files(path = work_dir, pattern = "\\.sas7bdat$");
#Extract file names without the extension:
filenamesWoExtension=tools::file_path_sans_ext(filenames);
#Create objects with the same name and assign the data in each file to its corresponding name
for(i in 1:length(filenames)){
  assign(filenamesWoExtension[i],haven::read_sas(filenames[i]));
}


# Remove unneeded studies:
# for year 20, remove g3f06raw, g4f06bh (diet intake questionnaines, too many rows (690k+))
rm(a5f06bh, c1f42, c1f42b, d2f06bh, g4f06bh, g3f06raw,fghctr01_dec07_2012)

# Remember to check all loaded dataframes: 
# sapply(sapply(ls(), get), glimpse)
 

#Get all studies names (data frames):
Studies <- sapply(sapply(ls(), get), is.data.frame);
Studies = names(Studies)[(Studies==TRUE)]






#Copy Labels:
SOI = get(Studies[1]);
varName=names(SOI);
objectLabel=vector(mode = "character", length = length(varName));
for(j in 1:length(varName)){
  colValues=eval(parse(text=paste(Studies[1], "$", varName[j],sep="")));
  if((is.null(attr(colValues,'label')) == TRUE)){
    objectLabel[j] = "";
  } else {
    objectLabel[j] = attr(colValues,'label');    
  }
}


#Initialize the comprehensive dataset
#And also change date format to character to avoid automatic date conversion to days since 1970:
inx <- sapply(SOI, function(x) inherits(x, "Date") || inherits(x, "POSIXt"))
SOI[inx] <- lapply(SOI[inx], as.character)
ComDataset <- SOI;

#Assign labels in the big dataset:

for(j in 1:length(objectLabel)){
  Hmisc::label(ComDataset[[names(ComDataset)[j]]]) <- objectLabel[j];
}


#Add SHORT ID column (will be necessary later):
SHORT_ID = substr(ComDataset$ID,1,5);
ComDataset = add_column(ComDataset,SHORT_ID,.after="ID");
names(ComDataset$SHORT_ID)<-"SHORT_ID"
label(ComDataset[["SHORT_ID"]]) <- "FIRST 5 ID DIGITS";


# keep track of which data collection form that each variable comes from:
datadoc <- rep(Studies[1],length = ncol(SOI))



# Main Computational Loop to compile the big dataset:===================================================#





##### Part I. Compile existing data reported in sas files: ##############################################
for(j in 2:length(Studies)){
  SOI = get(Studies[j]);
  
  
  #Change date format to character to avoid date conversion to days since 1970:
  inx <- sapply(SOI, function(x) inherits(x, "Date") || inherits(x, "POSIXt"))
  SOI[inx] <- lapply(SOI[inx], as.character)
  
  varName=names(SOI);
  
  # convert non-numeric column to character-type (to avoid non-compatibility error later)
  # SOI_char <- apply(SOI, 2, function(x){if (class(x) == 'character'){as.character(x)}})
  
  non_overlaping_cols <- names(SOI)[!(names(SOI) %in% names(ComDataset))]
  ncol_old_ComDataset <- ncol(ComDataset)
  if(!is.null(SOI$ID)==TRUE){ #If there is a ID column
    class(SOI$ID) <- 'character'
    class(ComDataset$ID) <- 'character'
    
    ComDataset <- ComDataset %>% dplyr::full_join(SOI %>% dplyr::select(c('ID',non_overlaping_cols)), by = 'ID')
  }
  
  else{
    if(!is.null(SOI$SHORT_ID)==TRUE){ #If there is a SHORT ID column
      class(SOI$SHORT_ID) <- 'character'
      class(ComDataset$SHORT_ID) <- 'character'
      ComDataset <- ComDataset %>% dplyr::full_join(SOI %>% dplyr::select(c('SHORT_ID',non_overlaping_cols)), by = 'SHORT_ID')
    }
  }
  ComDataset<- ComDataset %>% filter(!duplicated(ID))
  datadoc <- c(datadoc, rep(Studies[j], length = ncol(ComDataset)-ncol_old_ComDataset)) #minus 1 because excluding the ID column
  
}



 

#Sort the final dataset based on full ID in ascending order: 
ComDataset = ComDataset[order(ComDataset$ID),]; 








# Write the big dataset to a .CSV file:##########################################
#Include a row of variable labels, for easier view in Excel                     #


#Create a row of variable labels onto the big dataset:
AllVarLabel = sapply(ComDataset, function(x) attr(x, 'label'))
rowofAllVarLabel = vector(mode = "character", length = length(AllVarLabel));
for(k in 1:length(AllVarLabel)){
  if(is.null(AllVarLabel[[k]])){
    rowofAllVarLabel[k]="";
  }
  else{
    rowofAllVarLabel[k]=AllVarLabel[[k]];
  }
}

rowofAllVarLabelDf = as.data.frame(t(rowofAllVarLabel));
names(rowofAllVarLabelDf) = names(ComDataset);

# since <time> variables (<hms>, <difftime> class) cannot be written to csv, delete these variables: 
ComDataset2 <- remove_hms_time_type(ComDataset)

# remove NA values in SHORT_ID (avoid mistakenly overcounting number of participants):
ComDataset3 <- ComDataset2[!is.na(ComDataset2$SHORT_ID),]

saving.dir = paste0('U:/Hieu/CARDIA_longi_project/csv_files/', exam_year)
write.csv(ComDataset3, file = paste0(saving.dir,"/", exam_year, "_unimputed_featurespace.csv"),row.names=FALSE)













# Create a Variable Dictionary .CSV file##############################################


#Count the total of participants for each column:
#Also look for whether the variable for each column is Categorical or Continuous:
numParticipants = vector(mode = "numeric", length = length(names(ComDataset)));
CategoricalOrContinuous = rep(NA,length(names(ComDataset)));

for (y in 1:length(names(ComDataset))){
  colObject=eval(parse(text=paste("ComDataset", "$", names(ComDataset)[y],sep="")));
  numParticipants[y] = sum(sapply(colObject, function(x) (!is.na(x) && x!="")));
  
  if(length(unique(colObject)) <= 6){ #If there are fewer than 6 unique values, categorical 
    CategoricalOrContinuous[y] = 0; 
  }
  else{
    CategoricalOrContinuous[y] = 1; #else, variable is continuous  
  }
  
} 


datadoc <- c("",datadoc) # add empty doc for the ID variable



#Create Df of var dict: 
AllVarDf = data.frame(names(ComDataset), rowofAllVarLabel, numParticipants, CategoricalOrContinuous);
names(AllVarDf)= c("Variable Name","Variable Label","Number of Participants (total non-NA values)","Categorical=0, Continuous =1, Categorical in this context means 'there are fewer than 6 unique values', while Continuous means greater than 6 ");

#move ID row to the top:
AllVarDf = bind_rows(AllVarDf %>% filter(`Variable Name` == 'ID'), AllVarDf %>% filter(`Variable Name` != 'ID'))  

AllVarDf$Datadoc = datadoc

#Write to a csv file: 
saving.dir = paste0('U:/Hieu/CARDIA_longi_project/csv_files/', exam_year)
write.csv(AllVarDf, file = paste0(saving.dir,"/", exam_year, "_all_vars_dictionary.csv"),row.names=FALSE)


 



 
 