rm(list=ls()) #Clear all
cat("\014")
exam_year = 'Y25'
#work_dir=paste0('U:/CARDIA Other/CARDIACCdata/', exam_year, '/', exam_year, '/DATA')
#work_dir = paste0('U:/CARDIA Other/CARDIACCdata/Y10/Y10/DATA/SAS')
#work_dir = 'U:/CARDIA Other/CARDIACCdata/Y20/Y20/CORE\DATA'
#work_dir = 'U:/CARDIA Other/CARDIACCdata/Y30/Y30data_v13'
work_dir = 'U:/CARDIA Other/CARDIACCdata/Y25 7.26.17/DATA'
setwd(work_dir)

##### Load libraries:##################################################################
list.of.packages <- c('haven', 'tibble','Hmisc','labelled','DataCombine')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)


##### Initialization:##################################################################


#Extract file names from the SAS folder:
filenames=list.files(pattern = "\\.sas7bdat$");
#Extract file names without the extension:
filenamesWoExtension=tools::file_path_sans_ext(filenames);
#Create objects with the same name and assign the data in each file to its corresponding name
for(i in 1:length(filenames)){
  assign(filenamesWoExtension[i],read_sas(filenames[i]));
}

# Remember to check all loaded dataframes: 
sapply(sapply(ls(), get), glimpse)
# make sure ID column is in each df as the first column

# this one df has its ID and var column swiched
# c1apob = c1apob[c('ID','CL9APOB')]
# rm(c2f09mbc)
# c1hemo = c1hemo[c( "ID", "CLEFACT7", "CLEFACT8", "CLEFIBR",  "CLEVWANT", "CLEVWACT", "CLEBTYPE","CLELEWIS")]
# c1lip = c1lip[c( "ID" , "CL1CHOL" , "CL1VLDL",  "CL1HDL"  , "CL1LDL"  , "CL1NTRIG" , "CENTER")]
# c1gmp = c1gmp[c("ID" , "C1GMP"  ,  "COLLDAY",  "CENTER",     "C39LENG1", "C39LENG2", "C39LENG3", "C39VOL1",  "C39VOL2"  ,"C39VOL3",  "ADJ_GMP")] 
# c1cartda = c1cartda[c("SHORT_ID", names(c1cartda)[-which(names(c1cartda) == "SHORT_ID")])]
# c1echo = c1echo[c('ID', names(c1echo)[-which(names(c1echo) == "ID")])]


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
  label(ComDataset[[names(ComDataset)[j]]]) <- objectLabel[j];
}


#Add SHORT ID column (will be necessary later):
SHORT_ID = substr(ComDataset$ID,1,5);
ComDataset = add_column(ComDataset,SHORT_ID,.after="ID");
names(ComDataset$SHORT_ID)<-"SHORT_ID"
label(ComDataset[["SHORT_ID"]]) <- "FIRST 5 ID DIGITS";



# Main Computational Loop to compile the big dataset:===================================================#





##### Part I. Compile existing data reported in sas files: ##############################################
for(j in 1:length(Studies)){
  SOI = get(Studies[j]);
  
  #Change date format to character to avoid date conversion to days since 1970:
  inx <- sapply(SOI, function(x) inherits(x, "Date") || inherits(x, "POSIXt"))
  SOI[inx] <- lapply(SOI[inx], as.character)
  
  varName=names(SOI);
  
  
  #Look for ID column: 
  #IDcol = (sapply(varName,function(x) identical("ID",x)))
  if(!is.null(SOI$ID)==TRUE){ #If there is a ID column
    
    #Check if there is any ID number not already in ComDataset, add it to ComDataset 
    newID = SOI$ID[which(is.element(SOI$ID,ComDataset$ID) %in% FALSE)];
    if(length(newID)>0){
      for(i in 1:length(newID)){
        ComDataset[nrow(ComDataset)+1,] <- NA; #Add an empty row
        ComDataset[nrow(ComDataset),]['ID']=newID[i]; #Add new ID to the list
      }
      
    }
    
    
    overlapID = intersect(ComDataset$ID,SOI$ID); 
    
    if(length(overlapID) != 0){ #If there is at least one ID number overlapped with the exisiting dataset
      #This if statement should always be TRUE 
      
      #Append empty columns, ncol = varName - 1, nrow = # of IDs to ComDataset 
      #ComDataset[nrow(ComDataset)+1,] <- NA;
      newVarName=varName[sapply(varName,function(x) identical("ID",x))==FALSE]; #Return varName vector without "ID"
      ComDataset[,newVarName] <- NA; # Append new columns 
      
      #Add new values into the big dataset:    
      indicesOI = match(overlapID,SOI$ID); #return the indices of overlapped ID 
      subDfWoID = subset(SOI, select=-ID)[indicesOI,] #
      
      ##Copy Labels from SOI to subDfWoID:#########################################
      
      
      VarLabel=vector(mode = "character", length = length(newVarName));
      for(z in 1:length(newVarName)){
        colValues=eval(parse(text=paste(Studies[j], "$", newVarName[z],sep="")));
        if((is.null(attr(colValues,'label')) == TRUE)){
          VarLabel[z] = "";
        } else {
          VarLabel[z] = attr(colValues,'label');    
        }
      }
      
      indicesInComDataset = match(overlapID,ComDataset$ID);
      ComDataset[indicesInComDataset,newVarName]= subDfWoID; 
      
      #Assign labels in the big dataset:#####################################
      
      for(w in 1:length(VarLabel)){
        label(ComDataset[[names(ComDataset)[ncol(ComDataset)-length(VarLabel)+w]]]) <- VarLabel[w];
      }
      
    }
  }
  
  
  #For SHORT_ID Matching:##################################################################
  else { #If there is no ID column, check for the SHORT_ID column
    
    if(!is.null(SOI$SHORT_ID)==TRUE){ #If there is a SHORT ID column
      newID = SOI$SHORT_ID[which(is.element(SOI$SHORT_ID,ComDataset$SHORT_ID) %in% FALSE)];
      #If there is new ID that hasn't already been added, add it to the list:
      if(length(newID)>0){
        for(i in 1:length(newID)){
          ComDataset[nrow(ComDataset)+1,] <- NA; #Add an empty row
          ComDataset[nrow(ComDataset),]['SHORT_ID']=newID[i]; #Add new ID to the list
        }
        
      }
      
      
      overlapID = intersect(ComDataset$SHORT_ID,SOI$SHORT_ID);
      
      if(length(overlapID) != 0){ #If there is a overlap with the exisiting dataset
        
        #This if statement should always be TRUE
        
        #Append empty columns, ncol = varName - 1, nrow = # of IDs to ComDataset
        #ComDataset[nrow(ComDataset)+1,] <- NA;
        newVarName=varName[sapply(varName,function(x) identical("SHORT_ID",x))==FALSE];
        #Return varName vector without "SHORT_ID"
        ComDataset[,newVarName] <- NA; # Append new columns
        
        #Add new values into the big dataset:
        indicesOI = match(overlapID,SOI$SHORT_ID); #return the indices of overlapped ID
        subDfWoID = subset(SOI, select=-SHORT_ID)[indicesOI,] #
        
        
        
        ##Copy Labels from SOI to subDfWoID:
        
        VarLabel=vector(mode = "character", length = length(newVarName));
        for(z in 1:length(newVarName)){
          colValues=eval(parse(text=paste(Studies[j], "$", newVarName[z],sep="")));
          if((is.null(attr(colValues,'label')) == TRUE)){
            VarLabel[z] = "";
          } else {
            VarLabel[z] = attr(colValues,'label');    
          }
        }
        
        
        indicesInComDataset = match(overlapID,ComDataset$SHORT_ID);
        #for(i in 1:length(indicesInComDataset)){
        # ComDataset[indicesInComDataset[i],newVarName]= subDfWoID[i,];
        #}
        #OR, instead of a for loop:
        ComDataset[indicesInComDataset,newVarName]= subDfWoID;
        
        
        #Assign labels in the big dataset:
        
        for(w in 1:length(VarLabel)){
          label(ComDataset[[names(ComDataset)[ncol(ComDataset)-length(VarLabel)+w]]]) <- VarLabel[w];
        }
        
        
      }
    }
  }
  
}



#Sort the final dataset based on full ID in ascending order: 
ComDataset = ComDataset[order(ComDataset$ID),]; 








# Write the big dataset to a .CSV file:##########################################
#Include a row of variable labels, for easier view in Excel                     #

saving.dir = paste0('U:/Hieu/CARDIA_longi_project/csv_files/', exam_year)

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

csvComDataset <- DataCombine::InsertRow(ComDataset,rowofAllVarLabel,RowNum=1); 

write.csv(csvComDataset, file = paste0(saving.dir,"/", exam_year, "_unimputed_featurespace.csv"),row.names=FALSE)


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




#Create Df of 4 columns: 
AllVarDf = data.frame(names(ComDataset), rowofAllVarLabel, numParticipants, CategoricalOrContinuous);
names(AllVarDf)= c("Variable Name","Variable Label","Number of Participants (total non-NA values)","Categorical=0, Continuous =1, Categorical in this context means 'there are fewer than 6 unique values', while Continuous means greater than 6 ");

#Write to a csv file: 
#saving.dir = "U:/Hieu/CARDIA_project/csv_files"
write.csv(AllVarDf, file = paste0(saving.dir,"/", exam_year, "_all_vars_dictionary.csv"),row.names=FALSE)