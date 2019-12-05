
#installing packages containing bicor
install_packageIF("WGCNA")

library(WGCNA)
library(readxl)
library(cluster)
library(data.table)
library(tibble)
library(tidyverse)

allowWGCNAThreads()
options(stringsAsFactors = FALSE)

#This script will creates modules using the WGCNA package

#The following fuction receives a clean excel file in the form of a dataframe. 

networkAnalysis <- function(Data){
  
#Merges the first and second columns so each gene symbol and variable site is unique

Data$geneSymbol <-paste(Data$geneSymbol,"-",Data$variableSites)

#Removes variables sites columns since it merged with gene symbol

Data <- Data[,-(2)]

#Save the gene and variable site in vector for later use

geneList <- as_tibble(Data)

geneList<-geneList %>% pull(geneSymbol)

geneNum <-length(geneList)

#Remove Labels From Data

Data<-Data[,-(1:3)]

#Trapose the data since packages needs to read genes as columns and samples as rowsa

Data<-t(Data)

#Pick softthreshold for data

sft = pickSoftThreshold(Data, powerVector = c(seq(1, 10, by = 1)), verbose = 5)

#Check if soft threshold is a valid entry. If the data too small it will return NA.
#If it returns NA then set the threshold to 5. If data is too small then set min module to zero. 
#Otherwise default is 30

sft= sft$powerEstimate

minModule=30

if (is.na(sft))
{
  sft=5
  minModule=0
}

#Modular Identification

net = blockwiseModules(Data, power = sft,
                       TOMType = "unsigned", minModuleSize = minModule,
                       reassignThreshold = 0, mergeCutHeight = 0.25,
                       numericLabels = TRUE, pamRespectsDendro = FALSE,
                       saveTOMs = TRUE,
                       saveTOMFileBase = "BreastCancerTOM",
                       verbose = 3)

#This vector gives us the module number each gene belongs to

moduleList<-net$colors

#Create an index that holds number of modules in moduleList

col=max(moduleList)

#Loop through moduleList to get the size of each module
  
size <-vector()

for (i in 1:col)
{
  num<-length(which(moduleList==i ))
  size[c(i)]<-num
}

#Create an index that holds the max number of genes in the modules

row= max(size)

#Create a martix with rows being the max number of genes in each index and columns being the max number of modules

mat <-matrix(, nrow=row,ncol=col)

k=1 #counter

#populate martix with genes+path sites

for (i in 1:col)
{
  for (j in 1:geneNum)
  {
    if(moduleList[j]==i)
    {
      mat[k,i]=geneList[j]
      k=k+1
    }
  }
  
  k=1
  
}

network <- as.data.frame(mat)

#Create a label vector that will hold approriate labels

moduleLabel <-vector()

#Get module labels

for (i in 1:col)
{
  moduleLabel[i] <- paste("Module",i)
}

#Set module label

names(network)<-moduleLabel

return(network)

}
