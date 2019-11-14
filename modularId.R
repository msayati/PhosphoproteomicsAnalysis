#installing packages containing bicor
install_packageIF("WGCNA")

library(WGCNA)
library(readxl)
library(cluster)

allowWGCNAThreads()
options(stringsAsFactors = TRUE)

#This script will creates modules using the WGCNA package

#The following fuction receives the dataset and creates 
#network using WGCNA

#networkAnalysis <- function(Data){

#Remove Excess labels

breastCancerData <- read_excel("data/BreastCancerData.xlsx", sheet=2)
data<-clean.bcd(breastCancerData,2,5)
  
#data[1]<-NULL #do you want them to labels by site or gene

data <- data[,-(1:2)]             # vector
data <- data[,-(1:2),drop=FALSE]


#Transpose data

cleanData<-t(data)

#Set Column Names to Site 
#We can have one label for column and one for rows, but having trouble setting up


#Pick softthreshold for data


sft = pickSoftThreshold(cleanData, powerVector = c(seq(1, 10, by = 1)), verbose = 5)

#Modular Identification

net = blockwiseModules(cleanData, power = 5,
                       TOMType = "unsigned", minModuleSize = 30,
                       reassignThreshold = 0, mergeCutHeight = 0.25,
                       numericLabels = TRUE, pamRespectsDendro = FALSE,
                       saveTOMs = TRUE,
                       saveTOMFileBase = "BreastCancerTOM",
                       verbose = 3)

mergedColors=net$colors

moduleColors = labels2colors(net$colors)

mergedColors <- t(mergedColors)

max(mergedColors)

index=max(mergedColors)

size<-vector()

for (i in 1:index){
  
  print(paste0("Module: ", i))
  num<-length(which(mergedColors==i ))
  size[c(i)]<-num
  print(num)
  
}

hist(size)
