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

#commented out because I am still testing the function

#networkAnalysis <- function(Data){

#Remove Excess labels

#need to use special test file that has the gene and variable sites merged together

breastCancerData <- read_excel("data/BreastCancerData(merged.xlsx", sheet=2)

breastCancerData<-breastCancerData[,-(1:2)]

data<-clean.bcd(breastCancerData,2,5)

#Transpose data

cleanData<-t(data)

#Set Column Names to Site 
#We can have one label for column and one for rows, but having trouble setting up

#Pick softthreshold for data

sft = pickSoftThreshold(cleanData, powerVector = c(seq(1, 10, by = 1)), verbose = 5)

#Modular Identification

net = blockwiseModules(cleanData, power = sft$powerEstimate,
                       TOMType = "unsigned", minModuleSize = 30,
                       reassignThreshold = 0, mergeCutHeight = 0.25,
                       numericLabels = TRUE, pamRespectsDendro = FALSE,
                       saveTOMs = TRUE,
                       saveTOMFileBase = "BreastCancerTOM",
                       verbose = 3)
#This vector gives us both index of node and module number

mergedColors=net$colors

#moduleColors shows us the colors each index represents

moduleColors = labels2colors(net$colors)

#gives us the amount of modules in mergedColors

max(mergedColors)

index=max(mergedColors)

size<-vector()

for (i in 1:index){
  
  print(paste0("Module: ", i))
  num<-length(which(mergedColors==i ))
  size[c(i)]<-num
  print(num)
  
}

#Allows use to keep track of what index we are at

count=0

#This vector will have all the modular indexes in order. For example all indexes that belong in module one will show up first
#then those on module 2

modulesInOrder<-vector()

#Issues: modules in order should only be the vector indexes, but it shows both 
#array indexes and module number

for (i in 1:length(module)){

  for (j in 1:length(module)){
    if(module[j]==i)
    {
      modulesInOrder[c(count)]<-mergedColors[j]
      print(mergedColors[j])
      count=count+1
    }
}
  
}

#return(net)
#}
