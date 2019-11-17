#installing packages containing bicor
install_packageIF("WGCNA")

library(WGCNA)
library(readxl)
library(cluster)
library(data.table)

allowWGCNAThreads()
options(stringsAsFactors = TRUE)

#This script will creates modules using the WGCNA package

#The following fuction receives the dataset and creates network using WGCNA


#Normally whoever calls the function has to send a clean excel file, but for now you can send whatever file and it will work
#since it cleans a file. This is only for testing purposes

networkAnalysis <- function(Data){
  
#Reads file and returns a data frame

breastCancerData <- read_excel("data/BreastCancerDatatest(merged).xlsx", sheet=2)

#Removes columns labels since the the package gets an error with strings. Need to convert to proper labels

breastCancerData<-breastCancerData[,-(1:3)]

data<-clean.bcd(breastCancerData,2,5)

#Trapose the data since packages needs to read genes as columns and samples as rowsa

data<-t(data)

cleanData<-data


#Pick softthreshold for data

sft = pickSoftThreshold(cleanData, powerVector = c(seq(1, 10, by = 1)), verbose = 5)

#Check if soft threshold is a valid entry. If the data too small it may return NA. If it returns Na set the threhold to 5
#If data is too small then set min module to zero. Otherwise default is 30

sft= sft$powerEstimate

minModule=30

if (is.na(sft))
{
  sft=5
  minModule=0
  
}

#Modular Identification

net = blockwiseModules(cleanData, power = sft,
                       TOMType = "unsigned", minModuleSize = minModule,
                       reassignThreshold = 0, mergeCutHeight = 0.25,
                       numericLabels = TRUE, pamRespectsDendro = FALSE,
                       saveTOMs = TRUE,
                       saveTOMFileBase = "BreastCancerTOM",
                       verbose = 3)

#This vector gives us both index and module number

mergedColors<-net$colors

#moduleColors shows us the colors each index represents

moduleColors = labels2colors(net$colors)

#Gives us the amount of modules in mergedColors

index=max(mergedColors)

#need to declare size as a vector before using it

size<-vector()

#This loop shows us the amount of indexes in each module

for (i in 1:index){
  
  print(paste0("Module: ", i))
  num<-length(which(mergedColors==i ))
  size[c(i)]<-num
  print(num)
}

count=0

#This vector will have all the modular indexes in order. For example all indexes that belong in module one will show up first
#then those on module 2

modulesInOrder<-vector()

#Issues: modules in order should only be the vector indexes, but it shows both 
#array indexes and module number

for (i in 1:length(mergedColors)){

  for (j in 1:length(mergedColors)){
    if(module[j]==i)
    {
      modulesInOrder[count]<-mergedColors[i]
      print(mergedColors[j])
      count=count+1
    }
}
}

modulesInOrder

#hist(size)

#This data frame will hold modules as rows and path+gene index in that module as columns

#this dataframe is hardcoded and for only for testing purposes

index2 <- c(1,2,3,6,7,8,10,14,16,17,18,21,22,41,42,43,44,47,51,
           52,57,58,61,62,63,64,66,72,73,75,76,77,78,79,80,81,
           82,83,84,98,99,106,107,114,120,121,122,123,
           124,125,126,145,152,161,177,196,202,209,210,211,212,213,214,228,
           235,236,237,238,239,240,254,255,256,257,268,276,277,278,279,280,
           281,282,288,289,290,291,292,293,294,295,308,309,310,311,312,313,
           314,315,316,317,318,319,320,321,322,323,346,347,374,381,383,384,
           385,386,387,388,389,413,414,415,425,426,434,435,436,445,452,453,
           454,455,456,457,458,459,460,461,462,463,464,465,466,467,468,469,
           470,471,472,474,521,522,523,541,552,557,559,560,569,570,571,572,
           573,574,575,576,577,594,598,599,600,601,612,613,637,643,652,653,
           656,657,670,671,682,683,684,685,686,697,700,701,702,713,714,728,
           729,730,731,732,733,734,735,736,737,765,766,767,768,769,770,771,772)

indexModule <-c(8,7,2,9,9,9,9,14,22,14,22,5,1,1,15,1, 
                13,25,9,9,1,1,4,1,18,1,14,3,18,3,26,3, 
                26,24,3,26,24,24,3,18,5,9,20,6,13,6,13,13,
                6,13,13,1,17,3,2,3,2,16,16,16,10,10,3,16,
                8,5,8,17,3,8,27,27,27,4,1,8,21,8,5,8,
                8,4,30,30,2,2,7,2,2,3,10,11,10,1,10,17,
                17,22,10,10,10,14,20,10,2,20,11,11,18,5,5,5,
                3,5,5,5,5,28,15,28,8,6,17,2,3,1,7,4,
                4,4,4,4,2,16,4,2,1,23,4,21,21,7,21,15,
                4,15,9,7,11,11,4,2,7,9,23,23,1,1,1,8,
                6,1,1,1,11,7,7,7,7,11,4,5,1,15,2,2,3,18,2,7,
                14,6,20,28,6,4,25,1,25,3,3,6,
                6,19,19,29,12,19,19,6,12,29,6,12,12,6,12,12,12)

#create an empty martix

mat <-matrix(, nrow=index,ncol=20)

k=0 #counter

#populate martix with indexes

for (i in 1:30)
{
  for (j in 1:208)
  {
    if(indexModule[j]==i)
    {
      mat[i,k]=index2[j]
      k=k+1
    }
    
  }
  
  k=0
  
}

#Convert martix to data frame. All rows must have same num of columns so some columns will be equal to zero
#You can access specific columns by $V1-V30. If needed use t() to trapose

network <- as.data.frame(mat)
network[is.na(network)] <- 0

return(net)
}
