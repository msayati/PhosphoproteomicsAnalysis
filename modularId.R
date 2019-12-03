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

#breastCancerData <- read_excel("data/BreastCancerDatatest(merged).xlsx", sheet=2)

#Removes columns labels since the the package gets an error with strings. Need to convert to proper labels

#breastCancerData<-breastCancerData[,-(1:3)]

#data<-clean.bcd(breastCancerData,2,5)

#Trapose the data since packages needs to read genes as columns and samples as rowsa

#data<-t(data)

#cleanData<-data


#Pick softthreshold for data

#sft = pickSoftThreshold(cleanData, powerVector = c(seq(1, 10, by = 1)), verbose = 5)

#Check if soft threshold is a valid entry. If the data too small it may return NA. If it returns Na set the threhold to 5
#If data is too small then set min module to zero. Otherwise default is 30

#sft= sft$powerEstimate

#minModule=30

#if (is.na(sft))
#{
  #sft=5
  #minModule=0
  
#}

#Modular Identification

#net = blockwiseModules(cleanData, power = sft,
                       #TOMType = "unsigned", minModuleSize = minModule,
                       #reassignThreshold = 0, mergeCutHeight = 0.25,
                       #numericLabels = TRUE, pamRespectsDendro = FALSE,
                       #saveTOMs = TRUE,
                       #saveTOMFileBase = "BreastCancerTOM",
                       #verbose = 3)

#This vector gives us both index and module number

#mergedColors<-net$colors

#moduleColors shows us the colors each index represents

#moduleColors = labels2colors(net$colors)

#Gives us the amount of modules in mergedColors

#index=max(mergedColors)

#need to declare size as a vector before using it

#size<-vector()

#This loop shows us the amount of indexes in each module

#for (i in 1:index){
  
  #print(paste0("Module: ", i))
  #num<-length(which(mergedColors==i ))
  #size[c(i)]<-num
  #print(num)
#}

#count=0

#This vector will have all the modular indexes in order. For example all indexes that belong in module one will show up first
#then those on module 2

#modulesInOrder<-vector()

#Issues: modules in order should only be the vector indexes, but it shows both 
#array indexes and module number

#for (i in 1:length(mergedColors)){

  #for (j in 1:length(mergedColors)){
    #if(module[j]==i)
    #{
      #modulesInOrder[count]<-mergedColors[i]
      #print(mergedColors[j])
      #count=count+1
    #}
#}
#}

#modulesInOrder

#hist(size)

#This data frame will hold modules as rows and path+gene index in that module as columns

#this dataframe is hardcoded and for only for testing purposes


index2 <- c("AKT1-S475s","AKT1-S475s","AKT3-T445t","BRAF-S364s","BRAF-S729s","ERBB2-S1151s",
            "ERBB2-T1240t","42614-S253s","42614-S295s","42615-S218s","42617-S340s",
            "42618-S327s","42619-S388s","42622-S332s","42622-S30s T38t","42623-S434s",
            "42623-S28s","AAGAB-S310s","AAK1-T389t","AAK1-T620t","AKT3-T445t","BRAF-S447s",
            "BRAF-S729s","ERBB2-S1151s","ERBB2-S998s","ERBB2-T1240t","HSP90AB1-S226s","42617-S340s",
            "42618-S225s","42619-S388s","42619-S416s","42620-S77s","42620-S334s","42620-T228t",
            "42620-S423s","42621-S18s", "42622-S30s","42622-S2s","42622-T142t","AAGAB-S310s",
            "AAK1-S20s","AKT1-S475s S477s","AKT3-T445t","ERBB2-T1240t","42614-S295s","42615-S218s","42617-S340s","42618-S225s",
           "42618-S327s","42619-S388s","42619-S416s","42623-S28s","AAK1-T389t","BRAF-S729s","42620-S77s","AAAS-S541s","AAK1-T389t",
           "BRAF-T401t","BRAF-S364s","BRAF-S729s","ERBB2-S1151s","ERBB2-S998s","ERBB2-T1240t","42620-S334s",
           "42622-S111s","42622-S22s","42622-T38t","42622-S58s","42622-T49t","42622-S82s","AKT1-S475s",
           "AKT1- S475s S477s","AKT3-T445t","BRAF-S447s","42614-T364t","42620-S77s","42620-S334s",
           "42620-T228t","42620-S423s","42621-S18s","42622-S30s","42622-S2s","42622-S58s","42622-T49t",
           "42622-S82s","42622-S332s","42622-S30s T38t","42623-S434s","42623-S28s","AAAS-S541s","BRAF-T401t",
           "BRAF-S364s","BRAF-S729s","ERBB2-S1151s","ERBB2-S998s","ERBB2-T1240t")

indexModule <-c(8,7,2,9,9,9,9,14,22,14,22,5,1,1,15,1, 
                13,25,9,9,1,1,4,1,18,1,14,3,18,3,26,3, 
                26,24,3,26,24,24,3,18,5,9,20,6,13,6,13,13,
                6,13,13,1,17,3,2,3,2,16,16,16,10,10,3,16,
                8,5,8,17,3,8,27,27,27,4,1,8,21,8,5,8,
                8,4,30,30,2,2,7,2,2,3,10,11,10,1,10,17)

#create an empty martix
#create an empty martix

mat <-matrix(, nrow=20,ncol=30)

k=0 #counter

#populate martix with indexes

for (i in 1:30)
{
  for (j in 1:208)
  {
    if(indexModule[j]==i)
    {
      mat[k,i]=index2[j]
      k=k+1
    }
    
  }
  
  k=0
  
}

#Convert martix to data frame. All rows must have same num of columns so some columns will be equal to zero
  
network <- as.data.frame(mat)
  
#Doesn't work with string. So there is NA instead of zeros.
#network[is.na(network)] <- 0

#Create a label vector

moduleLabel <-vector()

#Get module label

for (i in 1:index)
{
  moduleLabel[i] <- paste("Module",i)
  print(moduleLabel[i])
}

#Set model label

names(network)<-moduleLabel

network$`Module 1`

return(network)
}
