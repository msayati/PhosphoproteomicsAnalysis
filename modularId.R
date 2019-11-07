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

networkAnalysis <- function(Data){

#Remove Excess labels

breastCancerData <- read_excel("data/BreastCancerDatatest2.xlsx", sheet=2)
data<-clean.bcd(breastCancerData,2,5)
  
data[1]<-NULL #do you want them to labels by site or gene

#Transpose data

cleanData<-t(data)

#Set Column Names to Site 
#We can have one label for column and one for rows, but having trouble setting up

#Modular Identification

net = blockwiseModules(cleanData, power = 3,
                       TOMType = "unsigned", minModuleSize = 10,
                       reassignThreshold = 0, mergeCutHeight = 0.25,
                       numericLabels = TRUE, pamRespectsDendro = FALSE,
                       saveTOMs = FALSE,
                       saveTOMFileBase = "BreastCancerTOM",
                       verbose = 3)

#Run to see what index is assigned what module
net$colors

#Run to see colors and table

mergedColors = labels2colors(net$colors)
# Plot the dendrogram and the module colors underneath
plotDendroAndColors(net$dendrograms[[1]], mergedColors[net$blockGenes[[1]]],
                    "Module colors",
                    dendroLabels = FALSE, hang = 0.03,
                    addGuide = TRUE, guideHang = 0.05)

return(net)
}
