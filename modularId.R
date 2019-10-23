#installing packages containing bicor
install_packageIF("WGCNA")

library(WGCNA)
library(readxl)
library(cluster)

allowWGCNAThreads()

#This script will creates modules using the WGCNA package

#The following fuction receives the dataset and creates 
#network using WGCNA

#Read data from the file. Evenually this will be modulize and will be pass through a function from the app.R script

breastCancerData <- read_excel("data/BreastCancerData.xlsx", sheet=2)

#Send the data to be cleaned and store in cleanData

cleanData <- clean.bcd(breastCancerData,5,2)

# Choose a set of soft-thresholding powers for weighted network


powers = c(c(1:10), seq(from = 12, to=20, by=2))

sft = pickSoftThreshold(cleanData, dataIsExpr = TRUE, powerVector = powers, verbose = 5)

