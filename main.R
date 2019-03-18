#required libraries
library("readxl")


#read in data files
breastCancerData <- read_excel("data/BreastCancerData.xlsx", sheet=2)
kinaseSubstrateData <- read.csv("data/Kinases_Substrates.txt")

#clean data
#if data has n/a use median

summary(breastCancerData)

dim(breastCancerData)

bdata <- matrix(c(breastCancerData), nrow=56874, ncol=29, byrow=TRUE)


