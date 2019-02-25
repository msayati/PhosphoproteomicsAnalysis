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

dim(bdata)

print(bdata[[1]][1])

for(x in bdata[[3]]){
  if(x == "NA"){
    x = 0
    print(x)
  }
}

for(r in 1:nrow(bdata)){
  for(c in 1:ncol(bdata)){
    if(bdata[r,c] == "NA") {
      bdata[r,c] = 0 
    }
  }
}

bdata[[1:29]] 

print(bdata[[3]])
