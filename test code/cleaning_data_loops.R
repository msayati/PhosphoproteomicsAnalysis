#my orignial code to clean excel data
#note: it takes a long time to run. Using lapply() is more efficient.

#required libraries
library("readxl")


# reading files: data frames are created from the given data files
breastCancerData <- read_excel("data/BreastCancerData.xlsx", sheet=2)
# class(breastCancerData)
# print the data frame		
print(breastCancerData) 
kinaseSubstrateData <- read.csv("data/Kinases_Substrates.txt")

# clean data & if data has n/a use median
nacounter <- 0
# should be 3, not 29
for(i in 3:ncol(breastCancerData)){
  for(j in 1:nrow(breastCancerData)){
    if(nchar(breastCancerData[[i]]) == 2 && breastCancerData[[i]] == "NA"){
      nacounter <- nacounter + 1
    }
  }
  print(nacounter) #NA found per col
  #checks if NA is more than 60%
  napercent <- ((nacounter * 100) / 56874)
  if(napercent <= 60){
    bcd <- cbind(bcd, breastCancerData[, i])
  }
}