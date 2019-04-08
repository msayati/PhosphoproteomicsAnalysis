#downloading needed packages for this script
#install.packages("readxl")

#setting library used
library(readxl)

# function to clean breast cancer data given the file location as a string
clean.bcd<-function(bcd, sh, threshold){
  
  #reads in excel file
  BreastCancerData1 <- read_excel(bcd,sheet = as.numeric(sh))
  
  #converts it into csv file
  write.csv(BreastCancerData1,"data/BreastCancerData.csv",row.names=FALSE)
  
  #reads in csv file
  BreastCancerData1 <- read.csv("data/BreastCancerData.csv")
  
  #creating a dataframe cleanBCD to store the columns that have more
  #less than 40%NA
  cleanBCD <- BreastCancerData1[ lapply( BreastCancerData1, function(cleanBCD) sum(is.na(cleanBCD)) / length(cleanBCD) ) < as.numeric(threshold) ]
  
  #delete rows with NA
  cleanBCD <- cleanBCD[complete.cases(cleanBCD), ]
  
  return(cleanBCD)
}

# Code to test working function:
# file <- "data/BreastCancerData.xlsx"
# cleanBCD <- clean.bcd(file, 2, .40)