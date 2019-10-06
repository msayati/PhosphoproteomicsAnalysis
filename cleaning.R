#function to check if package already installed
install_packageIF <- function(pack){
  if(pack %in% rownames(installed.packages()) == FALSE){
    print(paste0("Installing package."))
    install.packages(pack)
  }
  else{
    print(paste0("Package already installed."))
  }
}
#downloading needed packages for this script
install_packageIF("readxl")

#setting library used
library(readxl)


# function to clean
clean.bcd<-function(bcd, sh, threshold){
  
  #reads in excel file
  #BreastCancerData1 <- read_excel(bcd, sheet = as.numeric(sh))
  
  #converts it into csv file
  write.csv(bcd, "data/BreastCancerData.csv", row.names=FALSE)
  
  #reads in csv file
  BreastCancerData1 <- read.csv("data/BreastCancerData.csv")
  
  #creating a dataframe cleanBCD to store the columns that have less than 40% NA
  cleanBCD <- BreastCancerData1[ lapply( BreastCancerData1, function(cleanBCD) sum(is.na(cleanBCD)) / length(cleanBCD) ) < as.numeric(threshold) ]
  
  #delete rows with NA
  cleanBCD <- cleanBCD[complete.cases(cleanBCD), ]
  
  #if there are negative values in data, they are already in log2. Convert them back. 
  if(sum(cleanBCD[,-c(1,2)] < 0) > 0){
    cleanBCD <- data.frame(cleanBCD[,c(1,2)], 2^cleanBCD[,-c(1,2)])
  }
  
  return(cleanBCD)
}

# Code to test working function:
file <- "data/BreastCancerDatatest.xlsx"
cleanBCD <- clean.bcd(file, 2, .40)
