#downloading needed packages for this script
install.packages("readxl")

#setting library used
library(readxl)

#reads in excel file
BreastCancerData1 <- read_excel("data/BreastCancerData.xlsx",sheet = "S5.iTRAQ_phosphoproteome")

#converts it into csv file
write.csv(BreastCancerData1,"data/BreastCancerData.csv",row.names=FALSE)

#reads in csv file
BreastCancerData1 <- read.csv("data/BreastCancerData.csv")

#creating a dataframe cleanBCD to store the columns that have more
#less than 40%NA
cleanBCD <- BreastCancerData1[ lapply( BreastCancerData1, function(cleanBCD) sum(is.na(cleanBCD)) / length(cleanBCD) ) < .40 ]

#delete rows with NA
cleanBCD <- cleanBCD[complete.cases(cleanBCD), ]