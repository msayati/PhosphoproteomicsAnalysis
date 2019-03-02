#setting library used
library(readxl)

#creating data.frame from sheet=2 of BCD
BreastCancerData <- read_excel("data/BreastCancerData.xlsx",sheet = "S5.iTRAQ_phosphoproteome")

#replace "NA" with actual NA
BreastCancerData[BreastCancerData=="NA"]=NA

#creating a data.frame that contains sum of NA's per column
na_count <- sapply(BreastCancerData, function(y)
            sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

#view the dataframe
print(na_count)

#calculate columns that have more than 40%NA
forty_per_na <-colMeans(is.na(BreastCancerData)) > 0.4
forty_per_na <- data.frame(forty_per_na)

#delete those columns
#creating a dataframe x to store the columns that have more
#less than 40%NA
x <- BreastCancerData[ lapply( BreastCancerData, function(x) sum(is.na(x)) / length(x) ) < .40 ]