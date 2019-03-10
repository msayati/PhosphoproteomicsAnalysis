#setting library used
library(readxl)

## MUST BE SAVED AS A CSV FILE ##

BreastCancerData1 <- read.csv("BreastCancerData.csv")

#delete those columns
#creating a dataframe x to store the columns that have more
#less than 40%NA
x <- BreastCancerData1[ lapply( BreastCancerData1, function(x) sum(is.na(x)) / length(x) ) < .40 ]

#delete rows with NA
x <- x[complete.cases(x), ]