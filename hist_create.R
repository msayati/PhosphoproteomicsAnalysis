#installing packages containing bicor
install.packages("WGCNA")

#manually installing GO.db if error happens
source("http://bioconductor.org/biocLite.R") 
biocLite(c("GO.db", "preprocessCore", "impute"))

library(WGCNA)

#drops the first two columns because theyre not numeric value
cleanBCD <- cleanBCD[,-c(1,2)]

#does the correlation and saves it in dataframe corr
corr <- bicor(t(cleanBCD))

#dataframe upp contains the upper triangle of corr
upp <- upper.tri(corr, diag = FALSE)
all_corr <- corr[upp]

#creates histogram of data
hist(all_corr)