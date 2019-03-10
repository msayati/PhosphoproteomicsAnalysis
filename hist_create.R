#installing packages containing bicor
install.packages("WGCNA")

#manually installing GO.db if error happens
source("http://bioconductor.org/biocLite.R") 
biocLite(c("GO.db", "preprocessCore", "impute"))

#drops the first two columns because theyre not numeric value
x <- x[,-c(1,2)]

#does the correlation and saves it in dataframe corr
## NEED TO CHANGE TO BICOR
corr <- cor(t(x), use="all.obs", method="pearson")

#dataframe upp contains the upper triangle of corr
upp <- upper.tri(corr, diag = FALSE)
upcorr <- corr[upp]

#creates histogram of data
hist(upcorr)