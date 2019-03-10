#drops the first two columns because theyre not numeric value
x <- x[,-c(1,2)]

#does the correlation and saves it in dataframe corr
corr <- cor(t(x), use="all.obs", method="pearson")

#dataframe upp contains the upper triangle of corr
upp <- upper.tri(corr, diag = FALSE)
upcorr <- corr[upp]

#creates histogram of data
hist(upcorr)