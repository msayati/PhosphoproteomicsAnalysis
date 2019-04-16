#calculating the average of each column
FC <- colMeans(subset(cleanBCD, select = -c(1)))
log2(FC)