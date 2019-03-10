# generates random numbers for each vector
x1 <- sample(1:100, 1000, replace=T)
x2 <- sample(1:100, 1000, replace=T)
x3 <- sample(1:100, 1000, replace=T)
x4 <- sample(1:100, 1000, replace=T)
x5 <- sample(1:100, 1000, replace=T)
x6 <- sample(1:100, 1000, replace=T)
x7 <- sample(1:100, 1000, replace=T)
x8 <- sample(1:100, 1000, replace=T)
x9 <- sample(1:100, 1000, replace=T)
x10 <- sample(1:100, 1000, replace=T)
x11 <- sample(1:100, 1000, replace=T)

# Testing with 3 rows:

# makes a matrix with the vectors
mat1 <- matrix(c(x1, x2, x3), nrow=3, ncol=1000, byrow=TRUE)

# calculates pearson correlations
mat1cor <- cor(mat1,method="pearson")

# creating histogram
upper.tri(mat1cor, diag = FALSE)
hist(mat1cor[lower.tri(mat1cor)])

# Testing with 11 rows (more reliable):

# makes a matrix with the vectors
mat2 <- matrix(c(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11), nrow=11, ncol=1000, byrow=TRUE)

# calculates pearson correlations
mat2cor <- cor(mat2,method="pearson")

# creating histogram
upper.tri(mat2cor, diag = FALSE)
hist(mat2cor[lower.tri(mat2cor)])