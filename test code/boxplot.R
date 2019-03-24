#required libraries
library("readxl")

#read in data files
KinaseSubstrateData <- read.csv("data/Kinases_Substrates.txt")


#CODE: TO BUILD A BOXPLOT *************************************

# mtcars is a data set available in the R environment for testing
input <- mtcars[,c('mpg','cyl')]
print(head(input))

# Give the chart file a name.
png(file = "boxplot.png")

# Plot the chart.
boxplot(mpg ~ cyl, data = mtcars, xlab = "Number of Cylinders",
        ylab = "Miles Per Gallon", main = "Mileage Data")

# Save the file.
dev.off()

#