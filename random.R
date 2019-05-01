library(ggplot2)
#row randomization
randomPlot <- function(dataArray, allCorr, sharedKinaseCorr){
  #remove first column
  dataArray <- dataArray[-c(1)]
  #generate random data
  arraySize <- nrow(dataArray)
  colSize <- ncol(dataArray)
  cNames <- colnames(dataArray)
  newDataFrame <- data.frame()
  randArray <- sample(1:arraySize, 3, replace=F)
  randArray = lapply(randArray, function(x) dataArray[x,])
  
  print(length(randArray))
  #get random matrix to compute bicors
  randomMatrix <- matrix(ncol=colSize)
  
  for(i in 1:length(randArray)){
    r <- as.vector(unlist(randArray[i]))
    print(r)
    randomMatrix <- rbind(randomMatrix, c(r)) 
  }
  #remove first element
  randomMatrix <- randomMatrix[-1,]
  
  print(randomMatrix)
  
  #get bicors correlation values
  bcors <- bicor(randomMatrix)
  bcorsIndex <- upper.tri(bcors, diag = FALSE)
  bcors <- bcors[bcorsIndex == TRUE]
  print(bcors)
  
  #convert to data frame
  bcors <- data.frame(bcors)
  colnames(bcors) <- c("x")
  #names(bcors) <- NULL
  allCorr <- data.frame(allCorr)
  colnames(allCorr) <- c("x")
  #names(allCorr) <- NULL
  sharedKinaseCorr <- data.frame(sharedKinaseCorr)
  colnames(sharedKinaseCorr) <- c("x")
  #names(sharedKinaseCorr) <- NULL
  
  
  #set where data is from
  bcors$from <- 'random'
  allCorr$from <- 'all'
  sharedKinaseCorr$from <- 'shared'
  print("after")
  
  allLengths <- rbind(bcors, allCorr)
  allLengths <- rbind(allLengths, sharedKinaseCorr)
  
  print(allLengths)
  
  #normalize y axis
  
  ggplot(allLengths, aes(x = x, fill = from)) + geom_density(col=NA, alpha = 0.2)
  #plot correlation values
  
  #plot all pairs/shared-kinase pairs/randomized al pairs
  
  #plot historgram
}

realOne = sample(1:20, 8, replace=F) 
realTwo = sample(1:20, 8, replace=F)
realThree = sample(1:20, 8, replace=F)
realFour = sample(1:20, 8, replace=F)

data = data.frame(one = realOne, two = realTwo, three = realThree, four = realFour)
alls = c(0.2, 0.8, 0.9)
shared = c(0.9, 0.1, 0.1)



print(data)
randomPlot(data, alls, shared)
