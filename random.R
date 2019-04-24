

#row randomization
randomPlot <- function(dataArray){
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
  
  #normalize y axis
  hist(bcors)
  #plot correlation values
  
  #plot all pairs/shared-kinase pairs/randomized al pairs
  
  #plot historgram
}

realOne = sample(1:20, 8, replace=F) 
realTwo = sample(1:20, 8, replace=F)
realThree = sample(1:20, 8, replace=F)
realFour = sample(1:20, 8, replace=F)

data = data.frame(one = realOne, two = realTwo, three = realThree, four = realFour)

print(data)
randomPlot(data)
