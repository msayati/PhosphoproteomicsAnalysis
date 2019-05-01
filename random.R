library(ggplot2)


#row randomization
#function takes in data frame
randomPlot <- function(dataArray, allCorr, sharedKinaseCorr){
  print(dataArray)
  #remove first column(psite name)
  dataArray <- dataArray[-c(1)]
  
  arraySize <- nrow(dataArray)
  colSize <- ncol(dataArray)
  cNames <- colnames(dataArray)
  newDataFrame <- data.frame()
  
  #generate random data
  randArray <- sample(1:arraySize, 100, replace=FALSE)
  randArray = lapply(randArray, function(x) dataArray[x,])
  
  #get random matrix to compute bicors
  randomMatrix <- matrix(ncol=colSize)
  
  #remove column names from individual rows 
  for(i in 1:length(randArray)){
    r <- as.vector(unlist(randArray[i]))
    #add individual rows to random matrix
    randomMatrix <- rbind(randomMatrix, c(r)) 
  }
  #remove first row
  randomMatrix <- randomMatrix[-1,]
  
  
  #get random matrix bicors correlation values
  bcors <- bicor(randomMatrix)
  bcorsIndex <- upper.tri(bcors, diag = FALSE)
  bcors <- bcors[bcorsIndex == TRUE]
  
  #convert to data frame
  bcors <- data.frame(bcors)
  colnames(bcors) <- c("x")
  allCorr <- data.frame(allCorr)
  colnames(allCorr) <- c("x")
  sharedKinaseCorr <- data.frame(sharedKinaseCorr)
  colnames(sharedKinaseCorr) <- c("x")
  
  
  #set where data is from
  bcors$from <- 'random'
  allCorr$from <- 'all'
  sharedKinaseCorr$from <- 'shared'
  
  allLengths <- rbind(bcors, allCorr)
  allLengths <- rbind(allLengths, sharedKinaseCorr)
  
  ggplot(allLengths, aes(x = x, fill = from)) + geom_density(col=NA, alpha = 0.2) + labs(title="CoPhosphorylation Distribution", x ="CoPhos",  y = "Frequency")

}

