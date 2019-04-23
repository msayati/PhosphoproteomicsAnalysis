#row randomization
randomPlot <- function(dataArray){
  #generate random data
  arraySize = nrow(dataArray)
  randArray = sample(1:arraySize, 2, replace=F)
  
  randArray = lapply(randArray, function(x) dataArray[x])
  #get bicors correlation values
  print(randArray)
  
  #normalize y axis
  
  #plot correlation values
  
  #plot all pairs/shared-kinase pairs/randomized al pairs
  
  #plot historgram
}

realOne = sample(1:20, 4, replace=F)
realTwo = sample(1:20, 4, replace=F)
realThree = sample(1:20, 4, replace=F)

data = data.frame(one = realOne, two = realTwo, three = realThree)

print(data)
randomPlot(data)
