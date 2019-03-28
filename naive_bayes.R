library("WGCNA")
library(tidyr)


#assuming data is vector of vectors
build_graph <- function(){
  #grab all sites
  #combine gene and site name
  P <- paste(cleanBCD$geneSymbol, cleanBCD$variableSites, sep="-")
  #remove last letter off of site
  P <- substr(P, 1, nchar(P)-1)
  #get unique sites(not repeated)
  P <- unique(P)
  print(length(P))
  
  #create nodes
  
}

#get x and y vectors 
naive_bayes <- function(){
  
} 

build_graph()
