library("WGCNA")
library(tidyr)


#assuming data is vector of vectors
build_graph <- function(){
  newGraph <- list(
    edges <- list(),
    nodes <- list()
  )
  newGraph$grab_sites = function(){
    #grab all sites
    #combine gene and site name
    P <- paste(cleanBCD$geneSymbol, cleanBCD$variableSites, sep="-")
    #remove last letter off of site
    P <- substr(P, 1, nchar(P)-1)
    #get unique sites(not repeated)
    P <- unique(P)
    print(length(P))
    newGraph$nodes <- as.vector(P)
  }
  
  #create nodes(turns out R is object oriented)
  return(newGraph)
  
}

#get x and y vectors 
naive_bayes <- function(){
  graph <- build_graph()
} 

naive_bayes()

