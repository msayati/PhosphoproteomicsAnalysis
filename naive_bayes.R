
install.packages("magrittr")
install.packages("tidyr")
library("WGCNA")

library(tidyr)
library(magrittr)
library(dplyr)
library(stringr)

grab_substrates <- function(k){
  #remove last char on bcdata
  geneSymbolSite <- paste(cleanBCD$geneSymbol, cleanBCD$variableSites, sep="-")
  cleanBCD$geneSymbol <- geneSymbolSite
  cleanBCD <- subset(cleanBCD, select=-variableSites)
  
  #rename column
  names(cleanBCD) <- c("geneSymbol_Site")
  counter = 1
  for (sym in cleanBCD$geneSymbol_Site){
    cleanBCD$geneSymbol_Site[counter] <- str_sub(sym, end=-2)
    counter <- counter + 1
  }
  
  
  print(k)
  #find the substrate and site from kinase_human and store in subs
  substrates <- unique(kinase_human$SubstrateSite[which(k == kinase_human$Kinase)])
  print(substrates)
  #find the intersection of substrate-site in breast cancer data:
  #1. find the indices where the substrates-site from that kinase, match in BCD
  
  subsinexp1 <- match(substrates, cleanBCD$geneSymbol_Site)
  #print(subsinexp1)
  #2. if the substrate-site isnt found, remove na
  subsinexp1<-subsinexp1[!is.na(subsinexp1)]
  #print(subsinexp1)
  pSites <- cleanBCD[subsinexp1,]
}



naive_bayes <- function(){
  #S -> [.8, .9, .3]
  #A -> [.3, .4, .5, .4, .2, .... ]
  sharedLength <- length(S)
  
  for(psite in unknownSites){
    for(k in kinase_names){ 
      #grab substrates for k (while we figure out memory problem)
      grab_substrates(k)
      
    }
  }
  grab_substrates(kinase_names$Kinase[[57]])
  
} 

naive_bayes()





