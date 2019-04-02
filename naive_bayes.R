
install.packages("magrittr")
install.packages("tidyr")
library("WGCNA")

library(tidyr)
library(magrittr)
library(dplyr)
library(stringr)

grab_substrates <- function(k){
  #remove last char on bcdata
  #geneSymbolSite <- paste(cleanBCD$geneSymbol, cleanBCD$variableSites, sep="-")
  #cleanBCD$geneSymbol <- geneSymbolSite
  #cleanBCD <- subset(cleanBCD, select=-variableSites)
  
  #rename column
  #names(cleanBCD) <- c("geneSymbol_Site")
  #counter = 1
  #for (sym in cleanBCD$geneSymbol_Site){
    #cleanBCD$geneSymbol_Site[counter] <- str_sub(sym, end=-2)
    #counter <- counter + 1
  #}

  
  #find the substrate and site from kinase_human and store in subs
  substrates <- kinase_human$SubstrateSite[which(k == kinase_human$Kinase)]
  #print(substrates)
  #find the intersection of substrate-site in breast cancer data:
  #1. find the indices where the substrates-site from that kinase, match in BCD
  
  subsinexp1 <- match(substrates, cleanBCD$geneSymbol_Site)
  #print(subsinexp1)
  #2. if the substrate-site isnt found, remove na
  subsinexp1<-subsinexp1[!is.na(subsinexp1)]
  #print(subsinexp1)
  pSites <- cleanBCD[subsinexp1,]
  pSites <- as.vector(pSites)
  return(pSites)
}

naive_bayes <- function(){ 
  #S -> [.8, .9, .3]
  #A -> [.3, .4, .5, .4, .2, .... ]
  #sharedLength <- length(S)
  c <- vector()
  for(psite in 1:length(cleanBCD)){
    p <- cleanBCD[psite,]
    p <- as.vector(unlist(p))
    p <- p[2:length(p)]
    
    for(k in kinase_names$Kinase){ 
      #grab substrates for k (while we figure out memory problem)
      kinase_sites <- grab_substrates(k)
      
      if(nrow(kinase_sites) != 0) {
        for(i in nrow(kinase_sites)){
          #print(kinase_sites[i,], max.levels=0)
          #compute bicor correlation between sites and psite
          
          kinase_sites[i,] <- as.vector(unlist(kinase_sites[i,2:ncol(kinase_sites[i,])]))
    
          #print(kinase_sites[i,])
          print(bicor(p, kinase_sites[i,]))
          #naive bayes algorithm
          
        }
      }
    }
  }
} 

grab_substrates(kinase_names$Kinase[[49]])

naive_bayes()






