
install.packages("magrittr")
install.packages("tidyr")
library("WGCNA")

library(tidyr)
library(magrittr)
library(dplyr)
library(stringr)

grab_substrates <- function(k){
  #find the substrate and site from kinase_human and store in subs
  substrates <- kinase_human$SubstrateSite[which(k == kinase_human$Kinase)]
  #find the intersection of substrate-site in breast cancer data:
  #1. find the indices where the substrates-site from that kinase, match in BCD
  
  subsinexp1 <- match(substrates, cleanBCD$geneSymbol_Site)
  
  #2. if the substrate-site isnt found, remove na
  subsinexp1<-subsinexp1[!is.na(subsinexp1)]
  
  pSites <- cleanBCD[subsinexp1,]
  pSites <- as.vector(pSites)
  return(pSites)
}

naive_bayes <- function(){ 
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
          #compute bicor correlation between sites and psite
          
          ksite <- as.vector(unlist(kinase_sites[i,]))
          ksite <- ksite[2:length(ksite)]
          
          print(bicor(p, ksite))
          append(c, bicor(p, ksite))
          #naive bayes algorithm
          
        }
      }
    }
  }
} 

grab_substrates(kinase_names$Kinase[[49]])

naive_bayes()






