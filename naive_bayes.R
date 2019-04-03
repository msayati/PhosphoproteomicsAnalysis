
install.packages("magrittr")
install.packages("tidyr")
library("WGCNA")

library(tidyr)
library(magrittr)
library(dplyr)
library(stringr)
library(BiocGenerics)

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

naive_bayes <- function(S, A){ 
  #Read the KSA_human file in the data
  
  #sharedLength <- length(S)
  #summation term
  Sum <- 0
  index <- 0
  kinaseRank <- matrix(ncol=2)
  for(psite in 1:length(cleanBCD)){
    p <- cleanBCD[psite,]
    #convert to vector and remove kinase column
    p <- as.vector(unlist(p))
    p <- p[2:length(p)]
    index <- 0
    for(k in kinase_names$Kinase){ 
      index <- index + 1
      Sum <- 0
      #grab substrates for k (while we figure out memory problem)
      kinase_sites <- grab_substrates(k)
      if(nrow(kinase_sites) != 0) {
        for(i in nrow(kinase_sites)){
          #compute bicor correlation between sites and psite
          ksite <- as.vector(unlist(kinase_sites[i,]))
          ksite <- ksite[2:length(ksite)]
          c <- bicor(p, ksite)
          ########### naive bayes algorithm ###############
          #compare c with every S and A
          Sprob <- length(which(as.vector(unlist(lapply(S, function(x) c > x))))) + 1
          
          Aprob <- length(which(as.vector(unlist(lapply(A, function(x) c > x))))) + 1
          
          Sum <- Sum + log2(Sprob/Aprob)
          
        }
        #add sum for kinase i in kinaseRank vector
        kinaseRank <- rbind(kinaseRank, c(index, Sum))
        
      }
    }
    
  }
  print(kinaseRank)
} 

#placeholder vectors
S <- c(.5, .4, .3, .8, .7)
A <- c(.2, .3, .8, .3, .3, .2, .1, .1, .1, .7, .5, .4, .3, .4, .3, .1, .01, .01, .01, .01, .01)
naive_bayes(S, A)




m <- matrix(data=c(1,3,3,4,5,6,3,8,3), nrow=3, ncol=3)


