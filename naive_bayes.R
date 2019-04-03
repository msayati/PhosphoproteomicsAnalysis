
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
  kinase_human <- read.table("data/KSA_human.txt", header = TRUE, sep="", fill=TRUE)
  
  #clean data 
  kinase_human <- kinase_human[!apply(kinase_human, 1, function(x) any(x == "")),]
  
  #Create a list of unique kinase
  kinase_names <- unique(kinase_human[1])
  
  #concatenating substrate and site in kinase_human for easy retrieval
  kinase_human %<>% unite(SubstrateSite, Substrate, Site, sep="-", remove = TRUE)
  cleanBCD %<>% unite(geneSymbol_Site, geneSymbol, variableSites, sep="-", remove = TRUE)
  
  #removing the last char from cleanBCD
  cleanBCD$geneSymbol_Site = substr(cleanBCD$geneSymbol_Site,1,nchar(cleanBCD$geneSymbol_Site)-1)
  #sharedLength <- length(S)
  #summation term
  Sum <- 0
  kinaseRank <- c()
  for(psite in 1:length(cleanBCD)){
    p <- cleanBCD[psite,]
    #convert to vector and remove kinase column
    p <- as.vector(unlist(p))
    p <- p[2:length(p)]
    
    for(k in kinase_names$Kinase){ 
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
          
          print(Sprob)
          print(Aprob)
          
          Sum <- Sum + log2(Sprob/Aprob)
          print(Sum)
          
        }
        #add sum for kinase i in kinaseRank vector
        kinaseRank <- c(kinaseRank, Sum)
        
      }
    }
    
  }
  print(kinaseRank)
} 

#placeholder vectors
S <- c(.5, .4, .3, .8, .7)
A <- c(.2, .3, .8, .3, .3, .2, .1, .1, .1, .7, .5, .4, .3, .4, .3, .1, .01, .01, .01, .01, .01)
naive_bayes(S, A)






