
install.packages("magrittr")
install.packages("tidyr")
install.packages("impute")
install.packages("rlang")
library("WGCNA")

library(tidyr)
library(rlang)
library(impute)
library(magrittr)
library(dplyr)
library(stringr)
library(BiocGenerics)
library(foreach)
library(doParallel)


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


#returns the final table of all kinase correlations with psite
#and returns the top N rank in a data frame with kinases as columns
final_table <- function(kcorTable, topCount){
  #create table of all kinase ranks
  topNTable <- data.frame()
  kColNames <- unlist(rownames(kcorTable))
  
  for(psite in 1:nrow(kcorTable)){
    tempIndex <- order(kcorTable[psite,], decreasing=TRUE)
    tempRow <- sort(kcorTable[psite,], decreasing=TRUE)
    #grab top N 
    for (i in 1:topCount){
      topNTable[kColNames[psite], i] <- list(colnames(kcorTable)[tempIndex[i]], tempRow[i])
    }
  }
  
  print(topNTable)
  
}


naive_bayes <- function(S, A, topCount, test=FAlSE){ 
  ###### start parallel processing #########
  cores = detectCores()
  #creates parallel socket cluster(so threads can communicate)
  commNet <- makeCluster(cores[1]-1)
  #register parallel backend with foreach package
  registerDoParallel(commNet)
  
  allowWGCNAThreads()
  ########## required data structures ###########
  sharedLength <- length(S)
  allLength <- length(A)  
  #summation term
  Sum <- 0
  index <- 0
  kinaseRank <- matrix(ncol=2)
  currentTable <- data.frame()
  
  #set testing length to compute sample matrix
  if(test) {
    runningLength = 25
  }
  else{
    runningLength = nrow(cleanBCD)
  }
  ###############################################
  
  
  for(psite in 1:runningLength) {
    p <- cleanBCD[psite,]
    #site name 
    pSiteName <- unlist(p[1])
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
        #set current table column names
        registerDoSEQ()
        foreach(i=1:nrow(kinase_sites), .packages=c('WGCNA', 'rlang')) %dopar% {
          #compute bicor correlation between sites and psite
          ksite <- as.vector(unlist(kinase_sites[i,]))
          ksite <- ksite[2:length(ksite)]
          c <- bicor(p, ksite)
          ########### naive bayes algorithm ###############
          #compare c with every S and A
          Sprob <- length(which(as.vector(unlist(lapply(S, function(x) c > x))))) + 1
          
          Aprob <- length(which(as.vector(unlist(lapply(A, function(x) c > x))))) + 1
          
          Sum <- Sum + log2((Sprob/sharedLength)/(Aprob/allLength))
          
        }
        #stopCluster(commNet)
        #add sum for kinase i in kinaseRank matrix with index of kinase(kinase_names)
        kinaseRank <- rbind(kinaseRank, c(index, Sum))
        currentTable[pSiteName, k] <- Sum
        
      }
    }
    
  }
  
  finalTable <- final_table(currentTable, topCount)
  #returns table with all correlations between individual kinases(no order) and
  #returns table with topN Kinases(names currently, working on adding probability value)
  stopCluster(commNet)
  return(list(currentTable, finalTable))
  
} 

#placeholder vectors
S <- c(.5, .4, .3, .8, .7)
A <- c(.2, .3, .8, .3, .3, .2, .1, .1, .1, .7, .5, .4, .3, .4, .3, .1, .01, .01, .01, .01, .01)
table <- naive_bayes(S, A, 3, TRUE)






