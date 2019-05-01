source("cleaning.R")
source("kinase_correlation.R")


install_packageIF("magrittr")
install_packageIF("tidyr")
install_packageIF("impute")
install_packageIF("rlang")
install_packageIF("foreach")
install_packageIF("parallel")
install_packageIF("doSNOW")

library("WGCNA")
library(foreach)
library(parallel)

library(tidyr)
library(magrittr)
library(doSNOW)
library(dplyr)
library(stringr)
library(BiocGenerics)


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
  
  return(topNTable)
  
}

naive_bayes <- function(S, A, topCount, test=FAlSE, cleanBCD){

  kinase_human <- read.clean.KSA()
  cleanBCD %<>% unite(geneSymbol_Site, geneSymbol, variableSites, sep="-", remove = TRUE)
  kinase_human %<>% unite(SubstrateSite, Substrate, Site, sep="-", remove = TRUE)
  cleanBCD$geneSymbol_Site = substr(cleanBCD$geneSymbol_Site,1,nchar(cleanBCD$geneSymbol_Site)-1)
  
  #Create a list of unique kinase
  kinase_names <- unique(kinase_human[1])
  
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
      kinase_sites <- grab_substrates(k, cleanBCD, kinase_human)
      if(nrow(kinase_sites) != 0) {
        #set current table column names
        for(i in 1:nrow(kinase_sites)){
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
        #add sum for kinase i in kinaseRank matrix with index of kinase(kinase_names)
        kinaseRank <- rbind(kinaseRank, c(index, Sum))
        currentTable[pSiteName, k] <- Sum
        
      }
    }
    
  }
  
  
  finalTable <- final_table(currentTable, topCount)
  #returns table with all correlations between individual kinases(no order) and
  #returns table with topN Kinases(names currently, working on adding probability value)
  return(list(currentTable, finalTable))
  
} 


# naive_bayess <- function(S, A, topCount, test=FAlSE, cleanData, kinase_names, kinase_human){
#   #cleanBCD <- symbol.site(cleanData)
#   cleanBCD <- cleanData
#   cores = detectCores()
#   cl <- makeCluster(cores)
#   registerDoSNOW(cl)
#   ########## required data structures ###########
#   sharedLength <- length(S)
#   allLength <- length(A)  
#   #summation term
#   Sum <- 0
#   index <- 0
#   kinaseRank <- matrix(ncol=2)
#   currentTable <- data.frame()
#   
#   #set testing length to compute sample matrix
#   if(test) {
#     runningLength = 25
#   }
#   else{
#     runningLength = nrow(cleanBCD)
#   }
#   ###############################################
#   
#   
#   for(psite in 1:runningLength) {
#     p <- cleanBCD[psite,]
#     #site name 
#     pSiteName <- unlist(p[1])
#     #convert to vector and remove kinase column
#     p <- as.vector(unlist(p))
#     p <- p[2:length(p)]
#     index <- 0
#     #for(k in kinase_names$Kinase){
#     print("before foreach ")
#     foreach(i=1:length(kinase_names$Kinase), .packages = c("foreach", "WGCNA", "magrittr")) %dopar% {
#       print("after foreach")
#       source("cleaning.R")
#       source("kinase_correlation.R")
#       print(i)
#       k <- kinase_names$Kinase[i]
#       index <- index + 1
#       Sum <- 0
#       #grab substrates for k (while we figure out memory problem)
#       kinase_sites <- grab_substrates(kinase_names$Kinase[i], cleanBCD, kinase_human)
#       if(nrow(kinase_sites) != 0) {
#         #set current table column names
#         for(i in 1:nrow(kinase_sites)){
#           #compute bicor correlation between sites and psite
#           ksite <- as.vector(unlist(kinase_sites[i,]))
#           ksite <- ksite[2:length(ksite)]
#           c <- bicor(p, ksite)
#           ########### naive bayes algorithm ###############
#           #compare c with every S and A
#           Sprob <- length(which(as.vector(unlist(lapply(S, function(x) c > x))))) + 1
#           
#           Aprob <- length(which(as.vector(unlist(lapply(A, function(x) c > x))))) + 1
#           
#           print("got past lapplys")
#           Sum <- Sum + log2((Sprob/sharedLength)/(Aprob/allLength))
#           
#         }
#         #add sum for kinase i in kinaseRank matrix with index of kinase(kinase_names)
#         kinaseRank <- rbind(kinaseRank, c(index, Sum))
#         currentTable[pSiteName, k] <- Sum
#         
#       }
#     }
#     stopCluster(cl)
#     
#   }
#   
#   
#   finalTable <- final_table(currentTable, topCount)
#   #returns table with all correlations between individual kinases(no order) and
#   #returns table with topN Kinases(names currently, working on adding probability value)
#   return(list(currentTable, finalTable))
# 
# } 
# 
 # S <- c(.2, .2, .4, .6, .8)
 # A <- c(.9, .9, .5, .5, .8, .1, .2, .9, .9, .9, .4)
 # tes<-naive_bayes(S, A, 5, test=TRUE, cleanBCD)







