#installing necessary packages and loading libraries
install_packageIF("magrittr")
install_packageIF("tidyr")
library(magrittr)
library(tidyr)

read.clean.KSA<-function(){
  #Read the KSA_human file in the data
  kinase_human <- read.table("data/KSA_human.txt", header = TRUE, sep="", fill=TRUE)
  
  #clean data 
  kinase_human <- kinase_human[!apply(kinase_human, 1, function(x) any(x == "")),]
  
  return(kinase_human)
}

uniqueK.KSA<-function(kinase_human){
  #Create a list of unique kinase
  kinase_names <- unique(kinase_human[1])
  
  return(kinase_names)
}

symbol.site<-function(cleanBCD){
  #concatenating
  cleanBCD %<>% unite(geneSymbol_Site, geneSymbol, variableSites, sep="-", remove = TRUE)
  #removing the last char from cleanBCD
  cleanBCD$geneSymbol_Site = substr(cleanBCD$geneSymbol_Site,1,nchar(cleanBCD$geneSymbol_Site)-1)
  return(cleanBCD)
}

grab_substrates <- function(k, cleanBCD, kinase_human){ 
  #find the substrate and site from kinase_human and store in subs
  substrates <- kinase_human$SubstrateSite[which(k == kinase_human$Kinase)]
  #find the intersection of substrate-site in breast cancer data:
  #1. find the indices where the substrates-site from that kinase, match in BCD
  subsinexp1 <- match(substrates, cleanBCD$geneSymbol_Site)
  #2. if the substrate-site isnt found, remove na
  subsinexp1<-subsinexp1[!is.na(subsinexp1)]
  
  pSites <- unique(cleanBCD[subsinexp1,])
  pSites <- as.vector(pSites)
  return(pSites)
}

kinase.correlation<-function(cleanData){
  
  kinase_human <- read.clean.KSA()
  
  kinase_names <- uniqueK.KSA(kinase_human)
  
  
  #concatenating substrate and site in kinase_human for easy retrieval
  kinase_human %<>% unite(SubstrateSite, Substrate, Site, sep="-", remove = TRUE)
  cleanBCD <- symbol.site(cleanData)
  #cleanBCD %<>% unite(geneSymbol_Site, geneSymbol, variableSites, sep="-", remove = TRUE)
  
  #removing the last char from cleanBCD
  #cleanBCD$geneSymbol_Site = substr(cleanBCD$geneSymbol_Site,1,nchar(cleanBCD$geneSymbol_Site)-1)
  
  vectorS = vector('numeric')
  #For each unique kinase(k is the actual kinase, not an index)
  for (k in kinase_names$Kinase){
    
    subsinexp12 <- grab_substrates(k, cleanBCD, kinase_human)
  
    #for each susbtrate-site in subsinexp, compute the pairwise correlation
    if(nrow(subsinexp12) != 0 & nrow(subsinexp12) != 1){
      subsinexp13 <- subsinexp12[,-c(1)]#removes non-numeric column
      paired_corr <- bicor(t(subsinexp13))#computes the correlation
     # print(paired_corr) 
      shared_corr <- paired_corr[upper.tri(paired_corr, diag = FALSE)]
      
      vectorS <- append(vectorS, shared_corr, after = length(vectorS))
    }
  }
  
  return(vectorS)
}