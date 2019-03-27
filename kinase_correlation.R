#installing necessary packages and loading libraries
install.packages("magrittr")
install.packages("tidyr")
library(magrittr)
library(tidyr)

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

#For each unique kinase(k is the actual kinase, not an index)
for (k in kinase_names$Kinase){
  #find the substrate and site from kinase_human and store in subs
  print(paste0("kinase: ", k))
  subs <- kinase_human$SubstrateSite[which(k == kinase_human$Kinase)]
  #subs <- as.factor(subs)
  #print(subs)
  
  subsinexp <- intersect(subs, cleanBCD$geneSymbol_Site)
  if(length(subsinexp) == 0){
    subsinexp <- 0
  }
  print(paste0("subsinexp: ", subsinexp))
}




