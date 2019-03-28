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
  subs <- kinase_human$SubstrateSite[which(k == kinase_human$Kinase)]
  
  #find the intersection of substrate-site in breast cancer data:
  #1. find the indices where the substrates-site from that kinase, match in BCD
  subsinexp1 <- match(subs, cleanBCD$geneSymbol_Site)
  #2. if the substrate-site isnt found, remove na
  subsinexp1<-subsinexp1[!is.na(subsinexp1)]#intersect(subs, cleanBCD$geneSymbol_Site)
  print(subsinexp1)
  #3. store in data.frame
  #subsinexp12 <- cleanBCD[subsinexp1,]
  #if (length(subsinexp1) != 0)
  #{
   # print(cleanBCD$geneSymbol_Site[subsinexp1])
    #print(subsinexp12)
  #}
  
  #for each susbtrate-site in subsinexp, compute the pairwise correlation
  #if(nrow(subsinexp12) != 0){
    #subsinexp12 <- subsinexp12[,-c(1)]
    #paired_corr <- bicor(t(subsinexp12))
    #print(paired_corr)
  #}
}




