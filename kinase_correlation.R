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
kinase_names = unique(kinase_human$Kinase)

#concatenating substrate and site in kinase_human for easy retrieval
kinase_human %<>% unite(SubstrateSite, Substrate, Site, sep="-", remove = TRUE)
cleanBCD %<>% unite(geneSymbol_Site, geneSymbol, variableSites, sep="-", remove = TRUE)

#For each kinase, find its substrates in the phosphorylation data 
#and compute the correlation between all of them






