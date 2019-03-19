#Read the KSA_human file in the data
kinase_human <- read.table("data/KSA_human.txt", sep="", fill=TRUE)

#clean data 
kinase_human <- kinase_human[!apply(kinase_human, 1, function(x) any(x == "")),]

#Create a list of unique kinase
kinase_names = unique(kinase_human[1])

#For each kinase, find its substrates in the phosphorylation data 
#and compute the correlation between all of them






