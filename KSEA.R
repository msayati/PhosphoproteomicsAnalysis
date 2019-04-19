#calculating the average of each row
#FC <- rowMeans(subset(cleanBCD, select = -c(1)))
FC <- data.frame(cleanbc[,1], Means=rowMeans(cleanbc[,-1]))

#Create a list of unique kinase
kinase_names <- unique(kinase_human[1])

#log2 of FC
logFC <- data.frame(log2(FC[,-1]))

#For each unique kinase(k is the actual kinase, not an index)
for (k in kinase_names$Kinase){
  
  #find the substrate and site from kinase_human and store in subs
  s <- kinase_human$SubstrateSite[which(k == kinase_human$Kinase)]
  #find the intersection of substrate-site in breast cancer data:
  subsinexp1 <- match(subs, cleanBCD$geneSymbol_Site)
  subsinexp1 <- subsinexp1[!is.na(subsinexp1)]
  subsinexp12 <- unique(cleanBCD[subsinexp1,])
  
}