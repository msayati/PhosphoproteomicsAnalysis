#calculating the average of each row
#FC <- rowMeans(subset(cleanBCD, select = -c(1)))
FC <- data.frame(cleanBCD[1], Means=rowMeans(cleanBCD[,-1]))

#Create a list of unique kinase
kinase_names <- unique(kinase_human[1])

#log2 of FC
logFC <- data.frame(FC[1],log2FC=log2(FC[,-1]))

for (k in kinase_names$Kinase){
  
  #find the substrate and site from kinase_human and store in subs
  subs <- kinase_human$SubstrateSite[which(k == kinase_human$Kinase)]
  #find the intersection of substrate-site in breast cancer data:
  #1. find the indices where the substrates-site from that kinase, match in BCD
  subsinexp1 <- match(subs, cleanBCD$geneSymbol_Site)
  #2. if the substrate-site isnt found, remove na
  subsinexp1<-subsinexp1[!is.na(subsinexp1)]
  #3. store in data.frame
  subsinexp12 <- unique(cleanBCD[subsinexp1,])
  
  #find subsinexp in logFC
  if(nrow(subsinexp12) != 0){
    #getting indices where subsinexp is found in logFC
    indx <- match(subsinexp12$geneSymbol_Site, logFC$geneSymbol_Site)
    
    #storing the log2FC of subsinexp found in logFC
    commonFC <-logFC$log2FC[indx]
    print(paste0("*****commonFC: ",commonFC))
    
    #testing purposes#####
    print(paste0("mean commonFC: ", mean(commonFC)))
    print(paste0("mean logFC$log2FC: ", mean(logFC$log2FC)))
    print(paste0("sqrt subsinexp12: ", sqrt(length(subsinexp12))))
    print(paste0("sd logFC$log2FC: ", sd(logFC$log2FC)))
    #######
    
    #compute score of Kinase
    kinase_score = ((mean(commonFC) - mean(logFC$log2FC))*sqrt(length(subsinexp12)))/sd(logFC$log2FC)
    print(paste0("kinase score: ",kinase_score))
  }
}