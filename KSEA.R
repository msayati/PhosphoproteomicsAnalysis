#calculating the average of each row
FC <- data.frame(cleanBCD[1], Means=rowMeans(cleanBCD[,-1]))

#Create a list of unique kinase
kinase_names <- unique(kinase_human[1])

#log2 of FC
logFC <- data.frame(FC[1],log2FC=log2(FC[,-1]))

kinase_score = vector('numeric')
for (k in kinase_names$Kinase){
  
  kinase_sites <-grab_substrates(k, cleanBCD, kinase_human)
  
  #find subsinexp in logFC
  if(nrow(kinase_sites) != 0){
    
    #getting indices where subsinexp is found in logFC
    indx <- match(kinase_sites$geneSymbol_Site, logFC$geneSymbol_Site)
    
    #storing the log2FC of subsinexp found in logFC
    commonFC <-logFC$log2FC[indx]
    
    #compute score of Kinase
    kinase_score = append(kinase_score,((mean(commonFC) - mean(logFC$log2FC))*sqrt(length(subsinexp12)))/sd(logFC$log2FC), after = length(kinase_score))
  }
}

#sorting in ascending order
sorted_kinaseScores = sort(kinase_score, decreasing = FALSE)
