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
   if(nrow(kinase_sites) > 0){

    #getting indices where subsinexp is found in logFC
    indx <- match(kinase_sites$geneSymbol_Site, logFC$geneSymbol_Site)

    #storing the log2FC of subsinexp found in logFC
    commonFC <-logFC$log2FC[indx]
    
    #compute score of Kinase and store it in vector
    kinase_score = append(kinase_score,((mean(commonFC) - mean(logFC$log2FC))*sqrt(length(subsinexp12)))/sd(logFC$log2FC), after = length(kinase_score))
    
   }
  else {
    #if kinase has no substrates in experimental data, the score is 0
    kinase_score = append(kinase_score, 0, after = length(kinase_score))
  }
}

#creating dataframe with kinase name and their respective score
kscore1 <- data.frame(kinase_names[1], score = kinase_score)

#splitting kscore1 into negative and positive score
neg <- kscore1[which(kscore1$score < 0),]
pos <- kscore1[which(kscore1$score > 0),]

#ordering neg in descending order and pos in ascending
neg <- neg[with(neg, order(score, decreasing = TRUE)),]
pos <- pos[with(pos, order(score, decreasing = FALSE)),]

#getting the top 10 of neg and top 10 of pos
top10neg <- top10neg[1:10,]
top10pos <- top10pos[1:10,]