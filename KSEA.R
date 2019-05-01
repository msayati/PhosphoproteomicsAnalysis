install_packageIF("ggplot2")
library(ggplot2)

KSEA <-function(cleanBCD){
  
kinase_human <- read.clean.KSA()

cleanBCD %<>% unite(geneSymbol_Site, geneSymbol, variableSites, sep="-", remove = TRUE)
kinase_human %<>% unite(SubstrateSite, Substrate, Site, sep="-", remove = TRUE)
cleanBCD$geneSymbol_Site = substr(cleanBCD$geneSymbol_Site,1,nchar(cleanBCD$geneSymbol_Site)-1)

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
    kinase_score = append(kinase_score,((mean(commonFC) - mean(logFC$log2FC))*sqrt(length(kinase_sites)))/sd(logFC$log2FC), after = length(kinase_score))
    
   }
  else {
    #if kinase has no substrates in experimental data, the score is 0
    kinase_score = append(kinase_score, 0, after = length(kinase_score))
  }
}

#creating dataframe with kinase name and their respective score
kscore1 <- data.frame(kinase_names[1], score = kinase_score)

#ordering in ascending
kscore1 <- kscore1[with(kscore1, order(score)),]

#getting the top 10 of neg and top 10 of pos
top10pos <- tail(kscore1, 10) #kscore1[10:1,]
top10neg <- head(kscore1, 10) #kscore1[1:10,]

#combining the top 10 for graph
top10 <- rbind(top10neg, top10pos)

return(top10)
}

#testing <- KSEA(cleanBCD)
#plotting horizontal histogram
  # ggplot(data=top10, aes(x=Kinase,y=score)) +
  # geom_bar(stat="identity") +
  # scale_x_discrete(limits=top10$Kinase) +
  # coord_flip() + scale_color_brewer(palette="Paired") + theme_classic()