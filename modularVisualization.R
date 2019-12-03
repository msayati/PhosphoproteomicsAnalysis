install_packageIF("igraph")

# color palette
library(RColorBrewer)
library("WGCNA")
library("igraph")
source("modularId.R")

example <- function(network){
  # index=nrow(network)
  # 
  # linkage = adjacency(network,
  #             selectCols = ncol(network),
  #             type = "unsigned",
  #             power = 0.05,
  #             corFnc = "cor", corOptions = list(use = 'p', method = 'pearson'),
  #             weights = NULL,
  #             distFnc = "dist", distOptions = "method = 'euclidean'",
  #             weightArgNames = c("weights.x", "weights.y"))
  # 
  # 
  # return(linkage)
  
  # Does the bicorrelation and saves it in dataframe corr
  corr <- bicor(network, robustX = TRUE, robustY = TRUE, maxPOutliers = 1,
                pearsonFallback = "none", cosine = FALSE)
  
  # dataframe upp contains the upper triangle of corr
  # upp <- upper.tri(corr, diag = FALSE)
  # all_corr <- corr[upp]
  
  # Make a correlation matrix:
  new_network <- data.matrix(corr)
  
  #Keeping low correlation
  new_network[new_network<0.05] <- 0
  
  # Make an Igraph object from this matrix:
  modules <- graph_from_adjacency_matrix( new_network, weighted=T, mode="undirected", diag=F)
  
  # Plot with more vertice and edge features
  # coul <- brewer.pal(nlevels(as.factor(network$part)), "Set2")
  # 
  # # Map the color to cylinders
  # my_color <- coul[as.numeric(as.factor(network$part))]
  # 
  # # plot
  # par(bg="grey13", mar=c(0,0,0,0))
  # set.seed(ncol(network))
  # plot(modules, 
  #      vertex.size=12,
  #      vertex.color=my_color, 
  #      vertex.label.cex=0.7,
  #      vertex.label.color="white",
  #      vertex.frame.color="transparent"
  # )
  # 
  # # title and legend
  # text(0,0,"Sample of Modules",col="white", cex=1.5)
  # legend(x=-0.2, y=-0.12, 
  #        legend=paste( levels(as.factor(network$part)), " module", sep=""), 
  #        col = coul , 
  #        bty = "n", pch=20 , pt.cex = 2, cex = 1,
  #        text.col="white" , horiz = F)
  # 
  
  # Basic chart
  plot(modules)
  return(modules)
  
  # Testing for lower Triangle
  # (m2 <- matrix(1:20, 4, 5))
  # lower.tri(m2)
  # m2[lower.tri(m2)] <- NA
  # m2
}