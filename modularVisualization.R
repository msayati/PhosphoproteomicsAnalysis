install_packageIF("igraph")
install_packageIF('RColorBrewer')
install_packageIF("dplyr")
install_packageIF("visNetwork")

# color palette
library(RColorBrewer)
library(WGCNA)
library(igraph)
library(dplyr)
library(visNetwork)
source("modularId.R")


# Enabling multi-threading
allowWGCNAThreads()

# R often uses concept of factors to re-encode strings;
# In order to avoid problems about delay re-encoding of strings, stringAsFactors is FALSE
options(stringsAsFactors = FALSE)


# The follwing function gets the networkAnalysis cleanDataframe and the clean excel file
networkVisual <- function(moduleNetwork, data) {
  # First two columns merge to have a unique gene symbol and variable site
  data$geneSymbol <- paste(data$geneSymbol, "-", data$variableSites)
  
  # Delete the second column since it is merged with the first column.
  data <- data[, -2]
  
  # A cleanDataframe that holds each row's average from data file
  averageData  <-
    data.frame(geneSymbol_variableSite = data[, 1],
               avg = rowMeans(data[, -1]))
  
  
  #empty dataframe
  averageMod <- data.frame()
  for (i in 1:nrow(net)) {
    rowNumber <- which(net[i, "Module 1"] == data$geneSymbol)
    averageMod[i, "geneSymbol"] <- data$geneSymbol[rowNumber]
    averageMod[i, "avg"] <- averageData[rowNumber, "avg"]
    averageMod[i, "log2_avg"] <-
      log2(averageData[rowNumber, "avg"])
  }
  
  #changing the orer based on column
  #averageMod[order(averageMod$log2_avg),]
  
  
  #frame that holds all the data from the same gene in the current module
  # ncol has "-1" because ncol for data has 28 columns, we only need 27 since
  #don't count the first column because first column is the genesymbol
  modules_data <- matrix(, nrow = nrow(net), ncol = ncol(data) - 1)
  #loops through the length of the module
  for (i in 1:nrow(net)) {
    rowNumber <- which(net[i, "Module 1"] == data$geneSymbol)
    for (j in 2:ncol(data)) {
      #starts at column 2 so it doesn't take the column 'genesymbol' into consideration
      modules_data[i, j - 1] <-
        data[rowNumber, j] #frame will hold that current row's proteins
    }
  }
  module_bicor <-
    bicor(t(modules_data)) #find the bicorrelation for each gene
  module_bicor <-
    abs(module_bicor) # converts everything to absolute value
  
  #convert from matrix to dataframe
  module_bicor <- as.data.frame(module_bicor)
  
  #Empty vector
  moduleLabel <- vector()
  #Get module labels
  for (i in 1:nrow(net))
  {
    moduleLabel[i] <- paste(net[i, "Module 1"])
  }
  
  #Set module label for col and row
  names(module_bicor) <- moduleLabel
  rownames(module_bicor) <- moduleLabel
  
  #convert back to matrix with row and col names
  module_bicor <- as.matrix(module_bicor, rownames.force = NA)
  
  #dataframe upp contains the upper triangle of corr
  upp <- upper.tri(module_bicor, diag = FALSE)
  all_bicor <- module_bicor[upp]
  all_bicor <- data.frame(all_bicor)
  
  module_graph <-
    graph_from_adjacency_matrix(module_bicor,
                                weighted = TRUE,
                                mode = "upper",
                                diag = FALSE)
  
  modules_attributes <- get.edgelist(module_graph)
  colnames(modules_attributes) <- c("from", "to")
  modules_attributes <- cbind(modules_attributes, all_bicor)
  is.data.frame(modules_attributes)
  
  ## Use n equally spaced breaks to assign each value to n-1 equal sized bins
  ii <-
    cut(
      averageMod$log2_avg,
      breaks = seq(
        min(averageMod$log2_avg),
        max(averageMod$log2_avg),
        len = length(averageMod$log2_avg)
      ),
      include.lowest = TRUE
    )
  ## Use bin indices, ii, to select color from vector of n-1 equally spaced colors
  colors <-
    colorRampPalette(c("blue", "white", "dark red"))(length(averageMod$log2_avg) - 1)[ii]
  
  
  V(module_graph)$size <- averageMod$avg * 4
  V(module_graph)$color <- colors
  V(module_graph)$label <- "black"
  V(module_graph)$label.cex <- 0.5
  
  E(module_graph)$width <- modules_attributes$all_bicor * 10
  E(module_graph)$curve <- 1
  vis <- module_graph
  
  return (vis)
}