# library(igraph)
# library(WGCNA)
# library(rgl)
# library(tcltk)
# library(readxl)
# source("modularId.R")
# 
# ## Saving the tkplot()
# g <- make_star(10, center=10)
# E(g)$width <- sample(1:10, ecount(g), replace=TRUE)
# lay <- layout_nicely(g)
# 
# id <- tkplot(g, layout=lay)
# canvas <- tk_canvas(id)
# tcltk::tkpostscript(canvas, file="C:/Users/Jamila/Documents/UTRGV/Fall 2019/Senior Project/output.eps")
# tk_close(id)
# 
# #3D visualization: with coordinates, no pop-up
# rglplot(g)
# 
# ## Setting the coordinates title
# g <- make_ring(10)
# id <- tkplot(make_ring(10), canvas.width=450, canvas.height=500)
# 
# canvas <- tk_canvas(id)
# padding <- 20
# coords <- norm_coords(layout_in_circle(g), 0+padding, 450-padding,
#                       50+padding, 500-padding)
# tk_set_coords(id, coords)
# 
# width <- as.numeric(tkcget(canvas, "-width"))
# height <- as.numeric(tkcget(canvas, "-height"))
# tkcreate(canvas, "text", width/2, 25, text="Modular Visualization",
#          justify="center", font=tcltk::tkfont.create(family="helvetica",
#                                                      size=20,weight="bold"))
# 
# 
# 
# file <- read_excel("data/BreastCancerDatatest(merged).xlsx", sheet=2)
# example <- networkAnalysis(file)
# as.data.frame( example[1,], drop=false)
# data <-t(example)
# 
# # the link{with_vertex_}
# #modifier adds vertex attributes to the newly created graphs.
# 
# #Undirected small cluster; very plain and no interaction
# small_cluster <- make_tree(as.data.frame( example[1,], drop=false), mode="undirected")
# plot(small_cluster, layout=layout_with_dh(small_cluster), vertex.size=6, vertex.label=NA)
# 
# # Takes in first row and all columns
# g <- makeCluster(as.data.frame( example[1,], drop=false))
# plot(g, layout=layout_as_star(g), vertex.size=7, vertex.label=NA)
# ## Alternative form
# layout_(g, as_star())
# 
# #when reading the data, must know the rows and columns

install_packageIF("igraph")
install_packageIF("magrittr")
install_packageIF("tidyr")
install_packageIF("impute")
install_packageIF("rlang")
install_packageIF("foreach")
install_packageIF("parallel")
install_packageIF("doSNOW")

library("WGCNA")
library(foreach)
library(parallel)

library(tidyr)
library(magrittr)
library(doSNOW)
library(dplyr)
library(stringr)
library(BiocGenerics)
source("modularId.R")


#returns the final table of all kinase correlations with geneVar
#and returns the top N rank in a data frame with kinases as columns
final_table <- function(modularTable, topCount){
  #create table of all modular id's
  modularTable <- networkAnalysis()
  mColNames <- unlist(rownames(modularTable))
  
  
  for(geneVar in 1:nrow(modularTable)){
    #set and name first column
    topNTable[mColNames[geneVar], 1] <- rownames(modularTable)[geneVar]
    
    
    tempIndex <- order(modularTable[geneVar,], decreasing=TRUE)
    tempRow <- sort(modularTable[geneVar,], decreasing=TRUE)
    #grab top N 
    for (i in 1:topCount){
      topNTable[mColNames[geneVar], i+1] <- colnames(modularTable)[tempIndex[i]]
    }
  }
  x <- 1:topCount
  colnames(topNTable) <- c("PhosphoSites", x)
  
  return(topNTable)
  
}

example <- function(S, A, topCount, test=FAlSE, givenFile){
  
  # Column of the geneSymbol + variableSite
  geneVarSite <- unique(givenFile[3])
  
  ########## required data structures ###########
  sharedLength <- length(S) # all rows?
  allLength <- length(A)  # all columns?
  #summation term
  Sum <- 0
  index <- 0
  modularRank <- matrix(ncol=2)  #rank of all modules, need the two highest
  currentTable <- data.frame()
  
  #set testing length to compute sample matrix
  # if(test) {
  #   runningLength = 25
  # }
  # else{
  #   runningLength = nrow(givenFile) #was cleanBCD before
  # }
  
  runningLength = nrow(givenFile)
  ###############################################
  
  for(geneVar in 1:runningLength) {
    p <- givenFile[geneVar,]
    #gene and Var site column
    geneVarName <- unlist(p[3]) 
    #convert to vector and remove kinase column
    p <- as.vector(unlist(p))
    p <- p[4:length(p)]
    index <- 0
    
    for(k in kinase_names$Kinase){ #geneVarSite, is this by column?
      index <- index + 1
      Sum <- 0
      #grab substrates for k (while we figure out memory problem)
      #kinase_sites <- grab_substrates(k, cleanBCD, kinase_human)
      
      if(nrow(givenFile) != 0) {
        #set current table column names
        for(i in 1:nrow(givenFile)){
          #compute bicor correlation between module and the rest of it's indexes
          modSite <- as.vector(unlist(givenFile[i,]))
          modSite <- modSite[2:length(modSite)]
          c <- bicor(p, modSite)
          
          
          #compare c with every S and A
          Sprob <- length(which(as.vector(unlist(lapply(S, function(x) c > x))))) + 1
          
          Aprob <- length(which(as.vector(unlist(lapply(A, function(x) c > x))))) + 1
          
          Sum <- Sum + log2((Sprob/sharedLength)/(Aprob/allLength))
          
        }
        #add sum for kinase i in modularRank matrix with index of kinase(kinase_names)
        modularRank <- rbind(modularRank, c(index, Sum))
        currentTable[geneVarName, k] <- Sum
        
      }
    }
    
  }
  
  
  finalTable <- final_table(currentTable, topCount)
  #returns table with all correlations between individual kinases(no order) and
  #returns table with topN Kinases(names currently, working on adding probability value)
  return(list(currentTable, finalTable))
  
} 


S <- c(.2, .2, .4, .6, .8)
A <- c(.9, .9, .5, .5, .8, .1, .2, .9, .9, .9, .4)
file <- read_excel("data/BreastCancerData(merged.xlsx", sheet=2)
# S <- in app.R, call the nrows for the function networkAnalysis
# A <- in app.R, call the ncol for the function networkAnalysis
# move networkAnalysis to be called in app.R, for now, call it givenFile
tes<-example(S, A, 2, test=TRUE, givenFile)