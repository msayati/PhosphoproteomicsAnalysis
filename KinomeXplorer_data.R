
library(stringr)

#creates an initial data frame for the kinotomeXplorer data
KinomeXplorer_data <- read.table("data/KinomeXplorer_data.txt", header = TRUE,
                                 sep = "\t")
#creates an initial data frame for the PhosphoSitePlus data
KSA_human <- read.table("data/KSA_human.txt", header = TRUE,
                        sep = "\t")

#sites for matrix, Rows
sites <- unique(KinomeXplorer_data[,2])
#kinase for matrix, Columns
kinase <- unique(KinomeXplorer_data[,3])

#TODO kinase order sort (daniel sort), Ksort = kinase(daniel sort)
####################################################################################

#sites x kinase matrix
#will need ksort
#slow
KxS = matrix(data = NA, nrow = length(sites), ncol = length(kinase), byrow = FALSE, dimnames = list(sites, kinase))


#this function reads and filters cleanBCD's pairs, score, and inserts them into KxS
#takes in p sites, and k kinase
getPairs <- function(p, k){
  
  for (i in 1:length(k)){
  #since p may have multiple sites, you need to split and plot them
  #cleans multiple data inputs within a cell, saves as a temporary numeric array for that kinase
  temp <- as.numeric(unlist(regmatches(p[i],gregexpr("[[:digit:]]+\\.*[[:digit:]]*",p[i]))))
  
  #inserts them into the KxS data
  #TODO needs to get the log and then add it to danny's calculations
  #TODO if not in kinomeXplorer, use danny data
  
  #loop for the multiple sites
  for (j in temp){
    #has to check if its there first
    tryCatch({
      KxS[as.character(j), as.character(k[i])] = 3
    }, 
    
    warning = function(warning_condition) {
      print("This is a warning from the insert")
    }, 
    
    error = function(error_condition) {
      print("This is an error from the insert")
    }, 
    
    finally={
      #use danny's input here for that coordinate
    })
    
    
    }
  }
}

#testing variables
phosphositeP <- cleanBCDinitial[1:10,2]
kinaseK <- cleanBCDinitial[1:10,1]
getPairs(phosphositeP, kinaseK)


test = 0
i = 1
temp = c(154, 369, 404, 401)
for (j in temp){
  
  KxS[as.character(j), kinaseK[3]] = 3
  
}







  
  