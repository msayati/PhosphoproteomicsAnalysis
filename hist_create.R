#installing packages containing bicor
install_packageIF("WGCNA")

#manually installing GO.db if error happens
#source("http://bioconductor.org/biocLite.R") 
#biocLite(c("GO.db", "preprocessCore", "impute"))

#If the code on top doesn't work try and you keep getting biocLite error:

#BiocManager::install()  

#Installs missing packages if they are missing

install_packageIF("RSQLite")
install_packageIF("matrixStats")

library(WGCNA)

all_paircorr <- function(cbcd){
  #drops the first two columns because theyre not numeric value
  cbcd <- cbcd[,-c(1,2)]

  #does the correlation and saves it in dataframe corr
  corr <- bicor(t(cbcd))

  #dataframe upp contains the upper triangle of corr
  upp <- upper.tri(corr, diag = FALSE)
  all_corr <- corr[upp]
  
  return(all_corr)
}

# Code to test working function:
#all_corr <- all_paircorr(cleanBCD)
# Creates histogram of data
# hist(all_corr)
