#downloading needed packages for this script
install.packages("readxl")

#setting library used
library(readxl)


#reads KSA_human.txt and stores it as a data.frame object
#note: use "\t" to separate to avoid bugs when storing data.
#Ex. ksaH[3456,] stores incorrectly when using sep = "".
ksaH <- read.table("data/KSA_human.txt", header = TRUE, fill = TRUE, sep="\t")

#prints ksaH contents
ksaH

#concatenate site & kinases

#list of unique kinase names - questin: use incomparables param?
uniqueKinase <- unique(ksaH[1])

#for each unique kinase:
#find subrates, find intersection of the substrates w/ssites in breast cancer data
#compute pairwise correlation & store