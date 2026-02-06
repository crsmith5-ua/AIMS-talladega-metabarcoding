# Imports the Environmental Data from the MBSS Round 1 survey
# Tailored for Stream Size paper analysis
# written by blb 5/23/2011 and modified 1/20/2017

# The Inverts dataset needs to be in the workspace before you run this
library(vegan)
#setwd('C:/ACADEMIC FILES/COMMON ACADEMIC/PROJECTS/MBSS Data/Stream Size')
setwd("C://Users/Ross/Google Drive/BrownLab Paper/Stream Size")#LENOVO

# Import the Habitat Data          
Env <- data.frame(read.csv(file='Habitat.csv', header=TRUE))
  Env <- Env[,-c(2:8,10:13,16,17,19)]
  
# load the massaged dataset to limit the number of sites
load('Massaged.RData') # data.frame = Inverts

sheds <- unique(Invert$SHEDCODE)

a <- sheds%in%Env$SHEDCODE
  a <- a[which(a)==TRUE]
Habitat <- Env[a,]

# Dropping unwanted variables from the habitat matrix
# Hab <- Habitat[,-c(8,10,16,27,32,33,45,50,57,59:65)]
#  Hab1 <- Hab[complete.cases(Hab),]     
       
  

