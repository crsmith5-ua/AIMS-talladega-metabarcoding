# Creates a trait x site matrix to answer the question, How are traits distributed in network?
# Imports site x species & species x trait matrice
# Calculates FD at each site
# written by blb 11/14/2014 

#Set Working Drive

setwd("C://Users/Ross/Google Drive/BrownLab Paper/Stream Size")#LENOVO
setwd("C://Users/rossv83/Google Drive/BrownLab Paper/Stream Size")#DELL

library(vegan)                                                                                                                                                             


# Import the Trait matrix Data      
Trait <- data.frame(read.table(file="Trait Matrix.csv", sep = ',',  header=TRUE)) 
  names(Trait)[1] <- 'Taxa'
  save(Trait, file='Trait.RData')

# Import the Site x species matrix
load('Massaged.RData')
Sites <- Invert

###############################
AW <- TRUE  # chooses to abundance weight (TRUE) or not
Choose <-# 13:27# chooses the traits to keep
               2:60 #is all traits
              # 13:27 are the dispersal traits
##############################

# turning species matrix into presence absence
a <- ifelse(Sites[,-c(1:7)] > 0,1,0)
if(AW==FALSE) Sites[,-c(1:7)] <- a

# Matrix multiplication to get the quantitative Site x Trait matrix
A <- as.matrix(Sites[,-c(1:7)])
B <- as.matrix(Trait[,Choose])
Out <- A%*%B

SXT <- data.frame(Sites[,1:7], Out)
save(SXT, file='SXT.RData')

library(vegan)
All.mds <- metaMDS(SXT[,-c(1:7)], distance='altGower', k=2, autotransform=TRUE)

# making a figure of the results by stream order
plot(All.mds$points[,1], All.mds$points[,2], main='', xlab='NMDS1', ylab='NMDS2', type='n')
   points(All.mds$points[SXT$ORDER==1,1], All.mds$points[SXT$ORDER==1,2], pch=21, bg='black')
   points(All.mds$points[SXT$ORDER==2,1], All.mds$points[SXT$ORDER==2,2], pch=21, bg='white')
   points(All.mds$points[SXT$ORDER==3,1], All.mds$points[SXT$ORDER==3,2], pch=21, bg='blue')
                                                                                            