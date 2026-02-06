# 2nd Step in the analysis on the MBSS data
# Makes sure that the site x species matrix and the trait matrix agree
# Also removes sites with less than a particular number of obs
# Tailored for Stream Size paper analysis
# written by blb 5/23/2011 and modified 1/20/2017

#Set Working Drive
setwd("C://Users/Ross/Google Drive/BrownLab Paper/Stream Size")#LENOVO
setwd("C://Users/rossv83/Google Drive/BrownLab Paper/Stream Size")#DELL

# Import the Trait matrix Data      
Trait <- data.frame(read.table(file="Trait Matrix.csv", sep = ',',  header=TRUE)) 
  # save(Trait, file='Trait.RData')

# Import the Site x species matrix
 load('BUGS.RData') # loads data frame 'Inverts' with the raw site x species matrix
 
 # Massaging the Site x Species matrix in a few ways

   # 1) Remove watersheds with < 5 sites

Invert <- c() 
shed.list <- unique(Inverts$SHEDCODE)  # looping through watersheds
for(i in 1:length(shed.list)){
   a <-  Inverts[Inverts$SHEDCODE==shed.list[i],]
   # ignoring watersheds with less than 5 sites
   if(dim(a)[1] > 5) {Invert <- rbind(a,Invert)}
}                      #  Notice that the name of the data.frame changed here....to Invert (no 's')

    # 2) Ordering the Invert dataset
A <- Invert[,-c(1:7)]
  A <- A[,order(names(A))]  
Invert <- data.frame(Invert[,c(1:7)], A)
    
    # 3) Filter species from 'Invert' to match the Trait matrix 
 A <- Invert[,-c(1:7)]
 B <- A[,names(A)%in%Trait[,1]]     
 Invert <- data.frame(Invert[,1:7], B)      
     
    # 4) Get rid of any rows with no data                          
Invert <- Invert[-c(which(rowSums(Invert[,-c(1:7)])==0)),]  

# Save the results
    save(Invert, file='Massaged.RData')
    
    a <- objects()
    rm(list=a)