#set working directory for LENOVO computer
setwd("C://Users/Ross/Google Drive/BrownLab Paper/Stream Size")
#set working directory for  WC computer
#setwd("C://Users/rossv83/Google Drive/BrownLab Paper/Stream Size")

#load Packages
library(dplyr)
library(vegan)
library(ggplot2)
library(reshape2)

library(FD)
library(data.table)

#Load data set
load('Massaged.RData')
bugs<-Invert
load('Env.RData')
load('Trait.RData') #Data Frame Trait
load('ncbugs.RData') #ncbugs database
ncbugs <- FIN
load('ncfish.RData')
ncfish <- FIN
#ncfish <- read.csv("ncfish.transpose.csv", sep=",", header=TRUE)

mdfish<-read.csv("20170328_MBSS_Fish_Habitat.csv", sep=",",header=TRUE) #new fish data w many sites
env<-read.csv(file='Habitat.csv', header=TRUE)

fishtrait<-read.csv("fish trait database_subset.csv",sep=",",header=TRUE)
mdfishlist<-read.csv("mbss_fishlist.csv",sep=",",header=TRUE)
ncbug_traits<-read.csv("ncbug_traits.csv", sep=",", header=TRUE)
ncbug_list<-read.csv("ncbug_specieslist.csv", sep=",", header=TRUE)

ncfish_list<-read.csv("ncfish_specieslist.csv", sep=",", header=TRUE)
ncfishtrait<-read.csv("ncfish_traits.csv",sep=",",header=TRUE)

ncbugs <- ncbugs[,which(!apply(ncbugs==0,2,all))]#remove all species colums with sum=0
ncfish <- ncfish[,which(!apply(ncfish==0,2,all))]#remove all species colums with sum=0

strahler<-read.csv("nc site strahler.csv",sep=",",header=TRUE)
ncbugs<-left_join(ncbugs, strahler)
ncfish<-left_join(ncfish, strahler)


#load epa data
epafish <- read.csv("fishcts_pivot.csv",sep=",",header=TRUE) #EPA Fish Data
epafish[is.na(epafish)] <- 0 #replace NA to zeros
epafish_taxalist <- read.csv("fishcts_taxalist.csv",sep=",",header=TRUE) #EPA FISH Taxa List
epafish_traits <- read.csv("epa fish trait database.csv",sep=",",header=TRUE) #EPA Invert Taxa List

epainvert <- read.csv("nrsa0809bentcts_pivot.csv",sep=",",header=TRUE) #EPA Invert Data
epainvert[is.na(epainvert)] <- 0 #replace NA to zeros
epainvert_taxalist <- read.csv("epainvert_taxalist.csv",sep=",",header=TRUE) #EPA Invert Taxa List
epainvert_traits <- read.csv("Trait Matrix.csv",sep=",",header=TRUE) #EPA Invert Taxa List
epasiteinfo <- read.csv("epasiteinfo.csv",sep=",",header=TRUE) #EPA SITE INFO Data
epasiteinfo_sub <- epasiteinfo[epasiteinfo$VISIT_NO == 1, ]
epaphys <- read.csv("phablow.csv",sep=",",header=TRUE) #EPA PHYSICAL DATA
epaphys_sub <- epaphys[epaphys$VISIT_NO == 1, ]

epafish_fin <- merge(epafish, epasiteinfo_sub, by = "SITE_ID", all = T) # merge with site info
epafish_fin <- merge(epafish_fin, epaphys_sub, by = "SITE_ID", all = T) # merge with physical data

epainvert_fin <- left_join(epainvert, epasiteinfo_sub, by = "SITE_ID") # merge with site info
epainvert_fin <- left_join(epainvert_fin, epaphys_sub, by = "SITE_ID") # merge with physical data

#look at nc bug and fish data
#uni<-ncbugs %>%
#    group_by(Waterbody) %>%
#  summarise(avg_DA = mean(DA..sq.mi.., na.rm=TRUE))

#uni2<-ncfish %>%
#  group_by(Waterbody) %>%
#  summarise(avg_DA = mean(DA..sq.mi.., na.rm=TRUE))

#organize nc bug and fish data

# Transposing the Bug Data########

site.list <- unique(ncbugs$site)
which(ncbugs$site==site.list[35])
#ncbugs<-ncbugs[-which(ncbugs$site==site.list[35]),]
#ncbugs<-ncbugs[-35,]
# Providing a starting value
a <- ncbugs[ncbugs$site==site.list[1],]
b <- data.frame(c(a[1,1:12], a[,16]))#1-14 are ID colummns, not count data
names(b)[-c(1:12)] <-  as.character(a[,15])
FIN <- b   

for(i in 2:length(site.list)){
  a <- ncbugs[ncbugs$site==site.list[i],]
  b <- data.frame(c(a[1,1:12], a[,16]))
  names(b)[-c(1:12)] <-  as.character(a[,15])
  b=b[,!duplicated(colnames(b))]
  # print(b)                                    
  FIN <- merge(FIN, b, all=TRUE)   
}
FIN[is.na(FIN)] <- 0 
#names(FIN)[c(5, 42, 71, 72, 113, 394, 408, 339,410)] <- c("SHEDCODE", "CRICOTOPUS.ORTHOCLADIUS", "ORTHOCLADIINAE.A", "ORTHOCLADIINAE.A1", "PRODIAMESA.SP", "VALVATA.SP", "ORTHOCLADIINAE.B", 'UKS1', 'UKS2')

# Write to some external files        

save(FIN, file='ncbugs.RData')  # transposed bug set only  
write.table(FIN, file='ncbugs.transpose.csv', sep=',')  

# Transposing the Fish Data

site.list <- unique(ncfish$site)

# Providing a starting value
a <- ncfish[ncfish$site==site.list[1],]
b <- data.frame(c(a[1,1:12], a[,15]))#1-12 are ID colummns, not count data
names(b)[-c(1:12)] <-  as.character(a[,14])
FIN <- b   

for(i in 2:length(site.list)){
  a <- ncfish[ncfish$site==site.list[i],]
  b <- data.frame(c(a[1,1:12], a[,15]))
  names(b)[-c(1:12)] <-  as.character(a[,14])
  b=b[,!duplicated(colnames(b))]
  # print(b)                                    
  FIN <- merge(FIN, b, all=TRUE)   
}
FIN[is.na(FIN)] <- 0 
#names(FIN)[c(5, 42, 71, 72, 113, 394, 408, 339,410)] <- c("SHEDCODE", "CRICOTOPUS.ORTHOCLADIUS", "ORTHOCLADIINAE.A", "ORTHOCLADIINAE.A1", "PRODIAMESA.SP", "VALVATA.SP", "ORTHOCLADIINAE.B", 'UKS1', 'UKS2')

# Write to some external files        

save(FIN, file='ncfish.RData')  # transposed bug set only  
write.table(FIN, file='ncfish.transpose.csv', sep=',') 


############## Fish and bug data summaries #############
ncenv <-ncbugs %>%
  group_by(waterbody) %>%
  summarise_each(funs(mean), shed_area_sqmi, stream_width_m, huc_8)
ncbugs_full<-ncbugs %>%
  left_join(strahler)
ncfish_full<-ncfish %>%
  left_join(strahler)

species<-unique(ncbugs$scientific_name)
sites <- unique(ncbugs$site) #identify unique watersheds

bugs<-NULL

for (i in 1:length(sites)){
  
  #*********************************************************************************
  
  dat <- ncbugs[ncbugs$site==sites[i],]
  index=match(species, dat$scientific_name)
  new.abund=rep(NA,length(species))
  new.abund[!is.na(index)]<-dat[,16]
  options(warn=2)
  new=c(t(dat[1,1:13]), new.abund)
  bugs=rbind(bugs, new)

}  
sites_bugs<-cbind(sites, bugs)  

ncbugs_fix<-merge(sites_bugs, ncenv, by="Waterbody")



##### EPA DATASET ######

# Create new data frame named ssdiv from Diversity Metrics and Stream Size Metrics

epafish_ssdiv = data.frame(site = paste(epafish_fin[,"SITE_ID"]),
                           coord_lat = epafish_fin$LAT_DD83.x ,
                           coord_lon = epafish_fin$LON_DD83.y ,
                           strahler = epafish_fin$STRAHLERORDER.x ,
                           avgwidth = epafish_fin$XWIDTH ,
                           shed_huc8 = epafish_fin$HUC8,
                           state_name = epafish_fin$STATE,
                           shedarea_km2 = epafish_fin$WSAREA_NARS, 
                           epafish_Hshannon = diversity(epafish_fin[,c(2:624)],1,index="shannon"),
                           epafish_Hsimpson = diversity(epafish_fin[,c(2:624)],1,index="simpson"),
                           epafish_J = diversity(epafish_fin[,c(2:624)],1,index="shannon")/log(specnumber(epafish_fin[,c(2:624)])),
                           epafish_S = specnumber(epafish_fin[,c(2:624)]))

epainvert_ssdiv = data.frame(site = paste(epainvert_fin[,"SITE_ID"]),
                             coord_lat = epainvert_fin$LAT_DD83.x ,
                             coord_lon = epainvert_fin$LON_DD83.y ,
                             strahler = epainvert_fin$STRAHLERORDER.x ,
                             avgwidth = epainvert_fin$XWIDTH ,
                             shedarea_km2 = epainvert_fin$WSAREA_NARS ,
                             shed_huc8 = epainvert_fin$HUC8, 
                             state_name = epainvert_fin$STATE,
                             epainvert_Hshannon = diversity(epainvert_fin[,c(2:975)],1,index="shannon") ,
                             epainvert_Hsimpson = diversity(epainvert_fin[,c(2:975)],1,index="simpson") ,
                             epainvert_J = diversity(epainvert_fin[,c(2:975)],1,index="shannon")/log(specnumber(epainvert_fin[,c(2:975)])),
                             epainvert_S = specnumber(epainvert_fin[,c(2:975)]))

sites <- epainvert_ssdiv  %>% 
  group_by(site) %>%
  summarise(no_rows = length(site))

# write.table(epainvert_ssdiv, file='epa_invert_metrics.csv', sep=',')

################## Calculate Functional Diversity##################

#functional diversity for EPA INVERT
#trait dataframe
combined <- sort(union(levels((epainvert_taxalist$GENUSNAME)), levels(epainvert_traits$GENUSNAME)))
x <- left_join(mutate(epainvert_taxalist, GENUSNAME=factor(GENUSNAME, levels=combined)),
               mutate(epainvert_traits,  GENUSNAME=factor(GENUSNAME, levels=combined)))

rownames(x)<- x[,1]
x2 <- x[,-1] #make taxa names as rownames

#need to remove taxa that did not have trait data
xsubset <- na.omit(x2)
#abundance matrix
abun<-as.matrix(epainvert[,c(2:975)])
rownames(abun)<- epainvert[,1]
#need to remove taxa that did not have trait data
abunsubset <- abun[,colnames(abun) %in% rownames(xsubset)]
abunsubset2 <- na.omit(abunsubset)
#Do all Species have abundances > 0?
colSums(abunsubset3) #All taxa have have values for sites
#Do all Sites have abundances > 0?
rowSums(abunsubset3) #NOT all taxa have have values for sites
#remove Sites that have zero abundances
abunsubset3 = abunsubset2[rowSums(abunsubset2)!= 0, ] 
abunsubset4 = abunsubset3[, colSums(abunsubset3)!= 0] #remove any species without abundance
#need to remove taxa that did not have abundance from trait data
xsubset2 <- xsubset[rownames(xsubset) %in% colnames(abunsubset4),]
# create list of which species did not have trait data
xsubset2_list <- as.data.frame(row.names(xsubset2))
colnames(xsubset2_list)= paste("taxa") # Add Column Names
x_list <- as.data.frame(row.names(x))
colnames(x_list)= paste("taxa") # Add Column Names
#epabugs_misstrait <- anti_join (xsubset2_list, x_list)
epabugs_misstrait <- as.data.frame(as.character(x_list[!x_list$taxa%in%xsubset2_list$taxa,]))
colnames(epabugs_misstrait)= paste("taxa missing traits") # Add Column Names
write.csv(epabugs_misstrait, "epainvert_misstrait_list.csv")
#Do Species Names match in Trait and Species dataframes?
test<-function(abun,x){
  if(any(colnames(abun) != rownames(x)))
    stop("species names in abun and x do not match")
  abun<-abun*2
  abun        
}
test(abunsubset4, xsubset2)

#caclulate FD
fd <- dbFD(xsubset2, abunsubset4, corr="cailliez" )

#organizing FD results for merging with SSDIV
epainvert_fd <- cbind(fd$FRic, fd$FEve, fd$FDiv, fd$FDis, fd$RaoQ) #extract needed FD metrics, add site names
epainvert_fd <- as.data.frame(epainvert_fd)
setnames(epainvert_fd, old=c("V1","V2", "V3", "V4", "V5"), new=c("epainvert_FRic", "epainvert_FEve","epainvert_FDiv", "epainvert_FDis", "epainvert_RaoQ"))
setDT(epainvert_fd, keep.rownames = TRUE)[]
colnames(epainvert_fd)[1] <- "site"
#Join with INVERT SSDIV
epainvert_ssdiv <- left_join(epainvert_ssdiv, epainvert_fd, by="site")


#functional diversity for EPA FISH
#trait dataframe
combined <- sort(union(levels((epafish_taxalist$COMMONNAME)), levels(epafish_traits$COMMONNAME)))
x <- left_join(mutate(epafish_taxalist, COMMONNAME=factor(COMMONNAME, levels=combined)),
               mutate(epafish_traits,  COMMONNAME=factor(COMMONNAME, levels=combined)))

rownames(x)<- x[,1]
x2 <- x[,-1] #make taxa names as rownames
x2[x2==-1|x2==-555|x2==-999] = NA #give NA to -1 and -555 values

#need to remove taxa that did not have trait data
xsubset <- na.omit(x2)
#abundance matrix
abun<-as.matrix(epafish[,c(2:624)])
rownames(abun)<- epafish[,1]
#need to remove taxa that did not have trait data
abunsubset <- abun[,colnames(abun) %in% rownames(xsubset)]
#Do all Species have abundances > 0?
colSums(abunsubset) #All taxa have have values for sites
#Do all Sites have abundances > 0?
rowSums(abunsubset) #NOT all taxa have have values for sites
#remove Sites that have zero abundances
abunsubset2 = abunsubset[rowSums(abunsubset)!= 0, ] #remove samples w no abundance
#need to remove taxa that did not have abundance from trait data
xsubset2 <- xsubset[rownames(xsubset) %in% colnames(abunsubset2),]
# create list of which species did not have trait data
xsubset2_list <- as.data.frame(row.names(xsubset2))
colnames(xsubset2_list)= paste("taxa") # Add Column Names
x_list <- as.data.frame(row.names(x))
colnames(x_list)= paste("taxa") # Add Column Names
epafish_misstrait <- as.data.frame(as.character(x_list[!x_list$taxa%in%xsubset2_list$taxa,]))
colnames(epafish_misstrait)= paste("taxa missing traits") # Add Column Names
write.csv(epafish_misstrait, "epafish_misstrait_list.csv")
#Do Species Names match in Trait and Species dataframes?
test<-function(abun,x){
  if(any(colnames(abun) != rownames(x)))
    stop("species names in abun and x do not match")
  abun<-abun*2
  abun        
}
test(abunsubset2, xsubset2)

#caclulate FD
fd <- dbFD(xsubset2, abunsubset2, CWM.type="all", corr="cailliez" )
#organizing FD results for merging with SSDIV
epafish_fd <- cbind(fd$FRic, fd$FEve, fd$FDiv, fd$FDis, fd$RaoQ) #extract needed FD metrics, add site names
epafish_fd <- as.data.frame(epafish_fd)
setnames(epafish_fd, old=c("V1","V2", "V3", "V4", "V5"), new=c("epafish_FRic", "epafish_FEve","epafish_FDiv", "epafish_FDis", "epafish_RaoQ"))
setDT(epafish_fd, keep.rownames = TRUE)[]
colnames(epafish_fd)[1] <- "site"
#Join with INVERT SSDIV
epafish_ssdiv <- left_join(epafish_ssdiv, epafish_fd, by="site")

#functional diversity for MBSS fish

#create trait database with only species found on fishlist
combined <- sort(union(levels(mdfishlist$COMMONNAME), levels(fishtrait$COMMONNAME)))
x <- left_join(mutate(mdfishlist, COMMONNAME=factor(COMMONNAME, levels=combined)),
               mutate(fishtrait,  COMMONNAME=factor(COMMONNAME, levels=combined)))




#abundance matrix
abun<-as.matrix(newenvfish[,c(2:80)])
rownames(abun)<-newenvfish[,1] # need to add rownames because some sites from newenvfish will be lost due to insufficient trait data
#Do all Species have abundances > 0?
colSums(abun) #YAASSS
rowSums(abunsubset) #YAASSS
# make species as rownames in trait database
rownames(x)<-x[,1]
x2<-x[,-1]
#need to remove taxa that did not have trait data
xsubset<-x2[!rownames(x2) %in% c("American_eel", "lepomis_hybrid", "blue_ridge_sculpin", "cyprinid_unknown", "Notropis_sp", "cyprinid_hybrid",
                                 "lamprey_unknown", "sculpin_unknown", "Cyprinella_sp", "darter_unknown", "bullhead_unknown", "Luxilis_sp", "sunfish_unknown", 
                                 "sunfish_hybrid", "inland_silverside", "blueback_herring", "alewife", "American_shad"),]

xsubset[xsubset==-1|xsubset==-555] = NA #give NA to -1 and -555 values

xsubset2 <- data.frame(sapply(xsubset, function(x) as.numeric(as.character(x)))) #convert into Numeric for package FD
rownames(xsubset2)<-rownames(xsubset) #adding row names 

#xsubset<-xsubset[,-33] #removed from origincal trait_subset

# remove taxa with no trait info from abun
#Remove taxa w no values from abun
abunsubset<-abun[,!colnames(abun) %in% c("American_eel", "lepomis_hybrid", "blue_ridge_sculpin", "cyprinid_unknown", "Notropis_sp", "cyprinid_hybrid",
                                         "lamprey_unknown", "sculpin_unknown", "Cyprinella_sp", "darter_unknown", "bullhead_unknown", "Luxilis_sp", "sunfish_unknown", 
                                         "sunfish_hybrid", "inland_silverside", "blueback_herring","alewife", "American_shad")]

colSums(abunsubset) #YAASSS
abunsubset= abunsubset[ rowSums(abunsubset[,])!=0, ] #remove sites with zero abundances
fishsites<-rownames(abunsubset) #make a list of sites to later merge with fish_ssdiv
rownames(abunsubset) <- c() #need to remove rownames for package FD or they will remain and create havoc
# create list of which species did not have trait data
xsubset_list <- as.data.frame(row.names(xsubset))
colnames(xsubset_list)= paste("taxa") # Add Column Names
x_list <- as.data.frame(row.names(x))
colnames(x_list)= paste("taxa") # Add Column Names
mdbugs_misstrait <- as.data.frame(as.character(x_list[!x_list$taxa%in%xsubset_list$taxa,]))
colnames(mdbugs_misstrait)= paste("taxa missing traits") # Add Column Names
write.csv(mdbugs_misstrait, "mdfish_misstrait_list.csv")

#Do Species Names match in Trait and Species dataframes?
test<-function(abun,x){
  if(any(colnames(abun) != rownames(x)))
    stop("species names in abun and x do not match")
  abun<-abun*2
  abun        
}
test(abunsubset, xsubset) #if no errors then YASSS!

#functional composition
#comp<-functcomp(as.matrix(xsubset),as.matrix(abunsubset), CWM.type="all") #if func comp dataset is needed

#caclulate FD
fd <- dbFD(xsubset2, abunsubset, corr="cailliez" )
#combine FD results with ENV and Site Data

fish_FD<-cbind(fishsites, fd$FRic, fd$FEve, fd$FDiv, fd$FDis, fd$RaoQ) #extract needed FD metrics, add site names
fish_FD<-as.data.frame(fish_FD) #need to convert df before adding colnames
colnames(fish_FD) <- c("site","fish_FRic", "fish_FEve", "fish_FDiv", "fish_FDis", "fish_RaoQ") #add colnames

fish_ssdiv<-left_join(fish_ssdiv, fish_FD) #merge with fish_ssdiv
cols = c(11, 12, 13, 14, 15)    #need to convert these cols to numeric
fish_ssdiv[,cols] = apply(fish_ssdiv[,cols],2,function(x) as.numeric(as.character(x))) #converting cols to numeric

#fish_ssdiv<-cbind(fish_ssdiv, fd$FRic, fd$FEve, fd$FDiv, fd$FDis, fd$RaoQ)
#setnames(bug_ssdiv, old=c("fd$FRic","fd$FEve", "fd$FDiv", "fd$FDis", "fd$RaoQ"), new=c("bug_FRic", "bug_FEve","bug_FDiv", "bug_FDis", "bug_RaoQ"))



################ Calculate Beta Diversity ##############################

#EPA INVERT#
sheds <- unique(epainvert_fin$HUC8) #indentify unique watersheds
alpharich<-NULL
betarich<-NULL
gammarich<-NULL
for (i in 1:length(sheds)){
  
  #*********************************************************************************
  
  dat <- epainvert_fin[epainvert_fin$HUC8==sheds[i],]
  
  #numbers equivilents of alpha for watershed, species richness are diversity measures
  alpha <- d(dat[,c(2:975)], q=0) 
  beta <- d(dat[,c(2:975)], lev="beta", q=0)
  gamma <- d(dat[,c(2:975)], lev="gamma", q=0)
  
  #alpha<-d(dat[,-c(1:7)], q=1) 
  #beta<-d(dat[,-c(1:7)], lev="beta", q=1)
  #gamma<-d(dat[,-c(1:7)], lev="gamma", q=1)
  
  #Standard Diversity Indices of alpha for watershed, species richness are diversity measures
  #alphaH<-H(dat[,-c(1:7)], q=1) 
  #betaH<-H(dat[,-c(1:7)], lev="beta", q=1)
  #gammaH<-H(dat[,-c(1:7)], lev="gamma", q=1)
  
  
  alpharich[i]<-alpha
  betarich[i]<-beta
  gammarich[i]<-gamma
}  
abgvalues<-cbind(sheds, alpharich, betarich, gammarich)  
abgvalues<-data.frame(abgvalues) 

#FISH#
#fish_nozeros<- newenvfish[rowSums(newenvfish[,c(2:80)])!=0, ] #remove rows with ZERO fish abundance
#fish_final<- fish_nozeros[complete.cases(fish_nozeros[,c(2:80)]),] #remove NAs
sheds <- unique(epafish_fin$HUC8) #indentify unique watersheds
alpharich<-NULL
betarich<-NULL
gammarich<-NULL
for (i in 1:length(sheds)){
  
  #*********************************************************************************
  
  dat <- epafish_fin[epafish_fin$HUC8==sheds[i],]
  
  #numbers equivilents of alpha for watershed, species richness are diversity measures
  alpha<-d(dat[,c(2:624)], q=0) 
  beta<-d(dat[,c(2:624)], lev="beta", q=0)
  gamma<-d(dat[,c(2:624)], lev="gamma", q=0)
  
  #alpha<-d(dat[,-c(1:7)], q=1) 
  #beta<-d(dat[,-c(1:7)], lev="beta", q=1)
  #gamma<-d(dat[,-c(1:7)], lev="gamma", q=1)
  
  #Standard Diversity Indices of alpha for watershed, species richness are diversity measures
  #alphaH<-H(dat[,-c(1:7)], q=1) 
  #betaH<-H(dat[,-c(1:7)], lev="beta", q=1)
  #gammaH<-H(dat[,-c(1:7)], lev="gamma", q=1)
  
  
  alpharich[i]<-alpha
  betarich[i]<-beta
  gammarich[i]<-gamma
}  
fish_abgvalues<-cbind(sheds, alpharich, betarich, gammarich)  
fish_abgvalues<-data.frame(fish_abgvalues) 

###### Correct Diversity Based On Watershed Diversity (divide by Watershed Richness/Diversity) #####
#INVERTS#
#calculate mean shed richness
shedrich<-epainvert_ssdiv %>%
  group_by(shed_huc8) %>%
  summarise(shedrich = mean(epainvert_S, na.rm=TRUE))
#calculate mean shed diversity
sheddiv<-epainvert_ssdiv %>%
  group_by(shed_huc8) %>%
  summarise(sheddiv = mean(epainvert_Hshannon, na.rm=TRUE))
#match the mean shed richness and diversity with ssdiv
index=match(epainvert_ssdiv$shed_huc8,abgvalues$sheds)
index2=match(epainvert_ssdiv$shed_huc8,sheddiv$shed_huc8)
epainvert_ssdiv$shedrich_mean=shedrich$shedrich[index]
epainvert_ssdiv$sheddiv_mean=sheddiv$sheddiv[index2]
epainvert_ssdiv$betadiv=abgvalues$betarich[index]
# divide site richness/diversity by mean watershed richness/diversity
epainvert_ssdiv$epainvert_S_cor = epainvert_ssdiv$epainvert_S/epainvert_ssdiv$shedrich_mean
epainvert_ssdiv$epainvert_Hshannon_cor = epainvert_ssdiv$epainvert_Hshannon/epainvert_ssdiv$sheddiv_mean

#fish#

#calculate mean shed richness
shedrich<-epafish_ssdiv %>%
  group_by(shed_huc8) %>%
  summarise(shedrich = mean(epafish_S, na.rm=TRUE))
#calculate mean shed diversity
sheddiv<-epafish_ssdiv %>%
  group_by(shed_huc8) %>%
  summarise(sheddiv = mean(epafish_Hshannon, na.rm=TRUE))
#match the mean shed richness and diversity with ssdiv
index=match(epafish_ssdiv$shed_huc8,fish_abgvalues$sheds)
index2=match(epafish_ssdiv$shed_huc8,sheddiv$shed_huc8)
epafish_ssdiv$shedrich_mean=shedrich$shedrich[index]
epafish_ssdiv$sheddiv_mean=sheddiv$sheddiv[index2]
epafish_ssdiv$betadiv=abgvalues$betarich[index]
# divide site richness/diversity by mean watershed richness/diversity
epafish_ssdiv$epafish_S_cor = epafish_ssdiv$epafish_S/epafish_ssdiv$shedrich_mean
epafish_ssdiv$epafish_Hshannon_cor = epafish_ssdiv$epafish_Hshannon/epafish_ssdiv$sheddiv_mean

#######Correct Beta diversity Based on Watershed area##########
#Inverts#
#calculate total shed area
shedarea_total <- epainvert_ssdiv %>%
  group_by(shed_huc8) %>%
  summarise(shedarea_total= sum(shedarea_km2, na.rm=TRUE))
#match the mean shed richness and diversity with ssdiv
index=match(epainvert_ssdiv$shed_huc8,shedarea_total$shed_huc8)
epainvert_ssdiv$shedarea_total=shedarea_total$shedarea_total[index]
#Plot beta richness by total watershed area
plot(log(epainvert_ssdiv$shedarea_total), epainvert_ssdiv$betadiv)
# divide site beta richness by total watershed area
epainvert_ssdiv$betadiv_cor = epainvert_ssdiv$betadiv/epainvert_ssdiv$shedarea_total

#fish#
#calculate total shed area
shedarea_total <- epafish_ssdiv %>%
  group_by(shed_huc8) %>%
  summarise(shedarea_total= sum(shedarea_km2, na.rm=TRUE))
#match the mean shed richness and diversity with ssdiv
index=match(epafish_ssdiv$shed_huc8,shedarea_total$shed_huc8)
epafish_ssdiv$shedarea_total=shedarea_total$shedarea_total[index]
#Plot beta richness by total watershed area
plot(log(epafish_ssdiv$shedarea_total), epafish_ssdiv$betadiv)
# divide site beta richness by mean watershed area
epafish_ssdiv$betadiv_cor = epafish_ssdiv$betadiv/epafish_ssdiv$shedarea_total

#WRITE TO EXTERNAL FILE
write.csv(epainvert_ssdiv, "epainvert_ssdiv.csv")
write.csv(epafish_ssdiv, "epafish_ssdiv.csv")





##### MBSS DATASET ######

# Subset ENV Data for only SITES in Bugs
newenv<-merge(bugs,env,by="SITE")
newenvfish<-merge(mdfish,env,by="SITE")

# Create new data frame named ssdiv from Diversity Metrics and Stream Size Metrics
bug_ssdiv = data.frame(site = paste(newenv[,"SITE"]),
                   shedcode = paste(bugs[,"SHEDCODE"]),
                   strahler = paste(bugs[, "ORDER"]),
                   avgwidth = newenv$AVGWID,
                   flow = newenv$FLOW,
                   shedarea = newenv$ACREAGE*4046.86, # converted from acres to m2
                   bug_Hshannon = diversity(bugs[,c(8:250)],1,index="shannon"),
                   bug_Hsimpson = diversity(bugs[,c(8:250)],1,index="simpson"),
                   bug_J = diversity(bugs[,c(8:250)],1,index="shannon")/log(specnumber(bugs[,c(8:250)])),
                   bug_S = specnumber(bugs[,c(8:250)]))

# Create new data frame named ssdiv from Diversity Metrics and Stream Size Metrics
fish_ssdiv = data.frame(site = paste(newenvfish[,"SITE"]),
                       shedcode = paste(newenvfish[,"SHEDCODE"]),
                       strahler = paste(newenvfish[, "ORDER"]),
                       avgwidth = newenvfish$AVGWID,
                       flow = newenvfish$FLOW,
                       shedarea = newenvfish$ACREAGE,
                       fish_Hshannon = diversity(newenvfish[,c(2:80)],1,index="shannon"),
                       fish_Hsimpson = diversity(newenvfish[,c(2:80)],1,index="simpson"),
                       fish_J = diversity(newenvfish[,c(2:80)],1,index="shannon")/log(specnumber(newenvfish[,c(2:80)])),
                       fish_S = specnumber(newenvfish[,c(2:80)]))


################## Calculate Functional Diversity##################

#functional diversity for MBSS Bugs
#trait dataframe
Trait[,c(2:60)] <- lapply(Trait[,c(2:60),drop=FALSE],as.numeric) #convert O and 1 to Numeric
#abundance matrix
abun<-as.matrix(bugs[,c(8:250)])
#Do all Species have abundances > 0?
colSums(abun) #HOMOPLECTRA, PERLESTA, ZAVRELIA and ODONTOMESA do not have values for any site, need to remove from Trait and abun
#Remove taxa w no values from Trait
xsubset<-Trait[!rownames(Trait) %in% c("100","146", "175","242"),]
xsubset2<-xsubset[,-1]
rownames(xsubset2)<-xsubset[,1]
#Remove taxa w no values from abun
abunsubset<-abun[,!colnames(abun) %in% c("ODONTOMESA", "ZAVRELIA", "PERLESTA", "HOMOPLECTRA")]
# create list of which species did not have trait data
xsubset2_list <- as.data.frame(row.names(xsubset2))
colnames(xsubset2_list)= paste("taxa") # Add Column Names
x_list <- as.data.frame(Trait$Taxa)
colnames(x_list)= paste("taxa") # Add Column Names
mdbugs_misstrait <- as.data.frame(as.character(x_list[!x_list$taxa%in%xsubset2_list$taxa,]))
colnames(mdbugs_misstrait)= paste("taxa missing traits") # Add Column Names
write.csv(mdbugs_misstrait, "mdbugs_misstrait_list.csv")

#Do Species Names match in Trait and Species dataframes?
test<-function(abun,x){
  if(any(colnames(abun) != rownames(x)))
    stop("species names in abun and x do not match")
  abun<-abun*2
  abun        
}
test(abunsubset, xsubset2)
#caclulate FD
fd <- dbFD(xsubset2, abunsubset, CWM.type="all", corr="cailliez" )
#combine FD results with ENV and Site Data
bug_ssdiv<-cbind(bug_ssdiv, fd$FRic, fd$FEve, fd$FDiv, fd$FDis, fd$RaoQ)

setnames(bug_ssdiv, old=c("fd$FRic","fd$FEve", "fd$FDiv", "fd$FDis", "fd$RaoQ"), new=c("bug_FRic", "bug_FEve","bug_FDiv", "bug_FDis", "bug_RaoQ"))





#functional diversity for MBSS fish

#create trait database with only species found on fishlist
combined <- sort(union(levels(mdfishlist$COMMONNAME), levels(fishtrait$COMMONNAME)))
x <- left_join(mutate(mdfishlist, COMMONNAME=factor(COMMONNAME, levels=combined)),
               mutate(fishtrait,  COMMONNAME=factor(COMMONNAME, levels=combined)))




#abundance matrix
abun<-as.matrix(newenvfish[,c(2:80)])
rownames(abun)<-newenvfish[,1] # need to add rownames because some sites from newenvfish will be lost due to insufficient trait data
#Do all Species have abundances > 0?
colSums(abun) #YAASSS
rowSums(abunsubset) #YAASSS
# make species as rownames in trait database
rownames(x)<-x[,1]
x2<-x[,-1]
#need to remove taxa that did not have trait data
xsubset<-x2[!rownames(x2) %in% c("American_eel", "lepomis_hybrid", "blue_ridge_sculpin", "cyprinid_unknown", "Notropis_sp", "cyprinid_hybrid",
                                      "lamprey_unknown", "sculpin_unknown", "Cyprinella_sp", "darter_unknown", "bullhead_unknown", "Luxilis_sp", "sunfish_unknown", 
                                      "sunfish_hybrid", "inland_silverside", "blueback_herring", "alewife", "American_shad"),]

xsubset[xsubset==-1|xsubset==-555] = NA #give NA to -1 and -555 values

xsubset2 <- data.frame(sapply(xsubset, function(x) as.numeric(as.character(x)))) #convert into Numeric for package FD
rownames(xsubset2)<-rownames(xsubset) #adding row names 

#xsubset<-xsubset[,-33] #removed from origincal trait_subset

# remove taxa with no trait info from abun
#Remove taxa w no values from abun
abunsubset<-abun[,!colnames(abun) %in% c("American_eel", "lepomis_hybrid", "blue_ridge_sculpin", "cyprinid_unknown", "Notropis_sp", "cyprinid_hybrid",
                                         "lamprey_unknown", "sculpin_unknown", "Cyprinella_sp", "darter_unknown", "bullhead_unknown", "Luxilis_sp", "sunfish_unknown", 
                                         "sunfish_hybrid", "inland_silverside", "blueback_herring","alewife", "American_shad")]

colSums(abunsubset) #YAASSS
abunsubset= abunsubset[ rowSums(abunsubset[,])!=0, ] #remove sites with zero abundances
fishsites<-rownames(abunsubset) #make a list of sites to later merge with fish_ssdiv
rownames(abunsubset) <- c() #need to remove rownames for package FD or they will remain and create havoc
#Do Species Names match in Trait and Species dataframes?
test<-function(abun,x){
  if(any(colnames(abun) != rownames(x)))
    stop("species names in abun and x do not match")
  abun<-abun*2
  abun        
}
test(abunsubset, xsubset) #if no errors then YASSS!

#functional composition
#comp<-functcomp(as.matrix(xsubset),as.matrix(abunsubset), CWM.type="all") #if func comp dataset is needed

#caclulate FD
fd <- dbFD(xsubset2, abunsubset, corr="cailliez" )
#combine FD results with ENV and Site Data

fish_FD<-cbind(fishsites, fd$FRic, fd$FEve, fd$FDiv, fd$FDis, fd$RaoQ) #extract needed FD metrics, add site names
fish_FD<-as.data.frame(fish_FD) #need to convert df before adding colnames
colnames(fish_FD) <- c("site","fish_FRic", "fish_FEve", "fish_FDiv", "fish_FDis", "fish_RaoQ") #add colnames

fish_ssdiv<-left_join(fish_ssdiv, fish_FD) #merge with fish_ssdiv
cols = c(11, 12, 13, 14, 15)    #need to convert these cols to numeric
fish_ssdiv[,cols] = apply(fish_ssdiv[,cols],2,function(x) as.numeric(as.character(x))) #converting cols to numeric
                  
#fish_ssdiv<-cbind(fish_ssdiv, fd$FRic, fd$FEve, fd$FDiv, fd$FDis, fd$RaoQ)
#setnames(bug_ssdiv, old=c("fd$FRic","fd$FEve", "fd$FDiv", "fd$FDis", "fd$RaoQ"), new=c("bug_FRic", "bug_FEve","bug_FDiv", "bug_FDis", "bug_RaoQ"))



################ Calculate Beta Diversity ##############################

#BUGS#
sheds <- unique(Invert$SHEDCODE) #indentify unique watersheds
alpharich<-NULL
betarich<-NULL
gammarich<-NULL
for (i in 1:length(sheds)){
 
 #*********************************************************************************
  
  dat <- Invert[Invert$SHEDCODE==sheds[i],]
  
  #numbers equivilents of alpha for watershed, species richness are diversity measures
  alpha<-d(dat[,-c(1:7)], q=0) 
  beta<-d(dat[,-c(1:7)], lev="beta", q=0)
  gamma<-d(dat[,-c(1:7)], lev="gamma", q=0)
  
  #alpha<-d(dat[,-c(1:7)], q=1) 
  #beta<-d(dat[,-c(1:7)], lev="beta", q=1)
  #gamma<-d(dat[,-c(1:7)], lev="gamma", q=1)
  
  #Standard Diversity Indices of alpha for watershed, species richness are diversity measures
  #alphaH<-H(dat[,-c(1:7)], q=1) 
  #betaH<-H(dat[,-c(1:7)], lev="beta", q=1)
  #gammaH<-H(dat[,-c(1:7)], lev="gamma", q=1)
  
  
  alpharich[i]<-alpha
  betarich[i]<-beta
  gammarich[i]<-gamma
}  
abgvalues<-cbind(sheds, alpharich, betarich, gammarich)  
abgvalues<-data.frame(abgvalues) 

#FISH#
fish_nozeros<- newenvfish[rowSums(newenvfish[,c(2:80)])!=0, ] #remove rows with ZERO fish abundance
fish_final<- fish_nozeros[complete.cases(fish_nozeros[,c(2:80)]),] #remove NAs
sheds <- unique(fish_final$SHEDCODE) #indentify unique watersheds
alpharich<-NULL
betarich<-NULL
gammarich<-NULL
for (i in 1:length(sheds)){
  
  #*********************************************************************************
  
  dat <- fish_final[fish_final$SHEDCODE==sheds[i],]
  
  #numbers equivilents of alpha for watershed, species richness are diversity measures
  alpha<-d(dat[,c(2:80)], q=0) 
  beta<-d(dat[,c(2:80)], lev="beta", q=0)
  gamma<-d(dat[,c(2:80)], lev="gamma", q=0)
  
  #alpha<-d(dat[,-c(1:7)], q=1) 
  #beta<-d(dat[,-c(1:7)], lev="beta", q=1)
  #gamma<-d(dat[,-c(1:7)], lev="gamma", q=1)
  
  #Standard Diversity Indices of alpha for watershed, species richness are diversity measures
  #alphaH<-H(dat[,-c(1:7)], q=1) 
  #betaH<-H(dat[,-c(1:7)], lev="beta", q=1)
  #gammaH<-H(dat[,-c(1:7)], lev="gamma", q=1)
  
  
  alpharich[i]<-alpha
  betarich[i]<-beta
  gammarich[i]<-gamma
}  
fish_abgvalues<-cbind(sheds, alpharich, betarich, gammarich)  
fish_abgvalues<-data.frame(fish_abgvalues) 

###### Correct Diversity Based On Watershed Diversity (divide by Watershed Richness/Diversity) #####
#bugs#
#calculate mean shed richness
shedrich<-bug_ssdiv %>%
  group_by(shedcode) %>%
  summarise(shedrich = mean(bug_S, na.rm=TRUE))
#calculate mean shed diversity
sheddiv<-bug_ssdiv %>%
  group_by(shedcode) %>%
  summarise(sheddiv = mean(bug_Hshannon, na.rm=TRUE))
#match the mean shed richness and diversity with ssdiv
index=match(bug_ssdiv$shedcode,abgvalues$sheds)
index2=match(bug_ssdiv$shedcode,sheddiv$shedcode)
bug_ssdiv$shedrich_mean=shedrich$shedrich[index]
bug_ssdiv$sheddiv_mean=sheddiv$sheddiv[index2]
bug_ssdiv$betadiv=abgvalues$betarich[index]
# divide site richness/diversity by mean watershed richness/diversity
bug_ssdiv$bug_S_cor = bug_ssdiv$bug_S/bug_ssdiv$shedrich_mean
bug_ssdiv$bug_Hshannon_cor = bug_ssdiv$bug_Hshannon/bug_ssdiv$sheddiv_mean

#fish#
#calculate mean shed richness
shedrich<-fish_ssdiv %>%
  group_by(shedcode) %>%
  summarise(shedrich = mean(fish_S, na.rm=TRUE))
#calculate mean shed diversity
sheddiv<-fish_ssdiv %>%
  group_by(shedcode) %>%
  summarise(sheddiv = mean(fish_Hshannon, na.rm=TRUE))
#match the mean shed richness and diversity with ssdiv
index=match(fish_ssdiv$shedcode,fish_abgvalues$sheds)
index2=match(fish_ssdiv$shedcode,sheddiv$shedcode)
fish_ssdiv$shedrich_mean=shedrich$shedrich[index]
fish_ssdiv$sheddiv_mean=sheddiv$sheddiv[index2]
fish_ssdiv$betadiv=abgvalues$betarich[index]
# divide site richness/diversity by mean watershed richness/diversity
fish_ssdiv$fish_S_cor = fish_ssdiv$fish_S/fish_ssdiv$shedrich_mean
fish_ssdiv$fish_Hshannon_cor = fish_ssdiv$fish_Hshannon/fish_ssdiv$sheddiv_mean

#######Correct Beta diversity Based on Watershed area##########
#bugs#
#calculate total shed area
shedarea_total<-bug_ssdiv %>%
  group_by(shedcode) %>%
  summarise(shedarea_total= sum(shedarea, na.rm=TRUE))
#match the mean shed richness and diversity with ssdiv
index=match(bug_ssdiv$shedcode,shedarea_total$shedcode)
bug_ssdiv$shedarea_total=shedarea_total$shedarea_total[index]
#Plot beta richness by total watershed area
plot(log(bug_ssdiv$shedarea_total), bug_ssdiv$betadiv)
# divide site beta richness by total watershed area
bug_ssdiv$betadiv_cor = bug_ssdiv$betadiv/bug_ssdiv$shedarea_total

#fish#
#calculate total shed area
shedarea_total<-fish_ssdiv %>%
  group_by(shedcode) %>%
  summarise(shedarea_total= sum(shedarea, na.rm=TRUE))
#match the mean shed richness and diversity with ssdiv
index=match(fish_ssdiv$shedcode,shedarea_total$shedcode)
fish_ssdiv$shedarea_total=shedarea_total$shedarea_total[index]
#Plot beta richness by total watershed area
plot(log(fish_ssdiv$shedarea_total), fish_ssdiv$betadiv)
# divide site beta richness by mean watershed area
fish_ssdiv$betadiv_cor = fish_ssdiv$betadiv/fish_ssdiv$shedarea_total

#WRITE TO EXTERNAL FILE
write.csv(bug_ssdiv, "mdbug_ssdiv.csv")
write.csv(fish_ssdiv, "mdfish_ssdiv.csv")



####### NCBMP DATASET ########
## NC BMP BUGS ##
ncbug_ssdiv = data.frame(site = paste(ncbugs[,"site"]),
                       shedcode = paste(ncbugs[,"huc_10"]),
                       strahler = paste(ncbugs[, "strahler"]),
                       avgwidth = ncbugs$stream_width_m,
                       flow = NA,
                       shedarea = ncbugs$shed_area_sqmi*2589990.3998855,
                       bug_Hshannon = diversity(ncbugs[,c(13:1060)],1,index="shannon"),
                       bug_Hsimpson = diversity(ncbugs[,c(13:1060)],1,index="simpson"),
                       bug_J = diversity(ncbugs[,c(13:1060)],1,index="shannon")/log(specnumber(ncbugs[,c(13:1060)])),
                       bug_S = specnumber(ncbugs[,c(13:1060)]))

#NC BMP functional diversity for Bugs
#create trait database with only species found on nc bugs list

combined <- sort(union(levels(ncbug_list$Genus), levels(ncbug_traits$Genus)))
x <- left_join(mutate(ncbug_list, Genus=factor(Genus, levels=combined)),
               mutate(ncbug_traits,  Genus=factor(Genus, levels=combined)))




#abundance matrix
abun<-as.matrix(ncbugs[,c(13:1060)])
rownames(abun)<-ncbugs[,2] # need to add rownames because some sites from newenvfish will be lost due to insufficient trait data
#Do all Species have abundances > 0?
colSums(abun) #YAASSS
rowSums(abun)
rowSums(abunsubset) #YAASSS
# make species as rownames in trait database
rownames(x)<-x[,1]
x2<-x[,-c(1:2)]
#need to remove taxa that did not have trait data
xsubset<-x2[complete.cases(x2),]

xsubset2 <- data.frame(sapply(xsubset, function(x) as.numeric(as.character(x)))) #convert into Numeric for package FD
rownames(xsubset2)<-rownames(xsubset) #adding row names 

#xsubset<-xsubset[,-33] #removed from origincal trait_subset

# remove taxa with no trait info from abun
#Remove taxa w no values from abun
index=match(rownames(xsubset2),colnames(abun))
length(index)
sum(is.na(index))
abunsubset=abun[,index]


colSums(abunsubset) #YAASSS
abunsubset= abunsubset[ rowSums(abunsubset[,])!=0, ] #remove sites with zero abundances


ncbugsites<-rownames(abunsubset) #make a list of sites to later merge with ncbug_ssdiv
rownames(abunsubset) <- c() #need to remove rownames for package FD or they will remain and create havoc
# create list of which species did not have trait data
xsubset2_list <- as.data.frame(row.names(xsubset2))
colnames(xsubset2_list)= paste("taxa") # Add Column Names
x_list <- as.data.frame(row.names(x))
colnames(x_list)= paste("taxa") # Add Column Names
ncbugs_misstrait <- as.data.frame(as.character(x_list[!x_list$taxa%in%xsubset2_list$taxa,]))
colnames(ncbugs_misstrait)= paste("taxa missing traits") # Add Column Names
write.csv(ncbugs_misstrait, "ncbugs_misstrait_list.csv")
#Do Species Names match in Trait and Species dataframes?
test<-function(abun,x){
  if(any(colnames(abun) != rownames(x)))
    stop("species names in abun and x do not match")
  abun<-abun*2
  abun        
}
test(abunsubset, xsubset2) #if no errors then YASSS!

#functional composition
#comp<-functcomp(as.matrix(xsubset),as.matrix(abunsubset), CWM.type="all") #if func comp dataset is needed

#caclulate FD
fd <- dbFD(xsubset2, abunsubset,calc.FRic =F, calc.FDiv=F, CWM.type="all", corr="cailliez")
#combine FD results with ENV and Site Data

ncbug_FD<-cbind(ncbugsites, fd$RaoQ) #extract needed FD metrics, add site names

ncbug_FD<-data.frame(ncbug_FD) #need to convert df before adding colnames

colnames(ncbug_FD) <- c("site", "ncbug_RaoQ") #add colnames
ncbug_FD$site<-as.factor(ncbug_FD$site)
ncbug_FD$ncbug_RaoQ<-as.numeric(as.character((ncbug_FD$ncbug_RaoQ)))
ncbug_ssdiv<-left_join(ncbug_ssdiv, ncbug_FD) #merge with fish_ssdiv
ncbug_ssdiv$site<-as.factor(ncbug_ssdiv$site)

##NC BMP Fish Data##

ncfish_ssdiv = data.frame(site = paste(ncfish[,"site"]),
                         shedcode = paste(ncfish[,"huc_10"]),
                         strahler = paste(ncfish[, "strahler"]),
                         avgwidth = ncfish$stream_width_m,
                         flow = NA,
                         shedarea = ncfish$shed_area_sqmi*2589990.3998855,
                         fish_Hshannon = diversity(ncfish[,c(13:163)],1,index="shannon"),
                         fish_Hsimpson = diversity(ncfish[,c(13:163)],1,index="simpson"),
                         fish_J = diversity(ncfish[,c(13:163)],1,index="shannon")/log(specnumber(ncfish[,c(13:163)])),
                         fish_S = specnumber(ncfish[,c(13:163)]))

#NC BMP functional diversity for FISH
#create trait database with only species found on nc fish list

combined <- sort(union(levels(ncfish_list$Species), levels(ncfishtrait$Species)))
x <- left_join(mutate(ncfish_list, Species=factor(Species, levels=combined)),
               mutate(ncfishtrait,  Species=factor(Species, levels=combined)))



#abundance matrix
abun<-as.matrix(ncfish[,c(13:163)])
rownames(abun)<-ncfish[,2] # need to add rownames because some sites from newenvfish will be lost due to insufficient trait data
#Do all Species have abundances > 0?
colSums(abun) #YAASSS
rowSums(abun)
rowSums(abunsubset) #YAASSS
# make species as rownames in trait database
rownames(x)<-x[,1]
x2<-x[,-c(1:2)]

#need to remove taxa that did not have trait data

xsubset<-x2[complete.cases(x2),]

xsubset[xsubset==-1|xsubset==-555|xsubset==-999] = NA #give NA to -1 and -555 values

xsubset2 <- data.frame(sapply(xsubset, function(x) as.numeric(as.character(x)))) #convert into Numeric for package FD
rownames(xsubset2)<-rownames(xsubset) #adding row names 

#xsubset<-xsubset[,-33] #removed from origincal trait_subset

# remove taxa with no trait info from abun
#Remove taxa w no values from abun
index=match(rownames(xsubset2),colnames(abun))
length(index)
sum(is.na(index))
abunsubset=abun[,index]


colSums(abunsubset) #YAASSS
abunsubset= abunsubset[ rowSums(abunsubset[,])!=0, ] #remove sites with zero abundances

# create list of which species did not have trait data
xsubset2_list <- as.data.frame(row.names(xsubset2))
colnames(xsubset2_list)= paste("taxa") # Add Column Names
x_list <- as.data.frame(row.names(x))
colnames(x_list)= paste("taxa") # Add Column Names
ncfish_misstrait <- as.data.frame(as.character(x_list[!x_list$taxa%in%xsubset2_list$taxa,]))
colnames(ncfish_misstrait)= paste("taxa missing traits") # Add Column Names
write.csv(ncfish_misstrait, "ncfish_misstrait_list.csv")

ncfishsites<-rownames(abunsubset) #make a list of sites to later merge with ncbug_ssdiv
rownames(abunsubset) <- c() #need to remove rownames for package FD or they will remain and create havoc
#Do Species Names match in Trait and Species dataframes?
test<-function(abun,x){
  if(any(colnames(abun) != rownames(x)))
    stop("species names in abun and x do not match")
  abun<-abun*2
  abun        
}
test(abunsubset, xsubset2) #if no errors then YASSS!

#functional composition
#comp<-functcomp(as.matrix(xsubset),as.matrix(abunsubset), CWM.type="all") #if func comp dataset is needed

#caclulate FD
fd <- dbFD(xsubset2, abunsubset,calc.FRic =F, calc.FDiv=F, corr="cailliez")
#combine FD results with ENV and Site Data

ncfish_FD<-cbind(ncfishsites, fd$RaoQ) #extract needed FD metrics, add site names

ncfish_FD<-data.frame(ncfish_FD) #need to convert df before adding colnames

colnames(ncfish_FD) <- c("site", "ncfish_RaoQ") #add colnames
ncfish_FD$site<-as.factor(ncfish_FD$site)
ncfish_FD$ncfish_RaoQ<-as.numeric(as.character((ncfish_FD$ncfish_RaoQ)))
ncfish_ssdiv<-left_join(ncfish_ssdiv, ncfish_FD) #merge with fish_ssdiv
ncfish_ssdiv$site<-as.factor(ncfish_ssdiv$site)

#### NC BMP FISH BETA DIVERSITY #####
#BUGS#
sheds <- unique(ncbugs$huc_10) #indentify unique watersheds
alpharich<-NULL
betarich<-NULL
gammarich<-NULL
for (i in 1:length(sheds)){
  
  #*********************************************************************************
  
  dat <- ncbugs[ncbugs$huc_10==sheds[i],]
  
  #numbers equivilents of alpha for watershed, species richness are diversity measures
  alpha<-d(dat[,c(13:1060)], q=0) 
  beta<-d(dat[,c(13:1060)], lev="beta", q=0)
  gamma<-d(dat[,c(13:1060)], lev="gamma", q=0)
  
  #alpha<-d(dat[,-c(1:7)], q=1) 
  #beta<-d(dat[,-c(1:7)], lev="beta", q=1)
  #gamma<-d(dat[,-c(1:7)], lev="gamma", q=1)
  
  #Standard Diversity Indices of alpha for watershed, species richness are diversity measures
  #alphaH<-H(dat[,-c(1:7)], q=1) 
  #betaH<-H(dat[,-c(1:7)], lev="beta", q=1)
  #gammaH<-H(dat[,-c(1:7)], lev="gamma", q=1)
  
  
  alpharich[i]<-alpha
  betarich[i]<-beta
  gammarich[i]<-gamma
}  
abgvalues<-cbind(sheds, alpharich, betarich, gammarich)  
abgvalues<-data.frame(abgvalues) 

#FISH#
#fish_nozeros<- newenvfish[rowSums(newenvfish[,c(2:80)])!=0, ] #remove rows with ZERO fish abundance
#fish_final<- fish_nozeros[complete.cases(fish_nozeros[,c(2:80)]),] #remove NAs
sheds <- unique(ncfish$huc_10) #indentify unique watersheds
alpharich<-NULL
betarich<-NULL
gammarich<-NULL
for (i in 1:length(sheds)){
  
  #*********************************************************************************
  
  dat <- ncfish[ncfish$huc_10==sheds[i],]
  
  #numbers equivilents of alpha for watershed, species richness are diversity measures
  alpha<-d(dat[,c(13:163)], q=0) 
  beta<-d(dat[,c(13:163)], lev="beta", q=0)
  gamma<-d(dat[,c(13:163)], lev="gamma", q=0)
  
  #alpha<-d(dat[,-c(1:7)], q=1) 
  #beta<-d(dat[,-c(1:7)], lev="beta", q=1)
  #gamma<-d(dat[,-c(1:7)], lev="gamma", q=1)
  
  #Standard Diversity Indices of alpha for watershed, species richness are diversity measures
  #alphaH<-H(dat[,-c(1:7)], q=1) 
  #betaH<-H(dat[,-c(1:7)], lev="beta", q=1)
  #gammaH<-H(dat[,-c(1:7)], lev="gamma", q=1)
  
  
  alpharich[i]<-alpha
  betarich[i]<-beta
  gammarich[i]<-gamma
}  
fish_abgvalues<-cbind(sheds, alpharich, betarich, gammarich)  
fish_abgvalues<-data.frame(fish_abgvalues) 

### CORRECT NCBMP DIVERSITY AND BETA DIVERSITY ####

#bugs#
#calculate mean shed richness
shedrich<-ncbug_ssdiv %>%
  group_by(shedcode) %>%
  summarise(shedrich = mean(bug_S, na.rm=TRUE))
#calculate mean shed diversity
sheddiv<-ncbug_ssdiv %>%
  group_by(shedcode) %>%
  summarise(sheddiv = mean(bug_Hshannon, na.rm=TRUE))
#match the mean shed richness and diversity with ssdiv
index=match(ncbug_ssdiv$shedcode,abgvalues$sheds)
index2=match(ncbug_ssdiv$shedcode,sheddiv$shedcode)
ncbug_ssdiv$shedrich_mean=shedrich$shedrich[index]
ncbug_ssdiv$sheddiv_mean=sheddiv$sheddiv[index2]
ncbug_ssdiv$betadiv=abgvalues$betarich[index]
# divide site richness/diversity by mean watershed richness/diversity
ncbug_ssdiv$bug_S_cor = ncbug_ssdiv$bug_S/ncbug_ssdiv$shedrich_mean
ncbug_ssdiv$bug_Hshannon_cor = ncbug_ssdiv$bug_Hshannon/ncbug_ssdiv$sheddiv_mean

#fish#
#calculate mean shed richness
shedrich<-ncfish_ssdiv %>%
  group_by(shedcode) %>%
  summarise(shedrich = mean(fish_S, na.rm=TRUE))
#calculate mean shed diversity
sheddiv<-ncfish_ssdiv %>%
  group_by(shedcode) %>%
  summarise(sheddiv = mean(fish_Hshannon, na.rm=TRUE))
#match the mean shed richness and diversity with ssdiv
index=match(ncfish_ssdiv$shedcode,fish_abgvalues$sheds)
index2=match(ncfish_ssdiv$shedcode,sheddiv$shedcode)
ncfish_ssdiv$shedrich_mean=shedrich$shedrich[index]
ncfish_ssdiv$sheddiv_mean=sheddiv$sheddiv[index2]
ncfish_ssdiv$betadiv=abgvalues$betarich[index]
# divide site richness/diversity by mean watershed richness/diversity
ncfish_ssdiv$fish_S_cor = ncfish_ssdiv$fish_S/ncfish_ssdiv$shedrich_mean
ncfish_ssdiv$fish_Hshannon_cor = ncfish_ssdiv$fish_Hshannon/ncfish_ssdiv$sheddiv_mean

#bugs#
#calculate total shed area
shedarea_total<-ncbug_ssdiv %>%
  group_by(shedcode) %>%
  summarise(shedarea_total= sum(shedarea, na.rm=TRUE))
#match the mean shed richness and diversity with ssdiv
index=match(ncbug_ssdiv$shedcode,shedarea_total$shedcode)
ncbug_ssdiv$shedarea_total=shedarea_total$shedarea_total[index]
#Plot beta richness by total watershed area
plot(log(ncbug_ssdiv$shedarea_total), ncbug_ssdiv$betadiv)
# divide site beta richness by total watershed area
ncbug_ssdiv$betadiv_cor = ncbug_ssdiv$betadiv/ncbug_ssdiv$shedarea_total

#fish#
#calculate total shed area
shedarea_total<-ncfish_ssdiv %>%
  group_by(shedcode) %>%
  summarise(shedarea_total= sum(shedarea, na.rm=TRUE))
#match the mean shed richness and diversity with ssdiv
index=match(ncfish_ssdiv$shedcode,shedarea_total$shedcode)
ncfish_ssdiv$shedarea_total=shedarea_total$shedarea_total[index]
#Plot beta richness by total watershed area
plot(log(ncfish_ssdiv$shedarea_total), ncfish_ssdiv$betadiv)
# divide site beta richness by mean watershed area
ncfish_ssdiv$betadiv_cor = ncfish_ssdiv$betadiv/ncfish_ssdiv$shedarea_total

## WRITE TO EXTERNAL FILE
write.csv(ncbug_ssdiv, file="ncbugs_ssdiv.csv")
write.csv(ncfish_ssdiv, file="ncfish_ssdiv.csv")


################# ENVIRONMENTAL VARIABLE CORRELATION ############################
#Get environmental data for Pearson Correlation Map
#envonly <- ssdiv[, c(3,4,5,6)]
#envonly <-envonly[complete.cases(envonly),]
#cormat <- round(cor(envonly),2)
#melted_cormat <- melt(cormat)
# Get lower triangle of the correlation matrix
#  get_lower_tri<-function(cormat){
#    cormat[upper.tri(cormat)] <- NA
#    return(cormat)
#  }
# Get upper triangle of the correlation matrix
#  get_upper_tri <- function(cormat){
#    cormat[lower.tri(cormat)]<- NA
#    return(cormat)
#  }
#upper_tri <- get_upper_tri(cormat)
#melted_cormat <- melt(upper_tri, na.rm = TRUE)        

################## DIVERSITY BY STRAHLER ORDER  #################################

#Summarize Diverstiy Metrics by Strahler Order
sums <- bug_ssdiv %>%
  group_by(strahler) %>%
  summarise_each(funs(mean(., na.rm=TRUE), sd(.,na.rm=TRUE)), bug_S, bug_Hshannon, bug_Hsimpson, bug_J)

#summarizing corrected metrics, DOES NOT CHANGE PATTERNS #
sums_cor <- bug_ssdiv %>%
  group_by(strahler) %>%
  summarise_each(funs(mean(., na.rm=TRUE), sd(.,na.rm=TRUE)), bug_S_cor, bug_Hshannon_cor)

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

## EPA INVERT AND FISH SUMS 
sums_epainvert <- epainvert_ssdiv %>%
  group_by(strahler) %>%
  summarise_each(funs(mean(., na.rm=TRUE), sd(.,na.rm=TRUE)), epainvert_S, epainvert_Hshannon, epainvert_J)
sums_epainvert <- sums_epainvert[-9,] #remove strahler NA

sums_epafish <- epafish_ssdiv %>%
  group_by(strahler) %>%
  summarise_each(funs(mean(., na.rm=TRUE), sd(.,na.rm=TRUE)), epafish_S, epafish_Hshannon, epafish_J)
sums_epafish <- sums_epafish[-9,] #remove strahler NA

#summarizing corrected metrics, DOES NOT CHANGE PATTERNS #
sums_cor <- bug_ssdiv %>%
  group_by(strahler) %>%
  summarise_each(funs(mean(., na.rm=TRUE), sd(.,na.rm=TRUE)), bug_S_cor, bug_Hshannon_cor)

#fish_ssdiv$fish_S_cor<-as.numeric.factor(fish_ssdiv$fish_S_cor)
#fish_ssdiv[rowSums(fish_ssdiv[,14])!=0, ] #remove rows with ZERO fish abundance

fish_sums <- fish_ssdiv %>%
  group_by(strahler) %>%
  summarise_each(funs(mean(., na.rm=TRUE), sd(.,na.rm=TRUE)), fish_S, fish_Hshannon)

fish_sums_cor <- fish_ssdiv %>%
  group_by(strahler) %>%
  summarise_each(funs(mean(., na.rm=TRUE), sd(.,na.rm=TRUE)), fish_S_cor, fish_Hshannon_cor)

## NC BMP DATA ##
ncbug_sums <- ncbug_ssdiv %>%
  group_by(strahler) %>%
  summarise_each(funs(mean(., na.rm=TRUE), sd(.,na.rm=TRUE)), bug_S, bug_Hshannon, bug_Hsimpson, bug_J)
ncbug_sums<-ncbug_sums[-c(7,8),]
#summarizing corrected metrics, DOES NOT CHANGE PATTERNS #
ncbug_sums_cor <- ncbug_ssdiv %>%
  group_by(strahler) %>%
  summarise_each(funs(mean(., na.rm=TRUE), sd(.,na.rm=TRUE)), bug_S_cor, bug_Hshannon_cor)


as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
#fish_ssdiv$fish_S_cor<-as.numeric.factor(fish_ssdiv$fish_S_cor)
#fish_ssdiv[rowSums(fish_ssdiv[,14])!=0, ] #remove rows with ZERO fish abundance

ncfish_sums <- ncfish_ssdiv %>%
  group_by(strahler) %>%
  summarise_each(funs(mean(., na.rm=TRUE), sd(.,na.rm=TRUE)), fish_S, fish_Hshannon)
ncfish_sums<-ncfish_sums[-5,]

ncfish_sums_cor <- ncfish_ssdiv %>%
  group_by(strahler) %>%
  summarise_each(funs(mean(., na.rm=TRUE), sd(.,na.rm=TRUE)), fish_S_cor, fish_Hshannon_cor)


#fish_sums_cor <- fish_ssdiv %>%
#  group_by(strahler) %>%
#  summarise_each(funs(mean(., na.rm=TRUE), sd(.,na.rm=TRUE)), fish_S_cor, fish_Hshannon_cor)

############# FIGURES ####################

#Figure 1. Taxonomic richness by Strahler stream order
#bugs, not corrected
#md mbss
plot1 <- ggplot(sums, aes(x=as.factor(strahler), y=bug_S_mean, fill=as.factor(strahler)))
plot1 + geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymin=bug_S_mean-bug_S_sd, ymax=bug_S_mean+bug_S_sd), width=.2)+
  scale_fill_manual(values=c("white", "#999999", "black"))+
  labs(list(x ="strahler stream order", y ="species richness"))+
  theme_minimal() +
  theme(panel.grid.major.y = element_line(colour = "black"))+
  theme(panel.grid.minor.y = element_line(colour = "white"))+
  theme(panel.grid.major.x = element_line(colour = "white"))+
  theme(axis.ticks.x=element_blank())+
  theme(text = element_text(size=20))+
  theme(legend.position="none")

#nc bmp
plot1 <- ggplot(ncbug_sums, aes(x=as.factor(strahler), y=bug_S_mean, fill=as.factor(strahler)))
plot1 + geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymin=bug_S_mean-bug_S_sd, ymax=bug_S_mean+bug_S_sd), width=.2)+
  scale_fill_manual(values=c("white", "#CCCCCC", "#999999","#666666", "#333333", "black"))+
  labs(list(x ="strahler stream order", y ="species richness"))+
  theme_minimal() +
  theme(panel.grid.major.y = element_line(colour = "black"))+
  theme(panel.grid.minor.y = element_line(colour = "white"))+
  theme(panel.grid.major.x = element_line(colour = "white"))+
  theme(axis.ticks.x=element_blank())+
  theme(text = element_text(size=20))+
  theme(legend.position="none")

## epa data
plot1 <- ggplot(sums_epainvert, aes(x=as.factor(strahler), y=epainvert_S_mean, fill=as.factor(strahler)))
plot1 + geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymin=epainvert_S_mean-epainvert_S_sd, ymax=epainvert_S_mean+epainvert_S_sd), width=.2)+
  #scale_fill_manual(values=c("white", "#CCCCCC", "#999999","#666666", "#333333", "black"))+
  labs(list(x ="strahler stream order", y ="species richness"))+
  theme_minimal() +
  theme(panel.grid.major.y = element_line(colour = "black"))+
  theme(panel.grid.minor.y = element_line(colour = "white"))+
  theme(panel.grid.major.x = element_line(colour = "white"))+
  theme(axis.ticks.x=element_blank())+
  theme(text = element_text(size=20))+
  theme(legend.position="none")

#md mbss
plot1 <- ggplot(sums, aes(x=as.factor(strahler), y=bug_S_mean, fill=as.factor(strahler)))
plot1 + geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymin=bug_S_mean-bug_S_sd, ymax=bug_S_mean+bug_S_sd), width=.2)+
  scale_fill_manual(values=c("white", "#999999", "black"))+
  labs(list(x ="strahler stream order", y ="species richness"))+
  theme_minimal() +
  theme(panel.grid.major.y = element_line(colour = "black"))+
  theme(panel.grid.minor.y = element_line(colour = "white"))+
  theme(panel.grid.major.x = element_line(colour = "white"))+
  theme(axis.ticks.x=element_blank())+
  theme(text = element_text(size=20))+
  theme(legend.position="none")


#bugs corrected
plot1s <- ggplot(sums_cor, aes(x=as.factor(strahler), y=bug_S_cor_mean, fill=as.factor(strahler)))
plot1s + geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymin=bug_S_cor_mean-bug_S_cor_sd, ymax=bug_S_cor_mean+bug_S_cor_sd), width=.2)+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(list(x ="Strahler Stream Order", y ="Bug Species Richness corrected"))+
  theme_minimal() +
  theme(legend.position="none")

#md mbss
#fish not corrected
plot1sf <- ggplot(fish_sums, aes(x=as.factor(strahler), y=fish_S_mean, fill=as.factor(strahler)))
plot1sf + geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymin=fish_S_mean-fish_S_sd, ymax=fish_S_mean+fish_S_sd), width=.2)+
  scale_fill_manual(values=c("white", "#999999", "black"))+
  labs(list(x ="strahler stream order", y ="species richness"))+
  theme_minimal() +
  theme(panel.grid.major.y = element_line(colour = "black"))+
  theme(panel.grid.minor.y = element_line(colour = "white"))+
  theme(panel.grid.major.x = element_line(colour = "white"))+
  theme(axis.ticks.x=element_blank())+
  theme(text = element_text(size=20))+
  theme(legend.position="none")

#nc bmp
#fish not corrected
plot1sf <- ggplot(ncfish_sums, aes(x=as.factor(strahler), y=fish_S_mean, fill=as.factor(strahler)))
plot1sf + geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymin=fish_S_mean-fish_S_sd, ymax=fish_S_mean+fish_S_sd), width=.2)+
  scale_fill_manual(values=c("white", "#999999","#666666", "black"))+
  labs(list(x ="strahler stream order", y ="species richness"))+
  theme_minimal() +
  theme(panel.grid.major.y = element_line(colour = "black"))+
  theme(panel.grid.minor.y = element_line(colour = "white"))+
  theme(panel.grid.major.x = element_line(colour = "white"))+
  theme(axis.ticks.x=element_blank())+
  theme(text = element_text(size=20))+
  theme(legend.position="none")

# EPA Fish 
plot1 <- ggplot(sums_epafish, aes(x=as.factor(strahler), y=epafish_S_mean, fill=as.factor(strahler)))
plot1 + geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymin=epafish_S_mean-epafish_S_sd, ymax=epafish_S_mean+epafish_S_sd), width=.2)+
  #scale_fill_manual(values=c("white", "#CCCCCC", "#999999","#666666", "#333333", "black"))+
  labs(list(x ="strahler stream order", y ="species richness"))+
  theme_minimal() +
  theme(panel.grid.major.y = element_line(colour = "black"))+
  theme(panel.grid.minor.y = element_line(colour = "white"))+
  theme(panel.grid.major.x = element_line(colour = "white"))+
  theme(axis.ticks.x=element_blank())+
  theme(text = element_text(size=20))+
  theme(legend.position="none")

#fish corrected
plot1sf <- ggplot(fish_sums_cor, aes(x=as.factor(strahler), y=fish_S_cor_mean, fill=as.factor(strahler)))
plot1sf + geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymin=fish_S_cor_mean-fish_S_cor_sd, ymax=fish_S_cor_mean+fish_S_cor_sd), width=.2)+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(list(x ="Strahler Stream Order", y ="Fish Species Richness corrected"))+
  theme_minimal() +
  theme(legend.position="none")

#Figure 2. Taxonomic richness by Watershed Area
#bugs not corrected
#md bugs#
plot2 <- ggplot(bug_ssdiv, aes(x=log(shedarea), y=bug_S))
plot2 + geom_point() +
  labs(list(x ="Watershed Area(log transformed)", y ="Species Richness"))+
  theme_minimal() +
  theme(panel.grid.major.y = element_line(colour = "black"))+
  theme(panel.grid.minor.y = element_line(colour = "white"))+
  theme(panel.grid.major.x = element_blank())+
  theme(panel.grid.minor.x = element_blank())+
  theme(axis.ticks.x=element_blank())+
  theme(text = element_text(size=20))+
  theme(legend.position="none")

#nc bugs#
plot2 <- ggplot(ncbug_ssdiv, aes(x=log(shedarea+1), y=bug_S))
plot2 + geom_point() +
  labs(list(x ="Watershed Area(log transformed)", y ="Species Richness"))+
  theme_minimal() +
  theme(panel.grid.major.y = element_line(colour = "black"))+
  theme(panel.grid.minor.y = element_line(colour = "white"))+
  theme(panel.grid.major.x = element_blank())+
  theme(panel.grid.minor.x = element_blank())+
  theme(axis.ticks.x=element_blank())+
  theme(text = element_text(size=20))+
  theme(legend.position="none")

# EPA bugs
plot3 <- ggplot(epainvert_ssdiv, aes(x=log(shedarea_km2), y=epainvert_S))
plot3 + geom_point() +
  labs(list(x ="Watershed Area(log transformed)", y ="Species Richness"))+
  theme_minimal() +
  theme(panel.grid.major.y = element_line(colour = "black"))+
  theme(panel.grid.minor.y = element_line(colour = "white"))+
  theme(panel.grid.major.x = element_blank())+
  theme(panel.grid.minor.x = element_blank())+
  theme(axis.ticks.x=element_blank())+
  theme(text = element_text(size=20))+
  theme(legend.position="none")

# EPA FISH
plot3 <- ggplot(epafish_ssdiv, aes(x=log(shedarea_km2), y=epafish_S))
plot3 + geom_point() +
  labs(list(x ="Watershed Area(log transformed)", y ="Species Richness"))+
  theme_minimal() +
  theme(panel.grid.major.y = element_line(colour = "black"))+
  theme(panel.grid.minor.y = element_line(colour = "white"))+
  theme(panel.grid.major.x = element_blank())+
  theme(panel.grid.minor.x = element_blank())+
  theme(axis.ticks.x=element_blank())+
  theme(text = element_text(size=20))+
  theme(legend.position="none")

#bugs corrected
plot2s <- ggplot(bug_ssdiv, aes(x=log(shedarea), y=bug_S_cor))
plot2s + geom_point() +
  labs(list(x ="Watershed Area(log transformed)", y ="Bug Species Richness CORRECTED"))+
  theme_minimal() +
  theme(legend.position="none")
#fish not corrected
plot2sf <- ggplot(fish_ssdiv, aes(x=log(shedarea), y=fish_S))
plot2sf + geom_point() +
  labs(list(x ="Watershed Area(log transformed)", y ="Fish Species Richness"))+
  theme_minimal() +
  theme(legend.position="none")
#fish corrected
plot2sf <- ggplot(fish_ssdiv, aes(x=log(shedarea), y=log(fish_S_cor)))
plot2sf + geom_point() +
  labs(list(x ="Watershed Area(log transformed)", y ="Fish Species Richness CORRECTED"))+
  theme_minimal() +
  theme(legend.position="none")

#Figure 3. Taxonomic richness by average width
#bugs not corrected
plot3 <- ggplot(bug_ssdiv, aes(x=log(avgwidth+1), y=bug_S))
plot3 + geom_point() +
  labs(list(x ="Average Stream Width (log+1 transformed)", y ="Species Richness"))+
  theme_minimal() +
  theme(legend.position="none")
#bugs corrected
plot3s <- ggplot(bug_ssdiv, aes(x=log(avgwidth+1), y=bug_S_cor))
plot3s + geom_point() +
  labs(list(x ="Average Stream Width (log+1 transformed)", y ="Bug Species Richness CORRECTED"))+
  theme_minimal() +
  theme(legend.position="none")
#fish not coorecteed
plot3sf <- ggplot(fish_ssdiv, aes(x=log(avgwidth+1), y=fish_S))
plot3sf + geom_point() +
  labs(list(x ="Average Stream Width (log+1 transformed)", y ="Fish Species Richness"))+
  theme_minimal() +
  theme(legend.position="none")
#fish coorecteed
plot3sf <- ggplot(fish_ssdiv, aes(x=log(avgwidth+1), y=fish_S_cor))
plot3sf + geom_point() +
  labs(list(x ="Average Stream Width (log+1 transformed)", y ="Fish Species Richness CORRECTED"))+
  theme_minimal() +
  theme(legend.position="none")#Figure 4. Taxonomic richness by flow (discharge)
#Figure 4. Taxonomic richness by flow
plot4 <- ggplot(ssdiv, aes(x=log(flow+1), y=S))
plot4 + geom_point() +
  labs(list(x ="Flow (log+1 transformed)", y ="Species Richness"))+
  theme_minimal() +
  theme(legend.position="none")

plot4s <- ggplot(bug_ssdiv, aes(x=log(flow+1), y=bug_S_cor))
plot4s + geom_point() +
  labs(list(x ="Flow (log+1 transformed)", y ="Bug Species Richness CORRECTED"))+
  theme_minimal() +
  theme(legend.position="none")

plot4sf <- ggplot(fish_ssdiv, aes(x=log(flow+1), y=fish_S_cor))
plot4sf + geom_point() +
  labs(list(x ="Flow (log+1 transformed)", y ="Fish Species Richness CORRECTED"))+
  theme_minimal() +
  theme(legend.position="none")

#Figure 5. Shannon Diversity by Strahler stream order
#bug not corrected
plot5 <- ggplot(sums, aes(x=as.factor(strahler), y=bug_Hshannon_mean, fill=as.factor(strahler)))
plot5 + geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymin=bug_Hshannon_mean-bug_Hshannon_sd, ymax=bug_Hshannon_mean+bug_Hshannon_sd),width=.2)+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(list(x ="Strahler Stream Order", y ="Bug Shannon Diversity"))+
  theme_minimal() +
  theme(legend.position="none")
#bug corrected
plot5s <- ggplot(sums_cor, aes(x=as.factor(strahler), y=bug_Hshannon_cor_mean, fill=as.factor(strahler)))
plot5s + geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymin=bug_Hshannon_cor_mean-bug_Hshannon_cor_sd, ymax=bug_Hshannon_cor_mean+bug_Hshannon_cor_sd),width=.2)+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(list(x ="Strahler Stream Order", y ="Bug Shannon Diversity Corrected"))+
  theme_minimal() +
  theme(legend.position="none")
#fish not corrected
plot5sf <- ggplot(fish_sums, aes(x=as.factor(strahler), y=fish_Hshannon_mean, fill=as.factor(strahler)))
plot5sf + geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymin=fish_Hshannon_mean-fish_Hshannon_sd, ymax=fish_Hshannon_mean+fish_Hshannon_sd),width=.2)+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(list(x ="Strahler Stream Order", y ="Fish Shannon Diversity"))+
  theme_minimal() +
  theme(legend.position="none")
#fish corrected
plot5sf <- ggplot(fish_sums_cor, aes(x=as.factor(strahler), y=fish_Hshannon_cor_mean, fill=as.factor(strahler)))
plot5sf + geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymin=fish_Hshannon_cor_mean-fish_Hshannon_cor_sd, ymax=fish_Hshannon_cor_mean+fish_Hshannon_cor_sd),width=.2)+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(list(x ="Strahler Stream Order", y ="Fish Shannon Diversity Corrected"))+
  theme_minimal() +
  theme(legend.position="none")

#Figure 6. Shannon Diversity by Watershed Area
#bug not corrected
plot6 <- ggplot(bug_ssdiv, aes(x=log(shedarea), y=bug_Hshannon))
plot6 + geom_point() +
  labs(list(x ="Watershed Area(log transformed)", y ="Hshannon"))+
  theme_minimal() +
  theme(legend.position="none")
#bug corrected
plot6s <- ggplot(bug_ssdiv, aes(x=log(shedarea), y=bug_Hshannon))
plot6s + geom_point() +
  labs(list(x ="Watershed Area(log transformed)", y ="Hshannon"))+
  theme_minimal() +
  theme(legend.position="none")
#fish not corrected
plot6 <- ggplot(fish_ssdiv, aes(x=log(shedarea), y=fish_Hshannon))
plot6 + geom_point() +
  labs(list(x ="Watershed Area(log transformed)", y ="Hshannon"))+
  theme_minimal() +
  theme(legend.position="none")
#fish corrected
plot6s <- ggplot(fish_ssdiv, aes(x=log(shedarea), y=fish_Hshannon_cor))
plot6s + geom_point() +
  labs(list(x ="Watershed Area(log transformed)", y ="Hshannon"))+
  theme_minimal() +
  theme(legend.position="none")

#Figure 7. Shannon Diversity by average width

plot7 <- ggplot(ssdiv, aes(x=log(avgwidth+1), y=Hshannon))
plot7 + geom_point() +
  labs(list(x ="Average Stream Width (log+1 transformed)", y ="Hshannon"))+
  theme_minimal() +
  theme(legend.position="none")

plot7s <- ggplot(ssdiv, aes(x=log(avgwidth+1), y=Hshannon_cor))
plot7s + geom_point() +
  labs(list(x ="Average Stream Width (log+1 transformed)", y ="Hshannon CORRECTED"))+
  theme_minimal() +
  theme(legend.position="none")

#Figure 8. Shannon Diversity by flow (discharge)

plot8 <- ggplot(ssdiv, aes(x=log(flow+1), y=Hshannon))
plot8 + geom_point() +
  labs(list(x ="Flow (log+1 transformed)", y ="Hshannon"))+
  theme_minimal() +
  theme(legend.position="none")

plot8s <- ggplot(ssdiv, aes(x=log(flow+1), y=Hshannon_cor))
plot8s + geom_point() +
  labs(list(x ="Flow (log+1 transformed)", y ="Hshannon CORRECTED"))+
  theme_minimal() +
  theme(legend.position="none")

#Figure 9.Beta Diveristy by Watershed Area
ssdiv_cor <- bug_ssdiv %>%
  group_by(shedcode) %>%
  summarise_each(funs(mean(., na.rm=TRUE), sd(.,na.rm=TRUE)), betadiv, shedarea_total)

plot9 <- ggplot(ssdiv_cor, aes(x=log(shedarea_total_mean), y=betadiv_mean))
plot9 + geom_point() +
  labs(list(x ="Total Watershed Area(log transformed)", y ="Beta Diversity"))+
  theme_minimal() +
  theme(legend.position="none")

fish_ssdiv_cor <- fish_ssdiv %>%
  group_by(shedcode) %>%
  summarise_each(funs(mean(., na.rm=TRUE), sd(.,na.rm=TRUE)), betadiv, shedarea_total)

plot9s <- ggplot(fish_ssdiv_cor, aes(x=log(shedarea_total_mean), y=betadiv_mean))
plot9s + geom_point() +
  labs(list(x ="Total Watershed Area(log transformed)", y ="Beta Diversity"))+
  theme_minimal() +
  theme(legend.position="none")


#######################STOPPED HERE###############################################################
###################################################################################################
plot6 <- ggplot(ssdiv, aes(x=log(shedarea), y=Hshannon))
plot6 + geom_point() +
  labs(list(x ="Watershed Area(log transformed)", y ="Hshannon"))+
  theme_minimal() +
  theme(legend.position="none")

plot6s <- ggplot(ssdiv, aes(x=log(shedarea), y=Hshannon_cor))
plot6s + geom_point() +
  labs(list(x ="Watershed Area(log transformed)", y ="Hshannon CORRECTED"))+
  theme_minimal() +
  theme(legend.position="none")

#Figure 7. Shannon Diversity by average width

plot7 <- ggplot(ssdiv, aes(x=log(avgwidth+1), y=Hshannon))
plot7 + geom_point() +
  labs(list(x ="Average Stream Width (log+1 transformed)", y ="Hshannon"))+
  theme_minimal() +
  theme(legend.position="none")

plot7s <- ggplot(ssdiv, aes(x=log(avgwidth+1), y=Hshannon_cor))
plot7s + geom_point() +
  labs(list(x ="Average Stream Width (log+1 transformed)", y ="Hshannon CORRECTED"))+
  theme_minimal() +
  theme(legend.position="none")

#Figure 8. Shannon Diversity by flow (discharge)

plot8 <- ggplot(ssdiv, aes(x=log(flow+1), y=Hshannon))
plot8 + geom_point() +
  labs(list(x ="Flow (log+1 transformed)", y ="Hshannon"))+
  theme_minimal() +
  theme(legend.position="none")

plot8s <- ggplot(ssdiv, aes(x=log(flow+1), y=Hshannon_cor))
plot8s + geom_point() +
  labs(list(x ="Flow (log+1 transformed)", y ="Hshannon CORRECTED"))+
  theme_minimal() +
  theme(legend.position="none")
