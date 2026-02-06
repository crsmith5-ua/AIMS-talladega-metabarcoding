#Load Packages
library(tidyverse)
library(readxl)
#import data####
##load reads from all primers ####
COIBE_read_data_apr3 <- read_csv("Data/Raw_metabarcoding/MACR_COIBE_SE_approach3_TAL_20220609_20220610_V1.0.csv")
str(COIBE_read_data_apr3)

F230_read_data_apr3 <- read_csv("Data/Raw_metabarcoding/MACR_COIF230_SE_approach3_TAL_20220609_20220610_V1.0.csv")
str(F230_read_data_apr3)

##import taxa sheet ####
taxa_list_SE <- read_excel("Data/taxa_list_SE.xlsx")
str(taxa_list_SE)
#clean####
##BE####
#Filter matches >90%, then >=95 family, >=98 genus, and >=99 species
COIBE_apr3_filtered<-COIBE_read_data_apr3%>%
  filter(is.na(family) & is.na(genus) & is.na(species) & `% match`>=90 |
           is.na(genus) & is.na(species) & `% match`>=95 |
           is.na(species) & `% match`>=98 |
           !is.na(species) & `% match`>=99)

#remove terrestrial taxa-listing phylum, class and orders without families in taxa list,
#also 5 big all aquatic groups in case families are missed
#note orders at 90% match that are a mixed batch are excluded with this
COIBE_apr3_terr<-COIBE_apr3_filtered%>%
  filter(phylum %in% c("Nematoda", "Nematomorpha","Nemertea", "Platyhelminthes","Malacostraca", "Mollusca") |
           class %in% c("Clitellata", "Collembola", "Hexanauplia","Ostracoda")|
           order %in% c("Ephemeroptera", "Plecoptera","Trichoptera","Megaloptera","Odonata","Lumbriculida",
                        "Haplotaxida","Sarcoptiformes","Trombidiformes","Mesostigmata", "Tubificida","Decapoda")|
           family %in% taxa_list_SE[-c(1:5, 67:70),]$Family)

#random checking of what's in each data set to build above filter and make sure there aren't any taxa that are getting missed
unique(COIBE_apr3_filtered$phylum[!(COIBE_apr3_filtered$phylum %in% COIBE_apr3_terr$phylum)])
unique(COIBE_apr3_filtered$class[!(COIBE_apr3_filtered$class %in% COIBE_apr3_terr$class)])
unique(COIBE_apr3_filtered$order[!(COIBE_apr3_filtered$order %in% COIBE_apr3_terr$order)])
#Orthoptera shows up but not semi-aquatic genera
unique(COIBE_apr3_filtered$family[!(COIBE_apr3_filtered$family %in% COIBE_apr3_terr$family)])

#write csv post cleaning
write.csv(COIBE_apr3_terr, "Data/Cleaned_metabarcoding/COIBE_apr3_terr.csv", na= "", row.names = F)

##F230####
#Filter matches >90%, then >=95 family, >=98 genus, and >=99 species
F230_apr3_filtered<-F230_read_data_apr3%>%
  filter(is.na(family) & is.na(genus) & is.na(species) & `% match`>=90 |
           is.na(genus) & is.na(species) & `% match`>=95 |
           is.na(species) & `% match`>=98 |
           !is.na(species) & `% match`>=99)

#remove terrestrial taxa-listing phylum, class and orders without families in taxa list,
#also 5 big all aquatic groups in case families are missed
#note orders at 90% match that are a mixed batch are excluded with this
F230_apr3_terr<-F230_apr3_filtered%>%
  filter(phylum %in% c("Nematoda", "Nematomorpha","Nemertea", "Platyhelminthes","Malacostraca", "Mollusca") |
           class %in% c("Arachnida", "Clitellata", "Collembola", "Hexanauplia","Ostracoda")|
           order %in% c("Ephemeroptera", "Plecoptera","Trichoptera","Megaloptera","Odonata","Lumbriculida",
                        "Haplotaxida","Sarcoptiformes","Trombidiformes","Mesostigmata", "Tubificida","Decapoda")|
           family %in% taxa_list_SE[-c(1:5, 67:70),]$Family)

#random checking of what's in each data set to build above filter and make sure there aren't any taxa that are getting missed
unique(F230_apr3_filtered$phylum[!(F230_apr3_filtered$phylum %in% F230_apr3_terr$phylum)])
unique(F230_apr3_filtered$class[!(F230_apr3_filtered$class %in% F230_apr3_terr$class)])
unique(F230_apr3_filtered$order[!(F230_apr3_filtered$order %in% F230_apr3_terr$order)])
#Orthoptera shows up but not semi-aquatic genera, hymenoptera all ants
unique(F230_apr3_filtered$family[!(F230_apr3_filtered$family %in% F230_apr3_terr$family)])

#write csv post cleaning
write.csv(F230_apr3_terr, "Data/Cleaned_metabarcoding/F230_apr3_terr.csv", na= "", row.names = F)

#combine BE and F230####
apr3_read_join<-full_join(COIBE_apr3_terr[,c(2,3,6,13:20,22)],
                          F230_apr3_terr[,c(2,3,6,13:20,22)],relationship="many-to-many")
#combine reads by ID-
apr3_read_join_taxa<-apr3_read_join%>%
  group_by(site, date, approach,kingdom,phylum,class,order,family,genus,species)%>%
  summarise(tot_read=sum(reads))
#are all the sites there?
unique(apr3_read_join_taxa$site)

#make read before and after list####
before_read_join<-full_join(COIBE_read_data_apr3[,c(2,3,6,13:20,22)],
                            F230_read_data_apr3[,c(2,3,6,13:20,22)],relationship="many-to-many")
before_read_join<-before_read_join%>%
  group_by(date,site,kingdom,phylum,class,order,family,genus,species)%>%
  summarise(tot_read=sum(reads))

read_before<-before_read_join%>%
  group_by(date, site)%>%
  summarise(reads=sum(tot_read, na.rm=F))

read_after<-apr3_read_join_taxa%>%
  group_by(date, site)%>%
  summarise(reads_after=sum(tot_read, na.rm=F))

read_change_apr3<-left_join(read_before, read_after, by=c("date","site"))
read_change_apr3$diff<-read_change_apr3$reads-read_change_apr3$reads_after
read_change_apr3$per<-round(read_change_apr3$reads_after/read_change_apr3$reads*100,0)

#write final change table to csv ####
#write csv post cleaning
write.csv(read_change_apr3, "Data/Ancillary_cleaning_data/read_change_apr3.csv", na= "", row.names = F)
#Cleaned Keeping undescribed species####
##add lowest id name column for later analysis####
apr3_read_join_taxa_all<-apr3_read_join_taxa%>%
  mutate(Lowest_ID=species)
#remove taxa within samples that are identified at higher level but are present at lower level####
#remove genus that have species present by counting number of times a a unique lowest id appears within a sample
apr3_read_join_taxa_all<-apr3_read_join_taxa_all%>%
  group_by(site,date, approach, kingdom,phylum,class,order,family,genus)%>%
  mutate(dup=length(unique(Lowest_ID)))

#remove higher classification if lower exist by filtering rows without species id and a duplicate id>1 and keeping those with species ID
apr3_read_join_taxa_all<-apr3_read_join_taxa_all%>%
  filter(dup>1 & !is.na(species) | dup==1)

#remove family that have genus present by counting number of times a a unique lowest id appears within a sample
apr3_read_join_taxa_all<-apr3_read_join_taxa_all%>%
  group_by(site,date, approach,kingdom,phylum,class,order,family)%>%
  mutate(dup2=length(unique(Lowest_ID)))

#remove higher classification if lower exist by filtering rows without genus id and a duplicate id>1 and keeping those with genus ID
apr3_read_join_taxa_all<-apr3_read_join_taxa_all%>%
  filter(dup2>1 & !is.na(genus) | dup2==1)

#remove order that have family present by counting number of times a a unique lowest id appears within a sample
apr3_read_join_taxa_all<-apr3_read_join_taxa_all%>%
  group_by(site,date, approach,kingdom,phylum,class,order)%>%
  mutate(dup3=length(unique(Lowest_ID)))

#remove higher classification if lower exist by filtering rows without genus id and a duplicate id>1 and keeping those with genus ID
apr3_read_join_taxa_all<-apr3_read_join_taxa_all%>%
  filter(dup3>1 & !is.na(family) | dup3==1)

#remove class that have order present by counting number of times a a unique lowest id appears within a sample--only 1 of these
apr3_read_join_taxa_all<-apr3_read_join_taxa_all%>%
  group_by(site,date, approach,kingdom,phylum,class)%>%
  mutate(dup4=length(unique(Lowest_ID)))

#remove higher classification if lower exist by filtering rows without genus id and a duplicate id>1 and keeping those with genus ID
apr3_read_join_taxa_all<-apr3_read_join_taxa_all%>%
  filter(dup4>1 & !is.na(order) | dup4==1)
#remove dup columns
apr3_read_join_taxa_all<-apr3_read_join_taxa_all[,-c(13:16)]

##Finish fixing lowest id
#add genus if no species
apr3_read_join_taxa_all$Lowest_ID<-ifelse(is.na(apr3_read_join_taxa_all$Lowest_ID)==T,
                                      apr3_read_join_taxa_all$genus, 
                                      apr3_read_join_taxa_all$Lowest_ID)
#add family if no genus
apr3_read_join_taxa_all$Lowest_ID<-ifelse(is.na(apr3_read_join_taxa_all$Lowest_ID)==T,
                                      apr3_read_join_taxa_all$family, 
                                      apr3_read_join_taxa_all$Lowest_ID)

#add order if no family
apr3_read_join_taxa_all$Lowest_ID<-ifelse(is.na(apr3_read_join_taxa_all$Lowest_ID)==T,
                                      apr3_read_join_taxa_all$order, 
                                      apr3_read_join_taxa_all$Lowest_ID)

#add class if no order
apr3_read_join_taxa_all$Lowest_ID<-ifelse(is.na(apr3_read_join_taxa_all$Lowest_ID)==T,
                                      apr3_read_join_taxa_all$class, 
                                      apr3_read_join_taxa_all$Lowest_ID)
##check if any one at higher level missing
sum(is.na(apr3_read_join_taxa_all$Lowest_ID))
#write csv post combining
write.csv(apr3_read_join_taxa_all, "Data/Final_metabarcoding/undescribed/apr3_read_join_taxa_all.csv", na= "", row.names = F)
#Clean without undescribed species####
#combine species with designation genus sp. by removing anything in that column after sp####
##remove undescribed taxa at the species level by replacing with NA####
apr3_read_join_taxa$species<-
  replace(apr3_read_join_taxa$species, str_detect(apr3_read_join_taxa$species, "sp."), NA)

#combine genus designations after species removal
apr3_read_join_taxa<- apr3_read_join_taxa %>%
  group_by(site,date, approach,kingdom,phylum,class,order,family,genus,species)%>%
  summarise(tot_read=sum(tot_read))

##add lowest id name column for later analysis####
apr3_read_join_taxa<-apr3_read_join_taxa%>%
  mutate(Lowest_ID=species)
#remove taxa within samples that are identified at higher level but are present at lower level####
#remove genus that have species present by counting number of times a a unique lowest id appears within a sample
apr3_read_join_taxa<-apr3_read_join_taxa%>%
  group_by(site,date, approach, kingdom,phylum,class,order,family,genus)%>%
  mutate(dup=length(unique(Lowest_ID)))

#remove higher classification if lower exist by filtering rows without species id and a duplicate id>1 and keeping those with species ID
apr3_read_join_taxa<-apr3_read_join_taxa%>%
  filter(dup>1 & !is.na(species) | dup==1)

#remove family that have genus present by counting number of times a a unique lowest id appears within a sample
apr3_read_join_taxa<-apr3_read_join_taxa%>%
  group_by(site,date, approach,kingdom,phylum,class,order,family)%>%
  mutate(dup2=length(unique(Lowest_ID)))

#remove higher classification if lower exist by filtering rows without genus id and a duplicate id>1 and keeping those with genus ID
apr3_read_join_taxa<-apr3_read_join_taxa%>%
  filter(dup2>1 & !is.na(genus) | dup2==1)

#remove order that have family present by counting number of times a a unique lowest id appears within a sample
apr3_read_join_taxa<-apr3_read_join_taxa%>%
  group_by(site,date, approach,kingdom,phylum,class,order)%>%
  mutate(dup3=length(unique(Lowest_ID)))

#remove higher classification if lower exist by filtering rows without genus id and a duplicate id>1 and keeping those with genus ID
apr3_read_join_taxa<-apr3_read_join_taxa%>%
  filter(dup3>1 & !is.na(family) | dup3==1)

#remove class that have order present by counting number of times a a unique lowest id appears within a sample--only 1 of these
apr3_read_join_taxa<-apr3_read_join_taxa%>%
  group_by(site,date, approach,kingdom,phylum,class)%>%
  mutate(dup4=length(unique(Lowest_ID)))

#remove higher classification if lower exist by filtering rows without genus id and a duplicate id>1 and keeping those with genus ID
apr3_read_join_taxa<-apr3_read_join_taxa%>%
  filter(dup4>1 & !is.na(order) | dup4==1)
#remove dup columns
apr3_read_join_taxa<-apr3_read_join_taxa[,-c(13:16)]

##Finish fixing lowest id
#add genus if no species
apr3_read_join_taxa$Lowest_ID<-ifelse(is.na(apr3_read_join_taxa$Lowest_ID)==T,
                                      apr3_read_join_taxa$genus, 
                                      apr3_read_join_taxa$Lowest_ID)
#add family if no genus
apr3_read_join_taxa$Lowest_ID<-ifelse(is.na(apr3_read_join_taxa$Lowest_ID)==T,
                                      apr3_read_join_taxa$family, 
                                      apr3_read_join_taxa$Lowest_ID)

#add order if no family
apr3_read_join_taxa$Lowest_ID<-ifelse(is.na(apr3_read_join_taxa$Lowest_ID)==T,
                                      apr3_read_join_taxa$order, 
                                      apr3_read_join_taxa$Lowest_ID)

#add class if no order
apr3_read_join_taxa$Lowest_ID<-ifelse(is.na(apr3_read_join_taxa$Lowest_ID)==T,
                                      apr3_read_join_taxa$class, 
                                      apr3_read_join_taxa$Lowest_ID)
##check if any one at higher level missing
sum(is.na(apr3_read_join_taxa$Lowest_ID))
#write csv post combining
write.csv(apr3_read_join_taxa, "Data/Final_metabarcoding/apr3_read_join_taxa.csv", na= "", row.names = F)


#Final taxa list ####
#Taxa remaining after terrestrial and non-invertebrate taxa removed
apr3_taxa_list<-unique(apr3_read_join_taxa[c("phylum","class","order","family","genus","species","Lowest_ID")])
#write csv post cleaning
write.csv(apr3_taxa_list, "Data/Ancillary_cleaning_data/apr3_taxa_list.csv", na= "", row.names = F)
