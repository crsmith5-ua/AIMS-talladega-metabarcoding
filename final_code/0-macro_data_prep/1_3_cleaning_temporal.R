#Load Packages
library(tidyverse)
library(readxl)
#import data####
##load reads from all primers ####
#BE
COIBE_read_data_apr1 <- read_csv("Data/Raw_metabarcoding/MACR_COIBE_SE_approach1_TAL_20211027_20230130_V1.0.csv")

#F230
F230_read_data_apr1 <- read_csv("Data/Raw_metabarcoding/MACR_COIF230_SE_approach1_TAL_20211027_20230130_V1.0.csv")

##import taxa sheet ####
taxa_list_SE <- read_excel("Data/taxa_list_SE.xlsx")
str(taxa_list_SE)
#clean####
##BE####
#Filter matches >90%, then >=95 family, >=98 genus, and >=99 species
COIBE_apr1_filtered<-COIBE_read_data_apr1%>%
  filter(is.na(family) & is.na(genus) & is.na(species) & `% match`>=90 |
           is.na(genus) & is.na(species) & `% match`>=95 |
           is.na(species) & `% match`>=98 |
           !is.na(species) & `% match`>=99)

#remove terrestrial taxa-listing phylum, class and orders without families in taxa list,
#also 5 big all aquatic groups in case families are missed
#note orders at 90% match that are a mixed batch are excluded with this
COIBE_apr1_terr<-COIBE_apr1_filtered%>%
  filter(phylum %in% c("Nematoda", "Nematomorpha","Nemertea", "Platyhelminthes","Malacostraca", "Mollusca") |
           class %in% c("Arachnida", "Clitellata", "Collembola", "Hexanauplia","Ostracoda")|
           order %in% c("Ephemeroptera", "Plecoptera","Trichoptera","Megaloptera","Odonata","Lumbriculida",
                        "Haplotaxida","Sarcoptiformes","Trombidiformes", "Tubificida","Decapoda")|
           family %in% taxa_list_SE[-c(1:5, 67:70),]$Family)

#random checking of what's in each data set to build above filter and make sure there aren't any 
#taxa that are getting missed
unique(COIBE_apr1_filtered$phylum[!(COIBE_apr1_filtered$phylum %in% COIBE_apr1_terr$phylum)])
unique(COIBE_apr1_filtered$class[!(COIBE_apr1_filtered$class %in% COIBE_apr1_terr$class)])
unique(COIBE_apr1_filtered$order[!(COIBE_apr1_filtered$order %in% COIBE_apr1_terr$order)])
unique(COIBE_apr1_filtered$family[!(COIBE_apr1_filtered$family %in% COIBE_apr1_terr$family)])

#write csv post cleaning
write.csv(COIBE_apr1_terr, "Data/Cleaned_metabarcoding/COIBE_apr1_terr.csv", na= "", row.names = F)

##F230####
#Filter matches >90%, then >=95 family, >=98 genus, and >=99 species
F230_apr1_filtered<-F230_read_data_apr1%>%
  filter(is.na(family) & is.na(genus) & is.na(species) & `% match`>=90 |
           is.na(genus) & is.na(species) & `% match`>=95 |
           is.na(species) & `% match`>=98 |
           !is.na(species) & `% match`>=99)

#remove terrestrial taxa-listing phylum, class and orders without families in taxa list,
#also 5 big all aquatic groups in case families are missed
#note orders at 90% match that are a mixed batch are excluded with this
F230_apr1_terr<-F230_apr1_filtered%>%
  filter(phylum %in% c("Nematoda", "Nematomorpha","Nemertea", "Platyhelminthes","Malacostraca", "Mollusca") |
           class %in% c("Arachnida", "Clitellata", "Collembola", "Hexanauplia","Ostracoda")|
           order %in% c("Ephemeroptera", "Plecoptera","Trichoptera","Megaloptera","Odonata","Lumbriculida",
                        "Haplotaxida","Sarcoptiformes","Trombidiformes", "Tubificida","Decapoda")|
           family %in% taxa_list_SE[-c(1:5, 67:70),]$Family)

#random checking of what's in each data set to build above filter and make sure there aren't any taxa that are getting missed
unique(F230_apr1_filtered$phylum[!(F230_apr1_filtered$phylum %in% F230_apr1_terr$phylum)])
unique(F230_apr1_filtered$class[!(F230_apr1_filtered$class %in% F230_apr1_terr$class)])
unique(F230_apr1_filtered$order[!(F230_apr1_filtered$order %in% F230_apr1_terr$order)])
#Lepidoptera, Hymenoptera and Orthoptera check
unique(F230_apr1_filtered$family[!(F230_apr1_filtered$family %in% F230_apr1_terr$family)])

#write csv post cleaning
write.csv(F230_apr1_terr, "Data/Cleaned_metabarcoding/F230_apr1_terr.csv", na= "", row.names = F)

#combine BE and F230####
apr1_read_join<-full_join(COIBE_apr1_terr[,c(2,3,6,13:20,22)],
                          F230_apr1_terr[,c(2,3,6,13:20,22)],relationship="many-to-many")
#combine reads by ID-
apr1_read_join_taxa<-apr1_read_join%>%
  group_by(site, date, approach,kingdom,phylum,class,order,family,genus,species)%>%
  summarise(tot_read=sum(reads))
#are all the sites there?
unique(interaction(apr1_read_join_taxa$site,apr1_read_join_taxa$date))

#make read before and after list####
before_read_join<-full_join(COIBE_read_data_apr1[,c(2,3,6,13:20,22)],
                            F230_read_data_apr1[,c(2,3,6,13:20,22)],relationship="many-to-many")
before_read_join<-before_read_join%>%
  group_by(date,site,kingdom,phylum,class,order,family,genus,species)%>%
  summarise(tot_read=sum(reads))

read_before<-before_read_join%>%
  group_by(date, site)%>%
  summarise(reads=sum(tot_read, na.rm=F))

read_after<-apr1_read_join_taxa%>%
  group_by(date, site)%>%
  summarise(reads_after=sum(tot_read, na.rm=F))

read_change_apr1<-left_join(read_before, read_after, by=c("date","site"))
read_change_apr1$diff<-read_change_apr1$reads-read_change_apr1$reads_after
read_change_apr1$per<-round(read_change_apr1$reads_after/read_change_apr1$reads*100,0)
#extract only talladega watershed
read_change_apr1_TAL<-read_change_apr1%>%
  filter(substr(site, 1, 1) == "T")
write.csv(read_change_apr1_TAL, "Data/Ancillary_cleaning_data/read_change_apr1_TAL.csv", na= "", row.names = F)

#Clean with undescribed species####
##add lowest id name column for later analysis####
apr1_read_join_taxa_all<-apr1_read_join_taxa%>%
  mutate(Lowest_ID=species)
#remove taxa within samples that are identified at higher level but are present at lower level####
#remove genus that have species present by counting number of times a a unique lowest id appears within a sample
apr1_read_join_taxa_all<-apr1_read_join_taxa_all%>%
  group_by(site,date, approach, kingdom,phylum,class,order,family,genus)%>%
  mutate(dup=length(unique(Lowest_ID)))

#remove higher classification if lower exist by filtering rows without species id and a duplicate id>1 and keeping those with species ID
apr1_read_join_taxa_all<-apr1_read_join_taxa_all%>%
  filter(dup>1 & !is.na(species) | dup==1)

#remove family that have genus present by counting number of times a a unique lowest id appears within a sample
apr1_read_join_taxa_all<-apr1_read_join_taxa_all%>%
  group_by(site,date, approach,kingdom,phylum,class,order,family)%>%
  mutate(dup2=length(unique(Lowest_ID)))

#remove higher classification if lower exist by filtering rows without genus id and a duplicate id>1 and keeping those with genus ID
apr1_read_join_taxa_all<-apr1_read_join_taxa_all%>%
  filter(dup2>1 & !is.na(genus) | dup2==1)

#remove order that have family present by counting number of times a a unique lowest id appears within a sample
apr1_read_join_taxa_all<-apr1_read_join_taxa_all%>%
  group_by(site,date, approach,kingdom,phylum,class,order)%>%
  mutate(dup3=length(unique(Lowest_ID)))

#remove higher classification if lower exist by filtering rows without genus id and a duplicate id>1 and keeping those with genus ID
apr1_read_join_taxa_all<-apr1_read_join_taxa_all%>%
  filter(dup3>1 & !is.na(family) | dup3==1)

#remove class that have order present by counting number of times a a unique lowest id appears within a sample--only 1 of these
apr1_read_join_taxa_all<-apr1_read_join_taxa_all%>%
  group_by(site,date, approach,kingdom,phylum,class)%>%
  mutate(dup4=length(unique(Lowest_ID)))

#remove higher classification if lower exist by filtering rows without genus id and a duplicate id>1 and keeping those with genus ID
apr1_read_join_taxa_all<-apr1_read_join_taxa_all%>%
  filter(dup4>1 & !is.na(order) | dup4==1)
#remove dup columns
apr1_read_join_taxa_all<-apr1_read_join_taxa_all[,-c(13:16)]

##Finish fixing lowest id
#add genus if no species
apr1_read_join_taxa_all$Lowest_ID<-ifelse(is.na(apr1_read_join_taxa_all$Lowest_ID)==T,
                                      apr1_read_join_taxa_all$genus, 
                                      apr1_read_join_taxa_all$Lowest_ID)
#add family if no genus
apr1_read_join_taxa_all$Lowest_ID<-ifelse(is.na(apr1_read_join_taxa_all$Lowest_ID)==T,
                                      apr1_read_join_taxa_all$family, 
                                      apr1_read_join_taxa_all$Lowest_ID)

#add order if no family
apr1_read_join_taxa_all$Lowest_ID<-ifelse(is.na(apr1_read_join_taxa_all$Lowest_ID)==T,
                                      apr1_read_join_taxa_all$order, 
                                      apr1_read_join_taxa_all$Lowest_ID)

#add class if no order
apr1_read_join_taxa_all$Lowest_ID<-ifelse(is.na(apr1_read_join_taxa_all$Lowest_ID)==T,
                                      apr1_read_join_taxa_all$class, 
                                      apr1_read_join_taxa_all$Lowest_ID)
##check if any one at higher level missing
sum(is.na(apr1_read_join_taxa_all$Lowest_ID))
#write csv post combining
write.csv(apr1_read_join_taxa_all, "Data/Final_metabarcoding/undescribed/apr1_read_join_taxa_all.csv", na= "", row.names = F)
#subset just the Talladega watershed for analysis
apr1_read_join_taxa_all_TAL<-apr1_read_join_taxa_all%>%
  filter(substr(site, 1, 1) == "T")
write.csv(apr1_read_join_taxa_all_TAL, "Data/Final_metabarcoding/undescribed/apr1_read_join_taxa_all_TAL.csv", na= "", row.names = F)

#Clean without undescribed species####
#combine species with designation genus sp. by removing anything in that column after sp####
##remove undescribed taxa at the species level by replacing with NA####
apr1_read_join_taxa$species<-
  replace(apr1_read_join_taxa$species, str_detect(apr1_read_join_taxa$species, "sp."), NA)

#combine genus designations after species removal
apr1_read_join_taxa<- apr1_read_join_taxa %>%
  group_by(site,date, approach,kingdom,phylum,class,order,family,genus,species)%>%
  summarise(tot_read=sum(tot_read))

##add lowest id name column for later analysis####
apr1_read_join_taxa<-apr1_read_join_taxa%>%
  mutate(Lowest_ID=species)
#remove taxa within samples that are identified at higher level but are present at lower level####
#remove genus that have species present by counting number of times a a unique lowest id appears within a sample
apr1_read_join_taxa<-apr1_read_join_taxa%>%
  group_by(site,date, approach, kingdom,phylum,class,order,family,genus)%>%
  mutate(dup=length(unique(Lowest_ID)))

#remove higher classification if lower exist by filtering rows without species id and a duplicate id>1 and keeping those with species ID
apr1_read_join_taxa<-apr1_read_join_taxa%>%
  filter(dup>1 & !is.na(species) | dup==1)

#remove family that have genus present by counting number of times a a unique lowest id appears within a sample
apr1_read_join_taxa<-apr1_read_join_taxa%>%
  group_by(site,date, approach,kingdom,phylum,class,order,family)%>%
  mutate(dup2=length(unique(Lowest_ID)))

#remove higher classification if lower exist by filtering rows without genus id and a duplicate id>1 and keeping those with genus ID
apr1_read_join_taxa<-apr1_read_join_taxa%>%
  filter(dup2>1 & !is.na(genus) | dup2==1)

#remove order that have family present by counting number of times a a unique lowest id appears within a sample
apr1_read_join_taxa<-apr1_read_join_taxa%>%
  group_by(site,date, approach,kingdom,phylum,class,order)%>%
  mutate(dup3=length(unique(Lowest_ID)))

#remove higher classification if lower exist by filtering rows without genus id and a duplicate id>1 and keeping those with genus ID
apr1_read_join_taxa<-apr1_read_join_taxa%>%
  filter(dup3>1 & !is.na(family) | dup3==1)

#remove class that have order present by counting number of times a a unique lowest id appears within a sample--only 1 of these
apr1_read_join_taxa<-apr1_read_join_taxa%>%
  group_by(site,date, approach,kingdom,phylum,class)%>%
  mutate(dup4=length(unique(Lowest_ID)))

#remove higher classification if lower exist by filtering rows without genus id and a duplicate id>1 and keeping those with genus ID
apr1_read_join_taxa<-apr1_read_join_taxa%>%
  filter(dup4>1 & !is.na(order) | dup4==1)
#remove dup columns
apr1_read_join_taxa<-apr1_read_join_taxa[,-c(13:16)]

##Finish fixing lowest id
#add genus if no species
apr1_read_join_taxa$Lowest_ID<-ifelse(is.na(apr1_read_join_taxa$Lowest_ID)==T,
                                              apr1_read_join_taxa$genus, 
                                              apr1_read_join_taxa$Lowest_ID)
#add family if no genus
apr1_read_join_taxa$Lowest_ID<-ifelse(is.na(apr1_read_join_taxa$Lowest_ID)==T,
                                              apr1_read_join_taxa$family, 
                                              apr1_read_join_taxa$Lowest_ID)

#add order if no family
apr1_read_join_taxa$Lowest_ID<-ifelse(is.na(apr1_read_join_taxa$Lowest_ID)==T,
                                              apr1_read_join_taxa$order, 
                                              apr1_read_join_taxa$Lowest_ID)

#add class if no order
apr1_read_join_taxa$Lowest_ID<-ifelse(is.na(apr1_read_join_taxa$Lowest_ID)==T,
                                              apr1_read_join_taxa$class, 
                                              apr1_read_join_taxa$Lowest_ID)
##check if any one at higher level missing
sum(is.na(apr1_read_join_taxa$Lowest_ID))
#write csv post combining
write.csv(apr1_read_join_taxa, "Data/Final_metabarcoding/apr1_read_join_taxa.csv", na= "", row.names = F)
#subset just the talladega watershed
apr1_read_join_taxa_TAL<-apr1_read_join_taxa%>%
  filter(substr(site, 1, 1) == "T")
write.csv(apr1_read_join_taxa_TAL, "Data/Final_metabarcoding/apr1_read_join_taxa_TAL.csv", na= "", row.names = F)

#Final taxa list ####
#Taxa remaining after terrestrial and non-invertebrate taxa removed
apr1_taxa_list<-unique(apr1_read_join_taxa[c("site","phylum","class","order","family","genus","species", "Lowest_ID")])
#subset Talladega
apr1_taxa_list_TAL<-apr1_taxa_list%>%
  filter(substr(site, 1, 1) == "T")
#write csv post cleaning
write.csv(apr1_taxa_list_TAL, "Data/Ancillary_cleaning_data/apr1_taxa_list_TAL.csv", na= "", row.names = F)
