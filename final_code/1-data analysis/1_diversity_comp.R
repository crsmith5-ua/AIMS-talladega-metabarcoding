library(tidyverse)
library(lubridate)
library(iNEXT)
library(vegan)
library(adespatial)
library(readxl)
#import data
apr1_read_join_taxa_TAL <- read_csv("Data/Final_metabarcoding/apr1_read_join_taxa_TAL.csv")
apr1_read_join_taxa_TAL$date<-as_date(as.character(apr1_read_join_taxa_TAL$date), format="%Y%m%d")
apr3_read_join_taxa <- read_csv("Data/Final_metabarcoding/apr3_read_join_taxa.csv")
apr3_read_join_taxa$date<-as_date(as.character(apr3_read_join_taxa$date), format="%Y%m%d")
apr2_read_join_taxa_TAL <- read_csv("Data/Final_metabarcoding/apr2_read_join_taxa_TAL.csv")
apr2_read_join_taxa_TAL$date<-as_date(as.character(apr2_read_join_taxa_TAL$date), format="%Y%m%d")
#add subset of points from apr3 spatial sampling to complete apr 2 spatiotemporal dataset
#add in ltm sites from apr3
apr2_read_join_taxa_all<-apr3_read_join_taxa%>%
  filter(site %in% c("TLM01", "TLC01", "TLM06", "TLM10", "TLM16", "TLM19", "TLA01"))%>%
  bind_rows(apr2_read_join_taxa_TAL)
# Rarefaction curves ####
## iNext curves apr3 ####
#need to convert to species x sites matrix
#apr3
apr3_inext<-apr3_read_join_taxa%>%
  pivot_wider(id_cols = Lowest_ID, names_from = site, values_from = tot_read, values_fn=sum, values_fill = 0)%>%
  mutate_if(is.numeric, ~1 * (. != 0))%>%
  as.data.frame

rownames(apr3_inext)<-apr3_inext[,1]
apr3_inext<-apr3_inext[,-1]

#apr1
apr1_inext<-apr1_read_join_taxa_TAL%>%
  pivot_wider(id_cols = Lowest_ID, names_from = date, values_from = tot_read, values_fn=sum, values_fill = 0)%>%
  mutate_if(is.numeric, ~1 * (. != 0))%>%
  as.data.frame

rownames(apr1_inext)<-apr1_inext[,1]
apr1_inext<-apr1_inext[,-1]

#apr2
apr2_inext<-apr2_read_join_taxa_all%>%
  mutate(sample=paste(site,date, sep=":"))%>%
  pivot_wider(id_cols = Lowest_ID, names_from =sample , values_from = tot_read, values_fn=sum, values_fill = 0)%>%
  mutate_if(is.numeric, ~1 * (. != 0))%>%
  as.data.frame

rownames(apr2_inext)<-apr2_inext[,1]
apr2_inext<-apr2_inext[,-1]

#put three into list
write.csv(apr3_inext, "Data/graph_data/apr3_inext.csv", na= "", row.names = F)
write.csv(apr1_inext, "Data/graph_data/apr1_inext.csv", na= "", row.names = F)
write.csv(apr2_inext, "Data/graph_data/apr2_inext.csv", na= "", row.names = F)
#Alpha Richness ####
##Apr1####
#transpose for NMDS to site by low.name matrix with presence###
apr1_nmds_data<-apr1_read_join_taxa_TAL%>%
  pivot_wider(id_cols = c(date,site), names_from = Lowest_ID, values_from = tot_read, 
              values_fn=sum, values_fill = 0)%>%
  mutate_if(is.numeric, ~1 * (. != 0))

#Calculate alpha diversity of each site
apr1_richness<-apr1_nmds_data[]%>%
  group_by(date)%>%
  rowwise()%>%
  summarise(rich=sum(c_across(Elmidae:`Pycnopsyche luculenta`)))%>%
  bind_cols(Sampling=rep("Temporal",22))

##Apr3####
#transpose for NMDS to site by low.name matrix with presence
apr3_nmds_data<-apr3_read_join_taxa%>%
  pivot_wider(id_cols = c(date,site), names_from = Lowest_ID, values_from = tot_read, 
              values_fn=sum, values_fill = 0)%>%
  mutate_if(is.numeric, ~1 * (. != 0))

#Calculate alpha diversity of each site
apr3_richness<-apr3_nmds_data[,-1]%>%
  group_by(site)%>%
  rowwise()%>%
  summarise(rich=sum(c_across(Stenelmis:Staphylinidae)))%>%
  ungroup()%>%
  mutate(Sampling=rep("Spatial",28))


##Apr2####
#calculate richness persite
apr2_nmds_data<-apr2_read_join_taxa_all%>%
  pivot_wider(id_cols=c(site,date),
              names_from=Lowest_ID, values_from=tot_read, values_fill = 0, 
              values_fn=~sum(.x, na.rm=T))%>%
  mutate_if(is.numeric, ~1 * (. != 0))
#Calculate alpha diversity of each site
apr2_richness<-apr2_nmds_data%>%
  group_by(date,site)%>%
  rowwise()%>%
  summarise(rich=sum(c_across(Stenelmis:Corixidae)))%>%
  ungroup()%>%
  mutate(Sampling=rep("Spatiotemporal",28))

#Combine richness measures
apr_richness<-apr1_richness%>%
  bind_rows(apr2_richness)%>%
  bind_rows(apr3_richness)%>%
  select(date, rich, Sampling)
write.csv(apr_richness, "Data/apr_richness.csv", row.names = F)

#Beta diversity ####
##Apr1####
bd_as_comp_apr1<-beta.div.comp(apr1_nmds_data[,-c(1:2)], coef="J", quant=F)
bd_as_comp_apr1$part
bd_as_comp_apr1<-data.frame(bd_as_comp_apr1$part)
bd_as_comp_apr1<-bd_as_comp_apr1%>% 
  mutate(names=rownames(bd_as_comp_apr1))%>%
  pivot_wider(values_from = bd_as_comp_apr1.part, names_from = names)%>%
  rename("Repl.BDtotal"="Repl/BDtotal", "RichDif.BDtotal"="RichDif/BDtotal")%>%
  mutate(site="TLM01",Time=NA,Sampling="Temporal",Beta="Time")
##Apr2####
bd_as_comp_apr2<-beta.div.comp(apr2_nmds_data[,-c(1:2)], coef="J", quant=F)
bd_as_comp_apr2<-data.frame(bd_as_comp_apr2$part)
bd_as_comp_apr2<-bd_as_comp_apr2%>% 
  mutate(names=rownames(bd_as_comp_apr2))%>%
  pivot_wider(values_from = bd_as_comp_apr2.part, names_from = names)%>%
  rename("Repl.BDtotal"="Repl/BDtotal", "RichDif.BDtotal"="RichDif/BDtotal")%>%
  mutate(Sampling="Spatiotemporal")
##Apr3####
bd_as_comp_apr3<-beta.div.comp(apr3_nmds_data[,-c(1:2)], coef="J", quant=F)
bd_as_comp_apr3$part
bd_as_comp_apr3<-data.frame(bd_as_comp_apr3$part)
bd_as_comp_apr3<-bd_as_comp_apr3%>% 
  mutate(names=rownames(bd_as_comp_apr3))%>%
  pivot_wider(values_from = bd_as_comp_apr3.part, names_from = names)%>%
  rename("Repl.BDtotal"="Repl/BDtotal", "RichDif.BDtotal"="RichDif/BDtotal")%>%
  mutate(site=NA,Time=NA,Sampling="Spatial",Beta="Space")
#Combine overall
bd_as_comp_overall<-bd_as_comp_apr1%>%
  bind_rows(bd_as_comp_apr3)%>%
  bind_rows(bd_as_comp_apr2)%>%
  select(Repl,RichDif,Sampling)%>%
  pivot_longer(cols=Repl:RichDif,names_to="Beta div", values_to = "value")
#export bd_as_comp_all dataframe for final graphing
write.csv(bd_as_comp_overall, "Data/graph_data/bd_as_comp_overall.csv", na= "", row.names = F)

##Further split Apr2####
###Across space####
bd_as_comp_apr2_space<-apr2_nmds_data%>%
  group_by(month(date))%>%
  group_map(~beta.div.comp(.x[,-c(1:2)], coef="J", quant=F))

bd_as_comp_apr2_space<-do.call(rbind, bd_as_comp_apr2_space)
bd_as_comp_apr2_space<-data.frame(do.call(rbind, bd_as_comp_apr2_space[13:16]))
bd_as_comp_apr2_space<-bd_as_comp_apr2_space%>%
  mutate(site=NA, Sampling=rep("Spatiotemporal",4), Beta=rep("Space",4), Time=unique(month(apr2_nmds_data$date, label=T, abbr=T)))


###Across time####
bd_as_comp_apr2_time<-apr2_nmds_data%>%
  group_by(site)%>%
  group_map(~beta.div.comp(.x[,-c(1:2)], coef="J", quant=F))

bd_as_comp_apr2_time<-do.call(rbind, bd_as_comp_apr2_time)
bd_as_comp_apr2_time<-data.frame(do.call(rbind, bd_as_comp_apr2_time[22:28]))
bd_as_comp_apr2_time<-bd_as_comp_apr2_time%>%
  mutate(site=unique(apr2_nmds_data$site), Sampling=rep("Spatiotemporal",7), Beta=rep("Time",7), Time=NA)

##combine all datasets together####
bd_as_comp_all<-bd_as_comp_apr1%>%
  bind_rows(bd_as_comp_apr3)%>%
  bind_rows(bd_as_comp_apr2_space)%>%
  bind_rows(bd_as_comp_apr2_time)%>%
  mutate(S=1-BDtotal, Beta_comb=paste(Sampling, Beta, sep="-"),
         label=coalesce(Time,site))

#export bd_as_comp_all dataframe for final graphing
write.csv(bd_as_comp_all, "Data/graph_data/bd_as_comp_all.csv", na= "", row.names = F)

