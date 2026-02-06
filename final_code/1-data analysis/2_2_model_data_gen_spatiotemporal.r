#Load necessary packages####
library(pacman)
p_load(tidyverse,readxl,ggbiplot,ggrepel,ggthemes,rcartocolor, sf, corrplot, MASS, Hmisc, patchwork,
       lubridate, riverdist,adespatial, vegan)
#Approach 2-spatiotemporal ####
##import data####
ENVI_SE_TAL <- read_excel("Data/Hydroshare data/ENVI_SE_TAL.xlsx", 
                          sheet = "Final Data")
MAME_SE_TAL <- read_excel("Data/Hydroshare data/MAME_SE_TAL_20211027_20230131_V1.0.xlsx", 
                          sheet = "Final Data")
##macro environmental####
Macro_seasonal<-MAME_SE_TAL%>%
  filter(appr2==1|appr3==1)%>%
  filter(date<=as.POSIXct("2023-02-01"))%>%
  filter(siteId %in% c("TLM01","TLM06","TLM10","TLM16","TLM19","TLC01","TLA01"))%>%
  mutate(across(c(substrateSilt_percent:algaeFil_percent,
                  canopyCover_percent:depth5), as.numeric))
Macro_seasonal$date<-as.Date(as.character(Macro_seasonal$date), format="%Y%m%d")

#calculate reach specific sums/means
Seasonal_canopy<-Macro_seasonal%>%
  group_by(siteId,date)%>%
  summarise(mean_canopy=mean(canopyCover_percent, na.rm = T))%>%
  mutate(month=month(date))

#calculate number of substrates in each reach
substrate_count<-Macro_seasonal%>%
  dplyr::select(siteId, substrateSilt_percent:substrateBoulder_percent, date) %>%
  pivot_longer( cols = substrateSilt_percent:substrateBoulder_percent, names_to = "substrate", values_to = "per")%>%
  group_by(siteId,date)%>%
  filter(per>0)%>%
  summarise(n_sub=length(unique(substrate)))%>%
  mutate(month=month(date))

##hydro data ####
#import PTs
LTM_tal_fix_padded <- read_csv("Data/LTM_tal_fix_padded.csv")

#for temp since last seasonal, start of year for first seasonal and calculate mean
seasonal_dates<-c("2022-03-29", "2022-06-21","2022-08-11","2023-01-30")
seasonal_predates<-c("2022-01-01","2022-03-29", "2022-06-21","2022-08-11")
seasonal_dates<-as.POSIXct(seasonal_dates)
seasonal_predates<-as.POSIXct(seasonal_predates)

LTM_tal_30day<-map2_dfr(seasonal_predates,seasonal_dates, ~ {
  LTM_tal_fix_padded%>%
    filter(between(datetime, .x,.y))%>%
    filter(is.na(QAQC)|!QAQC %in% c("Z","E","ZD","ZB"))%>%
    filter(tempC>0)%>%
    group_by(siteId)%>%
    summarise(date=max(datetime), mean_temp=mean(tempC,na.rm=T),sd_temp=sd(tempC,na.rm=T))})%>%
  mutate(month=month(date))

#double check nothing weird in data
temp_data<-map2_dfr(seasonal_predates,seasonal_dates, ~ {
  LTM_tal_fix_padded%>%
    filter(between(datetime, .x,.y))%>%
    filter(is.na(QAQC)|!QAQC %in% c("Z","E","ZD","ZB"))})

ggplot(temp_data, aes(datetime, tempC))+
  facet_wrap(~siteId, scales="free")+
  geom_line()+
  theme_classic()

#What is the wet dry count since the last seasonal
LTM_tal_wetdry_30<-map2_dfr(seasonal_predates, seasonal_dates, ~{
  LTM_tal_fix_padded%>%
    filter(between(datetime, .x,.y))%>%
    filter(is.na(QAQC)|!QAQC %in% c("Z","E","ZD","ZB"))%>%
    mutate(wetdry=case_when(waterDepth_m_fix<="0"~1,
                            waterDepth_m_fix>"0"~0))%>%
    group_by(siteId, date(datetime))%>%
    summarise(date=max(datetime),dry=sum(wetdry, na.rm=T))})%>%
  mutate(month=month(date))

LTM_tal_wetdry_count<-map2_dfr(seasonal_predates, seasonal_dates, ~{
  LTM_tal_wetdry_30%>%
    filter(between(date, .x,.y))%>%
    mutate(dry_day=case_when(dry>"0"~1,
                             dry<="0"~0))%>%
    group_by(siteId)%>%
    summarise(date=max(date),dry_num=sum(dry_day, na.rm=T))})

#Does it look like flowing and wet dry and matching across sites, restrict to fall when know things were dry
#TLA01, TLM16 doing diel swings once things are low, maybe for this one if it hits zero during the day, no flow but that matches
#TLC01 we know is in pool,0.095 seems to be break where flow stops
#TLM01 tricky because also in pool but sensor not great around time, we know disconnected on Oct 7, 0.40 m
LTM_tal_fix_padded%>%
  filter(between(datetime, as.POSIXct("2021-10-01", tz="UTC"), as.POSIXct("2023-01-30", tz="UTC")))%>%
  filter(is.na(QAQC)|!QAQC %in% c("Z","E","ZD","ZB"))%>%
  filter(siteId=="TLC01")%>%
  ggplot(aes(datetime, waterDepth_m_fix))+
  facet_wrap(~siteId)+
  geom_line()+
  geom_vline(xintercept = seasonal_dates, color="#6599CD", linewidth=1)+
  #geom_hline(yintercept = 0.4)+
  #scale_y_continuous(breaks=seq(0.06,0.20, 0.01))+
  #scale_x_datetime(date_breaks = "1 week")+
  theme_few()

#calculate number of days without flow between sampling dates
no_flow_most<-map2_dfr(seasonal_predates, seasonal_dates, ~{
  LTM_tal_fix_padded%>%
    filter(between(datetime, .x,.y))%>%
    filter(is.na(QAQC)|!QAQC %in% c("Z","E","ZD","ZB"))%>%
    filter(!siteId %in% c("TLM01","TLC01"))%>%
    mutate(no_flow=case_when(waterDepth_m_fix<="0"~1,
                             waterDepth_m_fix>"0"~0))%>%
    group_by(siteId, date(datetime))%>%
    summarise(date=max(datetime),no_flow_count=sum(no_flow, na.rm=T))%>%
    ungroup()%>%
    mutate(no_flow_day=case_when(no_flow_count>"0"~1,
                                 no_flow_count<="0"~0))%>%
    group_by(siteId)%>%
    summarise(date=max(date), no_flow_sum=sum(no_flow_day))
})

no_flow_TLM01<-map2_dfr(seasonal_predates, seasonal_dates, ~{
  LTM_tal_fix_padded%>%
    filter(between(datetime, .x,.y))%>%
    filter(is.na(QAQC)|!QAQC %in% c("Z","E","ZD","ZB"))%>%
    filter(siteId =="TLM01")%>%
    mutate(no_flow=case_when(waterDepth_m_fix<="0.4"~1,
                             waterDepth_m_fix>"0.4"~0))%>%
    group_by(siteId, date(datetime))%>%
    summarise(date=max(datetime),no_flow_count=sum(no_flow, na.rm=T))%>%
    ungroup()%>%
    mutate(no_flow_day=case_when(no_flow_count>"0"~1,
                                 no_flow_count<="0"~0))%>%
    group_by(siteId)%>%
    summarise(date=max(date), no_flow_sum=sum(no_flow_day))
})
#fix sensor error in first couple of seasonals causing single no flow read
no_flow_TLM01$no_flow_sum<-c(0,0,0,12)

no_flow_TLC01<-map2_dfr(seasonal_predates, seasonal_dates, ~{
  LTM_tal_fix_padded%>%
    filter(between(datetime, .x,.y))%>%
    filter(is.na(QAQC)|!QAQC %in% c("Z","E","ZD","ZB"))%>%
    filter(siteId =="TLC01")%>%
    mutate(no_flow=case_when(waterDepth_m_fix<="0.095"~1,
                             waterDepth_m_fix>"0.095"~0))%>%
    group_by(siteId, date(datetime))%>%
    summarise(date=max(datetime),no_flow_count=sum(no_flow, na.rm=T))%>%
    ungroup()%>%
    mutate(no_flow_day=case_when(no_flow_count>"0"~1,
                                 no_flow_count<="0"~0))%>%
    group_by(siteId)%>%
    summarise(date=max(date), no_flow_sum=sum(no_flow_day))
})
#combine together
no_flow_all<-no_flow_most%>%
  bind_rows(no_flow_TLM01)%>%
  bind_rows(no_flow_TLC01)
#ended up using only for descriptive purposes since have to make a lot of assumptions
##pull waterquality for nh4, srp, doc####
NUTR_TAL <- read_excel("Data/Hydroshare data/NUTR_SE_TAL_20210915_20241004_V1.0.xlsx", 
                       sheet = "Final Data")
DOC_TAL <- read_excel("Data/Hydroshare data/DOCS_SE_TAL_20211007_20241004_V1.0.xlsx", 
                      sheet = "Final Data")

NUTR_TAL<-NUTR_TAL%>%
  mutate(across(c(SRPugL:NO3ugL), as.numeric))

DOC_TAL$date<-as.Date(DOC_TAL$dateReg)
#filter to just apr2 in first year with subset of sites that match from apr3
NUTR_TAL_apr2<-NUTR_TAL%>%
  filter(appr2==1|appr3==1)%>%
  filter(date<=as.POSIXct("2023-02-01"))%>%
  filter(siteId %in% c("TLM01","TLM06","TLM10","TLM16","TLM19","TLC01","TLA01"))%>%
  mutate(month=month(date))

DOC_TAL_apr2<-DOC_TAL%>%
  filter(appr2==1|appr3==1)%>%
  filter(date<=as.POSIXct("2023-02-01"))%>%
  filter(siteId %in% c("TLM01","TLM06","TLM10","TLM16","TLM19","TLC01","TLA01"))%>%
  mutate(month=month(date))

#remove extra seasonal in Janaury 2022 where we didn't sample bugs
DOC_TAL_apr2<-DOC_TAL_apr2[-c(1:7),]
#approach marking wrong for June sampling
##import discharge ####
DISL_SE_TAL_20211007_20241004_V1_0 <- read_excel("Data/Hydroshare data/DISL_SE_TAL_20211007_20241004_V1.0.xlsx", 
                                                 sheet = "Final Data")

DISL_apr2<-DISL_SE_TAL_20211007_20241004_V1_0%>%
  filter(appr2==1|appr3==1)%>%
  filter(dateReg<=as.POSIXct("2023-02-01"))%>%
  filter(siteId %in% c("TLM01","TLM06","TLM10","TLM16","TLM19","TLC01","TLA01"))%>%
  mutate(month=month(dateReg))
#remove extra discharge measurements from 6/21 taken after actual sampling of TLM01 and early partial seasonal
#with no bug data 1/22
DISL_apr2<-DISL_apr2[-c(1:7,19,22),]
#make pca dataframe
pca_abiotic_seasonal<-LTM_tal_30day[,c(1:3,5)]%>%
  left_join(NUTR_TAL_apr2[,c(2,3,16,19,33)], join_by(siteId,month))

pca_abiotic_seasonal$date<-as.Date(pca_abiotic_seasonal$date.x)  

pca_abiotic_seasonal<-pca_abiotic_seasonal%>%
  relocate(date, .after = siteId)%>%
  dplyr::select(-c(date.x,date.y))%>%
  left_join(DOC_TAL_apr2[,c(2,3,15,20)],join_by(siteId, month))%>%
  left_join(DISL_apr2[,c(3,14,16,17,19)],join_by(siteId, month))%>%
  left_join(Seasonal_canopy, join_by(siteId, month))%>%
  left_join(substrate_count,join_by(siteId, month))

pca_abiotic_seasonal<-pca_abiotic_seasonal%>%
  dplyr::select(-c(date.y,date.x.x,date.y.y,dateReg))%>%
  rename(date=date.x)

#make velocity a number
pca_abiotic_seasonal$vms<-as.numeric(pca_abiotic_seasonal$vms)
pca_abiotic_seasonal$QLs<-as.numeric(pca_abiotic_seasonal$QLs)

##run pca for each seasonal####
###subset data and run pca mar####
pca_mar_df<-pca_abiotic_seasonal[,-c(1,4)]%>%
  filter(month(date)==3)%>%
  dplyr::select(-date)

pca_mar<-pca_mar_df%>%
  prcomp(.,center=T, scale. = T)
pca_mar
summary(pca_mar)
plot(pca_mar)
ggbiplot(pca_mar)

#make better plot so can see how sites are splitting
#make dataframe so you can plot in ggplot yourself
pca_mar_graph_df<-data.frame(site=subset(pca_abiotic_seasonal$siteId, month(pca_abiotic_seasonal$date)==3),
                             date=subset(pca_abiotic_seasonal$date, month(pca_abiotic_seasonal$date)==3),
                             PC1=-1*pca_mar$x[,1], PC2=pca_mar$x[,2])
pca_mar_vec<-data.frame(var=row.names(pca_mar$rotation), PC1=-1*pca_mar$rotation[,1], PC2=pca_mar$rotation[,2])
pca_mar_vec$var<-c("mean temp","SRP µg/L", "NH4-N µg/L", "DOC mg/L", "Q L/S", "v m/s" ,"mean canopy", "n substrates")
#plot
pca_mar_graph_df<-pca_mar_graph_df%>%
  left_join(ENVI_SE_TAL[,c(3,16)], join_by("site"=="siteId"))

plot_vec_mar <- ggplot(pca_mar_graph_df, aes(x = PC1, y= PC2)) + 
  geom_point(aes(fill = as.numeric(drainage_area_m)/1e6),size=5, shape = 21) + 
  geom_segment(data = pca_mar_vec,
               aes(x = 0, xend =PC1*3, y = 0, yend = PC2*3),
               arrow = arrow(length = unit(0.25, "cm")), colour = "black") +
  geom_label_repel(data = pca_mar_vec, aes(PC1*3, PC2*3, label=var),
                   size = 4)+
  scale_fill_carto_c(palette="Geyser", direction = -1, 
                     name=bquote("Watershed Area ("~km^2~")"))+
  labs(x="PC1 48.0%", y="PC2 22.7%", title="March")+
  theme_few(base_size = 14)+
  theme(legend.position = "bottom");plot_vec_mar

#perform euclidean distance matrix
pca_apr2_mar<-dist(pca_mar$x, method="euclidean", diag = T)
pca_apr2_mar<-as.matrix(pca_apr2_mar)
#extract matrix for analysis
write.csv(pca_apr2_mar, "Data/apr2/pca_apr2_mar.csv", row.names = F)
###run pca jun####
pca_jun_df<-pca_abiotic_seasonal[,-c(1,4)]%>%
  filter(month(date)==6)%>%
  dplyr::select(-date)

pca_jun<-pca_jun_df%>%
  prcomp(.,center=T, scale. = T)
pca_jun
summary(pca_jun)
plot(pca_jun)
ggbiplot(pca_jun)

#make better plot so can see how sites are splitting
#make dataframe so you can plot in ggplot yourself
pca_jun_graph_df<-data.frame(site=subset(pca_abiotic_seasonal$siteId, month(pca_abiotic_seasonal$date)==6),
                             date=subset(pca_abiotic_seasonal$date, month(pca_abiotic_seasonal$date)==6),
                             PC1=-1*pca_jun$x[,1], PC2=pca_jun$x[,2])
pca_jun_vec<-data.frame(var=row.names(pca_jun$rotation), PC1=-1*pca_jun$rotation[,1], PC2=pca_jun$rotation[,2])
pca_jun_vec$var<-c("mean temp","SRP µg/L", "NH4-N µg/L", "DOC mg/L", "Q L/S", "v m/s" ,"mean canopy", "n substrates")
#plot
pca_jun_graph_df<-pca_jun_graph_df%>%
  left_join(ENVI_SE_TAL[,c(3,16)], join_by("site"=="siteId"))

plot_vec_jun <- ggplot(pca_jun_graph_df, aes(x = PC1, y= PC2)) + 
  geom_point(aes(fill = as.numeric(drainage_area_m)/1e6),size=5, shape = 21) + 
  geom_segment(data = pca_mar_vec,
               aes(x = 0, xend =PC1*3, y = 0, yend = PC2*3),
               arrow = arrow(length = unit(0.25, "cm")), colour = "black") +
  geom_label_repel(data = pca_mar_vec, aes(PC1*3, PC2*3, label=var),
                   size = 4)+
  scale_fill_carto_c(palette="Geyser", direction = -1, 
                     name=bquote("Watershed Area ("~km^2~")"))+
  labs(x="PC1 50.1%", y="PC2 22.0%", title="June")+
  theme_few(base_size = 14)+
  theme(legend.position = "bottom");plot_vec_jun
#perform euclidean distance matrix
pca_apr2_jun<-dist(pca_jun$x, method="euclidean", diag = T)
pca_apr2_jun<-as.matrix(pca_apr2_jun)
#extract matrix for analysis
write.csv(pca_apr2_jun, "Data/apr2/pca_apr2_jun.csv", row.names = F)
###run pca aug####
pca_aug_df<-pca_abiotic_seasonal[,-c(1,4)]%>%
  filter(month(date)==8)%>%
  dplyr::select(-date)

pca_aug<-pca_aug_df%>%
  prcomp(.,center=T, scale. = T)
pca_aug
summary(pca_aug)
plot(pca_aug)
ggbiplot(pca_aug)

#make better plot so can see how sites are splitting
#make dataframe so you can plot in ggplot yourself
pca_aug_graph_df<-data.frame(site=subset(pca_abiotic_seasonal$siteId, month(pca_abiotic_seasonal$date)==8),
                             date=subset(pca_abiotic_seasonal$date, month(pca_abiotic_seasonal$date)==8),
                             PC1=-1*pca_aug$x[,1], PC2=pca_aug$x[,2])
pca_aug_vec<-data.frame(var=row.names(pca_aug$rotation), PC1=-1*pca_aug$rotation[,1], PC2=pca_aug$rotation[,2])

#plot
pca_aug_graph_df<-pca_aug_graph_df%>%
  left_join(ENVI_SE_TAL[,c(3,16)], join_by("site"=="siteId"))
pca_aug_vec$var<-c("mean temp","SRP µg/L", "NH4-N µg/L", "DOC mg/L", "Q L/S", "v m/s" ,"mean canopy", "n substrates")
#plot
plot_vec_aug <- ggplot(pca_aug_graph_df, aes(x = PC1, y= PC2)) + 
  geom_point(aes(fill = as.numeric(drainage_area_m)/1e6),size=5, shape = 21) + 
  geom_segment(data = pca_aug_vec,
               aes(x = 0, xend =PC1*3, y = 0, yend = PC2*3),
               arrow = arrow(length = unit(0.25, "cm")), colour = "black") +
  geom_label_repel(data = pca_aug_vec, aes(PC1*3, PC2*3, label=var),
                   size = 4)+
  scale_fill_carto_c(palette="Geyser", direction = -1, 
                     name=bquote("Watershed Area ("~km^2~")"))+
  labs(x="PC1 40.2%", y="PC2 32.0%", title="August")+
  theme_few(base_size = 14)+
  theme(legend.position = "bottom");plot_vec_aug

#perform euclidean distance matrix
pca_apr2_aug<-dist(pca_aug$x, method="euclidean", diag = T)
pca_apr2_aug<-as.matrix(pca_apr2_aug)
#extract matrix for analysis
write.csv(pca_apr2_aug, "Data/apr2/pca_apr2_aug.csv", row.names = F)
###run pca jan####
pca_jan_df<-pca_abiotic_seasonal[,-c(1,4)]%>%
  filter(month(date)==1)%>%
  dplyr::select(-date)

pca_jan<-pca_jan_df%>%
  prcomp(.,center=T, scale. = T)

pca_jan
summary(pca_jan)
plot(pca_jan)
ggbiplot(pca_jan)

#make better plot so can see how sites are splitting
#make dataframe so you can plot in ggplot yourself
pca_jan_graph_df<-data.frame(site=subset(pca_abiotic_seasonal$siteId, month(pca_abiotic_seasonal$date)==1),
                             date=subset(pca_abiotic_seasonal$date, month(pca_abiotic_seasonal$date)==1),
                             PC1=-1*pca_jan$x[,1], PC2=pca_jan$x[,2])
pca_jan_vec<-data.frame(var=row.names(pca_jan$rotation), PC1=-1*pca_jan$rotation[,1], PC2=pca_jan$rotation[,2])
pca_jan_vec$var<-c("mean temp","SRP µg/L", "NH4-N µg/L", "DOC mg/L", "Q L/S", "v m/s" ,"mean canopy", "n substrates")

pca_jan_graph_df<-pca_jan_graph_df%>%
  left_join(ENVI_SE_TAL[,c(3,16)], join_by("site"=="siteId"))
#plot
plot_vec_jan <- ggplot(pca_jan_graph_df, aes(x = PC1, y= PC2)) + 
  geom_point(aes(fill = as.numeric(drainage_area_m)/1e6),size=5, shape = 21) + 
  geom_segment(data = pca_jan_vec,
               aes(x = 0, xend =PC1*3, y = 0, yend = PC2*3),
               arrow = arrow(length = unit(0.25, "cm")), colour = "black") +
  geom_label_repel(data = pca_jan_vec, aes(PC1*3, PC2*3, label=var),
                   size = 4)+
  scale_fill_carto_c(palette="Geyser", direction = -1, 
                     name=bquote("Watershed Area ("~km^2~")"))+
  labs(x="PC1 53.5%", y="PC2 21.1%", title="January")+
  theme_few(base_size = 14)+
  theme(legend.position = "bottom");plot_vec_jan

#perform euclidean distance matrix
pca_apr2_jan<-dist(pca_jan$x, method="euclidean", diag = T)
pca_apr2_jan<-as.matrix(pca_apr2_jan)
#extract matrix for analysis
write.csv(pca_apr2_jan, "Data/apr2/pca_apr2_jan.csv", row.names = F)
#make dataframe for models
apr2_data<-pca_mar_graph_df%>%
  bind_rows(pca_jun_graph_df)%>%
  bind_rows(pca_aug_graph_df)%>%
  bind_rows(pca_jan_graph_df)%>%
  dplyr::select(!PC2)


apr2_data$drainage_area_m<-as.numeric(apr2_data$drainage_area_m)

###plot pcas together ####
plot_vec_mar+plot_vec_jun+plot_vec_aug+plot_vec_jan+plot_layout(guides='collect')& theme(legend.position="bottom")

ggplot2::ggsave("./graphs/pca_seasonal.png",dpi=800, width=10, height=8)

##calculate alpha diversity to match to env data####
apr3_read_join_taxa <- read_csv("Data/Final_metabarcoding/apr3_read_join_taxa.csv")
apr3_read_join_taxa$date<-as_date(as.character(apr3_read_join_taxa$date), format="%Y%m%d")
apr2_read_join_taxa_TAL <- read_csv("Data/Final_metabarcoding/apr2_read_join_taxa_TAL.csv")
apr2_read_join_taxa_TAL$date<-as_date(as.character(apr2_read_join_taxa_TAL$date), format="%Y%m%d")
#add in ltm sites from apr3
apr2_read_join_taxa_all<-apr3_read_join_taxa%>%
  filter(site %in% c("TLM01", "TLC01", "TLM06", "TLM10", "TLM16", "TLM19", "TLA01"))%>%
  bind_rows(apr2_read_join_taxa_TAL)
#write to csv for future mantel test analysis
write.csv(apr2_read_join_taxa_all, "Data/apr2/apr2_read_join_taxa_all.csv", row.names = F)
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
  summarise(rich=sum(c_across(Stenelmis:`Lepidostoma carrolli`)))%>%
  mutate(month=month(date))

##put into single dataframe
apr2_data<-apr2_data%>%
  mutate(month=month(date))
apr2_data<-apr2_data%>%
  left_join(apr2_richness, join_by(site, month))%>%
  dplyr::select(-date.y)%>%
  rename(date=date.x)
#make area km2
apr2_data$drainage_area_km<-apr2_data$drainage_area_m/1e6
#extract variables for analysis
write.csv(apr2_data, "Data/apr2/apr2_data.csv", row.names = F)

###make taxa matrix for each seasonal####
TAL_macro_wide<-apr2_read_join_taxa_all%>%
  mutate(date=month(date, label=T, abbr=T))%>%
  group_by(date)%>%
  arrange(site)%>%
  group_nest()%>%
  mutate(data=lapply(data, function(d){
    wide<-pivot_wider(d,id_cols = site, names_from =Lowest_ID , values_from = tot_read, values_fn=sum, values_fill = 0)
    # Convert all values > 0 to 1 (excluding 'id' column)
    wide[-1] <- lapply(wide[-1], function(x) ifelse(x > 0, 1, x))
    as.data.frame(wide)
  })) %>%
  {setNames(.$data,.$date)}

#calculate dissimilarity matrix for each seasonal
tal_macro_dist_list<-lapply(TAL_macro_wide, function(x) vegdist(x[,-c(1:2)], method="jaccard"))

tal_macro_list_mat<-lapply(tal_macro_dist_list, as.matrix)

tal_macro_dist_long<-data.frame(do.call(rbind, tal_macro_list_mat))%>%
  pivot_longer(cols=X1:X7,names_to="site.y",values_to = "jac")%>%
  mutate(site.x=rep(rep(c("TLA01","TLC01","TLM01","TLM06","TLM10","TLM16","TLM19"),each=7),4),
         site.y=rep(rep(c("TLA01","TLC01","TLM01","TLM06","TLM10","TLM16","TLM19"),times=7),4))%>%
  dplyr::select(site.x,site.y, jac)%>%
  mutate(month=rep(c("Jan","Mar","Jun","Aug"), each=49))

##pull apr3 distance and subset for apr2####
apr3_data_long <- read_csv("Data/apr3/apr3_data_long.csv")
dmat_tal<-apr3_data_long%>%
  filter(site.x %in% c("TLM01","TLM06","TLM10","TLM16","TLM19","TLC01","TLA01") &
           site.y %in% c("TLM01","TLM06","TLM10","TLM16","TLM19","TLC01","TLA01"))%>%
  dplyr::select(!c(env_dist, jac))%>%
  pivot_wider(names_from=site.x, values_from = riv_dist)%>%
  dplyr::select(!site.y)%>%
  as.dist()

dmat_tal_log<-log(dmat_tal)
#write apr3 river dist to csv
write.csv(dmat_tal_log%>%as.matrix(), "Data/apr2/dmat_tal_log.csv", row.names=F)

##get long form to match for later graphing####
apr2_dist_long<-apr3_data_long%>%
  filter(site.x %in% c("TLM01","TLM06","TLM10","TLM16","TLM19","TLC01","TLA01") &
           site.y %in% c("TLM01","TLM06","TLM10","TLM16","TLM19","TLC01","TLA01"))%>%
  dplyr::select(!c(env_dist, jac))
##PCAs into long and combine####
pca_apr2_mar<-pca_apr2_mar%>%
  as.matrix()%>%
  as_tibble()%>%
  mutate(site.x=c("TLA01","TLC01","TLM01","TLM06","TLM10","TLM16","TLM19"))%>%
  pivot_longer(-site.x, names_to="site.y",values_to="env_dist")%>%
  mutate(site.y=rep(c("TLA01","TLC01","TLM01","TLM06","TLM10","TLM16","TLM19"), times=7),
         month="Mar")

pca_apr2_jun<-pca_apr2_jun%>%
  as.matrix()%>%
  as_tibble()%>%
  mutate(site.x=c("TLA01","TLC01","TLM01","TLM06","TLM10","TLM16","TLM19"))%>%
  pivot_longer(-site.x, names_to="site.y",values_to="env_dist")%>%
  mutate(site.y=rep(c("TLA01","TLC01","TLM01","TLM06","TLM10","TLM16","TLM19"), times=7),
         month="Jun")

pca_apr2_aug<-pca_apr2_aug%>%
  as.matrix()%>%
  as_tibble()%>%
  mutate(site.x=c("TLA01","TLC01","TLM01","TLM06","TLM10","TLM16","TLM19"))%>%
  pivot_longer(-site.x, names_to="site.y",values_to="env_dist")%>%
  mutate(site.y=rep(c("TLA01","TLC01","TLM01","TLM06","TLM10","TLM16","TLM19"), times=7),
         month="Aug")

pca_apr2_jan<-pca_apr2_jan%>%
  as.matrix()%>%
  as_tibble()%>%
  mutate(site.x=c("TLA01","TLC01","TLM01","TLM06","TLM10","TLM16","TLM19"))%>%
  pivot_longer(-site.x, names_to="site.y",values_to="env_dist")%>%
  mutate(site.y=rep(c("TLA01","TLC01","TLM01","TLM06","TLM10","TLM16","TLM19"), times=7),
         month="Jan")
pca_apr2<-pca_apr2_mar%>%
  rbind(pca_apr2_jun)%>%
  rbind(pca_apr2_aug)%>%
  rbind(pca_apr2_jan)
##Combine together####
apr2_data_long<-pca_apr2%>%
  left_join(tal_macro_dist_long)%>%
  left_join(apr2_dist_long)%>%
  relocate(month, .before = site.x)

#write.csv
write.csv(apr2_data_long, 'Data/apr2/apr2_data_long.csv', row.names=F)
