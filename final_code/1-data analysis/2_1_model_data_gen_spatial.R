#Load necessary packages####
library(pacman)
p_load(tidyverse,readxl,ggbiplot,ggrepel,ggthemes,rcartocolor, sf, corrplot, MASS, Hmisc, patchwork,
       lubridate, riverdist,adespatial, vegan)

#Spatial Sampling-Approach 3 ####
##import environmental data and spatial data ####
#spatial data (distance to outlet included)
ENVI_SE_TAL <- read_excel("Data/Hydroshare data/ENVI_SE_TAL.xlsx", 
                          sheet = "Final Data")
MAME_SE_TAL <- read_excel("Data/Hydroshare data/MAME_SE_TAL_20211027_20230131_V1.0.xlsx", 
                          sheet = "Final Data")

##Environmental data####
###macro environmental####
Synoptic_Macro_Field<-MAME_SE_TAL%>%
  filter(appr3==1)%>%
  mutate(across(c(substrateSilt_percent:algaeFil_percent,
                  canopyCover_percent:depth5), as.numeric))

#calculate number of substrates in each reach
substrate_count<-Synoptic_Macro_Field%>%
  dplyr::select(siteId, substrateSilt_percent:substrateBoulder_percent) %>%
  pivot_longer( cols = substrateSilt_percent:substrateBoulder_percent, names_to = "substrate", values_to = "per")%>%
  group_by(siteId)%>%
  filter(per>0)%>%
  summarise(n_sub=length(unique(substrate)))
#calculate mean canopy cover
#calculate reach specific sums/means
Synoptic_canopy<-Synoptic_Macro_Field%>%
  group_by(siteId)%>%
  summarise(mean_canopy=mean(canopyCover_percent, na.rm = T))
###Calculate temp- mean stics/ltms, full record %wet (calculated for stics )####
###import stics####
stic_tal<-list.files(path= "Data/Hydroshare data/stics/", pattern="*.csv", full.names=T, recursive=T)%>%
  map_df(.,~read_csv(., col_types = cols(QAQC = col_character())), id="file_name")
stic_tal<-stic_tal[,c(2,3,9,11:13)]
#for temp restrict to preceding 7 days before start of synoptic and calculate mean and sd
stic_tal_7day<-stic_tal%>%
  filter(between(datetime,as.POSIXct("2022-06-21 0:00:00"), as.POSIXct("2022-06-27 24:00:00")))%>%
  group_by(siteId)%>%
  summarise(mean_temp=mean(tempC), sd_temp=sd(tempC))

#missing TLM20 for this period. Look at full preceeding period and choose which site is most similar, or month of april??
##TLA04 most similar
stic_tal_1yr<-stic_tal%>%
  filter(between(datetime,as.POSIXct("2022-04-01 0:00:00"), as.POSIXct("2022-05-01 00:00:00")))%>%
  group_by(siteId)%>%
  summarise(min_temp=min(tempC), max_temp=max(tempC), mean_temp=mean(tempC), sd_temp=sd(tempC))
#full record for %wet dry at a site--what bug sites aren't long term stics?
stic_tal%>%
  group_by(siteId)%>%
  summarise(min_date=min(datetime), max=max(datetime),n=length(unique(date(datetime))))%>%
  filter(n<=400)%>%
  semi_join(substrate_count)%>%
  print(n=30)

stic_tal_wetdry<-stic_tal%>%
  filter(is.na(QAQC)|QAQC!="D")%>%
  group_by(siteId)%>%
  summarise(per_wet_full=sum(wetdry)/length(wetdry))

stic_tal_wetdry_30<-stic_tal%>%
  filter(is.na(QAQC)|QAQC!="D")%>%
  filter(between(datetime, as.POSIXct("2022-05-09"), as.POSIXct("2022-06-09")))%>%
  group_by(siteId)%>%
  summarise(per_wet_full=sum(wetdry)/length(wetdry))%>%
  semi_join(substrate_count)

###import cleaned PT data####
LTM_tal_fix_padded <- read_csv("Data/LTM_tal_fix_padded.csv")
#for temp restrict to preceding 7 days before start of synoptic and calculate mean and sd
LTM_tal_7day<-LTM_tal_fix_padded%>%
  filter(between(datetime,as.POSIXct("2022-06-21 0:00:00"), as.POSIXct("2022-06-27 24:00:00")))%>%
  group_by(siteId)%>%
  summarise( mean_temp=mean(tempC, na.rm=T),sd_temp=sd(tempC, na.rm=T))
#full record for %wet dry at a site
LTM_tal_wetdry<-LTM_tal_fix_padded%>%
  filter(is.na(QAQC)|!QAQC %in% c("Z","E","ZD","ZB"))%>%
  group_by(siteId)%>%
  mutate(wetdry=case_when(waterDepth_m_fix<="0"~0,
                         waterDepth_m_fix>"0"~1))%>%
  summarise(per_wet_full=sum(wetdry, na.rm=T)/length(wetdry))
#compile data together
temp<-bind_rows(stic_tal_7day, LTM_tal_7day)
#drop sites missing full data
wetdry<-bind_rows(stic_tal_wetdry, LTM_tal_wetdry)

#check what data is available for all sites
check_hydro<-substrate_count%>%
  left_join(wetdry)%>%
  left_join(temp)%>%
  mutate(record_length=case_when(siteId%in%c("TLB02","TLC03","TLCG1","TLMG5","TLM22","TLZ01")~"short",
                                 .default = "full"))
#missing data for TLM20 due to sensor error and have 6 listed above with limited preceeding record
###pull waterquality for nh4, srp, doc####
NUTR_TAL <- read_excel("Data/Hydroshare data/NUTR_SE_TAL_20210915_20241004_V1.0.xlsx", 
                       sheet = "Final Data")
DOC_TAL <- read_excel("Data/Hydroshare data/DOCS_SE_TAL_20211007_20241004_V1.0.xlsx", 
                      sheet = "Final Data")
#filter to just apr3
NUTR_TAL_apr3<-NUTR_TAL%>%
  filter(appr3==1)
DOC_TAL_apr3<-DOC_TAL%>%
  filter(appr3==1)

###make pca dataframe**check column choices since data updated####
pca_abiotic<-substrate_count%>%
  left_join(Synoptic_canopy)%>%
  left_join(temp[,c(1,2)])%>%
  left_join(NUTR_TAL_apr3[,c(3,16,19)])%>%
  left_join(DOC_TAL_apr3[,c(3,15)])%>%
  filter(siteId!="TLM20")

##Run PCA ####  
#run pca
pca<-prcomp(pca_abiotic[,-1], center=T, scale. = T)
pca
summary(pca)
plot(pca)
ggbiplot(pca)

#make better plot so can see how sites are splitting
#make dataframe so you can plot in ggplot yourself
pca_df<-data.frame(site=pca_abiotic$siteId,PC1=pca$x[,1], PC2=pca$x[,2])
pca_vec<-data.frame(var=row.names(pca$rotation), PC1=pca$rotation[,1], PC2=pca$rotation[,2])
pca_vec$var<-c("n substrates","mean canopy", "mean temp", "SRP µg/L",
               "NH4-N µg/L","DOC mg/L")
#plot
#add watershed area to plot
pca_df<-pca_df%>%
  left_join(ENVI_SE_TAL[,c(3,16)], join_by("site"=="siteId"))
#multiply pc1 by -1 so that increasing with majority of measures 
plot_vec <- ggplot(pca_df, aes(x = -PC1, y= -PC2)) + 
  geom_point(aes(fill=as.numeric(drainage_area_m)/1e6),size=5, shape=21) + 
  labs(x = "PC1 (29.9%)", y = "PC2 (19.5%)")+
  geom_segment(data = pca_vec,
               aes(x = 0, xend =-PC1*3, y = 0, yend = -PC2*3),
               arrow = arrow(length = unit(0.25, "cm")), colour = "black") +
  geom_label_repel(data = pca_vec, aes(-PC1*3, -PC2*3, label=var),
                   size = 4)+
  scale_fill_carto_c(palette="Geyser", direction = -1, 
                     name=bquote("Watershed Area ("~km^2~")"))+
  theme_few(base_size = 14)+
  theme(legend.position = "bottom");plot_vec

ggplot2::ggsave("./graphs/pca_spatial.png",dpi=800, width=7, height=6)

##make dataframe for models####
apr3_data<-pca_df[,c(1:2)]
apr3_data<-apr3_data%>%
  left_join(ENVI_SE_TAL[,c(3,16)], join_by("site"=="siteId"))

apr3_data$drainage_area_m<-as.numeric(apr3_data$drainage_area_m)

###calculate alpha richness####
apr3_read_join_taxa <- read_csv("Data/Final_metabarcoding/apr3_read_join_taxa.csv")
apr3_read_join_taxa$date<-as_date(as.character(apr3_read_join_taxa$date), format="%Y%m%d")
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
  ungroup()

###put into single dataframe and reverse direction of PC1 to match previous graph####
apr3_data<-apr3_data%>%
  mutate(PC1=-1*PC1)%>%
  left_join(apr3_richness)
#write csv for single site measures
write.csv(apr3_data, "Data/apr3/apr3_data.csv", row.names = F)

##All paired site comparisons####
###perform euclidean distance matrix####
pca_apr3<-dist(pca$x, method="euclidean", diag = T)
#write csv
write.csv(pca_apr3%>%as.matrix(),"Data/apr3/pca_apr3.csv", row.names=F)

##Calculate distance between all points####
#get distance between all points of interest
#import spatial layers
flow_net_tal<-st_read("Data/Map_layers/flow_net_tal.shp")

#make line 2 network
flow_net_tal_line<-line2network(flow_net_tal, tolerance=50)
plot(flow_net_tal_line)

#clean network
flow_net_tal_clean<-cleanup(flow_net_tal_line)
plot(flow_net_tal_clean)

#check connection
topologydots(rivers=flow_net_tal_clean)

#filter to points with environmental data 
ENVI_SE_TAL_apr3<-ENVI_SE_TAL%>%
  semi_join(apr3_data, join_by(siteId==site))%>%
  arrange(siteId)
#convert to spatial dataframe so you can transform to UTM coordinates
ENVI_SE_TAL_apr3<-ENVI_SE_TAL_apr3%>%
  st_as_sf(
    coords = c("long", "lat"),
    crs = '+proj=longlat +datum=WGS84 +no_defs')%>%
  st_transform("+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +type=crs")
#convert points to riverdist points 
TAL_apr3_points_xy<-as.data.frame(st_coordinates(ENVI_SE_TAL_apr3))

TAL_apr3_points<-xy2segvert(x=TAL_apr3_points_xy$X, TAL_apr3_points_xy$Y, rivers=flow_net_tal_clean)
head(TAL_apr3_points)
dmat_tal<-riverdistancemat(TAL_apr3_points$seg, TAL_apr3_points$vert, flow_net_tal_clean)
dmat_tal<-as.dist(dmat_tal)
dmat_tal_log<-log(dmat_tal)
#write distance matrix
write.csv(dmat_tal_log%>%as.matrix(), "Data/apr3/dmat_tal_log.csv", row.names = F)
#convert to long
rivdist_long<-dmat_tal%>%
  as.matrix()%>%
  as_tibble()%>%
  mutate(site.x=apr3_data$site)%>%
  pivot_longer(-site.x, names_to="site.y",values_to="riv_dist")%>%
  mutate(site.y=rep(apr3_data$site, times=27))

#convert env dist to long
apr3_env_long<-pca_apr3%>%
  as.matrix()%>%
  as_tibble()%>%
  mutate(site.x=apr3_data$site)%>%
  pivot_longer(-site.x, names_to="site.y",values_to="env_dist")%>%
  mutate(site.y=rep(apr3_data$site, times=27))

###get dissimilarity####
#calculate dissilimarity
#filter to only sites with environmental data
apr3_nmds_data<-apr3_nmds_data%>%
  semi_join(apr3_data)%>%
  arrange(site)
apr3_dist<-vegdist(apr3_nmds_data[,-c(1:2)], method = "jaccard")

#write to csv
write.csv(apr3_dist%>%as.matrix(), "Data/apr3/apr3_dist.csv", row.names = F)
#convert to long
apr3_dist_long<-apr3_dist%>%
  as.matrix()%>%
  as_tibble()%>%
  mutate(site.x=apr3_data$site)%>%
  pivot_longer(-site.x, names_to="site.y",values_to="jac")%>%
  mutate(site.y=rep(apr3_data$site, times=27))

##combine####
apr3_data_long<-rivdist_long%>%
  left_join(apr3_env_long)%>%
  left_join(apr3_dist_long)

#write to csv
write.csv(apr3_data_long, "Data/apr3/apr3_data_long.csv", row.names = F)
