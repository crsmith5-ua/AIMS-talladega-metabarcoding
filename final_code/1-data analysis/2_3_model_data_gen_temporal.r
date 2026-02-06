#Load necessary packages####
library(pacman)
p_load(tidyverse,readxl,ggbiplot,ggrepel,ggthemes,rcartocolor, sf, corrplot, MASS, Hmisc, patchwork,
       lubridate, riverdist,adespatial, vegan)

#Approach 1- temporal####
MAME_SE_TAL <- read_excel("Data/Hydroshare data/MAME_SE_TAL_20211027_20230131_V1.0.xlsx", 
                          sheet = "Final Data")

Macro_apr1<-MAME_SE_TAL%>%
  filter(appr1==1)%>%
  mutate(across(c(substrateSilt_percent:algaeFil_percent,
                  canopyCover_percent:depth5), as.numeric))
Macro_apr1$date<-as.Date(as.character(Macro_apr1$date), format="%Y%m%d")


#calculate reach specific sums/means
apr1_canopy<-Macro_apr1%>%
  group_by(siteId,date)%>%
  summarise( mean_canopy=mean(canopyCover_percent, na.rm = T))

#calculate number of substrates in each reach
substrate_count<-Macro_apr1%>%
  dplyr::select(siteId, substrateSilt_percent:substrateBoulder_percent, date) %>%
  pivot_longer( cols = substrateSilt_percent:substrateBoulder_percent, names_to = "substrate", values_to = "per")%>%
  group_by(siteId,date)%>%
  filter(per>0)%>%
  summarise(n_sub=length(unique(substrate)))
##import PTs####
LTM_tal_fix_padded <- read_csv("Data/LTM_tal_fix_padded.csv")
#What is the min,max and mean temp for preceeding time period
#need to create start and end date for each sampling event
apr1_dates<-data.frame(apr1_canopy$date)%>%
  rename(end_date=apr1_canopy.date)%>%
  mutate(start_date=lag(end_date))%>%
  relocate(start_date, .before = end_date)
#add first preceeding date
apr1_dates[1,1]<-"2021-10-06"
#make all datetime at 00:00:00
apr1_dates<-apr1_dates%>%
  mutate(start_date=as.POSIXct(start_date), end_date=as.POSIXct(end_date))

#calculate temp
LTM_tal_temp<-apr1_dates%>%
  mutate(temp = map2(start_date, end_date, ~ {
    LTM_tal_fix_padded %>%
      filter(datetime >= .x, datetime <= .y, siteId=="TLM01", tempC>0) %>%
      summarise(
        mean_temp = mean(tempC, na.rm = TRUE),
        n=length(unique(datetime))
      )
  })) %>%
  unnest(cols = temp)

##pull waterquality for nh4, srp, doc####
NUTR_TAL <- read_excel("Data/Hydroshare data/NUTR_SE_TAL_20210915_20241004_V1.0.xlsx", 
                       sheet = "Final Data")
DOC_TAL <- read_excel("Data/Hydroshare data/DOCS_SE_TAL_20211007_20241004_V1.0.xlsx", 
                      sheet = "Final Data")

NUTR_TAL<-NUTR_TAL%>%
  mutate(across(c(SRPugL:NO3ugL), as.numeric))

#filter to just apr1
NUTR_TAL_apr1<-NUTR_TAL%>%
  filter(appr1==1)
DOC_TAL_apr1<-DOC_TAL%>%
  filter(appr1==1)

##import discharge ####
#continuous
DISC_SE_TAL <- read_csv("Data/Hydroshare data/DISC_SE_TAL_TLM01_20210824_20241015.csv")

#what is the median flow in the preceeding time between sampling points. (3 weeks for first)
#need to create start and end date for each sampling event
apr1_dates<-data.frame(apr1_canopy$date)%>%
  rename(end_date=apr1_canopy.date)%>%
  mutate(start_date=lag(end_date))%>%
  relocate(start_date, .before = end_date)
#add first preceeding date
apr1_dates[1,1]<-"2021-10-06"
#make all datetime at 00:00:00
apr1_dates<-apr1_dates%>%
  mutate(start_date=as.POSIXct(start_date), end_date=as.POSIXct(end_date))

DISC_sum <- apr1_dates %>%
  mutate(Q = map2(start_date, end_date, ~ {
    DISC_SE_TAL %>%
      filter(datetime >= .x, datetime <= .y, Q_Ls<=300) %>%
      summarise(
        median_QLs = median(Q_Ls, na.rm = TRUE),
        n=length(unique(datetime))
      )
  })) %>%
  unnest(cols = Q)

##Combine everything together####
pca_abiotic_apr1<-apr1_canopy%>%
  left_join(DISC_sum[,-1], join_by(date==end_date))%>%
  left_join(LTM_tal_temp[,-1], join_by(date==end_date))

pca_abiotic_apr1<-pca_abiotic_apr1%>%
  mutate(date=as.Date(date))%>%
  left_join(NUTR_TAL_apr1[,c(2,16,19)])%>%
  left_join(DOC_TAL_apr1[,c(13,15)], join_by(date==dateReg))
#need to grab a couple of rows of water quality that were sampled on slightly different dates
pca_abiotic_apr1[2,c(8,9)]<-NUTR_TAL_apr1[4,c(16,19)]
pca_abiotic_apr1[11,c(8,9)]<-NUTR_TAL[13,c(16,19)]
pca_abiotic_apr1[18,c(8,9)]<-NUTR_TAL_apr1[19,c(16,19)]

pca_abiotic_apr1[1,10]<-DOC_TAL_apr1[2,15]
pca_abiotic_apr1[2,10]<-DOC_TAL_apr1[43,15]
pca_abiotic_apr1[18,10]<-DOC_TAL_apr1[18,15]

#drop unneeded columns
pca_abiotic_apr1<-pca_abiotic_apr1%>%
  dplyr::select(!c(n.x,n.y))

#write pca variables you are using for nmds graph later
write.csv(pca_abiotic_apr1, "Data/apr1/pca.csv", row.names=F)
##Run PCA ####  
#run pca
hist.data.frame(pca_abiotic_apr1[,-c(1,2)])
corrplot(cor(pca_abiotic_apr1[,-c(1,2)]), method="number")

pca<-prcomp(pca_abiotic_apr1[,-c(1,2)], center=T, scale. = T)
pca
summary(pca)
plot(pca)
ggbiplot(pca)

#make better plot so can see how sites are splitting
#make dataframe so you can plot in ggplot yourself
pca_df<-data.frame(date=pca_abiotic_apr1$date,PC1=-1*pca$x[,1], PC2=pca$x[,2])
pca_vec<-data.frame(var=row.names(pca$rotation), PC1=-1*pca$rotation[,1], PC2=pca$rotation[,2])
pca_vec$var<-c("mean canopy", "median Q L/s","mean temp","SRP µg/L", "NH4-N µg/L", "DOC mg/L")
pca_df<-pca_df%>%
  left_join(pca_abiotic_apr1[,c(2,7)])

#add julian day
pca_df$julian<-as.numeric(format(pca_df$date,"%j" ))



plot_vec <- pca_df%>%
  arrange(date)%>%
  ggplot(aes(x = PC1, y= PC2)) + 
  geom_point(aes(fill=julian),size=3, shape=21) + 
  labs(x = "PC1 (44.7%)", y = "PC2 (24.3%)")+
  geom_segment(data = pca_vec,
               aes(x = 0, xend =PC1*3, y = 0, yend = PC2*3),
               arrow = arrow(length = unit(0.25, "cm")), colour = "black") +
  geom_label_repel(data = pca_vec, aes(PC1*3, PC2*3, label=var),size = 4)+
  scale_fill_carto_c(palette="Geyser", direction = -1,
                     name=bquote("Ordinal Day"))+
  theme_few(base_size = 14)+
  theme(legend.position = "bottom");plot_vec

ggplot2::ggsave("./graphs/pca_temporal.png",dpi=800, width=7, height=6)

#need to calculate euclidean distance between all points as well as the number of days between all points
#perform euclidean distance matrix
pca_apr1<-dist(pca$x, method="euclidean", diag = T)
pca_apr1<-as.matrix(pca_apr1)%>%
  as_tibble()%>%
  mutate(date.x=pca_abiotic_apr1$date)%>%
  pivot_longer(-date.x, names_to="date.y",values_to="dist")%>%
  mutate(date.y=rep(pca_abiotic_apr1$date, times=22))
#get number of days difference between each
dates<-apr1_dates %>%
  dplyr::select(end_date)%>%
  mutate(end_date=as.Date(end_date))%>%
  arrange(end_date)

date_diff <- dates %>%
  full_join(dates,
            by = character(),
            suffix = c(".x", ".y")) %>%
  mutate(diff_days = end_date.y - end_date.x) %>%
  dplyr::select(end_date.x, end_date.y, diff_days) %>%
  mutate(diff_days=as.numeric(diff_days))%>%
  rename(date.x=end_date.x, date.y=end_date.y)

##calculate alpha difference match to variables####
apr1_read_join_taxa_TAL <- read_csv("Data/Final_metabarcoding/apr1_read_join_taxa_TAL.csv")
apr1_read_join_taxa_TAL$date<-as_date(as.character(apr1_read_join_taxa_TAL$date), format="%Y%m%d")
#transpose for NMDS to site by low.name matrix with presence
apr1_nmds_data<-apr1_read_join_taxa_TAL%>%
  pivot_wider(id_cols = date, names_from = Lowest_ID, values_from = tot_read, 
              values_fn=sum, values_fill = 0)%>%
  mutate_if(is.numeric, ~1 * (. != 0))
#Calculate alpha diversity of each site
apr1_richness_diff<-apr1_nmds_data%>%
  group_by(date)%>%
  rowwise()%>%
  summarise(rich=sum(c_across(Elmidae:`Pycnopsyche luculenta`)))%>%
  ungroup()
#check temporal autocorrelation
acf(apr1_richness_diff$rich, type="correlation")
pacf(apr1_richness_diff$rich)

apr1_richness_diff<-apr1_richness_diff%>%
  full_join(apr1_richness_diff,
            by = character(),
            suffix = c(".x", ".y")) %>%
  mutate(diff_rich = rich.y - rich.x) %>%
  dplyr::select(date.x, date.y, diff_rich) 

##calculate distance matrix for all dates####
apr1_dist<-vegdist(apr1_nmds_data[,-1], method="jaccard")

apr1_dist<-apr1_dist%>%
  as.matrix()%>%
  as_tibble()%>%
  mutate(date.x=apr1_nmds_data$date)%>%
  pivot_longer(-date.x, names_to="date.y",values_to="jac")%>%
  mutate(date.y=rep(apr1_nmds_data$date, times=22))

#join with variables
apr1_data_long<-pca_apr1%>%
  left_join(date_diff)%>%
  left_join(apr1_dist)%>%
  left_join(apr1_richness_diff)

apr1_data_long<-apr1_data_long%>%
  mutate(abs_dif_rich=abs(diff_rich))
#extract matrix for analysis
write.csv(apr1_data_long, "Data/apr1/apr1_data_long.csv", row.names = F)

#make date pc and rich data frame
#Calculate alpha diversity of each site
apr1_richness<-apr1_nmds_data%>%
  group_by(date)%>%
  rowwise()%>%
  summarise(rich=sum(c_across(Elmidae:`Pycnopsyche luculenta`)))%>%
  ungroup()

apr1_data<-apr1_richness%>%
  left_join(pca_df)

#extract matrix for analysis
write.csv(apr1_data, "Data/apr1/apr1_data.csv", row.names = F)
