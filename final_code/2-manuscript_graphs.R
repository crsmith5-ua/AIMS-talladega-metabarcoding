library(tidyverse)
library(readxl)
library(ggthemes)
library(rcartocolor)
library(iNEXT)
library(ggvenn)
library(sf)
library(terra)
library(tidyterra)
library(scatterpie)
library(ggspatial)
library(scico)
library(USA.state.boundaries)
library(patchwork)
library(magick)
library(ggpubr)
library(ggpmisc)
library(ggtext)
#Figure 1####
##Discharge####
#continuous
DISC_SE_TAL <- read_csv("Data/Hydroshare data/DISC_SE_TAL_TLM01_20210824_20241015.csv")


#pull MAME data for list of dates and approaches
MAME_SE_TAL <- read_excel("Data/Hydroshare data/MAME_SE_TAL_20211027_20230131_V1.0.xlsx", 
                          sheet = "Final Data")
dates<-MAME_SE_TAL%>%
  filter(siteId=="TLM01")%>%
  dplyr::select(date, appr1, appr2, appr3)%>%
  distinct()

#make synoptic also seasonal
dates[11,3]<-1

#calculate daily median from DISC
DISC_daily<-DISC_SE_TAL%>%
  mutate(date=date(datetime))%>%
  group_by(date)%>%
  reframe(Q_Ls=median(Q_Ls, na.rm=T))
#convert to datetime
dates$date<-as.Date(as.character(dates$date), format="%Y%m%d")

#add value for plotting
dates<-dates%>%
  left_join(DISC_daily)

#fill approximate missing data for point to match from nearby day DISC_daily
dates[14,5] <- 13.858936
dates[21,5] <- 16.04713
#add sampling column
dates<-dates%>%
  mutate(Sampling=case_when(appr1==1 & appr2==0 & appr3==0 ~ "Temporal",
                            appr1==1 & appr2==1 & appr3==0 ~ "Spatiotemporal",
                            appr1==1 & appr2==1 & appr3==1 ~ "Spatial"))

seasonal_dates<-dates%>%
  filter(appr2==1)
#graph
dis<-DISC_SE_TAL[-26896,]%>%
  filter(datetime<as.POSIXct("2023-03-01"), Q_Ls<300)%>%
  ggplot( aes(datetime, Q_Ls))+
  geom_point(size=.25)+
  geom_vline(xintercept = seasonal_dates$date, color="#6599CD", linewidth=1)+
  geom_vline(xintercept = as.POSIXct("2022-06-09"), color="#882155", linewidth=1, linetype="dashed")+
  geom_point(data=dates, aes(x=date, y=Q_Ls), fill="#999933", size=2, shape=21)+
  scale_y_log10(breaks=c(1,10,100,200,300), )+
  scale_x_datetime(date_labels = "%b-%y", date_breaks = "2 months")+
  labs(x="",y="Discharge (L/s)")+
  theme_classic(base_size = 10)+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5));dis

ggplot2::ggsave("./graphs/discharge.png",dpi=600, width=3.5, height=2)

#blank plot to get legend
legend<-ggplot(dates, aes(date,Q_Ls, fill=Sampling)) + 
  geom_point( shape=21)+
  scale_fill_manual(values=c("#882155","#6599CD","#999933"), name="Sampling\napproach")+
  theme_few(base_size=9)+
  theme(legend.position = "right",
        legend.key.spacing.x = unit(0.01, "cm"),
        legend.text = element_text(margin = margin(r = 0.01, unit = "cm")));legend

ggplot2::ggsave("./graphs/legend.png",dpi=600, width=3.5, height=2)
##Map####
#import other map layers
#import stic_points
stic_points <- read_excel("Data/Map_layers/ENVI_SE_TAL.xlsx",sheet = "Final Data")
apr3_read_join_taxa <- read_csv("Data/Final_metabarcoding/apr3_read_join_taxa.csv")
#subset to points where bugs were sampled
stic_points_bug<-stic_points%>%
  semi_join(apr3_read_join_taxa, by=join_by("siteId"=="site"))%>%
  arrange(siteId)
#add apporach column
stic_points_bug<-stic_points_bug%>%
  mutate(Spatial=case_when(siteId=="TLM01"~ 0.33,
                           siteId %in% c("TLM06","TLM10","TLM16","TLM19","TLC01","TLA01")~ 0.5,
                           .default = 100),
         Spatiotemporal=case_when(siteId=="TLM01"~ 0.33,
                                  siteId %in% c("TLM06","TLM10","TLM16","TLM19","TLC01","TLA01")~ 0.5,
                                  .default = 0),
         Temporal= case_when(siteId=="TLM01"~0.33,
                             .default=0))

hill_tal<-rast("Data/Map_layers/hill_tal.tif")
hill_tal_wgs<-project(hill_tal, "+proj=longlat +datum=WGS84")

flow_net_tal<-st_read("Data/Map_layers/streams_tal.shp")%>%
  st_set_crs("+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs +type=crs")
flow_net_tal_wgs<-st_transform(flow_net_tal, "+proj=longlat +datum=WGS84")

watershed<-st_read("Data/Map_layers/TAL_watershed.shp")
watershed_wgs<-st_transform(watershed, "+proj=longlat +datum=WGS84")

flow_net_tal<-st_intersection(flow_net_tal, watershed)
flow_net_tal_wgs<-st_transform(flow_net_tal,"+proj=longlat +datum=WGS84")
#plot
base<-ggplot() + 
  geom_spatraster(data=hill_tal_wgs, aes(fill=hillshade))+
  scale_fill_distiller(type = "seq",direction = -1,palette = "Greys", guide = "none",
                       na.value = NA)+
  geom_sf(data = watershed_wgs,fill="transparent", lwd = .5) +
  geom_sf(data = flow_net_tal_wgs, col = "dodgerblue4", linewidth=.4, alpha=.8)+ 
  annotation_north_arrow(location="bl",pad_x = unit(.2, "in"), pad_y = unit(.4, "in"), 
                         width=unit(.3,"in"),height=unit(.25,"in"))+
  annotation_scale(location="bl",pad_x = unit(0.2, "in"), pad_y = unit(.1, "in"), text_cex = 0.5)+
  theme_map(base_size = 6);base

ggplot2::ggsave("./graphs/base.pdf",dpi=600, width=3.5, height=3)

#separate into two so can color differently
stic_points_bug1_2<-stic_points_bug%>%
  filter(appr1==1 | appr2==1)

stic_points_bug3<-stic_points_bug%>%
  filter(appr3==1 & appr1==0 & appr2==0)

points_map<-ggplot() + 
  geom_sf(data = watershed_wgs,fill="transparent", lwd = .5) +
  geom_scatterpie(data=stic_points_bug3, aes(x=long, y=lat),bg_circle_radius=1,pie_scale = 1.8, 
                  cols=colnames(stic_points_bug[,c(21:23)]), linewidth=0.4, color="transparent")+
  geom_scatterpie(data=stic_points_bug1_2, aes(x=long, y=lat),bg_circle_radius=1,pie_scale = 1.8, 
                  cols=colnames(stic_points_bug[,c(21:23)]), linewidth=0.4, color="black")+
  scale_fill_manual(values=c("#882155", "#6599CD", "#999933"))+
  theme_map(base_size = 6)+
  theme(legend.position = "none");points_map

ggplot2::ggsave("./graphs/points.png",dpi=400, width=3.5, height=3)


#alabama inset
al_outline<-state_boundaries_wgs84%>%
  filter(STATE_ABBR=="AL")%>%
  st_simplify()
alabama<-ggplot() + 
  geom_sf(data=al_outline,col ="black", fill = NA, lwd = 1) +
  geom_sf(data = watershed_wgs, col ="black", fill=NA, lwd = 3) +
  annotation_scale(location = "br",text_cex = 0.8)+
  theme_map()+
  theme(legend.position = "none");alabama

ggplot2::ggsave("./graphs/alabama.png",dpi=600, width=2, height=2.1)
ggplot2::ggsave("./graphs_pres/alabama.pdf",dpi=600, width=5, height=6)
##all togther- add base and points together in acrobat and alabama inset with legend then bring back in####
#import base map
map<-image_read("./graphs/combined_map.png")
map_g<-image_ggplot(map)+theme_void(base_size = 9);map_g

#plot together
(map_g+ labs(tag = "a)"))/ (dis + labs(tag = "b)"))+plot_layout(heights=c(3,2))

ggplot2::ggsave("./graphs/figure1.png",dpi=800, width=3.5, height=4.7)
#Figure 2####
##Rarefaction curves####
#import data
apr1_inext <- read_csv("Data/graph_data/apr1_inext.csv")%>%as.data.frame()
apr2_inext <- read_csv("Data/graph_data/apr2_inext.csv")%>%as.data.frame()
apr3_inext <- read_csv("Data/graph_data/apr3_inext.csv")%>%as.data.frame()
#combine into single data set
inext_data<-list(Spatial=apr3_inext, Temporal=apr1_inext, Spatiotemporal=apr2_inext)

iraw<-iNEXT(inext_data, q=0, datatype = "incidence_raw")
iraw

rare<-ggiNEXT(iraw)+
  scale_x_continuous(breaks=seq(0,60,10))+
  labs(y="Taxa Richness")+
  geom_vline(xintercept = 40, color="black")+
  scale_color_manual(values=c("#882155", "#6599CD", "#999933"))+
  scale_fill_manual(values=c("grey50", "grey50", "grey50"))+
  labs(y=bquote(~gamma~"-diversity"))+
  theme_few(base_size = 10)+
  theme(legend.position = "none");rare

est<-iraw$iNextEst$size_based%>%filter(t==40)

##ven diagram of each approach####
#need a list of lowest id from each approach with lowest id 
apr1_taxa_list_TAL <- read_csv("Data/Ancillary_cleaning_data/apr1_taxa_list_TAL.csv")
apr3_taxa_list <- read_csv("Data/Ancillary_cleaning_data/apr3_taxa_list.csv")
#for apr2 need to add in LTMs from apr3
apr2_read_join_taxa_TAL <- read_csv("Data/Final_metabarcoding/apr2_read_join_taxa_TAL.csv")
apr3_read_join_taxa <- read_csv("Data/Final_metabarcoding/apr3_read_join_taxa.csv")
#add in ltm sites from apr3
apr2_read_join_taxa_all<-apr3_read_join_taxa%>%
  filter(site %in% c("TLM01", "TLC01", "TLM06", "TLM10", "TLM16", "TLM19", "TLA01"))%>%
  bind_rows(apr2_read_join_taxa_TAL)
  
#make taxa list with lowest id
#Taxa remaining after terrestrial and non-invertebrate taxa removed
apr2_taxa_list_all<-unique(apr2_read_join_taxa_all[c("phylum","class","order","family","genus","species", "Lowest_ID")])
#write csv post cleaning
write.csv(apr2_taxa_list_all, "Data/Ancillary_cleaning_data/apr2_taxa_list_TAL_all.csv", na= "", row.names = F)

#make list with all three
venn_approach <- list(
  Spatial = apr3_taxa_list$Lowest_ID, 
  Spatiotemporal = apr2_taxa_list_all$Lowest_ID, 
  Temporal = apr1_taxa_list_TAL$Lowest_ID)

#make venn diagram
venn<-ggvenn(
  venn_approach, 
  fill_color = c("#882155", "#6599CD", "#999933"),
  stroke_size = 0.5, set_name_size = 3,text_size=3.5,
  show_elements = F, show_percentage = F, padding=0.05)+
  theme_map(base_size = 10);venn


##make graph of richness across approaches####
apr_richness <- read_csv("Data/apr_richness.csv")

rich_g<-ggplot(apr_richness, aes(Sampling, rich))+
  geom_violin(aes(fill=Sampling), trim=F, alpha=0.8)+
  geom_jitter(shape=16, position=position_jitter(0.05), alpha=.5)+
  stat_summary(fun=median, geom="point", size=5, shape=23, fill="black")+
  scale_y_continuous(expand = c(0,0), breaks=seq(0,45,10), limits=c(0,45))+
  scale_fill_manual(values=c("#882155", "#6599CD", "#999933"), name="Sampling approach")+
  labs(x="", y=bquote(~alpha~"-diversity"))+
  theme_few(base_size=10)+
  theme(legend.position = "bottom")+
  guides(fill=guide_legend(title.position="top", title.hjust=0.5));rich_g

#import full beta dataset
bd_as_comp_all <- read_csv("Data/graph_data/bd_as_comp_all.csv")
##make dataset with beta values as markers####
bd_all_g<-ggplot(bd_as_comp_all, aes(Repl, RichDif))+
  geom_abline (slope=1, linetype = "dashed", color="black", linewidth=0.5)+
  geom_abline(slope=-1, intercept=0.35, linetype="dotted", color="grey20")+
  geom_abline(slope=-1, intercept=0.5,linetype="dotted", color="grey20")+
  geom_point(data=bd_as_comp_all%>%filter(Sampling=="Spatiotemporal"),aes(fill=Beta_comb, shape = Beta_comb), 
             size=2.5, stroke=1, alpha=0.95)+
  geom_point(data=bd_as_comp_all%>%filter(Sampling%in%c("Temporal","Spatial")),aes(fill=Beta_comb, shape = Beta_comb), 
             size=2.5, stroke=1, alpha=0.95)+
  scale_shape_manual(values=c(21,22,24,23), name=bquote(~beta~"-diversity Sampling approach"), 
                     label=c("Spatial","Spatiotemporal-Space","Spatiotemporal-Time","Temporal"))+
  scale_x_continuous(breaks=seq(0,.45,.1), limits=c(0,.5))+
  scale_y_continuous(breaks=seq(0,.45,.1),limits=c(0,.4))+
  scale_fill_manual(values=c("#882155","#6599CD", "#97CAEB","#999933"), name=bquote(~beta~"-diversity Sampling approach"), 
                    label=c("Spatial","Spatiotemporal-Space","Spatiotemporal-Time","Temporal"))+
  labs(x="Replacement", y="Richness Difference")+
  theme_few(base_size = 10)+
  theme(legend.position = "bottom")+
  guides(fill=guide_legend(title.position="top", title.hjust=0.5, nrow=2),
         shape=guide_legend(title.position="top", title.hjust=0.5, nrow=2));bd_all_g


##all together####
rare+venn+rich_g+bd_all_g+plot_annotation(tag_levels="a",tag_suffix = ")")+plot_layout(widths = c(1,1))
ggplot2::ggsave("./graphs/figure2.png",dpi=600, width=7, height=6)

#Figure 3####
apr2_data <- read_csv("Data/apr2/apr2_data.csv")
apr3_data <- read_csv("Data/apr3/apr3_data.csv")

#add sampling type column
apr2_data<-apr2_data%>%
  mutate(sampling="Spatiotemporal", month=month(date, label=T))

apr3_data<-apr3_data%>%
  mutate(sampling="Spatial", month="Jun")
#combine
richness_data<-apr3_data%>%
  bind_rows(apr2_data)%>%
  dplyr::select(site, month, PC1, drainage_area_m, rich, sampling)%>%
  mutate(color=paste(sampling,month, sep="-"), drainage_area_km=drainage_area_m/1e6)%>%
  mutate(color=factor(color, ordered = T, levels = c("Spatial-Jun","Spatiotemporal-Mar",
                                                     "Spatiotemporal-Jun","Spatiotemporal-Aug",
                                                     "Spatiotemporal-Jan")))
richness_data$`Sampling approach`<-richness_data$color
##richness vs area####
area_rich<-ggplot(richness_data, aes(x=drainage_area_km, y=rich, color=`Sampling approach`))+
  geom_point( size=1.5, alpha=0.2)+
  geom_smooth(method="lm", se=F, aes(linetype = color), linewidth=1.2)+
  scale_linetype_manual(values=c("solid","dashed","dashed","solid","dashed"))+
  scale_color_manual(values=c("#882155","#312383","#37753B","#3070AD","#6FB2E4"))+
  stat_poly_eq(use_label("R2"), size=4, fontface="bold", small.r = T, vstep = 0.1)+
  scale_x_continuous(trans = "log", breaks=c(0,0.05,0.1,.25,.5,1))+
  scale_y_continuous(limits=c(0,35), breaks=seq(0,35,10))+
  labs(x=bquote("Drainage Area"~(km^2)),y=bquote(~alpha~"-diversity"))+
  theme_few(base_size = 10)+
  guides(linetype="none");area_rich

##richness vs pc####
pca_rich<-ggplot(richness_data, aes(x=PC1, y=rich, color=`Sampling approach`))+
  geom_point( size=1.5, alpha=0.2)+
  geom_smooth(method="lm", se=F, aes(linetype = color), linewidth=1.2)+
  scale_linetype_manual(values=c("dashed","dashed","dashed","dashed","dashed"))+
  scale_color_manual(values=c("#882155","#312383","#37753B","#3070AD","#6FB2E4"))+
  stat_poly_eq(use_label("R2"), size=4, fontface="bold", small.r = T,vstep = 0.1)+
  scale_y_continuous(limits=c(0,35), breaks=seq(0,35,10))+
  scale_x_continuous(limits=c(-4.5,4.5), breaks=seq(-4,4,1))+
  labs(x="PC1",y=bquote(~alpha~"-diversity"))+
  theme_few(base_size = 10)+
  guides(linetype="none");pca_rich

##diss vs riv dist####
apr3_data_long <- read_csv("Data/apr3/apr3_data_long.csv")
apr2_data_long <- read_csv("Data/apr2/apr2_data_long.csv")

apr2_data_long<-apr2_data_long%>%
  mutate(sampling="Spatiotemporal")

apr3_data_long<-apr3_data_long%>%
  mutate(sampling="Spatial", month="Jun")

richness_data_long<-apr2_data_long%>%
  bind_rows(apr3_data_long)%>%
  mutate(`Sampling approach`=paste(sampling, month, sep="-"))%>%
  mutate(`Sampling approach`=factor(`Sampling approach`, ordered = T, levels = c("Spatial-Jun","Spatiotemporal-Mar",
                                                     "Spatiotemporal-Jun","Spatiotemporal-Aug",
                                                     "Spatiotemporal-Jan")))

##dissimilarity vs river distance####
riv_dist_diss<-richness_data_long%>%
  filter(site.y>site.x)%>%
  ggplot(aes(x=riv_dist, y=jac, color=`Sampling approach`))+
  geom_point(size=1.5, alpha=0.2)+
  geom_smooth(method="lm", se=F, linewidth=1.2)+
  scale_color_manual(values=c("#882155","#312383","#37753B","#3070AD","#6FB2E4"))+
  annotate("richtext",label="<i>r</i> = 0.29", x=1400, y=0.65,hjust=0, 
           color="#882155", size=4, fill=NA, label.color=NA)+
  annotate("richtext",label="<i>r</i> = 0.46", x=1400, y=0.6,hjust=0, 
           color="#312383", size=4,fill=NA, label.color=NA)+
  annotate("richtext",label="<i>r</i> = 0.34", x=1400, y=0.55,hjust=0, 
           color="#37753B", size=4,fill=NA, label.color=NA)+
  annotate("richtext",label="<i>r</i> = 0.48", x=1400, y=0.5,hjust=0, 
           color="#3070AD", size=4,fill=NA, label.color=NA)+
  annotate("richtext",label="<i>r</i> = 0.49", x=1400, y=0.45,hjust=0, 
           color="#6FB2E4", size=4, fill=NA, label.color=NA)+
  scale_x_continuous(trans="log", breaks=c(0,100,250,500,1000,1500,2500))+
  scale_y_continuous(limits=c(0.40,1.025), breaks=seq(0.5,1.0,0.1))+
  labs(x="River Distance (m)",y=bquote(~beta~"-diversity"))+
  theme_few(base_size = 10)+
  guides(linetype="none");riv_dist_diss

env_dist_diss<-richness_data_long%>%
  filter(site.y>site.x)%>%
  ggplot(aes(x=env_dist, y=jac, color=`Sampling approach`))+
  geom_point(size=1.5, alpha=0.2)+
  geom_smooth(method="lm", se=F, linewidth=1.2, aes(linetype=`Sampling approach`))+
  scale_linetype_manual(values=c("dashed","solid","dashed","solid","dashed"))+
  scale_color_manual(values=c("#882155","#312383","#37753B","#3070AD","#6FB2E4"))+
  annotate("richtext",label="<i>r</i> = 0.18", x=5.5, y=0.65,hjust=0, 
           color="#882155", size=4,fill=NA, label.color=NA)+
  annotate("richtext",label="<i>r</i> = 0.47", x=5.5, y=0.6,hjust=0, 
           color="#312383", size=4,fill=NA, label.color=NA)+
  annotate("richtext",label="<i>r</i> =-0.23", x=5.5, y=0.55,hjust=0, 
           color="#37753B", size=4,fill=NA, label.color=NA)+
  annotate("richtext",label="<i>r</i> = 0.53", x=5.5, y=0.5,hjust=0, 
           color="#3070AD", size=4,fill=NA, label.color=NA)+
  annotate("richtext",label="<i>r</i> = 0.28", x=5.5, y=0.45,hjust=0, 
           color="#6FB2E4", size=4,fill=NA, label.color=NA)+
  scale_y_continuous(limits=c(0.40,1.0), breaks=seq(0.5,1.0,0.1))+
  labs(x="Environmental Distance",y=bquote(~beta~"-diversity"))+
  theme_few(base_size = 10)+
  guides(linetype="none");env_dist_diss

##plot together####
col_label1<-wrap_elements(panel = text_grob("Dispersal", size = 10))
col_label2<-wrap_elements(panel = text_grob("Selection", size=10))
col_label1+col_label2+(area_rich+ labs(tag="a)"))+(pca_rich+labs(tags="b)"))+
  (riv_dist_diss+labs(tag="c)"))+(env_dist_diss+labs(tags="d)"))+
  plot_layout(guides='collect', axes='collect',ncol=2, heights = c(0.2,1,1))&
  theme(legend.position = "bottom")&
  guides(color=guide_legend(title.position="top", title.hjust=0.5, nrow=2))

ggplot2::ggsave("./graphs/figure3.png",dpi=800, width=7, height=6.5)

#Figure 4####
apr1_data <- read_csv("Data/apr1/apr1_data.csv")
bp_data_apr1 <- read_csv("Data/apr1/bp_data_apr1.csv")
apr1_data_long <- read_csv("Data/apr1/apr1_data_long.csv")
apr2_data <- read_csv("Data/apr2/apr2_data.csv")
#figure out how to plot residuals for each
env_resid<-resid(lm(dist~diff_days, data=(apr1_data_long%>%filter(date.y>date.x))))

jac_resid<-resid(lm(jac~diff_days, data=(apr1_data_long%>%filter(date.y>date.x))))

resid_jac<-jac_resid%>%
  bind_cols(env_resid)%>%
  rename(jac_r=...1, env_r=...2)
##what does richness look like over time####
time_rich<-ggplot(bp_data_apr1, aes(date,rich))+
  geom_line(color="#999933", size=1)+
  geom_line(aes(y=fitted), linewidth=1, linetype="dashed")+
  geom_smooth(se=F, color="black", linewidth=1)+
  geom_point(size=2.4,color="#999933")+
  labs(x="",y=bquote(~alpha~"-diversity"))+
  scale_x_date(date_breaks = "2 month", date_labels = "%b-%y")+
  scale_y_continuous(limits=c(0,35), breaks=seq(0,35,5))+
  theme_few(base_size = 10)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1));time_rich

#different sites over time
time_rich_apr2<-ggplot(apr2_data, aes(date,rich))+
  geom_line(aes(color=fct_reorder(site, -drainage_area_km)),size=1.2)+
  geom_point(aes(fill=fct_reorder(site, -drainage_area_km)),size=2, shape=21, stroke=1)+
  labs(x="",y=bquote(~alpha~"-diversity"))+
  scale_color_carto_d(palette = "Earth",direction=-1, name="Site")+
  scale_fill_carto_d(palette = "Earth",direction=-1, name="Site")+
  scale_x_date(date_breaks = "2 month", date_labels = "%b-%y", limits=c(as.Date("2022-03-01"), as.Date("2023-02-01")))+
  scale_y_continuous(limits=c(0,35), breaks=seq(0,35,5))+
  theme_few(base_size = 10)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1));time_rich_apr2

##dissimilarity####
jac_temp_env<-ggplot(resid_jac, aes(env_r, jac_r))+
  geom_point(size=1.5, alpha=0.2,color="#999933")+
  geom_smooth(method='lm', color="#999933", se=F, linewidth=1)+
  annotate("richtext",label="<i>r</i> = 0.288", x=0.75, y=-0.2,hjust=0, size=4,
           fill=NA, label.color=NA)+
  labs(y=bquote("Residuals"~beta~"-diversity"), x="Environmental distance")+
  theme_few(base_size = 10);jac_temp_env



##look at similar periods####
apr1_fall<-apr1_data%>%
  filter(month(date) %in% c(10,11,12,1))%>%
  filter(date!=as.Date("2022-10-07"))%>%
  mutate(type=case_when(between(date, as.Date("2021-10-01"), as.Date("2022-01-30"))~"Before",
                        .default = "After"))%>%
  mutate(Sampling="Temporal")


apr2_fall<-apr2_data%>%
  filter(month(date) %in% c(1,3))%>%
  mutate(type=case_when(month(date)==3~"Before",
                        .default="After"))%>%
  mutate(Sampling="Spatiotemporal")

comp_data<-apr1_fall%>%
  bind_rows(apr2_fall)%>%
  mutate(type=factor(type, ordered=T, levels=c("Before","After")))

boxplot<-ggplot(comp_data, aes(type,rich))+
  geom_boxplot(aes(fill=Sampling),color="black", outlier.shape=NA)+
  geom_jitter(aes(type,rich, fill=Sampling), shape=21,
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
              alpha = 0.65, size = 2, stroke=1)+
  scale_y_continuous(breaks=seq(5,25,5))+
  scale_fill_manual(values=c("#6599CD","#999933"),name="Sampling\napproach")+
  annotate("richtext", label="*<i>p</i> = 0.003", x="After", y=24, size=4, color="#999933", 
           fontface="bold", fill=NA, label.color=NA)+
  annotate("richtext", label="<i>p</i> = 0.240", x="After", y=26, size=4, color="#6599CD", 
           fontface="bold", fill=NA, label.color=NA)+
  labs(x="Contraction event", y=bquote(~alpha~"-diversity"))+
  theme_few(base_size=10);boxplot



##plot together####
time_rich+time_rich_apr2+jac_temp_env+boxplot+plot_layout(axes = 'collect')+
  plot_annotation(tag_levels="a", tag_suffix = ")") 

ggplot2::ggsave("./graphs/figure4.png",dpi=800, width=7, height=5)

#Figure 5####
#ross comparison graph