library(tidyverse)
library(ggthemes)
library(rcartocolor)
#import epa site id by bug wide data
epa_bug_data <- read_csv("Data/EPA_invert_vander_vorste/nrsa0809bentcts_pivot.csv")
glimpse(epa_bug_data)
#import epa site info
epasiteinfo <- read_csv("Data/EPA_invert_vander_vorste/epasiteinfo.csv")

#calculate richness of each epa site
epa_richness<-epa_bug_data%>%
  mutate(across(c(ABEDUS:ZAVRELIMYIA), as.numeric))%>%
  mutate_if(is.numeric, ~1 * (. != 0))%>%
  group_by(SITE_ID)%>%
  rowwise()%>%
  summarise(rich=sum(c_across(ABEDUS:ZAVRELIMYIA), na.rm=T))%>%
  ungroup()

#join watershed area from site info-NARS in km2
epa_richness<-epa_richness%>%
  left_join(epasiteinfo[,c(3,34)])%>%
  distinct()

#quick plot to look at patterns
epa_richness%>%
  filter(WSAREA_NARS<=1)%>%
  ggplot(aes(WSAREA_NARS,rich))+
  geom_point()+
  geom_smooth(method="lm")+
  scale_x_continuous(trans="log")+
  scale_y_continuous(trans="log1p")+
  theme_few()

#what is the range of diversity
epa_richness%>%
  filter(WSAREA_NARS<=1)%>%
  reframe(min=min(rich, na.rm=T), max=max(rich, na.rm=T), mean=mean(rich, na.rm=T))

#what does the data within 10% of our look like
epa_richness%>%
  filter(WSAREA_NARS<=1)%>%
  arrange(rich)%>%
  print(n=45)
#get number of states represented
epa_richness<-epa_richness%>%
  mutate(state=str_sub(SITE_ID, 5,6))

epa_richness%>%
  filter(WSAREA_NARS<=1)%>%
  group_by(state)%>%
  reframe(num=length(unique(SITE_ID)))%>%
  print(n=35)
#pull NC data set- load ncbugs.RData from EPA_vander_vorste, comes in as FIN
#fix name issue in column 43
names(FIN)[43]<-"Acentrella_turbida_gr"
nc_richness<-FIN%>%
  mutate_if(is.numeric, ~1 * (. != 0))%>%
  group_by(site)%>%
  rowwise()%>%
  summarise(rich=sum(c_across(Cheumatopsyche_spp:Saetheria_hirta), na.rm=T))%>%
  ungroup()

nc_richness<-nc_richness%>%
  left_join(FIN[,c(1,2,7)])
#need to convert sq miles to km2
nc_richness<-nc_richness%>%
  mutate(area_km2=shed_area_sqmi*2.59)

nc_richness%>%
  filter(between(area_km2,0,1))%>%
  reframe(min=min(rich, na.rm=T), max=max(rich, na.rm=T), mean=mean(rich, na.rm=T))
  

nc_richness%>%
  filter(area_km2<=1)%>%
  arrange(rich)%>%
  print(n=45)

#combine and add richness from our sites
epa_richness<-epa_richness%>%
  rename(area_km2=WSAREA_NARS)%>%
  mutate(data="EPA")

nc_richness<-nc_richness%>%
  mutate(data="NC")

comparison<-epa_richness%>%
  bind_rows(nc_richness)

#load data from current study
apr1_data <- read_csv("Data/apr1/apr1_data.csv")
apr2_data <- read_csv("Data/apr2/apr2_data.csv")
apr3_data <- read_csv("Data/apr3/apr3_data.csv")


apr1_data<-apr1_data%>%
  mutate(site="TLM01")

#fix synoptic date in apr2
apr2_data<-apr2_data%>%
  mutate(date=case_when(date==as.Date("2022-06-21")~as.Date("2022-06-09"),
                        .default = date))

apr3_data<-apr3_data%>%
  mutate(date=as.Date("2022-06-09"))
#combine#combiRichne
my_data<-apr1_data%>%
  bind_rows(apr2_data)%>%
  bind_rows(apr3_data)

my_data<-my_data%>%
  dplyr::select(date,site,rich)%>%
  distinct()

my_data<-my_data%>%
  mutate(data="Current Study")

comparison<-comparison%>%
  mutate(date=as.Date(as.character(date), format="%d-%b-%y"))%>%
  bind_rows(my_data)

#get number for each data source
comparison%>%
  filter(data %in% c("EPA", "NC") & area_km2<1 | data=="Current Study")%>%
  group_by(data)%>%
  reframe(count=n())

comparison%>%
  filter(data %in% c("EPA", "NC") & area_km2<1 | data=="Current Study")%>%
  ggplot(aes(data, rich))+
  geom_violin(aes(fill=data), trim=F, alpha=0.7)+
  geom_jitter(shape=16, position=position_jitter(0.05), alpha=.7, size=1)+
  stat_summary(fun=median, geom="point", size=2, shape=23, fill="white", color="black", stroke=1)+
  scale_y_continuous(expand = c(0,0), breaks=seq(0,150,25), limits=c(0,150))+
  annotate("richtext", label="<i>n</i> = 66", x="Current Study", y=55, fill=NA, label.color=NA)+
  annotate("richtext", label="<i>n</i> = 40", x="EPA", y=140, fill=NA, label.color=NA)+
  annotate("richtext", label="<i>n</i> = 47", x="NC", y=140, fill=NA, label.color=NA)+
  scale_fill_manual(values=c("#312383", "#5E1909", "#37753B"), name="Data Source")+
  labs(x="", y=bquote(~alpha~"-diversity"))+
  theme_few(base_size=10)+
  theme(legend.position = "bottom")+
  guides(fill=guide_legend(title.position="top", title.hjust=0.5))

ggsave("graphs/figure5.png", dpi = 600, width=3.5, height=3.3)
