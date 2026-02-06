library(tidyverse)
library(ggthemes)
library(rcartocolor)

#precip comparison
noaa_precip <- read_csv("Data/data (1).csv", skip = 2)
glimpse(noaa_precip)
noaa_precip<-noaa_precip%>%
  mutate(Date=paste(Date, "01"))%>%
  mutate(Date=as.Date(Date, format="%Y%m%d"))%>%
  filter(between(Date, as.Date("2020-10-01"), as.Date("2023-09-30")))



#what are the total for each water year
noaa_precip<-noaa_precip%>%
  mutate(wy=case_when(between(Date, as.Date("2020-10-01"),as.Date("2021-09-30"))~"2020",
                      between(Date, as.Date("2021-10-01"),as.Date("2022-09-30"))~"2021",
                      between(Date, as.Date("2022-10-01"),as.Date("2023-09-30"))~"2022"),
         season=case_when(between(month(Date), 10,12)~"Fall",
                          between(month(Date), 1,3)~"Winter",
                          between(month(Date), 4,6)~"Spring",
                          between(month(Date), 7,9)~"Summer"))

noaa_precip<-noaa_precip%>%
  mutate(season=factor(season, ordered = T, levels=c("Fall","Winter","Spring","Summer")))%>%
  mutate(value_mm=Value*25.4)
#what are the rainfall totals for each wateryear
noaa_precip%>%
  group_by(wy)%>%
  reframe(total=sum(value_mm))

noaa_precip_sum<-noaa_precip%>%
  group_by(wy, season)%>%
  reframe(total=sum(value_mm))

ggplot(noaa_precip_sum, aes(season, total, fill=wy))+
  geom_bar(stat="identity", position="dodge", color="grey20")+
  scale_fill_carto_d(palette="Earth", direction=-1)+
  scale_y_continuous(expand = c(0,0))+
  labs(x="", y="Precipitation (mm)", fill="Water Year")+
  theme_few(base_size=8)

ggsave("graphs/precip_bar.png", dpi = 600, width=3.5, height=3)
