library(pacman)
p_load(tidyverse,performance,vegan,MuMIn, nlme,ggeffects, car, ggthemes, patchwork, ggpmisc, strucchange)

#Approach 3 synoptic patterns####
##import data for each site built in data_gen code####
apr3_data <- read_csv("Data/apr3/apr3_data.csv")

#make area km2
apr3_data$drainage_area_km<-apr3_data$drainage_area_m/1e6
##Build alpha diversity model####
fit_alpha<-lm(rich~PC1+log(drainage_area_km), data=apr3_data, na.action='na.fail')
model_selec_fit_alpha<-dredge(fit_alpha, extra = list(
  "R^2", "*" = function(x) {
    s <- summary(x)
    c(Rsq = s$r.squared, adjRsq = s$adj.r.squared,
      F = s$fstatistic[[1]])
  })
);model_selec_fit_alpha
plot(fit_alpha)
check_collinearity(fit_alpha)

Anova(fit_alpha)

##run mantel test####
#load distance data
dmat_tal_log <- read_csv("Data/apr3/dmat_tal_log.csv")%>%as.dist()
#load environmental data
pca_apr3 <- read_csv("Data/apr3/pca_apr3.csv")%>%as.dist()
#load dissimilarity data
apr3_dist <- read_csv("Data/apr3/apr3_dist.csv")%>%as.dist()

#perform mantel test
apr3_dist_log<-mantel(apr3_dist, dmat_tal_log, method="pearson")
apr3_dist_log#p=0.001, stat=0.286

apr3_dist_pc<-mantel(apr3_dist, pca_apr3, method="pearson")
apr3_dist_pc# p=0.071, r=0.1813

#Approach 2 patterns####
##import variables-orginally created in spatial_env####
apr2_data <- read_csv("Data/apr2/apr2_data.csv")

##Run models####
###Mar####
####Build alpha diversity model####
data_mar<-apr2_data%>%
  filter(month(date)==3)
#alpha
fit_alpha_mar<-lm(rich~PC1+log(drainage_area_km),data=data_mar,na.action='na.fail')
summary(fit_alpha_mar)
dredge(fit_alpha_mar, extra = list(
  "R^2", "*" = function(x) {
    s <- summary(x)
    c(Rsq = s$r.squared, adjRsq = s$adj.r.squared,
      F = s$fstatistic[[1]])
  })
)
plot(fit_alpha_mar)
check_collinearity(fit_alpha_mar)

fit_alpha_mar_sel<-lm(rich~PC1,data=data_mar,na.action='na.fail')
Anova(fit_alpha_mar_sel)

fit_alpha_mar_disp<-lm(rich~log(drainage_area_km),data=data_mar,na.action='na.fail')
Anova(fit_alpha_mar_disp)

###jun####
####Build alpha diversity model####
data_jun<-apr2_data%>%
  filter(month(date)==6)
#alpha
fit_alpha_jun<-lm(rich~PC1+log(drainage_area_km),data=data_jun,na.action='na.fail')
summary(fit_alpha_jun)
dredge(fit_alpha_jun, extra = list(
  "R^2", "*" = function(x) {
    s <- summary(x)
    c(Rsq = s$r.squared, adjRsq = s$adj.r.squared,
      F = s$fstatistic[[1]])
  })
)
plot(fit_alpha_jun)
check_collinearity(fit_alpha_jun)

fit_alpha_jun_sel<-lm(rich~PC1,data=data_jun,na.action='na.fail')
Anova(fit_alpha_jun_sel)

fit_alpha_jun_disp<-lm(rich~log(drainage_area_km),data=data_jun,na.action='na.fail')
Anova(fit_alpha_jun_disp)

###aug####
####Build alpha diversity model####
data_aug<-apr2_data%>%
  filter(month(date)==8)
#alpha
fit_alpha_aug<-lm(rich~PC1+log(drainage_area_km),data=data_aug,na.action='na.fail')
summary(fit_alpha_aug)
dredge(fit_alpha_aug, extra = list(
  "R^2", "*" = function(x) {
    s <- summary(x)
    c(Rsq = s$r.squared, adjRsq = s$adj.r.squared,
      F = s$fstatistic[[1]])
  })
)
plot(fit_alpha_aug)
check_collinearity(fit_alpha_aug)

fit_alpha_aug_sel<-lm(rich~PC1,data=data_aug,na.action='na.fail')
Anova(fit_alpha_aug_sel)

fit_alpha_aug_disp<-lm(rich~log(drainage_area_km),data=data_aug,na.action='na.fail')
Anova(fit_alpha_aug_disp)


###jan####
####Build alpha diversity model####
data_jan<-apr2_data%>%
  filter(month(date)==1)
#alpha
fit_alpha_jan<-lm(rich~PC1+log(drainage_area_km),data=data_jan,na.action='na.fail')
summary(fit_alpha_jan)
dredge(fit_alpha_jan, extra = list(
  "R^2", "*" = function(x) {
    s <- summary(x)
    c(Rsq = s$r.squared, adjRsq = s$adj.r.squared,
      F = s$fstatistic[[1]])
  })
)
plot(fit_alpha_jan)
check_collinearity(fit_alpha_jan)

fit_alpha_jan_sel<-lm(rich~PC1,data=data_jan,na.action='na.fail')
Anova(fit_alpha_jan_sel)

fit_alpha_jan_disp<-lm(rich~log(drainage_area_km),data=data_jan,na.action='na.fail')
Anova(fit_alpha_jan_disp)

##Run Mantel Test for each matrix in each month####
dmat_tal_log <- read_csv("Data/apr2/dmat_tal_log.csv")
apr2_read_join_taxa_all <- read_csv("Data/apr2/apr2_read_join_taxa_all.csv")
pca_apr2_mar <- read_csv("Data/apr2/pca_apr2_mar.csv")%>%as.dist()
pca_apr2_jun <- read_csv("Data/apr2/pca_apr2_jun.csv")%>%as.dist()
pca_apr2_aug <- read_csv("Data/apr2/pca_apr2_aug.csv")%>%as.dist()
pca_apr2_jan <- read_csv("Data/apr2/pca_apr2_jan.csv")%>%as.dist()
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
#perform with log transformed distance
tal_macro_dist_log<-lapply(tal_macro_dist_list, function(x) mantel(x,dmat_tal_log, method="pearson"))
tal_macro_dist_log[1]#p=0.04, r=0.488 jan
tal_macro_dist_log[2]#p=0.006, r=0.455 mar
tal_macro_dist_log[3]#p=0.007, r=0.407 jun
tal_macro_dist_log[4]#p=0.023, r=0.483 aug

##Run Mantel on environmental####
#load environmental info
tal_pca_list<-list(pca_apr2_jan, pca_apr2_mar, pca_apr2_jun, pca_apr2_aug)

#run mantel
#perform with log transformed distance
tal_macro_pca<-map2(tal_macro_dist_list,tal_pca_list,~ mantel(.x,.y, method="pearson"))
tal_macro_pca[1]#p=0.114, r=0.277 jan
tal_macro_pca[2]#p=0.049, r=0.474 mar
tal_macro_pca[3]#p=0.799, r=-0.227 jun
tal_macro_pca[4]#p=0.013, r=530 aug

##Temporal Analysis####
apr2_fall<-apr2_data%>%
  filter(month(date) %in% c(1,3))%>%
  mutate(type=case_when(month(date)==3~"Before",
                        .default="After"))%>%
  mutate(Sampling="Spatiotemporal")

apr2_ba<-lm(rich~type, data=apr2_fall)
plot(apr2_ba)  
anova(apr2_ba)#(f=1.5304, p=0.2397)
#Approach 1 ####
#import data
apr1_data_long <- read_csv("Data/apr1/apr1_data_long.csv")

##perform partial mantel test controlling for time to examine envirnomental trends####
#first need all matrices as distance matrices
env<-apr1_data_long%>%
  dplyr::select(date.x, date.y, dist)%>%
  pivot_wider(names_from=date.x, values_from = dist)%>%
  column_to_rownames("date.y")%>%
  as.dist()

days<-apr1_data_long%>%
  dplyr::select(date.x, date.y, diff_days)%>%
  pivot_wider(names_from=date.x, values_from = diff_days)%>%
  column_to_rownames("date.y")%>%
  as.dist()

jac<-apr1_data_long%>%
  dplyr::select(date.x, date.y, jac)%>%
  pivot_wider(names_from=date.x, values_from = jac)%>%
  column_to_rownames("date.y")%>%
  as.dist()

mantel.partial(jac, env, days, method="pearson", permutations=999)##r=0.212, p=0.016

#compare samples collected from Oct-Jan in each year
apr1_data <- read_csv("Data/apr1/apr1_data.csv")

##similar time comparison####
apr1_fall<-apr1_data%>%
  filter(month(date) %in% c(10,11,12,1))%>%
  filter(date!=as.Date("2022-10-07"))%>%
  mutate(type=case_when(between(date, as.Date("2021-10-01"), as.Date("2022-01-30"))~"before",
                        .default = "after"))

apr1_ba<-lm(rich~type, data=apr1_fall)
plot(apr1_ba)  
anova(apr1_ba)#(f=17.361, p=0.003)
##look at trends over time####
ols.trend<-lm(rich~date, data=apr1_data)
summary(ols.trend)
plot(ols.trend)

gls.trend<-gls(rich~date, correlation=corAR1(form=~date), data=apr1_data)
summary(gls.trend)#intercept=512.3577, slope=-0.0260
plot(gls.trend)

##look at breakpoints####
break_pt=breakpoints(rich~1, data=apr1_data)
break_pt<- breakpoints(rich ~ 1, data=apr1_data, breaks = which.min(BIC(break_pt)))
bp1<-breakpoints(rich~1, breaks=1, data=apr1_data)
#choose 1 breakpoints based on BIC
mbp1<-lm(rich~breakfactor(break_pt), data=apr1_data)
summary(mbp1)
as.numeric(break_pt$breakpoints)#breaks between 11 and 12
#data for plotting
bp_data_ap1<-tibble(date=apr1_data$date, rich=apr1_data$rich, fitted=fitted(mbp1), segment=breakfactor(bp1))

#write to csv
write.csv(bp_data_ap1, "Data/apr1/bp_data_apr1.csv")
