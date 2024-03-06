# load required libraries -------------------------------------------------
library(tidyverse)
library(data.table) 
library(phenoutils)
library(viridis)
library(raster) 
library(scales)
library(furrr)
library(marcoUtils)
library(sf)
library(ranger)
# read in data ------------------------------------------------------------


stack("/mnt/data1tb/Dropbox/phenology/trends/trendsclipped.tif") %>%
  as.data.frame(na.rm = TRUE) %>%
  as_tibble() %>%
  rename(cat = b2) %>%
  dplyr::select(cat)->pafilt


#fread("/mnt/data1tb/alessandro_metric/final.csv")->res
fread("/home/marco/alessandro_metric/final.csv")->res

res %>%
  group_by(cat) %>%
  summarise(count = n()) %>%
  filter(count > 12) %>%
  dplyr::select(cat) %>%
  inner_join(res)->res

#res %>%
#  group_by(cat) %>%
#  summarise(count = n()) %>%
#  filter(count > 12) %>%
#  dplyr::select(cat) %>%
#  inner_join(grid5km) %>%
#  dplyr::select(x,y,cat) %>%
#  rasterFromXYZ() %>%
#  plot()
# read in climate data
#fread("/mnt/data1tb/alessandro_metric/terraclimate_combined.csv")->clim

fread("/home/marco/alessandro_metric/terraclimate_combined.csv")->clim

res %>%
  inner_join(clim) %>%
  inner_join(read_csv("./tmp/forestcount",col_names = FALSE) %>%
               rename(cat = X1, count = X2) %>%
               mutate(forest_prop = count /max(count))) %>%
  filter(forest_prop > 0.75) %>%
  #inner_join(read_csv("tmp/gridstats025degree",col_names = FALSE) %>%
  #             fread("/media/marco/marcodata19/phenologytraining/dat.csv")->dat
  #rename(cat = X1,cat025d=X2) %>%
  inner_join(st_read("./KG/kg_codesv1.shp") %>%
               as_tibble() %>%
               dplyr::select(lvl1_tx,lvl2_tx,lvl3_tx,lvl1_ct,lvl2_ct,lvl3_ct) %>%
               unique()) %>%
  as_tibble()->dat             
  

# prepare data for linear models ------------------------------------------


dat %>%
  mutate(p1_p2=ifelse(year < 2011,"p1","p2")) %>%
  mutate(p1_p2 = factor(p1_p2,levels = c("p1","p2")))->dat1

dat1 %>%
  write_csv(file="/home/marco/Desktop/dat1.csv")

dat1 %>%
  group_by(cat) %>%
  nest()->dat2


# prepare data for lmer models --------------------------------------------

dat %>%
  # add info for resampling in geographic space
  inner_join(read_csv("tmp/gridstats1degree",col_names = FALSE) %>%
             rename(cat = X1,cat1d=X2)) %>%
  inner_join(read_csv("tmp/gridstats025degree",col_names = FALSE) %>%
               rename(cat = X1,cat025d=X2)) %>%
  inner_join(read_csv("tmp/gridstats3degree",col_names = FALSE) %>%
               rename(cat = X1,cat3d=X2)) %>%
  inner_join(read_csv("tmp/gridstats2degree",col_names = FALSE) %>%
               rename(cat = X1,cat2d=X2)) %>% 
  write_csv(file = "/home/marco/Desktop/datglmm.csv")



# lm model for phenology --------------------------------------------------

mod_fit<-function(x){
  mod<-lm(value~p1_p2*bio1,data=x)
  mod1<-lm(value~p1_p2*bio12,data=x)
  tidy(mod) %>%
    mutate(variable="bio1") %>%
    bind_rows(tidy(mod1) %>%
                mutate(variable = "bio12"))->tmpres
  tmpres
}


dat2

dat2 %>%
  mutate(modres = map(data,mod_fit))->mod_clim

#dat2 %>%
#  mutate(modres = map(data,mod_fit))->mod_period

mod_clim %>%
  dplyr::select(-c(data)) %>%
  unnest(cols =c(modres))->mod_clim1


read_csv("./tmp/ecoregions.csv") %>%
  filter(!WWF_MHTNAM %in% c("Inland Water","Mangroves")) %>%
  inner_join(read_csv("/home/marco/Dropbox/phenology/ecoregions/biomes.csv")) %>%
  filter(is.na(out)) %>%
  filter(!is.na(broadclass)) %>%
  inner_join(mod_clim1 %>%
               filter(variable=="bio1")) %>%
  filter(p.value < 0.05) %>%
  mutate(pos_neg = ifelse(beta < 0,"neg","pos")) %>%
  group_by(WWF_MHTNAM,pos_neg) %>%
  summarise(bcount = n())->res


# mod_period1 %>%
#   filter(p.value < 0.05)->mod_period2
  
# lm model temperature ----------------------------------------------------

mod_fit1<-function(x){
  mod<-lm(bio12~p1_p2,data=x)
  tibble(beta=summary(mod)$coefficients[2,1],
         p.value=summary(mod)$coefficients[2,4])->tmpres
  tmpres
}

dat2 %>%
  mutate(modres = map(data,mod_fit1)) %>%
  unnest(cols =c(modres))->mod_prec



# plot lm models pheno and precipitation ------------------------------------
mod_period2 %>%
  inner_join(mod_prec %>%
               dplyr::select(-c(data,p.value)) %>%
               rename(beta_prec =beta)) %>%
  filter(beta < 200) %>%
  ggplot(aes(x=beta_prec,y=beta))+theme_minimal()+geom_bin2d(bins=60)+
  geom_smooth(method = "lm",color="red",size=1,linetype="dashed",se=FALSE)+
  scale_fill_viridis(option = 'magma',trans="log10")


read_csv("./tmp/ecoregions.csv") %>%
  filter(!WWF_MHTNAM %in% c("Inland Water","Mangroves")) %>%
  inner_join(read_csv("/home/marco/Dropbox/phenology/ecoregions/biomes.csv")) %>%
  filter(is.na(out)) %>%
  filter(!is.na(broadclass)) %>%
  inner_join(mod_period2) %>%
  filter(p.value < 0.05) %>%
  mutate(pos_neg = ifelse(beta < 0,"neg","pos")) %>%
  group_by(WWF_MHTNAM,pos_neg) %>%
  summarise(bcount = n())->res


# precipitation pheno stuff -----------------------------------------------
mod_period2 %>%
  



# test with linear models -----------------------------------------------------------------------

dat %>%
  filter(value > -180 & value < 180) %>%
  filter(forest_prop > 0.75)->dat

dat %>%
  group_by(cat) %>%
  summarise(bio1 = cor(bio1,value),bio5 = cor(bio5,value)) %>%
  inner_join(grid5km) %>%
  dplyr::select(x,y,bio5) %>%
  rasterFromXYZ(crs=latlon) %>%
  writeRaster(filename = "/home/marco/Desktop/corbio5.tif")

dat %>%
  group_by(cat) %>%
  summarise(bio1_cor = cor(bio1,value),bio5_cor = cor(bio5,value)) %>%
  inner_join(read_csv("./tmp/forestcount",col_names = FALSE) %>%
               rename(cat = X1, count = X2) %>%
               mutate(forest_prop = count /max(count)))->dd

dd %>%
  filter(bio1_cor < 0.80)->dd1
hist(dd1$bio1_cor)

cor(dd$bio1_cor,dd$forest_prop,remove.na=TRUE)

dd %>%
  dplyr::select(-c(lvl1_ct,lvl2_ct,lvl3_ct,cat,count,forest_prop,lvl1_tx,lvl2_tx,lvl3_tx)) %>%
  pivot_longer(values_to = "values",names_to = "variables",cols = -c(year)) %>%
  group_by(variables) %>%
  ggplot(aes(x=year,y=values))+geom_line()+geom_point(size=4)+theme_bw()+
  facet_wrap(~variables,scales = "free")


dd %>%
  dplyr::select(-c(lvl1_ct,lvl2_ct,lvl3_ct,cat,count,forest_prop,lvl1_tx,lvl2_tx,lvl3_tx)) %>%
  pivot_longer(values_to = "values",names_to = "variables",cols = -c(value,year)) %>%
  group_by(variables) %>%
  summarise(cor=cor(value,values)) %>%
  mutate(cor=abs(cor)) %>%
  arrange(desc(cor))

mod<-lm(value~bio5,data=dd)
mod1<-gam(value~s(bio5),data=dd)

dd %>%
  mutate(res=residuals(mod1)) %>%
  ggplot(aes(x=year,y=res))+geom_line()+geom_point(size=4)+theme_bw()
  

# create periods 0.25 degrees ----------------------------------------------------------

2003:2011->t1
2012:2020->t2

res %>%
  inner_join(read_csv("./tmp/forestcount",col_names = FALSE) %>%
               rename(cat = X1, count = X2) %>%
               mutate(forest_prop = count /max(count))) %>%
  filter(forest_prop > 0.75) %>%
  inner_join(read_csv("tmp/gridstats025degree",col_names = FALSE) %>%
               rename(cat = X1,cat025d=X2))->res1
res1 %>%
  group_by(cat025d,year) %>%
  summarise(value = median(value))->tmp

tmp %>%
  filter(year %in% t1) %>%
  group_by(cat025d) %>%
  summarise(value = median(value,na.rm = TRUE)) %>%
  rename(value_p1 = value)->p1

tmp %>%
  filter(year %in% t2) %>%
  group_by(cat025d) %>%
  summarise(value = median(value,na.rm = TRUE)) %>%
  rename(value_p2 = value)->p2  

# create 0.25 grid
raster(res=0.25)->myr
rasterize(world,myr) %>%
  rastertodf() %>%
  mutate(value = 1:length(value)) %>%
  rename(cat025d = value)->gridf

inner_join(p1,p2) %>%
  mutate(dif = value_p2-value_p1) %>%
  inner_join(gridf)->tmp1

tmp1 %>%
  write_csv("/home/marco/Desktop/difference/pheno025.csv")


  #mutate(posneg = ifelse(dif < 0,"neg","pos")) %>%
  #group_by(posneg) %>%
  #summarise(count = n())
  rasterFromXYZ(crs=latlon) %>%
  writeRaster(filename = "/home/marco/Desktop/dif_resampled.tif",overwrite = TRUE)
 

# create periods ----------------------------------------------------------
2003:2011->t1
2012:2020->t2
# first period
res %>%
  filter(year %in% t1) %>%
  #dplyr::select(year) %>%
  group_by(cat) %>%
  summarise(value = median(value,na.rm = TRUE)) %>%
  rename(value_p1 = value)->p1

# second period
res %>%
  filter(year %in% t2) %>%
  group_by(cat) %>%
  summarise(value = median(value,na.rm = TRUE)) %>%
  rename(value_p2 = value)->p2

inner_join(p1,p2) %>%
  mutate(dif = value_p2-value_p1) %>%
  inner_join(grid5km) %>%
  inner_join(read_csv("./tmp/forestcount",col_names = FALSE) %>%
               rename(cat = X1, count = X2) %>%
               mutate(forest_prop = count /max(count))) %>%
  # add climate data
  inner_join(read_csv("./GRASSGIS_files/bioclim.csv")) %>%
  inner_join(read_csv("./GRASSGIS_files/pet_mean.csv")) %>%
  # ecoregions
  inner_join(read_csv("./tmp/ecoregions.csv")) %>%
  inner_join(read_csv("tmp/humi",col_names = FALSE) %>%
               rename(cat = X1,humi=X2)) %>%
  # pristine
  left_join(read_csv("./tmp/pristine",col_names = FALSE) %>%
  rename(cat = X1,pristinefilt = X2)) %>%
  # add topography data
  inner_join(read_csv("./tmp/slope",col_names = F) %>%
               rename(cat = X1,mean_slope = X2)) %>%
  inner_join(read_csv("./tmp/elev",col_names = F) %>%
               rename(cat = X1,mean_elev = X2)) %>%
  inner_join(read_csv(
    "./tmp/std_elev",col_names = F) %>%
               rename(cat = X1,std_elev = X2)) %>%
  inner_join(read_csv("./tmp/std_slope",col_names = F) %>%
               rename(cat = X1,std_slope = X2))->dat_periods
  

write_csv(dat_periods,file = "/home/marco/Desktop/phenop1p2.csv")


dat %>%
  inner_join(read_csv("tmp/gridstats025degree",col_names = FALSE) %>%
               rename(cat = X1,cat025d=X2)) %>%
  filter(forest_prop > 0.75) %>% 
  dplyr::select(value_p1,value_p2,cat025d) %>%
  group_by(cat025d) %>%
  summarise(value_p1 = median(value_p1),value_p2 = median(value_p2)) %>%
  mutate(dif = value_p2-value_p1) %>%
  #filter(dif < 0) %>%
  inner_join(gridf) %>%
  dplyr::select(x,y,dif) %>%
  rasterFromXYZ(crs=latlon) %>%
  writeRaster(filename = "/home/marco/Desktop/difrast.tif")




  filter(is.na(pristinefilt)) %>%
  filter(forest_prop > 0.85) %>%
  group_by(WWF_MHTNAM) %>%
  summarise(dif = mean(dif))

dat %>%
  dplyr::select(x,y,dif) %>%
  rasterFromXYZ(crs=latlon) %>%
  writeRaster(filename="/home/marco/Desktop/dif.tif")
  


# create periods for climate ----------------------------------------------
2003:2011->t1
2012:2020->t2
# first period
clim %>%
  filter(year %in% t1) %>%
  #dplyr::select(year) %>%
  group_by(cat) %>%
  summarise(bio1_p1= median(bio1,na.rm = TRUE),
            bio12_p1= median(bio12,na.rm = TRUE))->p1

# second period
clim %>%
  filter(year %in% t2) %>%
  group_by(cat) %>%
  summarise(bio1_p2= median(bio1,na.rm = TRUE),
            bio12_p2= median(bio12,na.rm = TRUE))->p2

inner_join(p1,p2) %>%
  write_csv(file="/home/marco/Desktop/clim.csv")

# a few models here and there ----------------------------------------------------------------------


res %>%
  inner_join(read_csv("./tmp/pristine",col_names = FALSE) %>%
              rename(cat = X1,pristinefilt = X2)) %>%
  inner_join(read_csv("./tmp/ecoregions.csv")) %>%
  filter(!WWF_MHTNAM %in% c("Inland Water","Flooded Grasslands and Savannas",
                                                                         "Deserts and Xeric Shrublands",
                                                                         "Tundra",
                                                                         "Temperate Grasslands, Savannas and Shrublands",
                                                                         "Tropical and Subtropical Grasslands, Savannas and Shrublands",
                                                                         "Mangroves",
                            "Tropical and Subtropical Coniferous Forests",
                            "Montane Grasslands and Shrublands",
                            "Rock and Ice")) %>%
  inner_join(read_csv("./tmp/forestcount",col_names = FALSE) %>%
               rename(cat = X1, count = X2) %>%
               mutate(forest_prop = count /max(count)))->dd
  
dd %>%
  group_by(cat) %>%
  nest()->dd1
  
  
  filter(cat==2012982)->ts

mod<-lm(value~year,data=dd1[200000,]$data[[1]])

dat %>%
  #filter(forest_prop > 0.85) %>%
  #filter(dif > -10 & dif < 10) %>%
  filter(value_p2 < 50) %>%
  mutate(pristinefilt_f =ifelse(is.na(pristinefilt),"outside pristine","inside pristine")) %>%
  ggplot()+geom_boxplot(aes(x=pristinefilt_f,y=value_p2))


dd %>% mutate(year = as.numeric(scale(year)))->dd1

dd %>%
  filter(forest_prop > 0.85) %>%
  group_by(WWF_MHTNAM) %>%
  summarise(cor = cor(value,year))

dd %>%
  filter(forest_prop)


mod<-lmer(value~1+(year|WWF_MHTNAM),data=dd1)
mod1<-lmer(value~year+(1|WWF_MHTNAM),data=dd1)



# linear model ------------------------------------------------------------
res %>%
  group_by(cat) %>%
  summarise(count = n()) %>%
  filter(count > 9) %>%
  dplyr::select(cat) %>%
  inner_join(res)->dd

dd %>%
  inner_join(tibble(year=2003:2020,year_scld=as.numeric(scale(2003:2020)))) %>%
  group_by(cat) %>%
  nest()->dd1

plan(multisession,workers = 6)

dd1 %>%
  ungroup() %>%
  mutate(res = future_map(data,~lm(value~year_scld,data=.x)$coefficients[2]))->res

res %>%
  dplyr::select(cat,res) %>%
  unnest(cols =res) %>%
  inner_join(dat)->dat



lm(value~year_scld,data=x)$coefficients$year_scld
  

# test by ecoregion -------------------------------------------------------
dat %>%
  filter(!WWF_MHTNAM %in% c("Inland Water","Flooded Grasslands and Savannas",
                            "Deserts and Xeric Shrublands",
                            "Tundra",
                            "Temperate Grasslands, Savannas and Shrublands",
                            "Tropical and Subtropical Grasslands, Savannas and Shrublands",
                            "Mangroves")) %>%
  inner_join(pafilt) %>%
  #filter(dif > -100 & dif < 100) %>%
  filter(forest_prop > 0.85) %>%
  group_by(WWF_MHTNAM) %>%
  summarise(mean = mean(res)) 
  
  mutate(perc = (value_p2*1)/value_p1)  %>%
  relocate(perc) %>%
  mutate(perc = ifelse(perc < 1,-perc,perc)) %>%
  group_by(WWF_MHTNAM) %>%
  summarise(mean = mean(perc)) %>%
  arrange(mean) %>%
  mutate(WWF_MHTNAM = factor(WWF_MHTNAM,levels=c(WWF_MHTNAM))) %>%
  ggplot()+geom_bar(aes(x=mean,y=WWF_MHTNAM),stat="identity",fill="green4")+
  theme_minimal()+theme(axis.text.x = element_text(angle = 45, hjust = 1))


  
  ggplot()+geom_boxplot(aes(x=WWF_MHTNAM,y=dif))

  group_by(WWF_MHTNAM) %>%
  summarise(mean = mean(dif)) %>%


# general plot fread("/media/marco/marcodata19/phenologytraining/dat.csv")->dat
------------------------------------------------------------
dat %>%
  filter(bio_12 < 4001) %>%
  #filter(!is.na(pristinefilt)) %>%
  filter(forest_prop > 0.85) %>% 
  ggplot(aes(x=bio_01/10,y=bio_12,z=dif))+
  theme_minimal()+
  stat_summary_2d(fun = function(x) {if(length(x[!is.infinite(x)])>=5)(mean(x))else(NA)},bins = 30,na.rm = FALSE)+
  scale_fill_gradient2(low ="blue", mid = "white",high ="red",oob=squish,limits=c(-12,12))+
  xlab("Temperature")+ylab("Precipitation")+theme(axis.text=element_text(size=15),axis.title=element_text(size=20),
                                                  plot.title=element_text(size=30,hjust=0.5))->global
  ggsave(global,filename = "./figures/Aug21/globaldif5years.png",width = 10, height = 10,dpi = 400,bg="white")
    
dat %>%
    filter(bio_12 < 4001) %>%
    filter(!is.na(pristinefilt)) %>%
    filter(forest_prop > 0.85) %>% 
    ggplot(aes(x=bio_01/10,y=bio_12,z=dif))+
    theme_minimal()+
    stat_summary_2d(fun = function(x) {if(length(x[!is.infinite(x)])>=5)(mean(x))else(NA)},bins = 30,na.rm = FALSE)+
    scale_fill_gradient2(low ="blue", mid = "white",high ="red",oob=squish,limits=c(-12,12))+
    xlab("Temperature")+ylab("Precipitation")+theme(axis.text=element_text(size=15),axis.title=element_text(size=20),
                                                    plot.title=element_text(size=30,hjust=0.5))->pristine
  ggsave(pristine,filename = "./figures/Aug21/pristinedif5years.png",width = 10, height = 10,dpi = 400,bg="white")
  

dat %>%
    filter(bio_12 < 4001) %>%
    filter(is.na(pristinefilt)) %>%
    filter(forest_prop > 0.85) %>% 
    ggplot(aes(x=bio_01/10,y=bio_12,z=dif))+
    theme_minimal()+
    stat_summary_2d(fun = function(x) {if(length(x[!is.infinite(x)])>=5)(mean(x))else(NA)},bins = 30,na.rm = FALSE)+
    scale_fill_gradient2(low ="blue", mid = "white",high ="red",oob=squish,limits=c(-12,12))+
    xlab("Temperature")+ylab("Precipitation")+theme(axis.text=element_text(size=15),axis.title=element_text(size=20),
                                                    plot.title=element_text(size=30,hjust=0.5))->notpristine
  ggsave(notpristine,filename = "./figures/Aug21/notpristinedif5years.png",width = 10, height = 10,dpi = 400,bg="white")
  
  
  

# t-test plots ------------------------------------------------------------------


  
midpoint<-function(x){
    as.numeric(str_remove(str_split_fixed(x,",",n=2)[,1],"\\("))->lower 
    as.numeric(str_remove(str_split_fixed(x,",",n=2)[,2],"]"))->upper
    return(lower+(upper-lower)/2)
  }
  
dat %>%
  filter(bio_12 < 4001) %>%
  #filter(humi < 5) %>%
  #filter(!is.na(pristinefilt)) %>%
  #filter(forest_prop > 0.85) %>%
    dplyr::select(bio_01,bio_12,value_p1,value_p2) %>%
    mutate(
      bin_x = cut(bio_12, breaks = 30),
      bin_y = cut(bio_01, breaks = 30)
    ) %>%
    group_by(bin_x, bin_y) %>%
    filter(n() > 10)%>% 
    summarise(sig=wilcox.test(value_p1,value_p2,paired = T)$p.value) %>%
    ungroup()->sig


dat %>%
  filter(bio_12 < 4001) %>%
  #inner_join(pafilt) %>%
  #filter(humi < 5) %>%
  #filter(!is.na(pristinefilt)) %>%
  #filter(forest_prop > 0.85) %>%
  #dplyr::select(prec,temp,period1,period2) %>%
  mutate(
    bin_xf = cut(bio_12, breaks = 30),
    bin_yf = cut(bio_01, breaks = 30)
  ) %>%
  mutate(bin_x=bin_xf ,bin_y = bin_yf) %>%
  left_join(sig) %>%
  filter(sig < 0.05) %>%
  #mutate(absdif = (period2/period1)-1) %>%
  mutate(dif = case_when(is.na(sig)~NA_real_,TRUE~dif)) %>%
  mutate(
    bin_x = cut(bio_12, breaks = 30),
    bin_y = cut(bio_01, breaks = 30)
  ) %>%
  group_by(bin_xf, bin_yf) %>%
  summarise(meandif = mean(dif),prec=max(bio_12),temp=max(bio_01)) %>%
  tidyr::complete(bin_xf,bin_yf)->df1


df1 %>%
  mutate(bin_xn=midpoint(bin_xf),bin_yn=midpoint(bin_yf)) %>%
  mutate(bin_xn = bin_xn) %>%
  ggplot(aes(x=bin_yn/10, y=bin_xn, fill = meandif)) + geom_tile()+theme_minimal()+
  scale_fill_gradient2(low = "blue", high = "red",na.value=rgb(0, 0, 0, alpha=0),
                       limits=c(-10,10),oob=scales::squish)+ylab("Precipitation")+
  xlab("Temperature")+theme(axis.text=element_text(size=18),axis.title = element_text(size=16))->p1

ggsave(p1,filename = "./figures/Aug21/global_ttest_5years.png",width = 10, height = 10,dpi = 400,bg="white")



# sd deviation and count map ------------------------------------------------------------

######## standard deviation
res %>%
  na.exclude() %>%
  group_by(cat) %>%
  #summarise(sd = length(value)) %>%
  summarise(sd = sd(value,na.rm = TRUE)) %>%
  inner_join(grid5km) %>%
  dplyr::select(x,y,sd) %>%
  rasterFromXYZ(crs=latlon) %>%
  projectRaster(crs=CRS("+proj=robin +lon_0=0 +x_0=0 
                +y_0=0 +ellps=WGS84 +datum=WGS84 
                        +units=m +no_defs"))->myr1

myr1 %>%
  rastertodf(.) %>%
  mutate(value1=cut(value,breaks=round(unique(quantile(value,probs = seq(0, 1, length.out = 7 + 1))),digits = 2))) %>%
  mutate(title = paste0("Year ",yearl[i]))->tmpdf

# create labels
  str_replace("\\(","") %>%
  str_split_fixed(",",n=2) %>%
  as.data.frame() %>%
  as_tibble() %>%
  filter(V1!="") %>%
  mutate(V1=as.numeric(as.character(V1)),V2=as.numeric(as.character(V2))) %>%
  arrange(V1)->tmplabs

mylabs<-c(tmplabs[2:7,]$V1,paste0(">", tmplabs[1:6,]$V2[6]))

rev(brewer.pal(7,"Spectral"))->mypal

# smallest unit
tmpdf %>%
  mutate(value1=cut(value,breaks=round(unique(quantile(value,probs = seq(0, 1, length.out = 7 + 1))),digits = 2))) %>%
  #mutate(value2=as.factor(as.numeric(cut(value,breaks=breaksinfo)))) %>% 
  filter(!is.na(value1)) %>% {.->>tmp} %>%
  ggplot()+
  geom_sf(data=bbox_rb, colour='black',fill='black')+theme_classic()+
  geom_sf(data=wmap_rb, fill='grey70',size=0)+
  geom_sf(data=ocean_rb,fill='grey20',size=0)+
  geom_sf(data=wmap_rb, linetype="dotted", color="grey70",size=0.2)+
  geom_tile(data=,aes(x=x,y=y,fill=value1))+
  theme(legend.position="bottom",legend.title = element_blank(),
        legend.justification=c(0.5),legend.key.width=unit(2, "mm"),
        legend.text = element_text(size = rel(1)),
        strip.background = element_rect(colour = "white", fill = "white"),
        strip.text = element_text(size = rel(2), face = "bold"))+
  scale_fill_manual(labels =mylabs,values = mypal,
                    guide = guide_legend(direction = "horizontal",
            keyheight = unit(3, units = "mm"),keywidth = unit(150 / length(unique(tmp$value1)), 
                        units = "mm"),title.position = 'top',title.hjust = 0.5,label.hjust = 1,
              nrow = 1,byrow = T,reverse = F,label.position = "bottom"))+xlab("")+ylab("")->p

ggsave(p,filename = "./figures/Aug21/standard_deviationmap.png",height=9,width = 12,dpi=400)

# observation count
######## standard deviation

res %>%
  na.exclude() %>%
  group_by(cat) %>%
  summarise(sd = length(value)) %>%
  #summarise(sd = sd(value,na.rm = TRUE)) %>%
  inner_join(grid5km) %>%
  dplyr::select(x,y,sd) %>%
  rasterFromXYZ(crs=latlon) %>%
  projectRaster(crs=CRS("+proj=robin +lon_0=0 +x_0=0 
                +y_0=0 +ellps=WGS84 +datum=WGS84 
                        +units=m +no_defs"))->myr1

myr1 %>%
  rastertodf(.) %>%
  mutate(value = round(value)) %>%
  mutate(value1=cut(value,breaks=round(unique(quantile(value,probs = seq(0, 1, length.out = 7 + 1))),digits = 2))) %>%
  mutate(title = paste0("Year ",yearl[i]))->tmpdf

# create labels
str_replace(unique(tmpdf$value1),"]","") %>%
  str_replace("\\(","") %>%
  str_split_fixed(",",n=2) %>%
  as.data.frame() %>%
  as_tibble() %>%
  filter(V1!="") %>%
  mutate(V1=as.numeric(as.character(V1)),V2=as.numeric(as.character(V2))) %>%
  arrange(V1)->tmplabs

mylabs<-c(tmplabs[2:3,]$V1,paste0(">", tmplabs[1:2,]$V2[2]))

rev(brewer.pal(3,"Spectral"))->mypal

tmpdf %>%
  mutate(value1=cut(value,breaks=round(unique(quantile(value,probs = seq(0, 1, length.out = 7 + 1))),digits = 2))) %>%
  #mutate(value2=as.factor(as.numeric(cut(value,breaks=breaksinfo)))) %>% 
  filter(!is.na(value1)) %>% {.->>tmp} %>%
  ggplot()+
  geom_sf(data=bbox_rb, colour='black',fill='black')+theme_classic()+
  geom_sf(data=wmap_rb, fill='grey70',size=0)+
  geom_sf(data=ocean_rb,fill='grey20',size=0)+
  geom_sf(data=wmap_rb, linetype="dotted", color="grey70",size=0.2)+
  geom_tile(data=,aes(x=x,y=y,fill=value1))+
  theme(legend.position="bottom",legend.title = element_blank(),
        legend.justification=c(0.5),legend.key.width=unit(2, "mm"),
        legend.text = element_text(size = rel(1)),
        strip.backgrmylabs<-c(tmplabs[2:7,]$V1,paste0(">", tmplabs[1:6,]$V2[6]))
        
        rev(brewer.pal(7,"PiYG"))->mypal
        
        ound = element_rect(colour = "white", fill = "white"),
        strip.text = element_text(size = rel(2), face = "bold"))+
  scale_fill_manual(labels =mylabs,values = mypal,
                    guide = guide_legend(direction = "horizontal",
                                         keyheight = unit(3, units = "mm"),keywidth = unit(150 / length(unique(tmp$value1)), 
                                                                                           units = "mm"),title.position = 'top',title.hjust = 0.5,label.hjust = 1,
                                         nrow = 1,byrow = T,reverse = F,label.position = "bottom"))+xlab("")+ylab("")->p

ggsave(p,filename = "./figures/Aug21/countobs.png",height=9,width = 12,dpi=400)
mylabs<-c(tmplabs[2:7,]$V1,paste0(">", tmplabs[1:6,]$V2[6]))

rev(brewer.pal(7,"PiYG"))->mypal


# trends map 9-years and 5 years -------------------------------------------------------


dat %>% 
  #filter(forest_prop > 0.50) %>%
  dplyr::select(cat,dif) %>%
  inner_join(grid5km) %>%
  dplyr::select(x,y,dif) %>%
  rasterFromXYZ(crs=latlon) %>%
  aggregate(fun = mean,fact = 10) %>%
  projectRaster(crs=CRS("+proj=robin +lon_0=0 +x_0=0 
                +y_0=0 +ellps=WGS84 +datum=WGS84 
                        +units=m +no_defs")) %>%
  writeRaster(filename = "/home/marco/Desktop/template.tif")
  rastertodf(.) %>%
  mutate(value1=cut(value,breaks=round(unique(quantile(value,
  probs = seq(0, 1, length.out = 7 + 1))),digits = 2))) %>%
  mutate(title = paste0("Year ",yearl[i]))->tmpdf

# create labels
str_replace(unique(tmpdf$value1),"]","") %>%
  str_replace("\\(","") %>%
  str_split_fixed(",",n=2) %>%
  as.data.frame() %>%
  as_tibble() %>%
  filter(V1!="") %>%
  mutate(V1=as.numeric(as.character(V1)),V2=as.numeric(as.character(V2))) %>%
  arrange(V1)->tmplabs

# smallest unit
tmpdf %>%
  mutate(value1=cut(value,breaks=round(unique(quantile(value,probs = seq(0, 1, length.out = 7 + 1))),digits = 2))) %>%
  #mutate(value2=as.factor(as.numeric(cut(value,breaks=breaksinfo)))) %>% 
  filter(!is.na(value1)) %>% {.->>tmp} %>%
  ggplot()+
  geom_sf(data=bbox_rb, colour='black',fill='black')+theme_classic()+
  geom_sf(data=wmap_rb, fill='grey70',size=0)+
  geom_sf(data=ocean_rb,fill='grey20',size=0)+
  geom_sf(data=wmap_rb, linetype="dotted", color="grey70",size=0.2)+
  geom_tile(data=,aes(x=x,y=y,fill=value))+
  theme(legend.position="bottom",legend.title = element_blank(),
        legend.justification=c(0.5),legend.key.width=unit(35, "mm"),
        legend.text = element_text(size = rel(2)),legend.key.size = unit(10,"mm"),
        strip.background = element_rect(colour = "white", fill = "white"),
        strip.text = element_text(size = rel(2), face = "bold"))+
  scale_fill_gradient2(low ="blue", mid = "white",high ="red",oob=squish,limits=c(-50,50))+
  xlab("")+ylab("")->p


ggsave(p,filename = "./figures/Sep21/Gregsmetric_changes.png",height=9,width = 12,dpi=400)



# climate pheno relationships ---------------------------------------------

# get rid of regions with less 1000 observations
fwrite(dat,"/mnt/data1tb/alessandro_metric/finaldat.csv")
fread("/mnt/data1tb/alessandro_metric/finaldat.csv")->dat
  
dat %>%
  filter(value < 183) %>%
  group_by(lvl1_tx,lvl2_tx,lvl3_tx) %>%
  summarise(n = n()) %>%
  filter(n > 1000) %>%
  dplyr::select(-c(n)) %>%
  ungroup() %>%
  inner_join(dat) %>%
  group_by(lvl1_tx,lvl2_tx) %>% 
  nest()->dat2


# choices: 
# "two" (first half second half)
# "alternate": 1 year yes and 1 year no
# "last": last four years


# wrapper function for model fitting
fit_mod<-function(x,option){
  if(option=="two"){
    trainy <- 2003:2011
  } else if(option=="alternate") {
    trainy <- seq(from=2003,to=2020,by=2)
  } else if(option=="last") {
    trainy <- 2003:2016
  }
  x %>%
    filter(year %in% trainy)->train
  x %>%
    filter(!year %in% trainy)->test
  mod<-ranger(value~bio1+bio2+bio3+bio4+bio5+bio6+bio7+
                bio8+bio9+bio10+bio11+bio12+bio13+
                bio14+bio15+bio16+bio17+bio18+bio19,data=train,num.trees = 500)
  predict(mod,test)->predv
  cor(predv$predictions,test$value)->r2
  return(r2)
}

# fit models to first period and validate on second period
dat2[1,] %>%
  mutate(res = map(data,~ranger(value~bio1+bio2+bio3+bio4+bio5+bio6+bio7+
                                  bio8+bio9+bio10+bio11+bio12+bio13+
                                  bio14+bio15+bio16+bio17+bio18+bio19,data=.,num.trees = 500)$r.squared))->res

res %>%
  unnest(cols = res)


# create final raster with results
st_read("./KG/kg_codesv1.shp") %>%
  inner_join(res %>%
  dplyr::select(-c(data)) %>%
  unnest(cols =res))->dd

#---------- Two levels (level 1 and level2)
# everything (already done!)
# first 11 years vs. last 11 years
# 1 year in 1 year out
# 14 years and last 4 years to be predicted



  
raster(ex=extent(dd),res=1)->myr
fasterize(raster = myr,sf = dd,field="res")->myr1
writeRaster(myr1,filename="/home/marco/Desktop/myr_split.tif")




    



  

# delta plots by biome -------------------------------------------------------------------------
fread("/media/marco/marcodata19/tmpstats") %>%
  rename(cat = V1,esacat =V2, area = V3) %>%
  


2003:2011->t1
2011:2020->t2
# first period
res %>%
  filter(year %in% t1) %>%
  group_by(cat) %>%
  summarise(value = median(value,na.rm = TRUE)) %>%
  rename(value_p1 = value)->p1

# second period
res %>%
  filter(year %in% t2) %>%
  group_by(cat) %>%
  summarise(value = median(value,na.rm = TRUE)) %>%
  rename(value_p2 = value)->p2




inner_join(p1,p2) %>%
  inner_join(read_csv("./tmp/forestcount",col_names = FALSE) %>%
               rename(cat = X1, count = X2) %>%
               mutate(forest_prop = count /max(count))) %>%
  #filter(humi < 2) %>%
  filter(forest_prop > 0.75) %>%
  mutate(dif = value_p2-value_p1) %>%
  inner_join(clim %>% 
  dplyr::select(c(cat,lvl1_ct,lvl2_ct,lvl3_ct)) %>%
  unique()) %>%
  inner_join(st_read("./KG/kg_codesv1.shp") %>%
               as_tibble() %>%
               dplyr::select(lvl1_tx,lvl2_tx,lvl3_tx,lvl1_ct,lvl2_ct,lvl3_ct) %>%
               unique())->dat

#st_read("./KG/kg_codesv1.shp") %>%
#  as_tibble() %>%
#  dplyr::select(lvl1_tx) %>%
#  unique()


dat %>%
  mutate(lvl3_tx = case_when(is.na(lvl3_tx)~paste(lvl1_tx,lvl2_tx),TRUE ~ lvl3_tx)) %>%
  dplyr::select(dif,cat,lvl3_tx) %>%
  mutate(negpos =ifelse(dif < 0,'neg','pos')) %>%
  #filter(negpos=="pos") %>%
  inner_join(fread("/media/marco/marcodata19/tmpstats") %>%

  rename(cat = V1,esacat =V2, area = V3)) %>%
  inner_join(esacats1 %>%
               dplyr::select(description,cat) %>%
               rename(esacat=cat)) %>%
  filter(!esacat %in% c(10,20,30,190,210,40,130,11,140,150,160,170,201,200,121,122,12,120)) %>%
  ggplot()+geom_density(aes(dif),color="darkblue", fill="lightblue")+
  facet_wrap(~lvl3_tx,scales="free",labeller = label_wrap_gen(width=5))+
  theme_minimal()+theme(strip.text = element_text(face="bold"))->p1

ggsave(p1,filename = "./figures/Aug21trends/delta_lv3_density.png",width=13,height = 9)


dat %>%
  #mutate(lvl3_tx = case_when(is.na(lvl3_tx)~paste(lvl1_tx,lvl2_tx),TRUE ~ lvl3_tx)) %>%
  dplyr::select(dif,cat,lvl1_tx,lvl2_tx) %>%
  mutate(lvl2_tx = paste(lvl1_tx,lvl2_tx)) %>%
  mutate(negpos =ifelse(dif < 0,'neg','pos')) %>%
  #filter(negpos=="pos") %>%
  #inner_join(fread("/media/marco/marcodata19/tmpstats") %>%
               
  #             rename(cat = V1,esacat =V2, area = V3)) %>%
  #inner_join(esacats1 %>%
  #             dplyr::select(description,cat) %>%
  #             rename(esacat=cat)) %>%
  #filter(!esacat %in% c(10,20,30,190,210,40,130,11,140,150,160,170,201,200,121,122,12,120)) %>%
  ggplot()+geom_density(aes(dif),color="darkblue", fill="lightblue")+
  facet_wrap(~lvl2_tx,scales="free",labeller = label_wrap_gen(width=5))+
  theme_minimal()+theme(strip.text = element_text(face="bold"))->p2


dat %>%
  #mutate(lvl3_tx = case_when(is.na(lvl3_tx)~paste(lvl1_tx,lvl2_tx),TRUE ~ lvl3_tx)) %>%
  dplyr::select(dif,cat,lvl1_tx,lvl2_tx) %>%
  mutate(lvl2_tx = paste(lvl1_tx,lvl2_tx)) %>%
  mutate(negpos =ifelse(dif < 0,'neg','pos')) %>%
  #filter(lvl1_2=="Continental No dry season") %>%
  ggplot(aes(x=lvl2_tx,y=dif,fill = lvl2_tx, colour = lvl2_tx))+geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust =2)+
  geom_point(position = position_jitter(width = .15), size = .25,alpha=0.5)+coord_flip()+
  geom_boxplot(aes(x = as.numeric(as.factor(lvl2_tx))+0.25, y = dif),outlier.shape = NA, alpha = 0.3, width = .1, colour = "BLACK")+
  scale_colour_brewer(palette = "Paired")+
  scale_fill_brewer(palette = "Paired")+theme_minimal()+theme(strip.text=element_text(size=15),
                                                              axis.text.x = element_text(size=12,colour = "black"),
                                                              axis.text.y = element_text(size=12,colour = "black"),legend.position = "none")->p2

ggsave(p2,filename = "./figures/Sep21/density.png",width=13,height = 9)



#%>%
  group_by(description) %>%
  summarise(area = sum(area)/100000) %>%
  ggplot()+geom_bar(aes(x=description ,y=area),stat="identity")+coord_flip()+
  theme_minimal()

+facet_wrap(~description)
  
               


dat %>%
  mutate(negpos =ifelse(dif < 0,'neg','pos')) %>%
  #filter(dif < 50 & dif > -50) %>%
  filter(negpos=='neg') %>%
  ggplot()+geom_density(aes(dif),color="darkblue", fill="lightblue")+
  facet_wrap(~lvl2_tx,scales="free")


# calculate mode for KG using info from 0.25 grid
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

dat %>%
  dplyr::select(cat,lvl1_ct,lvl2_ct,lvl3_ct) %>%
  inner_join(read_csv("tmp/gridstats025degree",col_names = FALSE) %>%
  rename(cat = X1,cat025d=X2)) %>%
  group_by(cat025d) %>%
  summarise(lvl1_ct = Mode(lvl1_ct),lvl3_ct = Mode(lvl3_ct),lvl2_ct = Mode(lvl2_ct))->cats025

dat %>%
  dplyr::select(cat,value_p1,value_p2,humi) %>%
  inner_join(read_csv("tmp/gridstats025degree",col_names = FALSE) %>%
               rename(cat = X1,cat025d=X2)) %>%
  group_by(cat025d) %>%
  summarise(value_p1 = mean(value_p1),value_p2 = mean(value_p2),humi = mean(humi)) %>%
  mutate(dif = value_p2 - value_p1) %>%
  inner_join(cats025) %>%
  inner_join(st_read("./KG/kg_codesv1.shp") %>%
               as_tibble() %>%
               dplyr::select(lvl1_tx,lvl2_tx,lvl3_tx,lvl1_ct,lvl2_ct,lvl3_ct) %>%
               unique()) %>% {.->> tmp} %>%
  #filter(dif < 50 & dif > -50) %>%
  filter(humi < 50) %>%
  filter(dif < 200) %>%
  ggplot()+geom_point(aes(x=humi,y=dif))

  facet_wrap(~lvl2_tx,scales="free")

  
  
  ggplot()+geom_density(aes(dif),color="darkblue", fill="lightblue")+
  facet_wrap(~lvl2_tx,scales="free")
  

res %>%
  filter(year==2020) %>%
  inner_join(res %>%
               filter(year==2015) %>%
               dplyr::select(-c(year)) %>%
               rename(y_2015 = value))

# rf models R2 plots ------------------------------------------------------------------
plot_create<-function(x){
  x %>%
    arrange(res) %>%
    mutate(label = factor(label,levels=label)) %>%
    ggplot()+geom_bar(aes(x=label,y=res,fill=label),stat="identity")+coord_flip()+
    ylim(0,1)+facet_wrap(~file_id1)+theme_minimal()+theme(legend.position = 'none',
                                                          axis.text = element_text(size = 10,face = "bold",colour="black"),axis.title = 
                                                            element_text(size = 20,face = "bold",colour="black"),
                                                          strip.text = element_text(size = 25,face = "bold",colour="black"))+ylab("R2")
}

# Level 2 Koppen Geiger
list.files("./awsresults",full.names = TRUE) %>%
  .[str_detect(.,"lvl2")] %>%
  map_dfr(function(x) read_csv(x) %>% mutate(file_id=str_remove_all(basename(x),
                                                                    ".csv|_lvl2"))) %>%
  mutate(label = paste0(lvl1_tx," ",lvl2_tx)) %>%
  mutate(file_id1 = case_when(file_id=="two"~"two periods 11 years",
                              file_id =="alternate"~"alternate years",
                              file_id =="last"~"last 5 years",
                              file_id =="global"~"whole dataset")) %>%
  group_by(file_id) %>%
  arrange(res) %>%
  mutate(label = factor(label,levels=label)) %>%
  nest() %>%
  mutate(res = map(data,plot_create)) %>%
  # output files
  mutate(fileout = paste0("/mnt/data1tb/Dropbox/phenology/figures/Aug21trends/",file_id,"_lvl2.png")) %>% {.->> maps } %>%
  walk2(.x =.$fileout,.y=.$res,.f=~ggsave(filename=.x,plot=.y,width = 13,height=8))


# Level 3 Koppen Geiger
list.files("./awsresults",full.names = TRUE) %>%
  .[str_detect(.,"lvl3")] %>%
  map_dfr(function(x) read_csv(x) %>% mutate(file_id=str_remove_all(basename(x),
                                                                    ".csv|_lvl3"))) %>%
  #mutate(label = paste0(lvl1_tx," ",lvl2_tx)) %>%
  mutate(file_id1 = case_when(file_id=="two"~"two periods 11 years",
                              file_id =="alternate"~"alternate years",
                              file_id =="last"~"last 5 years",
                              file_id =="global"~"whole dataset")) %>%
  #mutate(lv1_lv2 = paste(lvl1_tx,lvl2_tx)) %>%
  mutate(label = case_when(is.na(lvl3_tx)~paste(lvl1_tx,lvl2_tx),TRUE ~ lvl3_tx)) %>%
  group_by(file_id) %>%
  nest() %>%
  mutate(res = map(data,plot_create)) %>%
  # output files
  mutate(fileout = paste0("/mnt/data1tb/Dropbox/phenology/figures/Aug21trends/",file_id,"_lvl3.png")) %>% {.->> maps } %>%
  walk2(.x =.$fileout,.y=.$res,.f=~ggsave(filename=.x,plot=.y,width = 13,height=8))




# rf models maps ----------------------------------------------------------

st_read("./KG/kg_codesv1.shp") %>%
  inner_join(list.files("./awsresults",full.names = TRUE) %>%
  .[str_detect(.,"lvl2")] %>%
  map_dfr(function(x) read_csv(x) %>% mutate(file_id=str_remove_all(basename(x),
                                                                    ".csv|_lvl2"))) %>%
  filter(file_id=="alternate")) %>%
  ggplot()+geom_sf(aes(fill=res),size = 0,colour="black")+scale_fill_distiller(palette="Spectral")+
  theme_minimal()+xlab("")+theme(axis.text=element_blank())+labs(fill='R2')->p1

ggsave(p1,filename = "./figures/Aug21trends/R2_map_Level2.png",width = 12,height=8)


st_read("./KG/kg_codesv1.shp") %>%
  inner_join(list.files("./awsresults",full.names = TRUE) %>%
               .[str_detect(.,"lvl3")] %>%
               map_dfr(function(x) read_csv(x) %>% mutate(file_id=str_remove_all(basename(x),
                                                                                 ".csv|_lvl3"))) %>%
               filter(file_id=="alternate")) %>%
  ggplot()+geom_sf(aes(fill=res),size = 0,colour="black")+scale_fill_distiller(palette="Spectral",limits = c(0.3,0.65),oob=squish)+
  theme_minimal()+xlab("")+theme(axis.text=element_blank())+labs(fill='R2')->p2
p2

ggsave(p2,filename = "./figures/Aug21trends/R2_map_Level3.png",width = 12,height=8)

# subsampling dataset -----------------------------------------------------

seq(from = 2003, to = 2020,by = 2)->v

dat %>%
  mutate(lvl1_2 = paste(lvl1_tx,lvl2_tx)) %>%
  filter(lvl1_2!="Arid Desert") %>%
  group_by(lvl1_2) %>%
  nest() ->dd1
# resample biggest dataset
dd1[1,]$data[[1]] %>%
  sample_frac(0.2)->tmp
dd1[1,]$data[[1]]<-tmp


modfit<-function(x){
  x %>%
  mutate(value1 = value/mean(value))->x
  as.formula(paste0('value1~',paste0('bio',1:19,collapse = '+')))->form
  ranger(form,data=x)->mod
  predict(mod,x)->preds
  x %>%
  mutate(predicted=preds$predictions) %>%
    dplyr::select(value1,predicted,year) %>%
    mutate(residuals = value1-predicted)->resdf
  return(resdf)
}

readRDS("/home/marco/Desktop/residuals.RDS")->dat
dat[3,]$data[[1]]->x

  
modfitr2<-function(x){
  # temporal component
  yearsl<-seq(from=2003,to=2020,by=2)
  x %>%
    mutate(value1 = value/mean(value)) %>% {.->> tmp}%>%
    filter(year %in% yearsl)->train
  tmp %>%
    filter(!year %in% yearsl)->test
  as.formula(paste0('value1~',paste0('bio',1:19,collapse = '+')))->form
  ranger(form,data=train)->mod
  # compute r2
  predict(mod,test)$predictions->pred
  test$value1->obs
  cor(pred,obs) %>%
    as_tibble() %>%
    rename(temporalr2 = value)->tempr2
  # spatial component
  x %>%
    dplyr::select(-c(year,lvl1_ct,lvl2_ct,lvl3_ct,lvl1_tx,
                     lvl2_tx,lvl3_tx)) %>%
    pivot_longer(names_to="variables",values_to="value",cols=-c(cat)) %>%
    group_by(cat,variables) %>%
    summarise(value = mean(value)) %>%
    pivot_wider(names_from = variables,values_from=value)->spattrain
  as.formula(paste0('value~',paste0('bio',1:19,collapse = '+')))->form1
  ranger(form1,data=spattrain)->mod
  tempr2 %>% mutate(spatialr2=mod$r.squared)->resdf
  return(resdf)
}

dat %>%
  mutate(r2=map(data,modfitr2))->resr2

saveRDS(resr2,file="./resR2.RDS")



dd1 %>%
  mutate(residuals=map(data,modfit))->res

res %>%
  dplyr::select(lvl1_2,residuals) %>%
  unnest(cols=residuals) %>%
  group_by(lvl1_2,year) %>%
  summarise(residuals1 = mean(residuals),median = median(residuals)) %>%
  ggplot(aes(x=year,y=residuals1))+facet_wrap(~lvl1_2,scales="free")+theme_minimal()+geom_line()+
  geom_point(size=4,alpha=0.5)+geom_smooth()->myp1

geom_errorbar(aes(ymin=residuals1-sd, ymax=residuals1+sd))


res %>%
  unnest(cols=residuals,data) %>%
  dplyr::select(lvl1_2,residuals,cat,year)->dat %>%
  filter(lvl1_2=="Tropical Tropical rainforest climate")->dat


mod<-lmer(residuals~year+(1|cat),data=dat)

res %>%
  dplyr::select(lvl1_2,residuals) %>%
  unnest(cols=residuals) %>%
  ggplot()+geom_density(aes(x=residuals))+facet_wrap(~lvl1_2)
  
  group_by(lvl1_2,year) %>%
  summarise(residuals1 = mean(residuals),median = median(residuals))->cc
  
 
  
res %>%
  dplyr::select(lvl1_2,residuals) %>%
  unnest(cols=residuals) %>%
  #filter(lvl1_2=="Continental No dry season") %>%
  ggplot(aes(x=lvl1_2,y=residuals,fill = lvl1_2, colour = lvl1_2))+geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust =2)+
  geom_point(position = position_jitter(width = .15), size = .05,alpha=0.5)+coord_flip()+facet_wrap(~year)+
  scale_colour_brewer(palette = "Paired")+
  scale_fill_brewer(palette = "Paired")+theme_minimal()+theme(strip.text=element_text(size=15),
                                                              axis.text.x = element_text(size=8,colour = "black"),
                                                              axis.text.y = element_text(size=7,colour = "black"),legend.position = "none")->resdist
  
ggsave(resdist,filename = "./figures/Sep21/resdist.png",width = 11,height=8,dpi=400)


  

ggsave(myp1,filename = "./figures/Sep21/trends_residuals_mean_v1.png",width = 10,height=8,dpi=400)

res %>%
  dplyr::select(lvl1_2,residuals) %>%
  unnest(cols=residuals) %>%
  ggplot(aes(x=year,y=residuals))+geom_smooth()+facet_wrap(~lvl1_2,scales="free")+theme_minimal()->myp2

ggsave(myp2,filename = "./figures/Sep21/trends_residuals_nomean.png",width = 10,height=8,dpi=400)

# raw data: smoothing
res %>%
  dplyr::select(lvl1_2,data) %>%
  unnest(cols=data) %>%
  ggplot(aes(x=year,y=value))+geom_smooth()+facet_wrap(~lvl1_2,scales="free")+theme_minimal()->myp3

ggsave(myp3,filename = "./figures/Sep21/trends_residuals_nomean_rawdata.png",width = 10,height=8,dpi=400)

# raw data: mean
res %>%
  dplyr::select(lvl1_2,data) %>%
  unnest(cols=data) %>%
  group_by(lvl1_2) %>%
  mutate(value1 = value/mean(value)) %>%
  group_by(lvl1_2,year) %>%
  summarise(value1 = mean(value1)) %>%
  ggplot(aes(x=year,y=value1))+geom_smooth()+facet_wrap(~lvl1_2,scales="free")+theme_minimal()->myp3

ggsave(myp3,filename = "./figures/Sep21/trends_residuals_mean_rawdata.png",width = 10,height=8,dpi=400)

dplyr::select(lvl1_2,residuals) %>%
  unnest(cols=residuals) %>%



res %>%
  dplyr::select(lvl1_2,residuals) %>%
  unnest(cols=residuals) %>%
  mutate(year = as.factor(year)) %>%
  filter(lvl1_2=="Arid Steppe") %>%
  ggplot(aes(x=year,y=residuals))+geom_boxplot()+facet_wrap(~lvl1_2,scales="free")+theme_minimal()->myp2

res %>%
  dplyr::select(lvl1_2,residuals) %>%
  unnest(cols=residuals) %>%
  #mutate(year = as.factor(year)) %>%
  filter(lvl1_2=="Tropical Tropical rainforest climate")->mydf

mod<-lm(residuals~as.factor(year),data=mydf)


tibble(value=mod$coefficients[-c(1)],year=2004:2020) %>%
  ggplot(aes(x=year,y=value))+geom_smooth()



st_read("./KG/kg_codesv1.shp") %>%
  inner_join(list.files("./awsresults",full.names = TRUE) %>%
               .[str_detect(.,"lvl2")] %>%
               map_dfr(function(x) read_csv(x) %>% mutate(file_id=str_remove_all(basename(x),
                                                                                 ".csv|_lvl2"))) %>%
               filter(file_id=="alternate")) %>%
  ggplot()+geom_sf(aes(fill=res),size = 0,colour="black")+scale_fill_distiller(palette="Spectral")+
  theme_minimal()+xlab("")+theme(axis.text=element_blank())+labs(fill='R2')->p1

ggsave(p1,filename = "./figures/Aug21trends/R2_map_Level2.png",width = 12,height=8)



read_csv("/mnt/data1tb/Dropbox/phenology/rsquared.csv") %>%
  arrange(temporalr2) %>%
  mutate(lvl1_2 = factor(lvl1_2,levels=lvl1_2)) %>%
  ggplot()+geom_bar(aes(x=lvl1_2,y=temporalr2,fill=lvl1_2),stat="identity")+coord_flip()+
  ylim(0,1)+theme_minimal()+theme(legend.position = 'none',
                                                        axis.text = element_text(size = 10,face = "bold",colour="black"),axis.title = 
                                                          element_text(size = 20,face = "bold",colour="black"),
                                                        strip.text = element_text(size = 25,face = "bold",colour="black"))+ylab("R2")->r2p1

ggsave(r2p1,filename = "./figures/Sep21/temporalR2.png",width=9,height=6,dpi=400)


read_csv("/mnt/data1tb/Dropbox/phenology/rsquared.csv") %>%
  arrange(spatialr2) %>%
  mutate(lvl1_2 = factor(lvl1_2,levels=lvl1_2)) %>%
  ggplot()+geom_bar(aes(x=lvl1_2,y=spatialr2,fill=lvl1_2),stat="identity")+coord_flip()+
  ylim(0,1)+theme_minimal()+theme(legend.position = 'none',
                                  axis.text = element_text(size = 10,face = "bold",colour="black"),axis.title = 
                                    element_text(size = 20,face = "bold",colour="black"),
                                  strip.text = element_text(size = 25,face = "bold",colour="black"))+ylab("R2")->r2p2


ggsave(r2p2,filename = "./figures/Sep21/spatialR2.png",width=9,height=6,dpi=400)



# descriptive statistics --------------------------------------------------
dat %>%
  #mutate(lvl3_tx = case_when(is.na(lvl3_tx)~paste(lvl1_tx,lvl2_tx),TRUE ~ lvl3_tx)) %>%
  dplyr::select(dif,cat,lvl1_tx,lvl2_tx) %>%
  mutate(lvl2_tx = paste(lvl1_tx,lvl2_tx)) %>%
  mutate(negpos =ifelse(dif < 0,'neg','pos')) %>%
  group_by(lvl2_tx,negpos) %>%
  summarise(count = n()) %>%
  group_by(lvl2_tx) %>%
  mutate(total = sum(count)) %>%
  mutate(prop = count/total) %>%
  ggplot(aes(x = lvl2_tx,
             y = prop,
             fill = negpos))+geom_bar(stat = "identity",
                                      position = "dodge")+coord_flip()+theme_minimal()+
  theme(axis.text= element_text(size=10,colour = "black"))->prop

ggsave(prop,filename="./figures/Sep21/prop_pos_neg.png",height = 6,width =10 ,dpi=400)


# calculate descriptive statistics
dat %>%
  mutate(lvl2_tx = paste(lvl1_tx,lvl2_tx)) %>%
  group_by(lvl2_tx) %>%
  rename(value =dif) %>%
  summarize(min = min(value), max = max(value), 
          mean = mean(value), q1= quantile(value, probs = 0.25), 
          median = median(value), q3= quantile(value, probs = 0.75),
          sd = sd(value), skewness=skewness(value), kurtosis=kurtosis(value))->sumstats

for (i in 4:12){
  
  # statistics in long format
  sumstats[i,] %>%
    pivot_longer(names_to = "variables",values_to = "values",cols = -c(lvl2_tx)) %>%
    dplyr::select(-c(lvl2_tx)) %>%
    rename(statistic = variables)->sumstats1  
  
  # basic density plot
  dat %>%
  dplyr::select(dif,cat,lvl1_tx,lvl2_tx) %>%
  mutate(lvl2_tx = paste(lvl1_tx,lvl2_tx)) %>%
  filter(lvl2_tx==sumstats[i,]$lvl2_tx) %>%
  ggplot()+geom_density(aes(dif),color="darkblue", fill="lightblue")+
  facet_wrap(~lvl2_tx,scales="free")+
  theme_minimal()+theme(strip.text = element_text(face="bold",size=29))->p2
  
  # final plot
  p2+geom_table_npc(data=sumstats1,label =list(sumstats1),npcx = 0.00, npcy = 1, hjust = -1.5, vjust = 1.2,size=6)->p3
  outf<-paste0("./figures/Sep21/summarystats/",str_replace_all(sumstats[i,]$lvl2_tx, fixed(" "), ""),".png")
  ggsave(p3,filename=outf,width = 8,height = 7,dpi=400)

}



# elimination of features -------------------------------------------------
readRDS("/home/marco/Desktop/residuals.RDS")->dat
seq(from = 2003, to = 2020,by=2)->yearl
# create datasets
dat[3,]$data[[1]]->x
x %>%
  mutate(value1 = value/mean(value)) %>% {.->> tmp}%>%
  filter(year %in% yearl)->train
tmp %>%
  filter(!year %in% yearl)->test
# fit model
as.formula(paste0('value1~',paste0('bio',1:19,collapse = '+')))->form
ranger(form,data=train,importance = "permutation")->mod

mod$variable.importance %>%
  as_tibble() %>%
  mutate(variables = names(mod$variable.importance)) %>%
  arrange(desc(value))->df




# ndvi vs phenodiversity --------------------------------------------------

# Global plot
dat %>%
  mutate(lvl2_tx = paste(lvl1_tx,lvl2_tx)) %>%
  dplyr::select(cat,dif,forest_prop,lvl2_tx) %>%
  filter(forest_prop > 0.75) %>%
  inner_join(read_csv("./ndvi_maxdif/ndvi_dif5km",col_names = F) %>%
               rename(cat = X1,ndvi_dif=X2)) %>%
  filter(dif < 150 & dif > -125) %>%
  filter(ndvi_dif > -0.15) %>%
  filter(!lvl2_tx %in% c("Arid Desert")) %>%
  ggplot(aes(x=ndvi_dif,y=dif))+theme_minimal()+geom_bin2d(bins=60)+
  geom_smooth(method = "lm",color="red",size=1,linetype="dashed",se=FALSE)+
  scale_fill_viridis(option = 'magma',trans="log10")+
  theme(legend.position = "none",axis.text = element_text(size=12,colour="black"),
        axis.title = element_text(size=20),
        plot.title = element_text(hjust = 0.5,size=20))+xlab("NDVI P2 vs. P1")+
  ylab("Pheno Div P2 vs. P1")->pndvi

ggsave(pndvi,filename = "./figures/paper/NDVI_PHENOLOGY_SCATTER.png",
       width = 10,height = 9,dpi=400,bg="white")

# Plot by climates

dat %>%
  mutate(lvl2_tx = paste(lvl1_tx,lvl2_tx)) %>%
  dplyr::select(cat,dif,forest_prop,lvl2_tx) %>%
  filter(forest_prop > 0.75) %>%
  inner_join(read_csv("./ndvi_maxdif/ndvi_dif5km",col_names = F) %>%
               rename(cat = X1,ndvi_dif=X2)) %>%
  filter(dif < 200 & dif > -200) %>%
  filter(ndvi_dif > -0.15) %>%
  filter(!lvl2_tx %in% c("Arid Desert")) %>%
  ggplot(aes(x=ndvi_dif,y=dif))+theme_minimal()+geom_bin2d(bins=60)+
  geom_smooth(method = "lm",color="red",size=1,linetype="dashed",se=FALSE)+
  scale_fill_viridis(option = 'magma',trans="log10")+
  theme(legend.position = "none",axis.text = element_text(size=12,colour="black"),
        axis.title = element_text(size=20),
        plot.title = element_text(hjust = 0.5,size=20))+xlab("NDVI P2 vs. P1")+
  ylab("Pheno Div P2 vs. P1")+facet_wrap(~lvl2_tx,scales = "fixed")->pndvi1

ggsave(pndvi1,filename = "./figures/paper/NDVI_PHENOLOGY_SCATTER_biome.png",
       width = 11,height = 9,dpi=400,bg="white")


# Plot by biomes
filtoutliers<-function(x){
  min<-quantile(x$dif,probs=c(0.0001))
  max<-quantile(x$dif,probs=c(0.9999))
  ndvimin<-quantile(x$ndvi_dif,probs=c(0.0001))
  ndvimax<-quantile(x$ndvi_dif,probs=c(0.9999))
  x %>%
    filter(dif > min & dif < max) %>%
    filter(ndvi_dif > ndvimin & ndvi_dif < ndvimax)->x1
  return(x1)
}

dat %>%
  inner_join(read_csv("./tmp/ecoregions.csv")) %>%
  filter(!WWF_MHTNAM %in% c("Inland Water","Mangroves")) %>%
  inner_join(read_csv("/home/marco/Dropbox/phenology/ecoregions/biomes.csv")) %>%
  filter(is.na(out)) %>%
  filter(!is.na(broadclass)) %>%
  #dplyr::select(WWF_MHTNAM) %>%
  #unique() %>%
  #write_csv("/home/marco/Desktop/biomes.csv")
  #filter(WWF_MHTNAM %in% c("Tropical and Subtropical Coniferous Forests")) %>%
  #inner_join(grid5km) %>%
  #dplyr::select(x,y,cat) %>%
  #st_as_sf(coords=c("x","y")) %>%
  #st_write("/home/marco/Desktop/test2.shp",delete_layer = TRUE)
  filter(forest_prop > 0.75) %>% {.->>cc} %>%
  inner_join(read_csv("./ndvi_maxdif/ndvi_dif5km",col_names = F) %>%
               rename (cat = X1,ndvi_dif=X2)) %>%
  group_by(broadclass) %>%
  nest() %>%
  mutate(res = map(data,filtoutliers)) %>%
  #dplyr::select(-c(data)) %>%
  #unnest(res) %>%
  dplyr::select(-c(res)) %>%
  unnest(data) %>%
  #filter(dif > -50.76106 & dif < 60.48476) %>%
  filter(ndvi_dif > -0.15) %>%
  filter(dif > -200 & dif < 200) %>%
  ggplot(aes(x=ndvi_dif,y=dif))+theme_minimal()+geom_bin2d(bins=60)+
  geom_smooth(method = "lm",color="red",size=1,linetype="dashed",se=FALSE)+
  scale_fill_viridis(option = 'magma',trans="log10")+
  theme(legend.position = "none",axis.text = element_text(size=12,colour="black"),
        axis.title = element_text(size=20),
        plot.title = element_text(hjust = 0.5,size=20),
        strip.text=element_text(size=18))+xlab("NDVI P2 vs. P1")+
  ylab("Pheno Div P2 vs. P1")+facet_wrap(~broadclass,scales = "fixed")->pndvi3

read_csv("./ndvi_maxdif/ndvi_dif5km",col_names = F) %>%
  rename(cat = X1,ndvi_dif=X2)->dd

ggsave(pndvi3,filename = "./figures/paper/NDVI_PHENOLOGY_SCATTER_biome_reduced.png",
       width = 10,height = 9,dpi=400,bg="white")





st_read("./KG/kg_codesv1.shp")->dd 
  plot(dd["lvl2_tx"])



# main plot example!
dat %>%
  mutate(lvl2_tx = paste(lvl1_tx,lvl2_tx)) %>%
  dplyr::select(cat,dif,forest_prop,lvl2_tx) %>%
  filter(forest_prop > 0.75) %>%
  inner_join(read_csv("./ndvi_maxdif/ndvi_dif5km",col_names = F) %>%
               rename(cat = X1,ndvi_dif=X2)) %>%
  filter(dif < 200 & dif > -200) %>%
  filter(ndvi_dif > -0.15) %>%
  filter(!lvl2_tx %in% c("Arid Desert")) %>%
  filter(lvl2_tx=="Continental Dry winter") %>%
  ggplot(aes(x=ndvi_dif,y=dif))+theme_minimal()+geom_bin2d(bins=60)+
  geom_smooth(method = "lm",color="red",size=1,linetype="dashed",se=FALSE)+
  scale_fill_viridis(option = 'magma',trans="log10")+
  theme(legend.position = "none",axis.text = element_text(size=12,colour="black"),
        axis.title = element_text(size=20),
        plot.title = element_text(hjust = 0.5,size=20))+xlab("NDVI P2 vs. P1")+
  ylab("Pheno Div P2 vs. P1")+facet_wrap(~lvl2_tx,scales = "fixed")->tmpmain

# inset plot
st_read("./KG/kg_codesv1.shp") %>%
  inner_join(dat %>%
               dplyr::select(lvl1_tx,lvl2_tx) %>%
               unique()) %>%
  mutate(lvl2_tx = paste(lvl1_tx,lvl2_tx)) %>%
  filter(lvl2_tx=="Continental Dry winter")->tmp

ggplot()+geom_sf(data = world, fill = "white",size=0.2,colour="black")+theme_linedraw()+
  geom_sf(data = tmp,fill="red",size=0.2,colour="black")+theme(panel.grid.major = element_blank())->w1

tmpmain+
annotation_custom(ggplotGrob(w1), xmin = 0.01, xmax = 0.12, 
                  ymin = 120, ymax = 180)



# greg's trends -----------------------------------------------------------------------
raster("./gregdataset/dif_1dg.tif") %>%
  projectRaster(crs=CRS("+proj=robin +lon_0=0 +x_0=0 
                +y_0=0 +ellps=WGS84 +datum=WGS84 
                        +units=m +no_defs")) %>%
  rastertodf(.) %>%
  mutate(value1=cut(value,breaks=round(unique(quantile(value,
  probs = seq(0, 1, length.out = 7 + 1))),digits = 2))) %>%
  mutate(title = paste0("Greg's delta"))->tmpdf

tmpdf %>%
  mutate(value1=cut(value,breaks=round(unique(quantile(value,probs = seq(0, 1, length.out = 7 + 1))),digits = 2))) %>%
  #mutate(value2=as.factor(as.numeric(cut(value,breaks=breaksinfo)))) %>% 
  filter(!is.na(value1)) %>% {.->>tmp} %>%
  ggplot()+
  geom_sf(data=bbox_rb, colour='black',fill='black')+theme_classic()+
  geom_sf(data=wmap_rb, fill='grey70',size=0)+
  geom_sf(data=ocean_rb,fill='grey20',size=0)+
  geom_sf(data=wmap_rb, linetype="dotted", color="grey70",size=0.2)+
  geom_tile(data=,aes(x=x,y=y,fill=value))+
  theme(legend.position="bottom",legend.title = element_blank(),
        legend.justification=c(0.5),legend.key.width=unit(35, "mm"),
        legend.text = element_text(size = rel(2)),legend.key.size = unit(10,"mm"),
        strip.background = element_rect(colour = "white", fill = "white"),
        strip.text = element_text(size = rel(2), face = "bold"))+
  scale_fill_gradient2(low ="blue", mid = "white",high ="red",oob=squish,limits=c(-15,15))+
  xlab("")+ylab("")->p


ggsave(p,filename = "./figures/Aug21/trends_9yearsv1.png",height=9,width = 12,dpi=400)



# trends in spat heterogeneity of climate ---------------------------------


read_csv("/home/marco/Desktop/temp_dif.csv") %>%
  filter(dif > -0.05  & dif < 0.05)->dd


read_csv("/home/marco/Desktop/temp_dif.csv") %>%
  dplyr::select(cat,dif) %>%
  rename(tempdif = dif) %>%
  inner_join(dat) %>%
  filter(forest_prop > 0.75) %>%
  filter(dif < 200 & dif > -150) %>%
  filter(tempdif > -0.10)->tmp

tmp %>%
  mutate(posneg = ifelse(tempdif < 0,"neg","pos")) %>%
  group_by(posneg) %>%
  summarise(count = n())
  
  ggplot(aes(x=tempdif,y=dif))+theme_minimal()+geom_bin2d(bins=60)+geom_smooth(method = "lm",color="red",size=1,
                                                                                linetype="dashed",se=FALSE)+
  scale_fill_viridis(option = 'magma',trans="log10")+
  theme(legend.position = "none",axis.text = element_text(size=15,colour="black"),axis.title = element_text(size=20),
        plot.title = element_text(hjust = 0.5,size=20))+xlab("NDVI P2 vs. P1")+ylab("Pheno Div P2 vs. P1")->ptemp




read_csv("./ndvi_maxdif/ndvi_dif5km",col_names = F) %>%
             rename(cat = X1,ndvi_dif=X2) %>%
  inner_join(grid5km) %>%
  dplyr::select(x,y,ndvi_dif) %>%
  rasterFromXYZ(crs=latlon) %>%
  writeRaster(filename = "/home/marco/Desktop/ndvimax.tif")


# test data ---------------------------------------------------------------
dat %>%
  filter(value < 180) ->dd

mod<-

dat %>%
  inner_join(grid5km) %>%
  mutate(res=residuals(lm(value~bio1,data=.))) %>%
  dplyr::select(x,y,res,year,cat) %>%
  inner_join(read_csv("./tmp/ecoregions.csv")) %>%
  group_by(WWF_MHTNAM,year) %>%
  summarise(median = median(res))->dd
  
dd %>%
  ggplot(aes(x=year,y=median))+geom_smooth(method = "lm")+facet_wrap(~WWF_MHTNAM,scales = "free")
  
  
  
# climatedifference -------------------------------------------------------
fread("/home/marco/alessandro_metric/terraclimate_combined.csv")->clim
2003:2011->t1
2012:2020->t2

clim %>%
  inner_join(read_csv("tmp/gridstats025degree",col_names = FALSE) %>%
               rename(cat = X1,cat025d=X2)) %>%
  group_by(cat025d,year) %>%
  summarise(bio_01 = median(bio1))->tmp

tmp %>%
  filter(year %in% t1) %>%
  group_by(cat025d) %>%
  summarise(bio_01 = median(bio_01,na.rm = TRUE)) %>%
  rename(bio_01_p1 = bio_01)->p1

tmp %>%
  filter(year %in% t2) %>%
  group_by(cat025d) %>%
  summarise(bio_01 = median(bio_01,na.rm = TRUE)) %>%
  rename(bio_01_p2 = bio_01)->p2  

inner_join(p1,p2) %>%
  mutate(dif_bio1 = bio_01_p2-bio_01_p1) %>%
  write_csv("/home/marco/Desktop/difference/temperature.csv")

  

# postprocess lm interaction results --------------------------------------
read_csv("/home/marco/Desktop/results.csv") %>%
  filter(term=="bio1") %>%
  filter(p.value < 0.05) %>%
  dplyr::select(cat,estimate)->dd


read_csv("./tmp/ecoregions.csv") %>%
  filter(!WWF_MHTNAM %in% c("Inland Water","Mangroves")) %>%
  inner_join(read_csv("/home/marco/Dropbox/phenology/ecoregions/biomes.csv")) %>%
  filter(is.na(out)) %>%
  filter(!is.na(broadclass)) %>%
  inner_join(dd) %>%
  mutate(pos_neg = ifelse(estimate < 0,"neg","pos")) %>%
  group_by(WWF_MHTNAM,pos_neg) %>%
  summarise(bcount = n())->res




# test mixed models -------------------------------------------------------
read_csv("/home/marco/Desktop/datglmm.csv") %>%
  group_by(cat1d) %>%
  summarise(count = n()) %>%
  filter(count > 68) %>%
  dplyr::select(-c(count)) %>%
  inner_join(read_csv("/home/marco/Desktop/datglmm.csv")) %>%
  group_by(cat1d) %>%
  mutate(bio1 = as.numeric(scale(bio1)))->dd
  nest()->dd

mod_fit<-function(x){
  mod<-lmer(value~bio1+(1|cat),data=x)
  tibble(beta = summary(mod)$coefficients[2,1],
         tval = summary(mod)$coefficients[2,3])->res
  return(res)
}


dd %>%
  mutate(modres=map(data,mod_fit))->dd1


read_csv("./tmp/ecoregions.csv") %>%
  filter(!WWF_MHTNAM %in% c("Inland Water","Mangroves")) %>%
  inner_join(read_csv("/home/marco/Dropbox/phenology/ecoregions/biomes.csv")) %>%
  
  filter(is.na(out)) %>%
    
  filter(!is.na(broadclass)) %>%
  inner_join(dd1 %>%
               unnest(cols = modres)) %>%
  mutate(pos_neg = ifelse(beta < 0,"neg","pos")) %>%
  group_by(WWF_MHTNAM,pos_neg) %>%
  summarise(bcount = n())->res

dd1 %>%
  unnest(modres) %>%
  mutate(tval1 =abs(tval)) %>%
  filter(tval1 > 2) %>%
  mutate(pos_neg = ifelse(beta < 0,"neg","pos")) %>%
  group_by(pos_neg) %>%
  summarise(count =n())

mod<-lmer(value~bio1+(1|cat2d/cat),data=dd)


# lm models ---------------------------------------------------------------


read_csv("/home/marco/Desktop/lmresults/lmresults.csv") %>%
  #filter(p.value < 0.05) %>%
  inner_join(read_csv("./tmp/ecoregions.csv") %>%
  filter(!WWF_MHTNAM %in% c("Inland Water","Mangroves")) %>%
  inner_join(read_csv("/home/marco/Dropbox/phenology/ecoregions/biomes.csv")) %>%
  filter(is.na(out)) %>%
  filter(!is.na(broadclass)))->dd 

dd %>%
  group_by(WWF_MHTNAM) %>%
  summarise(count = n()) %>%
  inner_join(dd %>%
  filter(p.value < 0.05) %>%
  group_by(WWF_MHTNAM) %>%
  summarise(count_sig = n())) %>%
  mutate(prop = count_sig/count) %>%
  arrange(desc(prop))

dd %>%
  group_by(broadclass) %>%
  summarise(count = n()) %>%
  inner_join(dd %>%
               filter(p.value < 0.05) %>%
               group_by(broadclass) %>%
               summarise(count_sig = n())) %>%
  mutate(prop = count_sig/count) %>%
  arrange(desc(prop))

dd %>%
  mutate(pos_neg = ifelse(beta < 0,'neg','pos')) %>%
  group_by(broadclass,pos_neg) %>%
  summarise(n = n())



# postprocessing 025 degree lm models - only change -----------------------

raster(res=0.25)->myr
rasterize(world,myr) %>%
  rastertodf() %>%
  mutate(value = 1:length(value)) %>%
  rename(cat025d = value)->g025

read_csv("./jeodpp_results/results_lm_025d.csv") %>%
  filter(term =="p1_p2p2") %>%
  filter(p.value < 0.05) %>%
  inner_join(g025)->cc
  
mod<-gam(estimate~s(x)+s(y),data=cc)

g025 %>%
  inner_join(read_csv("./jeodpp_results/results_lm_025d.csv")) %>%
  dplyr::select(x,y)->cc1 


  mutate(value = predict(mod,cc1)) %>%
  #rasterFromXYZ(crs = latlon) %>%
  #writeRaster(filename = "/home/marco/Desktop/change1.tif",overwrite = TRUE)
cc %>%
  st_as_sf(coords =c("x","y"))->cc_sf

fit_NN <- gstat::gstat( # using package {gstat} 
  formula = estimate ~ 1,    # The column `NH4` is what we are interested in
  data = as(cc_sf, "Spatial"), # using {sf} and converting to {sp}, which is expected
  nmax = 10, nmin = 3 # Number of neighboring observations used for the fit
)


g025 %>%
  mutate(x1 =x,y1 = y) %>%
  st_as_sf(coords=c("x1","y1"))->predgrid

predict(fit_IDW,predgrid) %>%
  as_tibble() %>%
  dplyr::select(var1.pred) %>%
  bind_cols(g025) %>%
  dplyr::select(x,y,var1.pred) %>%
  rasterFromXYZ(crs = latlon) %>%
  writeRaster(filename = "/home/marco/Desktop/idw.tif")
  


interp_TIN <- raster::rasterFromXYZ(fit_IDW, crs = latlon)


fit_TIN <- interp::interp( # using {interp}
  x = g025$X,           # the function actually accepts coordinate vectors
  y = g025$Y,
  z = pts_NH4$NH4,
  xo = g025$X,     # here we already define the target grid
  yo = g025$Y,
  output = "points"





library(gstat)
gs <- gstat(formula=prec~1, locations=dta)
idw <- interpolate(r, gs)
## [inverse distance weighted interpolation]
idwr <- mask(idw, vr)
plot(idwr)


predict(mod,)

  dplyr::select(x,y,estimate) %>%
  rasterFromXYZ(crs=latlon)->change




raster::aggregate(change,fact=8,fun=mean)->change1
writeRaster(change1,filename = "/home/marco/Desktop/change.tif")
dim(read_csv("./jeodpp_results/results_lm_025d.csv"))[1]->all

%>%
  
  inner_join(read_csv("./tmp/ecoregions025degree.csv") %>%
               rename(cat025d = cat1d)) %>%
  group_by(WWF_MHTNAM) %>%
  ggplot(aes(x=estimate,fill="red"))+geom_density(alpha=.25)+
  theme_minimal()+facet_wrap(~WWF_MHTNAM,scale="free",labeller = label_wrap_gen(width=30))

  
  summarise(count = n())


