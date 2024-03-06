#######################################################
# Data exploration and validation against other products
########################################################

# preamble stuff ----------------------------------------------------------
# load required libraries
library(raster);library(tidyverse)
library(marcoUtils);library(vegan)
library(scales);library(RColorBrewer)
library(viridis);library(phenoutils)
library(relaimpo);library(performance)
library(furrr);library(ggcorrplot)
library(sf)

tibble(variables =c("bio_01","bio_02","bio_03","bio_04","bio_05","bio_06","bio_07","bio_08","bio_09",
                    "bio_10","bio_11","bio_12","bio_13","bio_14","bio_15","bio_16","bio_17",
                    "bio_18","bio_19","pet_mean","aridity","mean_elev","mean_slope","std_elev","std_slope",
                    "popdensity","humi") ,
       variables_renamed=c("Annual Mean Temperature","Mean Diurnal Range","Isothermality","Temperature Seasonality",
                           "Max Temperature of Warmest Month","Min Temperature of Coldest Month",
                           "Temperature Annual Range","Mean Temperature of Wettest Quarter","Mean Temperature of Driest Quarter",
                           "Mean Temperature of Warmest Quarter","Mean Temperature of Coldest Quarter",
                           "Annual Precipitation","Precipitation of Wettest Month","Precipitation of Driest Month",
                           "Precipitation Seasonality","Precipitation of Wettest Quarter","Precipitation of Driest Quarter",
                           "Precipitation of Warmest Quarter","Precipitation of Coldest Quarter","PET","Aridity","mean_elev","mean_slope","std_elev",
                           "std_slope","popdensity","humi"),
                           group =c("Temperature","Temperature","Temperature","Temperature","Temperature","Temperature","Temperature","Temperature",
                                    "Temperature","Temperature","Temperature","Precipitation","Precipitation","Precipitation","Precipitation","Precipitation",
                                    "Precipitation","Precipitation","Precipitation","Precipitation","Precipitation",
                                    "Topography","Topography","Topography","Topography","Human impact (columbia)","Human impact"))->labs

labs %>%
  write_csv("/home/marco/Desktop/variables.csv")
# Read in data ----------------------------------------------------------


  inner_join(grid5km) %>%
  #dplyr::select(x,y) %>%
  #inner_join(read_csv("./data/phenodata.csv") %>%
  #             dplyr::select(-c(y_2003))) %>%
  inner_join(read_csv("./tmp/forestcount",col_names = FALSE) %>%
               rename(cat = X1, count = X2) %>%
               mutate(forest_prop = count /max(count))) %>%
  # add topography data
  inner_join(read_csv("./tmp/slope",col_names = F) %>%
   rename(cat = X1,mean_slope = X2)) %>%
  inner_join(read_csv("./tmp/elev",col_names = F) %>%
               rename(cat = X1,mean_elev = X2)) %>%
  inner_join(read_csv("./tmp/std_elev",col_names = F) %>%
               rename(cat = X1,std_elev = X2)) %>%
  inner_join(read_csv("./tmp/std_slope",col_names = F) %>%
               rename(cat = X1,std_slope = X2)) %>%
  # add jetz data
  inner_join(read_delim("./results_tmp/jetzvsy2002",delim = " ",col_names = F) %>%
  rename(jetz = X1,cat = X2)) %>%
  # add land cover product 
  inner_join(read_delim("/media/marco/marcodata19/validationmetrics/pheno_grid_ESAvalues2002",delim = " ",col_names = F) %>%
  rename(lc=X1,cat=X2,area=X3) %>%
  mutate(lc = paste0("lc_",lc)) %>%
  pivot_wider(names_from = lc,values_from=area,values_fill=0)) %>%
  # add climate data
  inner_join(read_csv("./GRASSGIS_files/bioclim.csv")) %>%
  inner_join(read_csv("./GRASSGIS_files/pet_mean.csv")) %>%
  # add info for resampling in geographic space
  inner_join(read_csv("tmp/gridstats1degree",col_names = FALSE) %>%
               rename(cat = X1,cat1d=X2)) %>%
  inner_join(read_csv("tmp/gridstats025degree",col_names = FALSE) %>%
               rename(cat = X1,cat025d=X2)) %>%
  inner_join(read_csv("tmp/gridstats3degree",col_names = FALSE) %>%
               rename(cat = X1,cat3d=X2)) %>%
  inner_join(read_csv("tmp/gridstats2degree",col_names = FALSE) %>%
               rename(cat = X1,cat2d=X2)) %>%
  # calculate aridity and rescale temp seasonality
  mutate(aridity = pet_mean/bio_12,bio_04 = bio_04/1000) %>%
  mutate(bio_01 = bio_01/10) %>%
  # add population density
  inner_join(read_csv("tmp/humi",col_names = FALSE) %>%
               rename(cat = X1,humi=X2)) %>%
  # ecoregions
  inner_join(read_csv("./tmp/ecoregions.csv"))->phenodat

# aggregate data
phenodat %>%
  filter(forest_prop > 0.85) %>%
  dplyr::select(-c(cat,WWF_MHTNUM,WWF_MHTNAM)) %>%
  pivot_longer(cols=-c(cat025d),values_to = "values",names_to = "variables") %>%
  group_by(cat025d,variables) %>%
  summarise(values = mean(values)) %>%
  pivot_wider(names_from = "variables",values_from = "values",id_cols = c("cat025d"))->phenodat1 




#phenodat %>%
#  filter(forest_prop > 0.85) %>%
#  dplyr::select(-c(cat,WWF_MHTNUM,WWF_MHTNAM)) %>%
#  pivot_longer(cols=-c(cat2d),values_to = "values",names_to = "variables") %>%
#  group_by(cat2d,variables) %>%
#  summarise(values = mean(values)) %>%
#  pivot_wider(names_from = "variables",values_from = "values",id_cols = c("cat2d"))->phenodat2

# Summary by ecoregions ----------------------------------------------------

read_csv("./tmp/ecoregions1degree.csv") %>%
  inner_join(phenodat1) %>%
  filter(!WWF_MHTNAM %in% c("Flooded Grasslands and Savannas","Mangroves",
                            "Inland Water","Rock and Ice")) %>%
  dplyr::select(y_2004,WWF_MHTNAM) %>%
  ggplot()+geom_boxplot(aes(x=WWF_MHTNAM,y=y_2004,fill=WWF_MHTNAM))+theme_minimal()+
  theme(axis.text.x = element_text(hjust = 1, angle = 45, size = 11,colour ="black"),
        axis.text.y=element_text(size=11,colour="black"),legend.position = "none")+
  xlab("Ecoregion")+ylab("Pheno diversity")->bplot
  
ggsave(bplot,filename = "./figures/July21/bplot1degree.png",height=8,width = 10,dpi=400,bg="white")
  
phenodat %>%
  filter(y_2004 < 400) %>%
  filter(!WWF_MHTNAM %in% c("Flooded Grasslands and Savannas","Mangroves",
                            "Inland Water","Rock and Ice")) %>%
  dplyr::select(y_2004,WWF_MHTNAM) %>%
  ggplot()+geom_boxplot(aes(x=WWF_MHTNAM,y=y_2004,fill=WWF_MHTNAM))+theme_minimal()+
  theme(axis.text.x = element_text(hjust = 1, angle = 45, size = 11,colour ="black"),
        axis.text.y=element_text(size=11,colour="black"),legend.position = "none")+
  xlab("Ecoregion")+ylab("Pheno diversity")->bplot

ggsave(bplot,filename = "./figures/July21/bplot.png",height=8,width = 10,dpi=400,bg="white")

# model by ecoregions ----------------------------------------------------

read_csv("./tmp/ecoregions1degree.csv") %>%
  inner_join(phenodat1) %>%
  filter(!WWF_MHTNAM %in% c("Flooded Grasslands and Savannas","Mangroves",
                            "Inland Water","Rock and Ice","Montane Grasslands and Shrublands",
                            "Tropical and Subtropical Coniferous Forests")) %>%
  pivot_longer(names_to = "variables",values_to = "values",cols = -c(y_2004,cat1d,WWF_MHTNUM,WWF_MHTNAM)) %>%
  group_by(WWF_MHTNAM,variables) %>%
  mutate(values = as.numeric(base::scale(values))) %>%
  pivot_wider(names_from =variables,values_from = values) %>%
  group_by(WWF_MHTNAM) %>%
  nest() %>%
  mutate(mod = map(data,~lm(y_2004~ bio_04+bio_01+aridity+bio_12+humi+std_elev+bio_03+bio_08+bio_07,data=.x)),tidied = map(mod, tidy),
         fit=map(mod,glance)) %>%
  unnest(cols = tidied) %>%
  filter(term!="(Intercept)") %>%
  arrange(WWF_MHTNAM,desc(statistic)) %>%
  dplyr::select(WWF_MHTNAM,term,estimate) %>%
  rename(variables = term) %>%
  inner_join(labs) %>%
  #mutate(statistic = abs(statistic)) %>%
  #filter(term=="humi") %>%
  ggplot(aes(x=WWF_MHTNAM,y=estimate,fill = WWF_MHTNAM))+geom_bar(stat="identity", position="dodge")+theme_bw()+
  #theme(axis.text.x =element_text(hjust = 1, angle = 45))+
  facet_wrap(~variables_renamed,labeller = label_wrap_gen(width=5),scales = "free")+theme(axis.text.x = element_blank())->implocal

ggsave(implocal,filename = "./figures/July21/implocal.png",width = 13,height=10,dpi = 400)


# climatic dissimilarity --------------------------------------------------


climdis<-function(x){
  x %>%
    dplyr::select(!!c(paste0("bio_0",1:9),paste0("bio_",10:19))) %>%
    decostand(.,method="standardize") %>%
    dist() %>%
    mean() %>%
    as_tibble()->res
  return(res)
}

plan(multisession, workers = 7)

phenodat %>%
  group_by(cat2d) %>%
  nest() %>%
  ungroup() %>%
  mutate(dis = future_map(data,~climdis(.)))->res

res %>%
  dplyr::select(-c(data)) %>%
  unnest(dis) %>%
  rename(dis = value) %>%
  inner_join(phenodat1)->mydf

mydf %>%
  #dplyr::select(y_2003,dis,std_elev) %>%
  filter(y_2004 < 200) %>%
  filter(dis < 1000) %>%
  na.exclude()->mydf1

model1 <- psem(lm(y_2004 ~std_elev+dis,mydf1),lm(dis  ~ std_elev, mydf1))
cat('-----indirect effects of std_elev on climate + direct effects of climate----\n') 
AIC(model1)
BIC(model1)
cat('---- climate driven model----\n') 
model2 <- psem(lm(y_2004 ~dis,mydf1))
AIC(model2)
BIC(model2)
cat('----topography driven model----\n') 
model3 <- psem(lm(y_2004 ~std_elev,mydf1))
AIC(model3)
BIC(model3)

mydf2<-as.data.frame(mydf1)
mod<-lm(y_2004 ~dis+std_elev,data=mydf1)

b <- summary(MOD)$coef[-1, 1]
sx <- sd(MOD$model[-1])
sy <- sd(MOD$model[1])
beta <- b * sx/sy



mydf %>%
  filter(y_2004 < 200) %>%
  filter(dis < 1000) %>%
  #mutate(y_2004 = log(y_2004),dis = log(dis)) %>%
  #filter(y_2004 > 0) %>%
  ggplot(aes(y=y_2004,x=dis))+geom_point(size=4,color="red",alpha=0.5)+geom_smooth(method ="lm")+
  theme_minimal()

->cor


# collinearity ------------------------------------------------------------



# multicollinearity


phenodat1 %>%
  ungroup() %>%
  dplyr::select(bio_01,bio_12,bio_15,bio_04,aridity) %>%
  cor()->corr


mod<-randomForest()

cor(dd)->corr

lambda_seq <- 10^seq(2, -2, by = -.1)
# Using glmnet function to build the ridge regression in r
fit <- glmnet(x_var, y_var, alpha = 0, lambda  = lambda_seq)
# Checking the model
summary(fit)


ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE)

hc = findCorrelation(corr, cutoff=0.9) # putt any value as a "cutoff" 
hc = sort(hc)
reduced_Data = dd[,-c(hc)]
print (reduced_Data)
# choose a set of core variables (meantemp, totprec, sd temperature,sd prec,aridity)
# bio_01,bio_12,bio_04,bio_15,

labs %>%
  filter(variables_renamed=="Temperature Seasonality")

# Correlation with other products -----------------------------------------
# plot correlation between jetz's product and phenology one
phenodat1 %>%
  filter(y_2004 < 200) %>%
  #filter(bio_12 < 4500) %>%
  filter(forest_prop > 0.85) %>% {.->>tmp}  %>%
  ggplot(aes(x=jetz,y=y_2004))+theme_minimal()+geom_bin2d(bins=60)+geom_smooth(method = "lm",color="red",size=1,
                                                                                          linetype="dashed",se=FALSE)+
  scale_fill_gradient(low="lightblue1",high="darkblue",trans="log10")+xlab("Jetz - standard deviation of EVI (composite)")+ylab("Phenology dataset (Year 2004)")+
  ggtitle("Spearman rho = 0.40")+
  theme(legend.position = "none",axis.text = element_text(size=15,colour="black"),axis.title = element_text(size=20),
        plot.title = element_text(hjust = 0.5,size=20))->jetzcor


cor(tmp$jetz,tmp$y_2004,method = "spearman")

ggsave(jetzcor,filename="/mnt/data1tb/Dropbox/phenology/figures/June21/jetzcom.png",width=8,height = 8,dpi = 400)




# import land cover data and calculate simpson's index
phenodat %>%
  filter(y_2004 < 200) %>%
  filter(forest_prop > 0.85) %>%
  mutate(S = diversity(.[20:31],index = "simpson"),H = diversity(.[20:31],index = "shannon"),invsimp = diversity(.[20:31],index = "invsimpson")) %>% 
  mutate(rowsum = rowSums(.[20:31])) %>%
  filter(rowsum > 0) %>%
  dplyr::select(y_2004,cat1d,S,H,invsimp,bio_01,bio_12) %>%
  pivot_longer(names_to = "variables",values_to ="values",cols = -c(cat1d)) %>%
  group_by(variables,cat1d) %>%
  summarise(values = mean(values)) %>%
  ungroup() %>%
  pivot_wider(names_from = variables,values_from=values) %>% {.->>tmp} %>%
  ggplot(aes(x=S ,y=y_2004))+theme_minimal()+geom_bin2d(bins=30)+geom_smooth(method = "lm",color="red",size=1,
                                                                                        linetype="dashed",se=FALSE)+
  scale_fill_gradient(low="lightblue1",high="darkblue",trans="log10")+xlab("Simpson's div. index from ESA CCI")+ylab("Phenology dataset (Year 2002)")+
  ggtitle("Spearman rho = 0.25")+
  theme(legend.position = "none",axis.text = element_text(size=15,colour="black"),axis.title = element_text(size=20),
        plot.title = element_text(hjust = 0.25,size=20))->esacci


cor(tmp$S,tmp$y_2004,method = "spearman")
cor(log(tmp$S),log(tmp$y_2004),method = "spearman")
cor(tmp$H,tmp$y_2004,method = "spearman")

ggsave(esacci,filename="/mnt/data1tb/Dropbox/phenology/figures/June21/esacci.png",width=8,height = 8,dpi = 400)

# import land cover data and calculate simpson's index
phenodat %>%
  filter(!is.na(y_2004)) %>%
  filter(y_2004 < 200) %>%
  filter(forest_prop > 0.85) %>%
  mutate(S = diversity(.[20:31],index = "simpson"),H = diversity(.[20:31],index = "shannon"),invsimp = diversity(.[20:31],index = "invsimpson")) %>% 
  mutate(rowsum = rowSums(.[20:31])) %>%
  filter(rowsum > 0) %>%
  mutate(y_2004_res = residuals(gam(y_2004~std_elev))) %>%
  dplyr::select(y_2004,cat1d,S,H,invsimp,bio_01,bio_12,y_2004_res) %>%
  pivot_longer(names_to = "variables",values_to ="values",cols = -c(cat1d)) %>%
  group_by(variables,cat1d) %>%
  summarise(values = mean(values)) %>%
  ungroup() %>%
  pivot_wider(names_from = variables,values_from=values) %>% {.->>tmp} %>%
  ggplot(aes(x=S ,y=y_2004))+theme_minimal()+geom_bin2d(bins=30)+geom_smooth(method = "lm",color="red",size=1,
                                                                             linetype="dashed",se=FALSE)+
  scale_fill_gradient(low="lightblue1",high="darkblue",trans="log10")+xlab("Simpson's div. index from ESA CCI")+ylab("Phenology dataset (Year 2002)")+
  ggtitle("Spearman rho = 0.25")+
  theme(legend.position = "none",axis.text = element_text(size=15,colour="black"),axis.title = element_text(size=20),
        plot.title = element_text(hjust = 0.25,size=20))->esacci


cor(tmp$S,tmp$y_2004,method = "spearman")
cor(log(tmp$S),log(tmp$y_2004),method = "spearman")
cor(tmp$H,tmp$y_2004,method = "spearman")

ggsave(esacci,filename="/mnt/data1tb/Dropbox/phenology/figures/June21/esacci.png",width=8,height = 8,dpi = 400)

phenodat %>%
  filter(!is.na(y_2004)) %>%
  filter(y_2004 < 200) %>%
  filter(forest_prop > 0.85) %>%
  mutate(bleaf = lc_50+lc_60+lc_61+lc_62,needlel = lc_70+lc_71+lc_72+lc_80+lc_81+lc_82)  %>%
  mutate(deltatreediv = bleaf/1000000 - needlel/1000000) %>%
  mutate(S = diversity(.[20:31],index = "simpson"),H = diversity(.[20:31],index = "shannon"),invsimp = diversity(.[20:31],index = "invsimpson")) %>% 
  mutate(rowsum = rowSums(.[20:31])) %>%
  filter(rowsum > 0) %>%
  mutate(y_2004_res = residuals(gam(y_2004~std_elev))) %>%
  dplyr::select(y_2004,cat1d,S,H,invsimp,bio_01,bio_12,y_2004_res,deltatreediv,20:31) %>%
  pivot_longer(names_to = "variables",values_to ="values",cols = -c(cat1d)) %>%
  group_by(variables,cat1d) %>%
  summarise(values = mean(values)) %>%
  ungroup() %>%
  pivot_wider(names_from = variables,values_from=values) %>% {.->>tmp} %>%
  ggplot(aes(x=deltatreediv ,y=y_2004))+theme_minimal()+geom_bin2d(bins=30)+geom_smooth(method = "lm",color="red",size=1,
                                                                             linetype="dashed",se=FALSE)+
  scale_fill_gradient(low="lightblue1",high="darkblue",trans="log10")+xlab("delta (area bleaf - area ndleaf")+ylab("Phenology dataset (Year 2002)")+
  ggtitle("Spearman rho = -0.12")+
  theme(legend.position = "none",axis.text = element_text(size=15,colour="black"),axis.title = element_text(size=20),
        plot.title = element_text(hjust = 0.25,size=20))->esacci1

ggsave(esacci1,filename="/mnt/data1tb/Dropbox/phenology/figures/June21/esacci_delta.png",width=8,height = 8,dpi = 400)

cor(tmp$deltatreediv,tmp$y_2004)

tibble(cat=names(phenodat)[20:31]) %>%
inner_join(esacats1 %>%
             mutate(cat= paste0("lc_",cat)))
  

#--------------------- Map production 

# Pheno Diversity map --------------------------------------------------------------------
#pheno_2002 %>%

yearl<-2003:2020
# res comes from latest results
for(i in 1:length(yearl)){
  print(i)
  # yearly maps!
  res %>%
    filter(year==yearl[i]) %>%
    inner_join(grid5km) %>%
    dplyr::select(x,y,value) %>%
    rasterFromXYZ(crs=latlon) %>%
    projectRaster(crs=CRS("+proj=robin +lon_0=0 +x_0=0 
                +y_0=0 +ellps=WGS84 +datum=WGS84 
                        +units=m +no_defs"))->myr1
  
  #phenodat %>%
  #  dplyr::select(x,y,y_2004) %>%
  #  rasterFromXYZ(crs=latlon) %>%
  #  projectRaster(.,crs=CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))->myr1
  
  myr1 %>%
    rastertodf(.) %>%
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
          legend.text = element_text(size = rel(4)),
          strip.background = element_rect(colour = "white", fill = "white"),
          strip.text = element_text(size = rel(2), face = "bold"))+
    scale_fill_manual(labels =mylabs,values = mypal,
                      guide = guide_legend(direction = "horizontal",
                                           keyheight = unit(3, units = "mm"),keywidth = unit(150 / length(unique(tmp$value1)), 
                                                                                             units = "mm"),title.position = 'top',title.hjust = 0.5,label.hjust = 1,
                                           nrow = 1,byrow = T,reverse = F,label.position = "bottom"))+xlab("")+ylab("")+facet_wrap(~title)->p
  
  fileout<-paste0("./figures/Aug21/Y_",yearl[i],".png")
  ggsave(p,filename = fileout,height=9,width = 12,dpi=400)
  rm(tmpdf)
  rm(myr1)
  rm(mylabs)
  rm(mypal)
  rm(tmplabs)
  rm(p)
  rm(fileout)
  
}


# Jetz map ----------------------------------------------------------------

grid5km %>%
  inner_join(read_delim("./results_tmp/jetzvsy2002",delim = " ",col_names = F) %>%
               rename(jetz = X1,cat = X2)) %>%
  dplyr::select(x,y,jetz) %>%
  rasterFromXYZ(crs=latlon)->dd

raster("/home/marco/Desktop/tmp2.tif") %>%
#%>%
# projectRaster(.,crs=CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"),over = TRUE)->dd
  rastertodf(.) %>%
  mutate(value1=cut(value,breaks=round(unique(quantile(value,probs = seq(0, 1, length.out = 7 + 1))),digits = 2))) %>%
  mutate(title = "Jetz product")->tmpdf


# create labels
str_replace(unique(tmpdf$value1),"]","") %>%
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
                                         nrow = 1,byrow = T,reverse = F,label.position = "bottom"))+xlab("")+ylab("")+facet_wrap(~title)->p1

ggsave(p1,filename = "./figures/May21/jetz_sdv1.png",height=9,width = 12,dpi=400)

# ESA CCI map -------------------------------------------------------------
grid5km %>%
  inner_join(lcdiv1 %>%
               dplyr::select(S,cat)) %>%
  #left_join(lcdiv1 %>%
               #dplyr::select(S,cat)) %>%
  #mutate(S = ifelse(is.na(S),0,S)) %>%
  dplyr::select(-c(cat)) %>%
  rasterFromXYZ(crs = latlon) %>%
  projectRaster(.,crs=CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) %>%
  rastertodf(.) %>%
  as_tibble() %>%
  #mutate(value1=cut(value,breaks=round(unique(quantile(value,probs = seq(0, 1, length.out = 7 + 1))),digits = 2),include.lowest=T)) %>%
  mutate(value1 = cut(value,breaks=c(0.00,0.01,0.11,0.26,0.35,0.41,0.52,0.82),include.lowest = T)) %>%
  mutate(title = "ESA CCI forest classes")->tmpdf

tmpdf %>% pull(value)->value
round(unique(quantile(value,probs = seq(0, 1, length.out = 7 + 1))),digits = 2)
cut(value,breaks=round(unique(quantile(value,probs = seq(0, 1, length.out = 7 + 1))),digits = 2),include.lowest=T) %>%
  summary()

cut(value,breaks=c(0.00,0.01,0.11,0.26,0.35,0.41,0.52,0.82),include.lowest = T) %>%
  summary()

round(unique(quantile(value,probs = seq(0, 1, length.out = 7 + 1))),digits = 2)


# create labels
str_replace(unique(tmpdf$value1),"]","") %>%
  str_replace("\\(","") %>%
  str_split_fixed(",",n=2) %>%
  as.data.frame() %>%
  as_tibble() %>%
  mutate(V1=str_replace(V1,"\\[|\\]","")) %>%
  filter(V1!="") %>%
  mutate(V1=as.numeric(as.character(V1)),V2=as.numeric(as.character(V2))) %>%
  arrange(V1)->tmplabs

mylabs<-c(tmplabs[2:7,]$V1,paste0(">", tmplabs[1:7,]$V2[6]))

rev(brewer.pal(7,"Spectral"))->mypal

# smallest unit
tmpdf %>%
  #mutate(value1=cut(value,breaks=round(unique(quantile(value,probs = seq(0, 1, length.out = 6 + 1))),digits = 2),include.lowest=T)) %>%
  #mutate(value2=as.factor(as.numeric(cut(value,breaks=breaksinfo)))) %>% 
  filter(!is.na(value1)) %>% {.->>tmp} %>%
  ggplot()+
  geom_sf(data=bbox_rb, colour='black',fill='black')+theme_classic()+
  geom_sf(data=wmap_rb, fill='greyn70',size=0)+
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
                                         nrow = 1,byrow = T,reverse = F,label.position = "bottom"))+xlab("")+ylab("")+facet_wrap(~title)->p2

ggsave(p2,filename = "./figures/May21/ESACCI_diversity.png",height=9,width = 12,dpi=400)




# Univariate scatterplots with climate variables ----------------------------------------------------------------------

# climate
phenodat1 %>%
  pivot_longer(cols=-c(cat1d,y_2004,forest_prop,x,y),values_to = "values",names_to = "variables") %>%
  filter(y_2004 < 200) %>% {.->> tmp} %>%
  group_by(variables) %>%
  mutate(values = scales::rescale(values,to=c(0,1))) %>%
  inner_join(labs) %>%
  ggplot(aes(x=values,y=y_2004))+theme_minimal()+geom_bin2d(bins=60)+geom_smooth(method="lm",se=FALSE,color="red")+
  scale_fill_gradient(low="lightblue1",high="darkblue",trans="log10")+xlab("Predictor")+ylab("Pheno diversity")+
  theme(legend.position = "none",axis.text = element_text(size=12,colour="black"),axis.title = element_text(size=20),
        plot.title = element_text(hjust = 0.5,size=20))+facet_wrap(~variables_renamed)+theme(axis.text.x = element_text(angle = 30,hjust = 1))->uniscatter

ggsave(uniscatter,filename = "./figures/July21/univariatescatterplots.png",width = 12,heigh=11,dpi = 400,bg = "white")
  
# Variable importance  ----------------------------------------------------------------------



mod1 <- lm(y_2004 ~ bio_01+bio_02+bio_03+bio_04+bio_05+bio_06+bio_07+bio_08+bio_09+
             bio_10+bio_11+bio_12+bio_13+bio_14+bio_15+bio_16+bio_17+
             bio_18+bio_19+aridity+std_elev+humi,data = phenodat1)
varimp <- calc.relimp(mod1,type = c("lmg"), rela = TRUE)


tibble(imp = varimp@lmg,variables = names(varimp@lmg)) %>%
  inner_join(labs) %>%
  arrange(imp) %>%
  mutate(variables_renamed = forcats::as_factor(variables_renamed)) %>%
  ggplot()+geom_bar(aes(x=variables_renamed,y=imp,fill=group),stat="identity")+coord_flip()+theme_minimal()+
  xlab("Variable")+ylab("Relative importance")+theme(axis.text=element_text(colour = "black",size=12),
                                                 axis.title = element_text(size=19,colour="black"))+
  scale_fill_manual(values=c("black","deepskyblue3","brown3","dark green"))->vimp1


ggsave(vimp1,filename = "./figures/July21/varimplmg.png",width = 10,height = 7,dpi=400,bg="white")  


t
# Climate space stuff -----------------------------------------------------

# T and P Space
phenodat %>%
  filter(value < 200) %>%
  filter(bio_12 < 4500) %>%
  filter(forest_prop > 0.75) %>% {.->> tmp}  %>%
  ggplot(aes(x=bio_01,y=bio_12,z=value))+
  theme_minimal()+
  stat_summary_2d(fun = function(x) {if(length(x[!is.infinite(x)])>=5)(mean(x))else(NA)},bins = 45,na.rm = FALSE)+
  scale_fill_viridis(option = "magma",limits=c(0,100),oob=squish)+
  xlab("Temperature")+ylab("Precipitation")+theme(axis.text=element_text(size=15),axis.title=element_text(size=20),
                                                  plot.title=element_text(size=30,hjust=0.5))->climspacep
climspacep

ggsave(climspacep,filename = "./figures/July21/P_T_y_2004v1.png",width = 8,height = 8,dpi = 400,bg = NULL)



# Aridity and Temperature seasonality
phenodat %>%
  filter(y_2004 < 200) %>%
  filter(bio_12 < 4500) %>%
  filter(forest_prop > 0.85) %>%
  ggplot(aes(x=bio_04,y=aridity,z=y_2004))+
  stat_summary_2d(fun = function(x) {if(length(x[!is.infinite(x)])>=5)(mean(x))else(NA)},bins = 35,na.rm = FALSE)+
  theme_minimal()+
  #scale_fill_distiller(palette = "RdYlBu",limits=c(3,150),oob=squish)+
  scale_fill_viridis(option = "magma",limits=c(0,100),oob=squish)+
  xlab("Temperature seasonality")+ylab("Aridity")+theme(axis.text=element_text(size=15),axis.title=element_text(size=20),
                                                  plot.title=element_text(size=30,hjust=0.5))->climspacep1

ggsave(climspacep1,filename = "./figures/July21/Aridity_Seasonality_y_2004.png",width = 8,height = 8,dpi = 400,bg="white")


# Precipitation and Temperature seasonality
phenodat %>%
  filter(y_2004 < 200) %>%
  filter(bio_12 < 4500) %>%
  filter(forest_prop > 0.85) %>%
  ggplot(aes(x=bio_04,y=bio_15,z=y_2004))+
  stat_summary_2d(fun = function(x) {if(length(x[!is.infinite(x)])>=5)(mean(x))else(NA)},bins = 35,na.rm = FALSE)+
  theme_minimal()+
  scale_fill_viridis(option = "magma",limits=c(0,100),oob=squish)+
  xlab("Temperature Seasonality")+ylab("Precipitation Seasonality")+theme(axis.text=element_text(size=15),axis.title=element_text(size=20),
                                                        plot.title=element_text(size=30,hjust=0.5))->climspacep2

ggsave(climspacep2,filename = "./figures/July21/Tseasonality_Pseasonality_y_2004.png",width = 8,height = 8,dpi = 400,bg="white")



# Aridity and Annual Mean Temperature
phenodat %>%
  filter(y_2004 < 200) %>%
  filter(bio_12 < 4500) %>%
  filter(forest_prop > 0.85) %>%
  ggplot(aes(x=bio_01,y=aridity,z=y_2004))+
  stat_summary_2d(fun = function(x) {if(length(x[!is.infinite(x)])>=5)(mean(x))else(NA)},bins = 35,na.rm = FALSE)+
  theme_minimal()+
  scale_fill_viridis(option = "magma",limits=c(0,100),oob=squish)+
  xlab("Annual Mean Temperature")+ylab("Aridity")+theme(axis.text=element_text(size=15),axis.title=element_text(size=20),
                                                        plot.title=element_text(size=30,hjust=0.5))->climspacep3

ggsave(climspacep3,filename = "./figures/July21/MAT_aridity_y_2004.png",width = 8,height = 8,dpi = 400,bg="white")

phenodat  %>%
  filter(y_2004 < 200) %>%
  filter(bio_12 < 4500) %>%
  filter(forest_prop > 0.85) %>%
  ungroup() %>%
  dplyr::select(y_2004,bio_12,bio_01,mean_elev,mean_slope,std_elev,std_slope) %>%
  pivot_longer(names_to = "variables",values_to ="values",cols=-c(bio_01,bio_12,y_2004)) %>% 
  group_by(variables) %>%
  mutate(y_2004res = residuals(gam(y_2004~values))) %>%
  filter(variables=="std_elev") %>% {.->>tmp}%>%
  ggplot(aes(x=bio_01,y=bio_12,z=y_2004res))+
  stat_summary_2d(fun = function(x) {if(length(x[!is.infinite(x)])>=5)(mean(x))else(NA)},bins = 35,na.rm = FALSE)+
  theme_minimal()+
  scale_fill_distiller(palette = "RdYlBu",limits=c(-50,90),oob = squish)+
  xlab("Temperature")+ylab("Precipitation")+theme(axis.text=element_text(size=15),axis.title=element_text(size=20),
                                                  plot.title=element_text(size=30,hjust=0.5))+
  scale_y_continuous(limits = c(0, 5000), breaks = c(0,1000,2000,3000,4000))+facet_wrap(~variables)->smoothedtopography
ggsave(smoothedtopography,filename = "./figures/June21/P_T_y_2004_smoothed.png",width = 8,height = 8,dpi = 400)


# Local correlation between phenology and climatic variables -----------------------------------------------------------------------

# original data
phenodat %>%
  filter(y_2004 < 200) %>%
  filter(bio_12 < 4500) %>%
  filter(forest_prop > 0.85) %>% 
  filter(!is.na(y_2004)) %>%
  dplyr::select(-c(cat,WWF_MHTNUM,WWF_MHTNAM)) %>%
  pivot_longer(cols=-c(cat2d,forest_prop,y_2004),values_to = "values",names_to = "variables") %>% {.->>tmp}  %>%
  group_by(cat2d,variables) %>%
  mutate(nobs = length(variables)) %>%
  filter(nobs > 29) %>%
  dplyr::select(-c(nobs)) %>%
  summarise(p.value =  cor.test(values,y_2004,method="spearman")$p.value,corvalues =  cor.test(values,y_2004,method="spearman")$estimate)->cormat

cormat %>%
  ungroup() %>%
  filter(p.value < 0.05) %>%  
  inner_join(labs) %>%
  inner_join(phenodat2 %>%
               dplyr::select(cat2d,bio_01,bio_12) %>%
               filter(bio_12 < 4500))->dd


unique(dd$variables)->varl

for(i in 1:length(varl)){
  print(i)
  dd %>%
    inner_join(tmpgrid1d %>%
                 rename(cat2d =value)) %>%
    filter(variables==varl[i]) %>%
    ggplot(.)+geom_sf(data=world,color=alpha("white",0.1),fill = NA)+geom_raster(aes(x=x,y=y,fill=corvalues))+facet_wrap(~variables_renamed)+
    dark_theme_gray()+scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0)+
    theme(strip.text = element_text(size=16))->tmpplot
  fout <- paste0("./figures/July21/",trimws(varl[i]),".png")
  ggsave(tmpplot,filename = fout,width=10,height=9,dpi=400,bg='transparent')
}


  
  


  rasterFromXYZ(crs=latlon) %>%
  writeRaster(filename="/home/marco/Desktop/corhumi.tif")

dd %>%
  ggplot(aes(x=bio_01,y=bio_12,z=corvalues))+
  stat_summary_2d(fun = function(x) {if(length(x[!is.infinite(x)])>=1)(mean(x))else(NA)},bins = 35,na.rm = FALSE)+
  theme_minimal()+
  scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0)+ 
  xlab("Temperature")+ylab("Precipitation")+theme(axis.text=element_text(size=11),axis.title=element_text(size=12),
                                                  plot.title=element_text(size=30,hjust=0.5),strip.text = element_text(size=8))+
  facet_wrap(~variables_renamed,labeller = label_wrap_gen(width=5))->localcor

  ggsave(localcor,filename="./figures/July21/local_correlation_precipitation_minustop.png",
         width = 9,height = 8,dpi=400,bg="white")





# Check whether turnover in PFTs corresponds to high diversity ------------
# function for calculating beta diversity
tcalc<-function(x){
  #pb$tick()
  beta.multi.abund(as.data.frame(x))->tmp
  tibble(turnover = tmp$beta.BRAY.BAL, GRA = tmp$beta.BRAY.GRA, BRAY= tmp$beta.BRAY)->tmp1
  return(tmp1)
}

read_delim("/media/marco/marcodata19/validationmetrics/pheno_grid_ESAvalues2002",delim = " ",col_names = F) %>%
  rename(lc=X1,cat=X2,area=X3) %>%
  mutate(lc = paste0("lc_",lc)) %>%
  inner_join(read_csv("tmp/gridstats025degree",col_names = FALSE) %>%
               rename(cat = X1,cat025d=X2)) %>%
  group_by(cat025d,lc) %>%
  summarise(area = sum(area)) %>%
    pivot_wider(names_from = lc,values_from=area,values_fill=0) %>%
    # get forest proportions
    #dplyr::select(cat,lc_61,lc_62,lc_71,lc_72,lc_81,lc_82,lc_90) %>% 
    mutate(rowsum = rowSums(.[2:13])) %>%
    filter(rowsum > 0) %>%
    dplyr::select(-c(rowsum)) %>%
    inner_join(read_csv("tmp/gridstats025degree",col_names = FALSE) %>%
                            rename(cat = X1,cat025d=X2)) ->tmpdat


tmpdat %>%
  dplyr::select(-c(cat,cat025d)) %>%
  clr() %>%
  as_tibble()->dd1
dd1 %>%
  prcomp(.)->pclc


res %>%
  group_by(cat) %>%
  summarise(value = median(value,na.rm = TRUE)) %>%
  rename(value = value)->p1

tmpdat %>%
  dplyr::select(cat) %>%
  bind_cols(as_tibble(pclc$x)) %>%
  dplyr::select(1:10) %>%
  inner_join(p1) %>%
  inner_join(phenodat %>%
               dplyr::select(cat,forest_prop))->dd

dd %>%
  inner_join(grid5km) %>%
  dplyr::select(x,y,value) %>%
  rasterFromXYZ() %>%
  writeRaster(filename = "/home/marco/Desktop/phenodiv.tif")



                
  dplyr::selec
  filter(value < 200) %>%
  #filter(bio_12 < 4500) %>%
  filter(forest_prop > 0.85) %>% {.->>tmp}  %>%
  ggplot(aes(x=PC3,y=value))+theme_minimal()+geom_bin2d(bins=60)+geom_smooth(method = "lm",color="red",size=1,
                                                                               linetype="dashed",se=FALSE)+
  scale_fill_gradient(low="lightblue1",high="darkblue",trans="log10")+xlab("Jetz - standard deviation of EVI (composite)")+ylab("Phenology dataset (Year 2004)")+
  ggtitle("Spearman rho = 0.40")+
  theme(legend.position = "none",axis.text = element_text(size=15,colour="black"),axis.title = element_text(size=20),
        plot.title = element_text(hjust = 0.5,size=20))

cor(dd$y_2004,dd$PC3)
  
mod<-lm(log(value+1)~PC1+PC2+PC3,data=dd)

tmpdat %>%
  dplyr::select(cat) %>%
  bind_cols(as_tibble(pclc$x)) %>%
  dplyr::select(1:10) %>%
  inner_join(grid5km) %>%
  relocate(x,y) %>%
  dplyr::select(-c(cat)) %>%
  dplyr::select(x,y,PC1,PC2,PC3) %>%
  rasterFromXYZ()->myr4
  
writeRaster(myr4,filename = "/home/marco/Desktop/myr4.tif",overwrite=TRUE)

tmpdat %>%
  relocate(cat025d) %>%
  names()
  group_by(cat) %>%
  nest()->dd 


tmpdat %>%
  inner_join(grid5km) %>%
  relocate(x,y) %>%
  dplyr::select(-c(cat)) %>%
  rasterFromXYZ() %>%
  writeRaster(filename="./tmp/esaforesttypes.tif",overwrite =TRUE)

tmpdat %>%
  mutate(H = diversity(.[2:13],index ="simpson")) %>%
  inner_join(grid5km) %>%
  dplyr::select(x,y,H) %>%
  rasterFromXYZ() %>%
  writeRaster(filename="./tmp/simpson.tif",overwrite =TRUE)

plan(multisession, workers = 7)

tmpdat %>%
  group_by(cat) %>%
  nest() %>%
  ungroup()->dd

%>%
  .[1:100,] %>%
  mutate(res = future_map(data,~tcalc(x =.))) %>%
  dplyr::select(-c(data)) %>%
  unnest(cols = res)->res



  
#options(future.globals.maxSize= 2097152000,packages = c("dplyr", "furrr"))
#plan(multisession,workers=7)



#----------- Forest types a bit more liberal
pb <- progress_bar$new(total = 4660)

 %>%
  # join with 5km and 110 km grid info
  inner_join(tmp %>%
  dplyr::select(cat,cat05d)) %>%
  dplyr::select(-c(cat)) %>%
  group_by(cat05d) %>%
  nest() %>%
  mutate(res = future_map(data,tcalc)) %>%
  dplyr::select(-c(data)) %>%
  unnest(cols = res)->res



read_csv("./GRASSGIS_files/bioclim.csv") %>%
  inner_join(read_csv("./GRASSGIS_files/pet_mean.csv")) %>%
  inner_join(phenodat %>% dplyr::select(-c(bio1,bio12))) %>%
  filter(forest_prfilter(forest_prop > 0.85) %>%op > 0.8) %>%
  inner_join(read_csv("tmp/gridstats50km",col_names = FALSE) %>%
               rename(cat = X1,cat05d=X2)) %>%
  dplyr::select(-c(cat)) %>%
  pivot_longer(cols=-c(cat05d),values_to = "values",names_to = "variables") %>%
  group_by(cat05d,variables) %>%
  summarise(values = mean(values)) %>%
  filter(variables=="bio_01"|variables=="bio_12"|variables=="y_2004") %>%
  pivot_wider(names_from = variables,values_from = values) %>%
  inner_join(res)->res1

res1 %>%
  ungroup() %>%
  ggplot(aes(x=bio_01,y=bio_12,z=turnover))+
  stat_summary_2d(fun = function(x) {if(length(x[!is.infinite(x)])>=3)(mean(x))else(NA)},bins = 35,na.rm = FALSE)+
  #geom_bin2d(bins=35)+
  theme_minimal()+
  #scale_fill_gradient2(low="blue", mid="yellow", high="red",limits=c(0,1))+ 
  scale_fill_distiller(palette = "RdYlBu",limits=c(0,0.98))+
  xlab("Temperature")+ylab("Precipitation")+theme(axis.text=element_text(size=15),axis.title=element_text(size=20),
                                                  plot.title=element_text(size=30,hjust=0.5))+
  scale_y_continuous(limits = c(0, 5000), breaks = c(0,1000,2000,3000,4000))


# check what sort of tree categories in different climates

wrapit <- function(text) {
  wtext <- paste(strwrap(text,width=40),collapse=" \n ")
  return(wtext)
}

read_delim("/media/marco/marcodata19/validationmetrics/pheno_grid_ESAvalues2002",delim = " ",col_names = F) %>%
  rename(lc=X1,cat=X2,area=X3) %>%
  mutate(lc = paste0("lc_",lc)) %>%
  pivot_wider(names_from = lc,values_from=area,values_fill=0) %>% {.->>lcats} %>%
  #dplyr::select(cat,lc_61,lc_62,lc_71,lc_72,lc_81,lc_82,lc_90) %>% 
  mutate(rowsum = rowSums(.[2:13])) %>%
  filter(rowsum > 0) %>%
  dplyr::select(-c(rowsum)) %>%
  inner_join(phenodat %>% dplyr::select(cat,bio1,bio12)) %>%
  pivot_longer(names_to = "variables",values_to = "values",cols = -c(cat,bio1,bio12)) %>%
  inner_join(esacats1 %>% mutate(variables=paste0("lc_",cat)) %>% dplyr::select(-c(cat))) %>%
  mutate(# function for calculating beta diversity
description = wrapit(description)) %>%
  group_by(variables) %>%
  mutate(values = scales::rescale(values,to=c(0,1))) %>%
  ungroup() %>%
  filter(values > 0.03) %>%
  #mutate(values = log(values+1)) %>%
  ggplot(aes(x=bio1,y=bio12,z=values))+
  stat_summary_2d(fun = function(x) {if(length(x[!is.infinite(x)])>=9)(mean(x))else(NA)},bins = 35,na.rm = FALSE)+
  #geom_bin2d(bins=35)+
  theme_minimal()+
  #scale_fill_gradient2(low="blue", mid="yellow", high="red",limits=c(0,1))+ 
  scale_fill_distiller(palette = "RdYlBu",limits=c(0,1))+
  xlab("Temperature")+ylab("Precipitation")+theme(axis.text=element_text(size=15),axis.title=element_text(size=20),
                                                  plot.title=element_text(size=30,hjust=0.5))+
  scale_y_continuous(limits = c(0, 5000), breaks = c(0,1000,2000,3000,4000))+facet_wrap(~description)->lcover_categories

ggsave(lcover_categories,filename="./figures/June21/landcover_categories.png",width=12,heigh=10,dpi=400)
  
                             
# binary version of land cover
read_delim("/media/marco/marcodata19/validationmetrics/pheno_grid_ESAvalues2002",delim = " ",col_names = F) %>%
  rename(lc=X1,cat=X2,area=X3) %>%
  mutate(lc = paste0("lc_",lc)) %>%
  pivot_wider(names_from = lc,values_from=area,values_fill=0) %>% {.->>lcats} %>%
  #dplyr::select(cat,lc_61,lc_62,lc_71,lc_72,lc_81,lc_82,lc_90) %>% 
  mutate(rowsum = rowSums(.[2:13])) %>%
  filter(rowsum > 0) %>%
  dplyr::select(-c(rowsum)) %>%
  inner_join(phenodat %>% dplyr::select(cat,bio1,bio12)) %>%
  pivot_longer(names_to = "variables",values_to = "values",cols = -c(cat,bio1,bio12)) %>%
  group_by(variables) %>%
  mutate(values = scales::rescale(values,to=c(0,1))) %>%
  ungroup() %>%
  filter(values > 0.01) %>%
  mutate(values = case_when(values > 0~1)) %>%
  pivot_wider(names_from = variables,values_from = values,values_fill = 0) %>%
  mutate(rowsum = rowSums(.[4:15])) %>%
  filter(rowsum > 0) %>%
  dplyr::select(-c(rowsum))->lcoverbin

tcalc<-function(x){
  #pb$tick()
  beta.multi(as.data.frame(x),index.family="jaccard")->tmp
  tibble(JTU = tmp$beta.JTU, JNE = tmp$beta.JNE, JAC= tmp$beta.JAC)->tmp1
  return(tmp1)
}

# get forest proportions
lcoverbin %>%
  # join with 5km and 110 km grid info
  inner_join(tmp %>%
               dplyr::select(cat,cat05d)) %>%
  dplyr::select(-c(cat,bio1,bio12)) %>%
  group_by(cat05d) %>%
  nest() %>%
  mutate(res = map(data,tcalc)) %>%
  dplyr::select(-c(data)) %>%
  unnest(cols = res)->res



read_csv("./GRASSGIS_files/bioclim.csv") %>%
  inner_join(read_csv("./GRASSGIS_files/pet_mean.csv")) %>%
  inner_join(phenodat %>% dplyr::select(-c(bio1,bio12))) %>%
  filter(forest_prop > 0.8) %>%
  inner_join(read_csv("tmp/gridstats50km",col_names = FALSE) %>%
               rename(cat = X1,cat05d=X2)) %>%
  dplyr::select(-c(cat)) %>%
  pivot_longer(cols=-c(cat05d),values_to = "values",names_to = "variables") %>%
  group_by(cat05d,variables) %>%
  summarise(values = mean(values)) %>%
  filter(variables=="bio_01"|variables=="bio_12"|variables=="y_2004") %>%
  pivot_wider(names_from = variables,values_from = values) %>%
  inner_join(res)->res1

res1 %>%
  mutate(JTU = ifelse(is.na(JTU),0,JTU)) %>%
  ungroup() %>%
  ggplot(aes(x=bio_01,y=bio_12,z=JTU))+
  stat_summary_2d(fun = function(x) {if(length(x[!is.infinite(x)])>=3)(mean(x))else(NA)},bins = 35,na.rm = FALSE)+
  #geom_bin2d(bins=35)+
  theme_minimal()+
  #scale_fill_gradient2(low="blue", mid="yellow", high="red",limits=c(0,1))+ 
  scale_fill_distiller(palette = "RdYlBu",limits=c(0,0.5))+
  xlab("Temperature")+ylab("Precipitation")+theme(axis.text=element_text(size=15),axis.title=element_text(size=20),
                                                  plot.title=element_text(size=30,hjust=0.5))+
  scale_y_continuous(limits = c(0, 5000), breaks = c(0,1000,2000,3000,4000))

phenodat %>%
  filter(bio12 > 1500 & bio12 < 3001 & bio1 > 5 & bio1 < 15) %>%
  pull(cat)->cc

phenodat %>%
  filter(!cat %in% cc) %>%
  ggplot(aes(x=bio1,y=bio12,z=y_2004))+
  stat_summary_2d(fun = function(x) {if(length(x[!is.infinite(x)])>=9)(mean(x))else(NA)},bins = 35,na.rm = FALSE)+
  #geom_bin2d(bins=35)+
  theme_minimal()+
  #scale_fill_gradient2(low="blue", mid="yellow", high="red",limits=c(0,1))+ 
  scale_fill_distiller(palette = "RdYlBu",limits=c(0,150))+
  xlab("Temperature")+ylab("Precipitation")+theme(axis.text=element_text(size=15),axis.title=element_text(size=20),
                                                  plot.title=element_text(size=30,hjust=0.5))+
  scale_y_continuous(limits = c(0, 5000), breaks = c(0,1000,2000,3000,4000))


# spatial autocorrelation -------------------------------------------------
phenodat1 %>%
  dplyr::select(-c(x,y)) %>%
  inner_join(tmpgrid1d  %>%
               rename(cat1d = value))->phenodat1b

# SAR models
# chooses neighbourhood
coor<-phenodat1b[,c("x","y")]
coor<-as.matrix(coor)

# create neighbourhood
cor.knn <- knn2nb(knearneigh(coor, k=1))
dsts<-unlist(nbdists(cor.knn,coor))
vic<-dnearneigh(coor,d1=0,d2=7)
nbw<-nb2listw(vic,style="W",zero.policy = TRUE)
mod1<-errorsarlm(y_2004~bio_04+std_elev+bio_10+bio_03+bio_07+
                  humi,nbw,data=phenodat1b,zero.policy=TRUE,
               method="eigen")


# prepare 0.25 degree data for modelling ----------------------------------
# empty raster with desired resolution
raster(res=0.25)->myr
# create raster grid and write out file
rasterize(world,myr) %>%
  rastertodf() %>%
  mutate(value = 1:length(value)) %>%
  rename(cat025d = value) %>% {.->>tmp} %>%
  mutate(x1 = x, y1= y) %>%
  st_as_sf(coords =c("x1","y1"),crs=latlon) %>%
  st_transform(crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs") %>% {.->>tmp} %>%
  st_coordinates() %>%
  as_tibble() %>%
  bind_cols(as_tibble(tmp) %>% rename(lon = x, lat =y) %>% dplyr::select(-c(geometry)))->g025





phenodat5 %>%
  #filter(dis > 0.025) %>%
  ggplot(aes(x=dis,y=value))+geom_point(size=1,colour = "red",alpha=0.2)+geom_smooth(method="lm")

