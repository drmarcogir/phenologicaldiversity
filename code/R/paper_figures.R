# load required libraries -------------------------------------------------
library(raster);library(phenoutils)
library(tidyverse);library(sf)
library(data.table);library(scales)
library(marcoUtils);library(tmap)
library(RColorBrewer);library(grid)
library(viridis);library(mgcViz)
library(gridExtra);library(ggridges)
st_read("./worldshp/worldsimple.shp")->world2


# read data ---------------------------------------------------------------
fread("/home/marco/alessandro_metric/final.csv") %>%
  #fread("/mnt/data1tb/alessandro_metric/finaldat.csv") %>%
  group_by(cat) %>%
  summarise(value = median(value)) %>%
  inner_join(grid5km) %>%
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
  # add climate data
  inner_join(read_csv("./GRASSGIS_files/bioclim_sd.csv")) %>%
  #inner_join(read_csv("./GRASSGIS_files/pet_mean.csv")) %>%
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
  #mutate(aridity = pet_mean/bio_12,bio_04 = bio_04/1000) %>%
  #mutate(bio_01 = bio_01/10) %>%
  # add population density
  inner_join(read_csv("tmp/humi",col_names = FALSE) %>%
               rename(cat = X1,humi=X2)) %>%
  # ecoregions
  inner_join(read_csv("./tmp/ecoregions.csv")) %>%
  inner_join(read_csv("./tmp/shannontop.csv") %>%
               rename(Htop= variable)) %>% 
  # bioclim background climate
  inner_join(read_csv("./GRASSGIS_files/bioclim.csv") %>%
               dplyr::select(c(cat,bio_01,bio_12,bio_15,bio_04)) %>%
               rename(bio_01_mean = bio_01,bio_12_mean = bio_12,
                      bio_15_mean = bio_15,
                      bio_04_mean = bio_04)) %>%
  inner_join(read_csv("./GRASSGIS_files/pet_mean.csv")) %>%
  # calculate aridity and rescale temp seasonality
  mutate(aridity = pet_mean/bio_12,bio_04 = bio_04/1000) %>%
  mutate(bio_01 = bio_01/10)->phenodat

# does a bit of filtering
phenodat %>%
  filter(value < 180) %>%
  #filter(forest_prop > 0.75) %>%
  #dplyr::select(x,y,value) %>%
  mutate(value2 = value) %>%
  mutate(value2 = cut(value2, breaks = c(0,37.3,56.7,83.2,122.6,179.9), 
                      right = TRUE,  labels = c('0', '37.3', '56.8',
                                                '83.3', '> 122.6'))) %>%
  filter(!is.na(value2)) %>%
  inner_join(tibble(WWF_MHTNAM = c("Tundra","Tropical and Subtropical Dry Broadleaf Forests",
                                   "Tropical and Subtropical Grasslands, Savannas and Shrublands",
                                   "Temperate Broadleaf and Mixed Forests",
                                   "Boreal Forests/Taiga",
                                   "Tropical and Subtropical Moist Broadleaf Forests",
                                   "Temperate Conifer Forests") ,
                    WWFMHTNAM1= c("Boreal","Arid","Arid",
                                  "Temperate","Boreal","Tropical","Temperate"))) %>%
               mutate(WWFMHTNAM1= factor(WWFMHTNAM1,levels=c("Boreal","Temperate","Arid","Tropical")))->phenodat


# Fig.1
# baseline map -----------------------------------------------------------
ggplot(data = phenodat)+
  theme(legend.position="none",
        panel.background = element_rect(fill = 'white',color="white"),
        strip.text = element_text(size = rel(2.5), face = "bold"),
        axis.text = element_blank(),axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = 'transparent'))+
  geom_sf(data=world2,color='white',fill='grey5',size=0.09)+
  geom_tile(aes(x, y, fill = value2))+
  xlab("") + ylab("")+
  theme(legend.position = c(.55, .05), 
        legend.key = element_blank(),
        legend.text = element_text(size = rel(0.8),colour = "black"),
        legend.background = element_rect(colour = NA, fill = NA)) + 
  scale_fill_manual(values = rev(brewer.pal(5, 'Spectral')), na.translate = F,
                    guide = guide_legend(nrow = 1, direction = 'horizontal', 
                                         label.hjust = 0, label.position = 'bottom', 
                                         keywidth = 3.5, keyheight = 0.5, title = ""))->basemap

# histogram with pixel counts within each class -----------------------------------------------------------

phenodat %>%
  ggplot(aes(value2))+
  geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(..x..)))+
  ylab("relative frequencies")+
  scale_fill_manual(values= rev(brewer.pal(5, 'Spectral')))+
  scale_y_continuous(labels=scales::percent)+
  scale_x_discrete(drop=FALSE) +
  theme_bw()+theme(axis.text.y = element_text(size=10),legend.position="none",
                   axis.text.x = element_blank(),axis.title.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.title.y = element_text(size = 11))+
  guides(scale="none")->histf

# combining things together 
# p2+annotation_custom(grob=ggplotGrob(histf), 
#                      xmin=-180, xmax=-110,ymin = -60, ymax=-5)+
#   theme(panel.border = element_rect(color = "black",
#                                     fill = NA,
#                                     size = 1))->p3



# density plots: these go on the side of the map -------------------------------------------------------------

# calculate 25th, 50th and 75th percentiles
phenodat %>%
  group_by(WWFMHTNAM1) %>%
  summarise(p25 = quantile(value,probs=c(.25)),
            p50 = quantile(value, probs = c(.50)),
            p75 = quantile(value, probs = c(.75)))->pdat


# get density estimates from ggplots for each biome
phenodat %>%
  group_by(WWFMHTNAM1) %>%
  nest() %>%
  mutate(dens = map(data,~ggplot_build(ggplot(data=.x,aes(value))+geom_density())$data[[1]])) %>%
  # get rid of data column
  dplyr::select(-c(data)) %>%
  unnest(cols = dens) %>%
  inner_join(pdat) %>%
  # use function approx to get values for vertical bars
  group_by(WWFMHTNAM1) %>%
  mutate(dens.25p = approx(x, y, xout = p25)[[2]],
         dens.50p = approx(x, y, xout = p50)[[2]],
         dens.75p = approx(x, y, xout = p75)[[2]])->densdat

# final density plot
ggplot() +
  geom_density(data = phenodat, aes(x = value,fill = WWFMHTNAM1))+
  geom_segment(data = densdat, aes(x = p25, xend = p25, y = 0, yend = dens.25p))+
  geom_segment(data = densdat, aes(x = p50, xend = p50, y = 0, yend = dens.50p),colour = "red")+
  geom_segment(data = densdat, aes(x = p75, xend = p75, y = 0, yend = dens.75p))+
  facet_wrap(~WWFMHTNAM1,ncol = 1)+scale_fill_manual(values = c("#009E73","#E69F00","#0072B2","#CC79A7"))+
  theme_void()+theme(legend.position = "none",strip.text = element_text(size=12))->pdens

# small biome maps
st_read("./ecoregions_dissolved/ecodissolved_simple.shp") %>%
  mutate(id = c(2,1,4,3)) %>%
  arrange(id) %>%
  mutate(fillcol = c("#009E73","#E69F00","#0072B2","#CC79A7")) %>%
  group_by(WWF_MHT) %>%
  nest() %>%
  mutate(bmap = map(data,~ggplot(data =.x,aes())+
                      geom_sf(data=world2,fill = "grey60",color="white")+
                          geom_sf(fill=.x$fillcol)+theme_void()+
                      theme(legend.position = "none",strip.text = element_blank())))->biomemaps


# add inset maps to density maps
pdens +inset_element(biomemaps[1,]$bmap[[1]], 0.6, 0.7, 1, 1.2)+
  inset_element(biomemaps[2,]$bmap[[1]], 0.6, 0.6, 1, 0.8)+
  inset_element(biomemaps[3,]$bmap[[1]], 0.6, 0.35, 1, 0.55)+
  inset_element(biomemaps[4,]$bmap[[1]], 0.6, 0.07, 1, 0.3)->pdens1


# Fig 1 combine everything together ---------------------------------------

theme_border <- theme_gray() + 
  theme(plot.background = element_rect(fill = NA, colour = 'black', size = 1))

basemap+inset_element(histf, 0.1, 0.15, 0.25, 0.4)+plot_annotation(theme = theme_border)+pdens1 +
  plot_layout(widths = c(5, 0.7)) +plot_annotation(theme = theme_border)->finalp

ggsave(finalp,filename = "./figures/paper/Fig1/Fig1June22v2.png",width = 15,height=7,dpi=400,
       bg="white")





# Supplementary Fig. 4 localcoef ---------------------------------------------------------------
  
to_rast<-function(x){
  x %>%
  bind_cols(phenodat5 %>%
              dplyr::select(cat025d,lon,lat)) %>%
    dplyr::select(lon,lat,estimate) %>%
    rasterFromXYZ(crs=latlon)->tmpr
  return(rastertodf(raster::aggregate(tmpr,fact=4,fun=median)))
}  

read_csv("./jeodpp_results/local_coefs.csv") %>%
  group_by(varname) %>%
  nest() %>%
  mutate(raster = map(data,to_rast))->tmpdat

st_read("./worldshp/worlmp.shp")->world2

map_create<-function(x,lims){
  x %>%
    ggplot() +
    #theme_bw()+
    theme(legend.position="none",
          panel.background = element_rect(fill = 'white',color="white"),
          strip.text = element_text(size = rel(2.5), face = "bold"),
          axis.text = element_blank(),axis.ticks = element_blank(),
          panel.grid.major = element_line(colour = 'transparent'))+
    geom_sf(data=world2,color='white',fill='grey25',size=0.09)+
    geom_tile(aes(x=x,y=y,fill=value))+
    xlab("") + ylab("")+
    theme(legend.position = 'bottom', 
          legend.key = element_blank(),
          legend.text = element_text(size = rel(0.8),colour = "black"),
          plot.title = element_text(hjust = 0.5,vjust = 0,size = 10)) + 
    scale_fill_viridis(option = "plasma", oob=squish,limits=lims)+
    guides(fill = guide_colourbar(title="",barwidth=5,barheight =0.2),
           direction = 'horizontal')->p2
  return(p2)
}

map_create(tmpdat[1,]$raster[[1]],lims=c(-0.5,2.5))+ggtitle("Heterogeneity in Mean Annual Temperature")->p1
map_create(tmpdat[2,]$raster[[1]],lims=c(-0.04,0.05))+ggtitle("Heterogeneity in Total Annual Precipitation")->p2
map_create(tmpdat[3,]$raster[[1]],lims=c(-0.5,0.5))+ggtitle("Heterogeneity in Temperature Seasonality")->p3
map_create(tmpdat[4,]$raster[[1]],lims=c(-0.4,1.8))+ggtitle("Heterogeneity in Precipitation Seasonality")->p4
map_create(tmpdat[5,]$raster[[1]],lims=c(-0.01,0.04))+ggtitle("Human impact index")->p5
map_create(tmpdat[6,]$raster[[1]],lims=c(-15,15))+ggtitle("Topographic heterogeneity")->p6
map_create(tmpdat[7,]$raster[[1]],lims=c(-16,20))+ggtitle("Turnover of forest types")->p7


png("./figures/supplementary/local_response.png",width = 4000,height=3000,res = 400)
pushViewport(viewport(layout = grid.layout(3, 3)))
#print(pres_plots[4,]$plot[[1]],vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p1,vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p2,vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(p3,vp = viewport(layout.pos.row = 1, layout.pos.col = 3))
print(p4,vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(p5,vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
print(p6,vp = viewport(layout.pos.row = 2, layout.pos.col = 3))
print(p7,vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
dev.off()


grid.arrange(p1,p2,nrow=2)->dd
ggsave(dd,"./figures/supplementary/local_response.png",width = 12,height = 10,dpi = 400)


# Supplementary Fig. 4 localcoef ---------------------------------------------------------------

read_csv("./jeodppfiles/phenodat5_5km.csv")->phenodat5km

to_rast<-function(x){
  x %>%
    bind_cols(phenodat5km %>%
                dplyr::select(cat,x,y)) %>%
    dplyr::select(x,y,estimate) %>%
    rasterFromXYZ(crs=latlon)->tmpr
  return(rastertodf(raster::aggregate(tmpr,fact=10,fun=median)))
}  


read_csv("./jeodpp_results/ice_betas.csv") %>%
  group_by(variable) %>%
  nest() %>%
  mutate(rast_tbl = map(data,to_rast))->res

map_create<-function(x){
  x %>%
    pull(value)->val
  lims =c (quantile(val,probs=c(0.01))[1],quantile(val,probs=c(0.99))[1])
  x %>%
    ggplot() +
    #theme_bw()+
    theme(legend.position="none",
          panel.background = element_rect(fill = 'white',color="white"),
          strip.text = element_text(size = rel(2.5), face = "bold"),
          axis.text = element_blank(),axis.ticks = element_blank(),
          panel.grid.major = element_line(colour = 'transparent'))+
    geom_sf(data=world2,color='white',fill='grey25',size=0.09)+
    geom_tile(aes(x=x,y=y,fill=value))+
    xlab("") + ylab("")+
    theme(legend.position = 'bottom', 
          legend.key = element_blank(),
          legend.text = element_text(size = rel(0.8),colour = "black"),
          plot.title = element_text(hjust = 0.5,vjust = 0,size = 11)) + 
    scale_fill_gradient2(low ="blue", mid = "white",high ="red",oob=squish,limits=lims)+
    guides(fill = guide_colourbar(title="",barwidth=12,barheight =0.5),
           direction = 'horizontal')->p2
  return(p2)
}

res %>%
  mutate(plot = map(rast_tbl,map_create)) %>%
  ungroup() %>%
  mutate(rown = c(3,4,2,1,10,9,8,7,6,5)) %>%
  arrange(rown)->res1


res1[1,]$plot[[1]] + theme(plot.margin = unit(c(0,0,0,-0.5), "cm"))+
  ggtitle("Mean annual temperature")->p1
res1[2,]$plot[[1]] + theme(plot.margin = unit(c(0,0,0,-0.5), "cm"))+
  ggtitle("Annual cumulated precipitation")->p2
res1[3,]$plot[[1]] + theme(plot.margin = unit(c(0,0,0,-0.5), "cm"))+
  ggtitle("Temperature seasonality")->p3
res1[4,]$plot[[1]] + theme(plot.margin = unit(c(-1.5,0,0,-0.5), "cm"))+
  ggtitle("Precipitation seasonality")->p4
res1[5,]$plot[[1]] + theme(plot.margin = unit(c(-1.5,0,0,-0.5), "cm"))+
  ggtitle("Heterogeneity in mean annual temperature")->p5
res1[6,]$plot[[1]] + theme(plot.margin = unit(c(-1.5,0,0,-0.5), "cm"))+
  ggtitle("Heterogeneity in annual cumulated precipitation")->p6
res1[7,]$plot[[1]] + theme(plot.margin = unit(c(-3,0,0,-0.5), "cm"))+
  ggtitle("Heterogeneity in temperature seasonality")->p7
res1[8,]$plot[[1]] + theme(plot.margin = unit(c(-3,0,0,-0.5), "cm"))+
  ggtitle("Heterogeneity in precipitation seasonality")->p8
res1[9,]$plot[[1]] + theme(plot.margin = unit(c(-3,0,0,-0.5), "cm"))+
  ggtitle("Human footprint")->p9
res1[10,]$plot[[1]] + theme(plot.margin = unit(c(-4,0,0,-0.5), "cm"))+
  ggtitle("Topographic heterogeneity")->p10



grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,ncol=3,nrow=4)->combined

ggsave(combined,filename = "./figures/supplementary/local_response_5kmJune22.png",width=10,height=11,dpi=400)

# create new labels -------------------------------------------------------
predictors %>%
  dplyr::select(-c(label)) %>%
  rename(label = label1) %>%
  mutate(label = case_when(label=="Temperature seasonality"~"Temperature seasonality [°C]",
                           label=="Annual mean temperature"~"Annual mean temperature [°C]",
                           label=="Annual precipitation"~"Annual precipitation [mm]",
                           label=="sd Annual mean temperature"~"sd Annual mean temperature [°C]",
                           label=="Precipitation seasonality"~"Precipitation seasonality [mm]",
                           label=="sd Temperature seasonality"~"sd Temperature seasonality [°C]",
                           label=="sd Annual precipitation"~"sd Annual precipitation [mm]",
                           label=="Human impact"~"Human footprint",
                           label=="sd Precipitation seasonality"~"sd Precipitation seasonality [mm]",
                           TRUE ~ as.character(label)))->predictors1


# panel b
predictors1 %>%
  slice(1) %>%
  mutate(xlab =label) %>%
  rename(xvar_name = varname) %>%
  dplyr::select(xvar_name,xlab) %>%
  bind_cols(predictors1 %>%
              slice(5) %>%
              mutate(ylab =label) %>%
              rename(yvar_name = varname) %>%
              dplyr::select(yvar_name,ylab)) %>%
  # panel c
  bind_rows(predictors1 %>%
              slice(2) %>%
              mutate(xlab =label) %>%
              rename(xvar_name = varname) %>%
              dplyr::select(xvar_name,xlab) %>%
              bind_cols(predictors1 %>%
                          slice(13) %>%
                          mutate(ylab =label) %>%
                          rename(yvar_name = varname) %>%
                          dplyr::select(yvar_name,ylab))) %>%
  # panel d
  bind_rows(predictors1 %>%
              slice(9) %>%
              mutate(xlab =label) %>%
              rename(xvar_name = varname) %>%
              dplyr::select(xvar_name,xlab) %>%
              bind_cols(predictors1 %>%
                          slice(8) %>%
                          mutate(ylab =label) %>%
                          rename(yvar_name = varname) %>%
                          dplyr::select(yvar_name,ylab))) %>%

  # panel e
  bind_rows(predictors1 %>%
              slice(10) %>%
              mutate(xlab =label) %>%
              rename(xvar_name = varname) %>%
              dplyr::select(xvar_name,xlab) %>%
              bind_cols(predictors1 %>%
                          slice(6) %>%
                          mutate(ylab =label) %>%
                          rename(yvar_name = varname) %>%
                          dplyr::select(yvar_name,ylab))) %>%


  # panel f
  bind_rows(predictors1 %>%
              slice(4) %>%
              mutate(xlab =label) %>%
              rename (xvar_name = varname) %>%
              dplyr::select(xvar_name,xlab) %>%
              bind_cols(predictors1 %>%
                          slice(7) %>%
                          mutate(ylab =label) %>%
                          rename(yvar_name = varname) %>%
                          dplyr::select(yvar_name,ylab)))->predictors2




# Fig. 2 -5 km resolution data ----------------------------------------------
# variable importance



read_csv("./jeodpp_results/varimp_5km_aridity_Mar22.csv") %>% 
  inner_join(predictors %>%
               dplyr::select(-c(label)) %>%
                               rename(label = label1)) %>%
  arrange(value) %>%
  mutate(label = as_factor(label)) %>%
  
  ggplot(aes(y=label,x=value,fill=col))+geom_bar(stat = "identity",color="black",
                                                 size=0.5)+
  theme_bw()+
  scale_fill_manual(values =  c("#0072B2","#999999","#D55E00"))+
  theme(legend.position = "none",axis.text=element_text(size=12,colour = "black"),
        axis.title = element_text(size = 15))+
  xlab("Relative importance")+ylab("")+xlim(0, 1100)+
  annotate("text",x = Inf, y = Inf,label = "a ", 
            vjust = "top", hjust = "right",
            size=6)->varimp
  
# 2d pdp
tibblefix<-function(x){
  as_tibble(x) %>%
    rename(x = 1, y = 2)->res
  return(res)
}

readRDS("./jeodpp_results/pd2dres_Mar22.RDS") %>%
  mutate(pd2d = map(pd2d,~tibblefix(x=.x))) %>%
  unnest(cols = c(pd2d)) %>%
  rename(xvar_name = var1, yvar_name = var2) %>%
  mutate(x = case_when(xvar_name == "bio_04_mean" ~ x/1000,
                       xvar_name == "bio_01_mean" ~ x/10,
                       xvar_name == "bio_15" ~ x/10,
                       TRUE~x)) %>%
  inner_join(predictors2) %>%
  write_csv("./gregory/data/phenology/pd2d.csv")
  group_by(xvar_name,yvar_name) %>%
  nest() %>%
  mutate(plot = map(data,~ggplot(data = .x,aes(x=x,y=y,fill=yhat))+geom_raster()+
                      scale_fill_viridis(option="inferno")+theme_bw()+  
                      theme(axis.text = element_text(size = 14,colour = "black"),
                            axis.title = element_text(size=16),legend.title =element_blank())+
                      xlab(unique(.x$xlab))+ylab(unique(.x$ylab))))->pd2d


  
grid.arrange(varimp,pdp2dres[1,]$plots[[1]]+ scale_x_continuous(labels = div_format())+
               annotate("text",x = Inf, y = Inf,label = "b ",
                        vjust = "top", hjust = "right",
                        size=6),
             pdp2dres[2,]$plots[[1]]+scale_x_continuous(labels = div_format1())+
               annotate("text",x = Inf, y = Inf,label = "c ",
                        vjust = "top", hjust = "right",
                        size=6),
             pdp2dres[3,]$plots[[1]]+
               annotate("text",x = Inf, y = Inf,label = "d ",
                        vjust = "top", hjust = "right",
                        size=6),
             pdp2dres[4,]$plots[[1]]+
               scale_x_continuous(labels = div_format1())+
               annotate("text",x = Inf, y = Inf,label = "e ",
                        vjust = "top", hjust = "right",
                        size=6),
             pdp2dres[5,]$plots[[1]]+
               annotate("text",x = Inf, y = Inf,label = "f ",
                        vjust = "top", hjust = "right",
                        size=6),ncol=2,nrow=3)->fig2combined





ggsave(fig2combined,filename = "./figures/paper/Fig2/Fig2_10variablesMay22_arid.png",
       width = 11, height = 10, dpi = 400)

# univariate pdp ----------------------------------------------------------
read_csv("./jeodpp_results/univariate_pdp_Mar22.csv") %>%
  mutate(pred = case_when(variable=="bio_01_mean"~pred*0.1,
                          variable=="bio_04_mean"~(pred/100)*0.1,
                          TRUE ~ pred)) %>%
  rename(varname = variable) %>%
  inner_join(predictors %>%
               mutate(label1 = case_when(label1=="Human impact"~"Human footprint",TRUE~as.character(label1)))) %>%
  ggplot(aes(x=pred,y=yhat))+theme_bw()+facet_wrap(~label1,scales = "free",labeller = label_wrap_gen(width=20))+geom_line()+
  ylab("Phenological Diversity")+xlab("Predictor")+theme(axis.title=element_text(size=12))->pdp1 

ggsave(pdp1,filename = "./figures/supplementary/Fig4/pdp_average_Jun22.png",width = 8,height = 7,dpi = 400)


predictors %>%
  dplyr::select(-c(label)) %>%
  rename(label = label1) %>%
  mutate(label = case_when(label=="Temperature seasonality"~"Temperature seasonality [°C]",
                           label=="Annual mean temperature"~"Annual mean temperature [°C]",
                           label=="Annual precipitation"~"Annual precipitation [mm]",
                           label=="sd Annual mean temperature"~"sd Annual mean temperature [°C]",
                           label=="Precipitation seasonality"~"Precipitation seasonality [mm]",
                           label=="sd Temperature seasonality"~"sd Temperature seasonality [°C]",
                           label=="sd Annual precipitation"~"sd Annual precipitation [mm]",
                           label=="Human impact"~"Human footprint",
                           label=="sd Precipitation seasonality"~"sd Precipitation seasonality [mm]",
                           TRUE ~ as.character(label)))->predictors1


predictors %>%
  mutate(label1 = case_when(label1=="Human impact"~"Human footprint",TRUE~as.character(label1)))


# Aridity results ---------------------------------------------------------
# Fig. 2 --
read_csv("./rfresults/varimp_5km_aridity.csv") %>%
  inner_join(predictors %>%
               bind_rows(tibble(varname = "aridity",label ="Aridity"))) %>%
  arrange(value) %>%
  mutate(label = as_factor(label)) %>%
  mutate(col = c("climate","topography","human impact",
                 "climate","climate","climate","climate",
                 "climate","climate","climate")) %>%
  ggplot(aes(y=label,x=value,fill=col))+geom_bar(stat = "identity",color="black",
                                                 size=0.5)+
  theme_minimal()+
  scale_fill_manual(values =  c("#0072B2","#999999","#009E73","#D55E00"))+
  theme(legend.position = "none",axis.text=element_text(size=12,colour = "black"),
        axis.title = element_text(size = 15))+
  xlab("Relative importance")+ylab("Variable")->varimp

readRDS("./rfresults/pd2dres_aridity.RDS") %>%
  mutate(plots = map(pd2d,pdp2_create))->pdp2dres

grid.arrange(varimp,pdp2dres[1,]$plots[[1]]+ scale_x_continuous(labels = div_format()),
             pdp2dres[2,]$plots[[1]]+scale_x_continuous(labels = div_format1()),
             pdp2dres[3,]$plots[[1]],pdp2dres[4,]$plots[[1]],
             pdp2dres[5,]$plots[[1]],ncol=2,nrow=3)->fig2combined

ggsave(fig2combined,filename = "./figures/paper/Fig2_10variables_aridity.png",width = 11, height = 10, dpi = 400)
# univariate pdp aridity  ----------------------------------------------------------
read_csv("./rfresults/univariate_pdp_aridity.csv") %>%
  rename(varname = variable) %>%
  inner_join(predictors %>%
               bind_rows(tibble(varname = "aridity",label ="Aridity"))) %>%
  ggplot(aes(x=pred,y=yhat))+theme_bw()+facet_wrap(~label,scales = "free")+geom_line()->pdp1 

ggsave(pdp1,filename = "./figures/supplementary/pdp_average_aridity.png",width = 8,height = 7,dpi = 400)



# localcoef 5 km aridity  ---------------------------------------------------------------

read_csv("./jeodppfiles/phenodat5_5km.csv") %>%
  filter(bio_12_mean < 4000) %>%
  filter(!is.infinite(aridity))->phenodat5km

to_rast<-function(x){
  x %>%
    bind_cols(phenodat5km %>%
                dplyr::select(cat,x,y)) %>%
    dplyr::select(x,y,estimate) %>%
    rasterFromXYZ(crs=latlon)->tmpr
  return(rastertodf(raster::aggregate(tmpr,fact=10,fun=median)))
}  


readRDS("./rfresults/iceres2_aridity.RDS")->dd

dd %>%
  #filter(variable == "bio_01_mean")
  dplyr::select(-c(data)) %>%
  unnest(cols = c(res)) -> ices_aridity

ices_aridity %>%
  group_by(variable) %>%
  nest() %>%
  mutate(rast_tbl = map(data,to_rast))->res

map_create<-function(x){
  x %>%
    pull(value)->val
  lims =c (quantile(val,probs=c(0.01))[1],quantile(val,probs=c(0.99))[1])
  x %>%
    ggplot() +
    #theme_bw()+
    theme(legend.position="none",
          panel.background = element_rect(fill = 'white',color="white"),
          strip.text = element_text(size = rel(2.5), face = "bold"),
          axis.text = element_blank(),axis.ticks = element_blank(),
          panel.grid.major = element_line(colour = 'transparent'))+
    geom_sf(data=world2,color='white',fill='grey25',size=0.09)+
    geom_tile(aes(x=x,y=y,fill=value))+
    xlab("") + ylab("")+
    theme(legend.position = 'bottom', 
          legend.key = element_blank(),
          legend.text = element_text(size = rel(0.8),colour = "black"),
          plot.title = element_text(hjust = 0.5,vjust = 0,size = 11)) + 
    scale_fill_gradient2(low ="blue", mid = "white",high ="red",oob=squish,limits=lims)+
    guides(fill = guide_colourbar(title="",barwidth=12,barheight =0.5),
           direction = 'horizontal')->p2
  return(p2)
}

res %>%
  mutate(plot = map(rast_tbl,map_create)) %>%
  ungroup() %>%
  mutate(rown = c(4,3,2,1,10,9,8,7,6,5)) %>%
  arrange(rown)->res1


res1[1,]$plot[[1]] + theme(plot.margin = unit(c(0,0,0,-0.5), "cm"))+
  ggtitle("Mean annual temperature")->p1
res1[2,]$plot[[1]] + theme(plot.margin = unit(c(0,0,0,-0.5), "cm"))+
  ggtitle("Aridity")->p2
res1[3,]$plot[[1]] + theme(plot.margin = unit(c(0,0,0,-0.5), "cm"))+
  ggtitle("Temperature seasonality")->p3
res1[4,]$plot[[1]] + theme(plot.margin = unit(c(-1.5,0,0,-0.5), "cm"))+
  ggtitle("Precipitation seasonality")->p4
res1[5,]$plot[[1]] + theme(plot.margin = unit(c(-1.5,0,0,-0.5), "cm"))+
  ggtitle("Heterogeneity in mean annual temperature")->p5
res1[6,]$plot[[1]] + theme(plot.margin = unit(c(-1.5,0,0,-0.5), "cm"))+
  ggtitle("Heterogeneity in annual cumulated precipitation")->p6
res1[7,]$plot[[1]] + theme(plot.margin = unit(c(-3,0,0,-0.5), "cm"))+
  ggtitle("Heterogeneity in temperature seasonality")->p7
res1[8,]$plot[[1]] + theme(plot.margin = unit(c(-3,0,0,-0.5), "cm"))+
  ggtitle("Heterogeneity in precipitation seasonality")->p8
res1[9,]$plot[[1]] + theme(plot.margin = unit(c(-3,0,0,-0.5), "cm"))+
  ggtitle("Human footprint")->p9
res1[10,]$plot[[1]] + theme(plot.margin = unit(c(-4,0,0,-0.5), "cm"))+
  ggtitle("Topographic heterogeneity")->p10



grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,ncol=3,nrow=4)->combined

ggsave(combined,filename = "./figures/supplementary/local_response_5km_aridity.png",width=10,height=11,dpi=400)


# 2d pdp
pdp2_create<-function(x){
  xax <- names(x)[1]
  yax <- names(x)[2]
  predictors %>%
    bind_rows(tibble(varname = "aridity",label ="Aridity"))->predictors
  xlabel <- tibble(varname = xax) %>% inner_join(predictors) %>% pull(label)
  ylabel <- tibble(varname = yax) %>% inner_join(predictors) %>% pull(label)
  ggplot(data = x)+geom_raster(data = x,aes_string(x=xax,y=yax,fill="yhat"))+
    scale_fill_viridis(option="inferno")+theme_bw()+  
    theme(axis.text = element_text(size = 15,colour = "black"),
          axis.title = element_text(size=18),legend.title =element_blank())+xlab(xlabel)+
    ylab(ylabel)->p1
  return(p1)
}

div_format <- function() {
  function(x) format(x/1000,digits = 0) 
}

div_format1 <- function() {
  function(x) format(x/10,digits = 0) 
}

readRDS("./jeodpp_results/pd2dres_aridity.RDS") %>%
  mutate(plots = map(pd2d,pdp2_create))->pdp2dres

grid.arrange(varimp,pdp2dres[1,]$plots[[1]]+ scale_x_continuous(labels = div_format()),
             pdp2dres[2,]$plots[[1]]+ylim(c(0,4000))+scale_x_continuous(labels = div_format1()),
             pdp2dres[3,]$plots[[1]],pdp2dres[4,]$plots[[1]],
             pdp2dres[5,]$plots[[1]],ncol=2,nrow=3)->fig2combined

ggsave(fig2combined,filename = "./figures/paper/Fig2_10variables.png",width = 11, height = 10, dpi = 400)


pdp2dres[2,]$plots[[1]]


read_csv("./jeodpp_results/bio_01_mean_aridity.csv") %>%
  ggplot(aes(x=bio_01_mean,y=aridity,fill=yhat))+geom_raster()+
  scale_fill_viridis(option="inferno")+theme_bw()+
  theme(axis.text = element_text(size = 15,colour = "black"),
        axis.title = element_text(size=18),legend.title =element_blank())+xlab("Temperature")+
  ylab("Aridity")->p1


# local coefficients: lime and breakdown ------------------------------------------------------
# This particular implementation of lime uses ridge regression 
#explained here: <https://cran.r-project.org/web/packages/lime/vignettes/Understanding_lime.html> 

# lime 
readRDS("./jeodpp_results/limeres_Apr22.RDS") %>%
  inner_join(readRDS("./rastergrids/g2d.RDS")) %>%
  unnest(cols = res) %>%
  dplyr::select(x_2,y_2,feature,feature_weight) %>%
  rename(x = x_2,y = y_2,value = feature_weight)->res


readRDS("./jeodpp_results/limeres.RDS") %>%
  dplyr::select(-c(data)) %>%
  unnest(cols=c(res)) %>%
  dplyr::select(feature) %>%
  unique() %>%
  anti_join(res %>%
              dplyr::select(feature) %>%
              unique())

readRDS("./jeodpp_results/limeres_Apr22.RDS") %>%
  unnest(cols=c(res)) %>%
  filter(feature=="bio_01_mean") %>%
  dplyr::select(cat2d,feature_weight) %>%
  rename(new = feature_weight) %>%
  inner_join(readRDS("./jeodpp_results/limeres.RDS") %>%
  dplyr::select(-c(data)) %>%
  unnest(cols=c(res)) %>%
  filter(feature=="bio_01_mean") %>%
  dplyr::select(cat2d,feature_weight) %>%
  rename(old = feature_weight)
  )->dd

dd %>%
  inner_join(readRDS("./rastergrids/g2d.RDS")) %>%
  dplyr::select(x_2,y_2,new) %>%
  rasterFromXYZ(crs=latlon) %>%
  writeRaster(filename ="/home/marco/Desktop/new.tif")

readRDS("./jeodpp_results/limeres_Apr22.RDS") %>%
  unnest(cols=c(res)) %>%
  filter(feature=="bio_04_mean") %>%
  dplyr::select(cat2d,feature_weight) %>%
  inner_join(readRDS("./rastergrids/g2d.RDS")) %>%
  dplyr::select(x_2,y_2,feature_weight) %>%
  rasterFromXYZ(crs=latlon) %>%
  writeRaster(filename ="/home/marco/Desktop/new_bio_04.tif")



#/home/marco/Dropbox/phenology/jeodpp_results/

res %>%
  filter(feature=="bio_01_mean") %>%
  dplyr::select(x,y,value) %>%
  rasterFromXYZ(crs=latlon) %>%
  writeRaster(filename="/home/marco/Desktop/test.tif")
  

map_create<-function(x){
  #x %>%
  #  pull(value)->val
  #lims =c (quantile(val,probs=c(0.01))[1],quantile(val,probs=c(0.99))[1])
  
  # create labels
  x %>% 
    mutate(value1 = cut(value,quantile(value,probs=c(seq(from=0,to=1,length.out=10))),
                       include.lowest = T))->tmpdf
  str_replace(unique(tmpdf$value1),"]","") %>%
    str_replace("\\(","") %>%
    str_split_fixed(",",n=2) %>%
    as.data.frame() %>%
    as_tibble() %>%
    mutate(V1 =str_replace_all(V1, "\\[|\\]", "")) %>%
    mutate(V1=as.numeric(as.character(V1)),V2=as.numeric(as.character(V2))) %>%
    arrange(V1)->tmplabs
  mylabs<-c(paste0(tmplabs[1:8,]$V1),paste0(">", tmplabs[1:9,]$V1[9]))
  

  # colour palette
  rwb <- colorRampPalette(colors = c("red", "white", "blue"))
  rev(rwb(10))->mypal
  
  x %>%
    mutate(value1 = cut(value,quantile(value,probs=c(seq(from=0,to=1,length.out=10))),
                        include.lowest = T)) %>%
    ggplot() +
    #theme_bw()+
    theme(legend.position="none",
          panel.background = element_rect(fill = 'white',color="white"),
          strip.text = element_text(size = rel(2.5), face = "bold"),
          axis.text = element_blank(),axis.ticks = element_blank(),
          panel.grid.major = element_line(colour = 'transparent'))+
    geom_sf(data=world2,color='white',fill='grey25',size=0.09)+
    geom_tile(aes(x=x,y=y,fill=value1))+
    xlab("") + ylab("")+
    theme(legend.position=c(0.5, -0.1),legend.title = element_blank(),
          legend.justification=c(0.5),legend.key.width=unit(2, "mm"),
          legend.text = element_text(size =8),
          strip.background = element_rect(colour = "white", fill = "white"),
          strip.text = element_text(size = rel(2.5), face = "bold"),
          plot.title = element_text(hjust = 0.5,vjust = 0,size = 11))+
    scale_fill_manual(labels =mylabs,values = mypal,
                      guide = guide_legend(direction = "horizontal",
                                           keywidth = 1.4,keyheight = 0.2
                  ,title.position = 'top',title.hjust = 0.5,label.hjust = 0,
        nrow = 1,byrow = T,reverse = F,label.position = "bottom"))+xlab("")+ylab("")->p2
  
  return(p2)
}


res %>%
  group_by(feature) %>%
  nest() %>%
  mutate(plot = map(data,map_create)) %>%
  ungroup() %>%
  rename(varname = feature) %>%
  inner_join(predictors) %>%
  #mutate(currentindex=c(8,7,9,10,1,2,3,4,5,6),
  #       newindex = 1:10)
  
  #mutate(rn = as.numeric(row.names(.))) %>%
  #filter(rn %in% c(8,7,9,10,1,2,3,4,5,6))
  mutate(rown = c(5,6,7,8,9,10,2,1,3,4)) %>%
  arrange(rown)->res1



res1[1,]$plot[[1]] + theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  ggtitle("Annual Mean Temperature")->p1
res1[2,]$plot[[1]] + theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  ggtitle("Aridity")->p2
res1[3,]$plot[[1]] + theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  ggtitle("Temperature seasonality")->p3
res1[4,]$plot[[1]] + theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  ggtitle("Precipitation seasonality")->p4
res1[5,]$plot[[1]] + theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  ggtitle("sd Annual mean temperature")->p5
res1[6,]$plot[[1]] + theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  ggtitle("sd  Annual Precipitation")->p6
res1[7,]$plot[[1]] + theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  ggtitle("sd Temperature Seasonality")->p7
res1[8,]$plot[[1]] + theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  ggtitle("sd Precipitation Seasonality")->p8
res1[9,]$plot[[1]] + theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  ggtitle("Human footprint")->p9
res1[10,]$plot[[1]] + theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  ggtitle("Topographic heterogeneity")->p10



grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,ncol=3,nrow=4)->combined

ggsave(combined,filename = "./figures/supplementary/Fig5/local_response_LIME_June22.png",width=10,height=11,dpi=400)

# breakdown
read_csv("./jeodpp_results/dat1_DALEX.csv") %>%
  pull(cat2d)->cat2d

bind_rows(readRDS("./jeodpp_results/dalex_predict_parts.RDS")) %>%
  filter(varname !="intercept",varname!="") %>%
  group_by(varname) %>%
  mutate(cat2d = cat2d) %>%
  inner_join(readRDS("./rastergrids/g2d.RDS")) %>%
  rename(x = x_2,y = y_2,value = contribution)->res

res %>%
  ungroup() %>%
  filter(varname=="bio_01_mean") %>%
  dplyr::select(x,y,value) %>%
  rasterFromXYZ(crs=latlon) %>%
  writeRaster(filename = "/home/marco/Desktop/breakdown_latest.tif")


res %>%
  group_by(varname) %>%
  nest() %>%
  mutate(plot = map(data,map_create)) %>%
  ungroup() %>%
  inner_join(varorder) %>%
  arrange(index)->res1

tibble(varname = c("bio_01_mean","aridity","bio_04_mean","bio_15_mean",
  "bio_01","bio_12","bio_04","bio_15","humi","Htop"),
  index = 1:10)->varorder


res1[1,]$plot[[1]] + theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  ggtitle("Mean annual temperature")->p1
res1[2,]$plot[[1]] + theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  ggtitle("Aridity")->p2
res1[3,]$plot[[1]] + theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  ggtitle("Precipitation seasonality")->p4
res1[4,]$plot[[1]] + theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  ggtitle("Heterogeneity in mean annual temperature")->p5
res1[5,]$plot[[1]] + theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  ggtitle("Heterogeneity in annual cumulated precipitation")->p6
res1[6,]$plot[[1]] + theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  ggtitle("Heterogeneity in temperature seasonality")->p7
res1[7,]$plot[[1]] + theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  ggtitle("Heterogeneity in precipitation seasonality")->p8
res1[8,]$plot[[1]] + theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  ggtitle("Human impact index")->p9
res1[9,]$plot[[1]] + theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  ggtitle("Topographic heterogeneity")->p10



grid.arrange(p1,p2,p4,p5,p6,p7,p8,p9,p10,ncol=3,nrow=4)->combined

ggsave(combined,filename = "./figures/supplementary/local_response_breakdown.png",width=10,height=11,dpi=400)



# Fig 3. time series analysis ---------------------------------------------
# change period1 vs period2 
read_csv("./jeodpp_results/lmresults.csv") |>
    filter(beta > -50 & beta < 50) %>%
    inner_join(read_csv("./tmp/ecoregions.csv") |>
                 filter(!WWF_MHTNAM %in% c("Inland Water","Mangroves"))) |>
                 inner_join(read_csv("./ecoregions/biomes.csv"))  %>% {.->> tmp} |>
                 #filter(is.na(beta))) |> {.->> tmp} |>
    bind_rows(tmp |> mutate(WWF_MHTNAM = "Global")) |>
    mutate(WWF_MHTNAM = factor(WWF_MHTNAM,
                               levels=c("Global","Tundra",
                                        "Boreal Forests/Taiga",
                                        "Temperate Conifer Forests",
                                        "Temperate Broadleaf and Mixed Forests",
                                        "Tropical and Subtropical Moist Broadleaf Forests",
                                        "Tropical and Subtropical Dry Broadleaf Forests",
                                        "Tropical and Subtropical Grasslands, Savannas and Shrublands"))) |>
    mutate(pcol = ifelse(p.value < 0.05,"sig","nsig")) %>%
  filter(pcol=="sig")->p1vsp2
# betas climate coefficients
read_csv("./trends/clim_sensitivity/results.csv") %>%
  mutate(WWF_MHTNAM= "Global")  %>%
  bind_rows(read_csv("./trends/clim_sensitivity/results.csv") %>%
              inner_join(read_csv("./tmp/ecoregions.csv") %>%
                           filter(!WWF_MHTNAM %in% c("Inland Water","Mangroves"))) %>%
                           inner_join(read_csv("./ecoregions/biomes.csv"))) %>%
                           #filter(is.na(out)))) %>%
  mutate(WWF_MHTNAM = factor(WWF_MHTNAM,
                             levels=c("Global","Tundra",
                                      "Boreal Forests/Taiga",
                                      "Temperate Conifer Forests",
                                      "Temperate Broadleaf and Mixed Forests",
                                      "Tropical and Subtropical Moist Broadleaf Forests",
                                      "Tropical and Subtropical Dry Broadleaf Forests",
                                      "Tropical and Subtropical Grasslands, Savannas and Shrublands")))->betaclim




# alternative figure
fig3<-function(x)
  {
  # summary of coefficients
  x %>%
    #inner_join(read_csv("./data/phenodat5_5km.csv") %>%
    #             dplyr::select(cat)) %>%
    filter(WWF_MHTNAM!="Global") %>%
    group_by(WWF_MHTNAM) %>%
    summarise(betamed = median(beta),
              beta05 = as.numeric(quantile(beta,probs=c(0.25))),
              beta95 = quantile(beta,probs=c(0.75)))->betamed
  # global average
  x %>%
    #filter(beta > -25 & beta < 25) %>%
    #inner_join(read_csv("./data/phenodat5_5km.csv") %>%
    #             dplyr::select(cat)) %>%
    filter(WWF_MHTNAM=="Global") %>%
    summarise(betamed = median(beta),
              beta05 = as.numeric(quantile(beta,probs=c(0.25))),
              beta95 = quantile(beta,probs=c(0.75))) %>%
    pull(betamed)->globalavg
  # plot
  ggplot(data= betamed,aes(x=WWF_MHTNAM,y=beta,color = WWF_MHTNAM)) +coord_flip()+
    geom_segment(aes(x = WWF_MHTNAM, xend = WWF_MHTNAM,
                     y = 0, yend = betamed),
                 size = 0.8)+geom_hline(aes(yintercept = 0), color = "gray70", size = 0.6,
                                        linetype="dashed")+
    geom_point(aes(WWF_MHTNAM, betamed), size = 5)+theme_minimal()+
    theme(legend.position ='none',axis.text = element_text(color="black"))+
    scale_x_discrete(labels = function(x) str_wrap(str_replace_all(x, "foo" , " "),
                                                   width = 25))+
    geom_hline(aes(yintercept = globalavg), color = "red", size = 0.6)+
    geom_point(aes(WWF_MHTNAM, beta05),shape=17, size = 2)+
    geom_point(aes(WWF_MHTNAM, beta95),shape=17, size = 2)+xlab("Biome")->p1
  return(p1)
}


fig3b<-function(x)
{
  # summary of coefficients+ theme(plot.margin = unit(c(-1,0,0,0), "cm"))
  x %>%
    #inner_join(read_csv("./data/phenodat5_5km.csv") %>%
    #             dplyr::select(cat)) %>%
    filter(WWF_MHTNAM!="Global") %>%
    inner_join(tibble(WWF_MHTNAM = c("Tundra","Tropical and Subtropical Dry Broadleaf Forests",
                                     "Tropical and Subtropical Grasslands, Savannas and Shrublands",
                                     "Temperate Broadleaf and Mixed Forests",
                                     "Boreal Forests/Taiga",
                                     "Tropical and Subtropical Moist Broadleaf Forests",
                                     "Temperate Conifer Forests") ,
                      WWFMHTNAM1= c("Boreal","Arid","Arid",
                                    "Temperate","Boreal","Tropical","Temperate")) %>%
                 mutate(WWFMHTNAM1= factor(WWFMHTNAM1,levels=c("Tropical","Arid","Temperate","Boreal")))) %>%
    group_by(WWFMHTNAM1) %>%
    summarise(betamed = median(beta),
              beta05 = as.numeric(quantile(beta,probs=c(0.25))),
              beta95 = quantile(beta,probs=c(0.75)))->betamed
  # global average
  x %>%
    #filter(beta > -25 & beta < 25) %>%
    #inner_join(read_csv("./data/phenodat5_5km.csv") %>%
    #             dplyr::select(cat)) %>%
    filter(WWF_MHTNAM=="Global") %>%
    summarise(betamed = median(beta),
              beta05 = as.numeric(quantile(beta,probs=c(0.25))),
              beta95 = quantile(beta,probs=c(0.75))) %>%
    pull(betamed)->globalavg
  # plot+ theme(plot.margin = unit(c(-1,0,0,0), "cm"))
  ggplot(data= betamed,aes(x=WWFMHTNAM1,y=beta,color = WWFMHTNAM1)) +coord_flip()+
    geom_segment(aes(x = WWFMHTNAM1, xend =WWFMHTNAM1,
                     y = 0, yend = betamed),
                 size = 1)+geom_hline(aes(yintercept = 0), color = "gray70", size = 0.6,
                                        linetype="dashed")+
    geom_point(aes(WWFMHTNAM1, betamed), size = 7)+theme_minimal()+
    theme(legend.position ='none',axis.text = element_text(color="black",size=12),
          axis.title = element_text(size=15))+
    geom_hline(aes(yintercept = globalavg), color = "red", size = 0.6)+
    geom_point(aes(WWFMHTNAM1, beta05),shape=17, size = 3)+
    geom_point(aes(WWFMHTNAM1, beta95),shape=17, size = 3)+xlab("Biome")+
    scale_colour_manual(values=rev(c("#009E73","#E69F00","#0072B2","#CC79A7")))->p1
  
  return(p1)
}


fig3b(p1vsp2)+ylab("Change in forest phenological diversity\n (2020-2011 vs. 2003-2010)")+
  theme(axis.title.x = element_blank())+
  theme(panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 1))->p1

fig3b(p1vsp2)+ylab("Change in forest phenological diversity\n (2020-2011 vs. 2003-2010)")+
  theme(panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 1))+
  annotate("text",x = Inf, y = Inf,label = "\n A  ", 
           vjust = "top", hjust = "right",
           size=6)->p1


betaclim %>%
  rename(beta = estimate) %>%
  filter(term=="bio1") %>%
  filter(p.value < 0.05) %>%
  fig3b()+ylab("Annual Mean Temperature\n")+theme(axis.text.y = element_blank(),
                                               axis.title.y = element_blank())+
  theme(panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 1))+
  annotate("text",x = Inf, y = Inf,label = "\n B  ", 
           vjust = "top", hjust = "right",
           size=6)->p2

betaclim %>%
  rename(beta = estimate) %>%
  filter(term=="bio12") %>%
  filter(p.value < 0.05) %>%
  fig3b()+ylab("Annual Precipitation\n")+theme(axis.text.y = element_blank(),
  axis.title.y = element_blank())+
  theme(panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 1))+
  annotate("text",x = Inf, y = Inf,label = "\n C  ", 
           vjust = "top", hjust = "right",
           size=6)->p3


biome_map<-function(x)
{
  x %>%
    ggplot()+geom_sf(fill=x$fillcol)+theme_void()+
    theme(legend.position = 'none',strip.text = element_blank())+
    geom_sf(data=world2,fill = NA,size = 0.3)+theme_void()->p
  return(p)
}

st_read("./ecoregions_dissolved/ecodissolved_simple.shp") %>%
  mutate(id = c(2,1,4,3)) %>%
  arrange(id) %>%
  mutate(fillcol = c("#009E73","#E69F00","#0072B2","#CC79A7")) %>%
  group_by(WWF_MHT) %>%
  nest() %>%
  ungroup() %>%
  mutate(biomes = map(data,biome_map))->biome_guide


st_read("./ecoregions_dissolved/ecodissolved_simple.shp") %>%
  mutate(title = WWF_MHT) %>%
  mutate(id = c(2,1,4,3)) %>%
  arrange(id) %>%
  mutate(fillcol = c("#009E73","#E69F00","#0072B2","#CC79A7")) %>%
  group_by(WWF_MHT) %>%
  nest() %>%
  mutate(bmap = map(data,~ggplot(data =.x,aes())+
                      geom_sf(data=world2,fill = "grey60", size = 0)+
                      geom_sf(fill=.x$fillcol, size = 0) + theme_void() +
                      theme(legend.position = "none",
                            plot.title = element_text(hjust = 0.5),
                            strip.text = element_blank()) +
                      ggtitle(.x$title)))->biomemaps


lay <- rbind(c(1,2,3,4),
             c(1,2,3,5),
             c(1,2,3,6),
             c(1,2,3,7))


# grid.arrange(p1,p2,p3,
#              biome_guide[1,]$biomes[[1]],
#              biome_guide[2,]$biomes[[1]]+ theme(plot.margin = unit(c(-1,0,0,0), "cm")),
#              biome_guide[3,]$biomes[[1]]+ theme(plot.margin = unit(c(-2,0,0,0), "cm")),
#              biome_guide[4,]$biomes[[1]]+ theme(plot.margin = unit(c(-3.5,0,0,0), "cm")),
#              layout_matrix = lay,widths = c(1.6,1.3,1.3,0.8))->trendfig

grid.arrange(p1,p2,p3,
             biomemaps[1,]$bmap[[1]],
             biomemaps[2,]$bmap[[1]]+ theme(plot.margin = unit(c(-1,0,0,0), "cm")),
             biomemaps[3,]$bmap[[1]]+ theme(plot.margin = unit(c(-2,0,0,0), "cm")),
             biomemaps[4,]$bmap[[1]]+ theme(plot.margin = unit(c(-3.5,0,0,0), "cm")),
             layout_matrix = lay,widths = c(1.6,1.3,1.3,0.8))->trendfig


             
ggsave(trendfig,filename="./figures/paper/Fig3/Fig3_August22.png",dpi =400,height = 8,
       width = 15)




# Summary of local coefficients at biome-level ------------------------------------------------
# get a biome level coefficients


# biomes  
read_csv("./tmp/ecoregions.csv") |>
  filter(!WWF_MHTNAM %in% c("Inland Water","Mangroves")) |>
  inner_join(read_csv("./ecoregions/biomes.csv"))  %>% {.->> tmp} |>
  #filter(is.na(beta))) |> {.->> tmp} |>
  bind_rows(tmp |> mutate(WWF_MHTNAM = "Global")) |>
  mutate(WWF_MHTNAM = factor(WWF_MHTNAM,
                             levels=c("Global","Tundra",
                                      "Boreal Forests/Taiga",
                                      "Temperate Conifer Forests",
                                      "Temperate Broadleaf and Mixed Forests",
                                      "Tropical and Subtropical Moist Broadleaf Forests",
                                      "Tropical and Subtropical Dry Broadleaf Forests",
                                      "Tropical and Subtropical Grasslands, Savannas and Shrublands")))->biomes
  
# grid 2 degrees
read_csv("tmp/gridstats2degree",col_names = FALSE) |>
  rename(cat = X1,cat2d=X2) |> 
  inner_join(biomes)->catbiome

catbiome |>
  dplyr::select(cat2d) |>
  unique() |>
  dim()

catbiome |>
  group_by(cat2d,WWF_MHTNAM_agg) |>
  summarise(WWFf = length(WWF_MHTNAM_agg)) |>
  group_by(cat2d) |>
  filter(WWFf==max(WWFf)) |>
  unique() |>
  inner_join(readRDS("./jeodpp_results/limeres_Apr22.RDS") %>%
                 inner_join(readRDS("./rastergrids/g2d.RDS")) %>%
                 unnest(cols = res)) |>
  mutate(direction = ifelse(feature_weight < 0,"neg","pos")) |>
  group_by(WWF_MHTNAM_agg,direction,feature) |>
  summarise(dir = length(direction)) |>
  filter(!is.na(WWF_MHTNAM_agg))->dirdf

dirdf |>
  group_by(WWF_MHTNAM_agg,feature) |>
  summarise(sum_dir = sum(dir)) |>
  inner_join(dirdf) |>
  mutate(prop = dir/sum_dir) |>
  inner_join(predictors %>%
               rename(feature = varname) %>%
               mutate(label1 = case_when(label1=="Human impact"~"Human footprint",TRUE~as.character(label1)))) %>%
  ggplot(aes(fill=direction, y=prop, x=label1)) + coord_flip()+
  geom_bar(position="stack", stat="identity")+facet_wrap(~WWF_MHTNAM_agg)+
  theme_bw()+
  theme(axis.text.x=element_text(hjust=1,size=15,colour="black"),strip.text=element_text(size=17,face="bold"),
        axis.text.y=element_text(size=15,colour="black"),axis.title.x=element_text(size=17))+
  xlab("")+ylab("Proportion of cofficients")->propcoefs

ggsave(propcoefs,filename="./figures/supplementary/Fig6/Fig6Jun22.png",width=12,heigh=12,
       dpi = 400)

filtprop<-function(x){
  return(x |> filter(prop==max(prop)))
}

  
dirdf |>
  group_by(WWF_MHTNAM_agg,feature) |>
  summarise(sum_dir = sum(dir)) |>
  inner_join(dirdf) |>
  mutate(prop = dir/sum_dir) |>
  inner_join(predictors |> rename(feature = varname)) |>
  dplyr::select(c(WWF_MHTNAM_agg,label1,prop,direction)) |>
  group_by(WWF_MHTNAM_agg,label1) |>
  nest() |>
  mutate(res = map(data,filtprop)) |>
  dplyr::select(-c(data)) |>
  unnest(cols =c(res)) |>
  #summarise(max = max(prop)) |>
  write_csv("/home/marco/Desktop/prop.csv")

# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


# 
#   
#   readRDS("./jeodpp_results/limeres_Apr22.RDS") %>%
#   inner_join(readRDS("./rastergrids/g2d.RDS")) %>%
#   unnest(cols = res) %>%
#   dplyr::select(x_2,y_2,feature,feature_weight) %>%
#   rename(x = x_2,y = y_2,value = feature_weight)->res


# 
#   
# read_csv("tmp/gridstats2degree",col_names = FALSE) |>
#   rename(cat = X1,cat2d=X2)
# 
# read_csv("./GRASSGIS_files/pft.csv") |>
#   inner_join(grid5km)
# 
# readRDS("./jeodpp_results/pd2dres_Mar22.RDS")




# summary of pdpa by biom -----------------------------------------------------------------------

readRDS("./jeodpp_results/pd2dres_Mar22.RDS") %>%
  slice(1)->dat

dat$pd2d[[1]] |>
  as_tibble()->df

biomes |>
  # bioclim background climate
  inner_join(read_csv("./GRASSGIS_files/bioclim.csv") %>%
               dplyr::select(c(cat,bio_01,bio_12,bio_15,bio_04)) %>%
               rename(bio_01_mean = bio_01,bio_12_mean = bio_12,
                      bio_15_mean = bio_15,
                      bio_04_mean = bio_04)) |>
  filter(!is.na(WWF_MHTNAM_agg)) |>
  dplyr::select(cat,WWF_MHTNAM_agg,bio_15_mean,bio_04_mean) |>
  filter((bio_15_mean >= 5 &  bio_15_mean <= 156) & (bio_04_mean >= 160.4 &  bio_04_mean <= 22924.1)) |>
  mutate(bio_04_mean = bio_04_mean/1000) |>
  #filter((bio_04_mean >= 160.4 &  bio_04_mean <= 22924.1)) |>
  group_by(WWF_MHTNAM_agg) |>
  summarise(bio_15_min = min(bio_15_mean),
            bio_15_max = max(bio_15_mean),
            bio_04_min = min(bio_04_mean),
            bio_04_max = max(bio_04_mean))->lookupseas
  


as_tibble(dat$pd2d[[1]]) |>
  mutate(biome = "Global")->tseaspdp

tseaspdp |>
  mutate(bio_04_mean = bio_04_mean/1000) |>
  filter((bio_15_mean > 7 &  bio_15_mean < 156) & (bio_04_mean > 0.183 &  bio_04_mean < 6.55)) |>
  mutate(biome = "Arid") |>
  bind_rows(tseaspdp |>
              mutate(bio_04_mean = bio_04_mean/1000) |>
              filter((bio_15_mean > 5 &  bio_15_mean < 128) & (bio_04_mean > 1.38 &  bio_04_mean < 22.9)) |>
              mutate(biome = "Boreal")) |>
  bind_rows(tseaspdp |>
              mutate(bio_04_mean = bio_04_mean/1000) |>
              filter((bio_15_mean > 5 &  bio_15_mean < 140) & (bio_04_mean > 1.50 &  bio_04_mean < 17.4)) |>
              mutate(biome = "Temperate")) |>
bind_rows(tseaspdp |>
            mutate(bio_04_mean = bio_04_mean/1000) |>
            filter((bio_15_mean > 5.5 &  bio_15_mean < 156) & (bio_04_mean > 0.160 &  bio_04_mean < 8.36)) |>
            mutate(biome = "Tropical")) |>
  bind_rows(tseaspdp |> mutate(bio_04_mean = bio_04_mean/1000))->biomespace


|>
  ggplot()+geom_raster(aes(x=bio_04_mean,y=bio_15_mean,fill=yhat))+
  scale_fill_viridis(option="inferno")+theme_bw()+  
  theme(axis.text = element_text(size = 14,colour = "black"),
        axis.title = element_text(size=16),legend.title =element_blank())+xlab("Temperature seasonality")+
  ylab("Precipitation seasonality")+facet_wrap(~biome,scale="fixed")


phenodat |>
  inner_join(biomes) |>
  filter(bio_12_mean < 4000) |>
  filter(!is.infinite(aridity)) |>
  dplyr::select(bio_04_mean,bio_15_mean,value,cat) |>
  filter((bio_15_mean >= 5 &  bio_15_mean <= 156) & (bio_04_mean >= 160.4 &  bio_04_mean <= 22924.1)) |>
  mutate(bio_04_mean = bio_04_mean/1000) |>
  inner_join(biomes)->dat1
dat1 |>
  


breaks <- seq(from = 0, to = 20, by = 1)
breaksD <- seq(from = 0, to = 156, by = 8)

dat1 %>%
  # group_by(cat2d) |>
  # summarise(bio_04_mean = mean(bio_04_mean),
  #           bio_15_mean = mean(bio_15_mean),
  #           value = mean(value)) |>
  mutate(bio_15_meanf = cut(bio_15_mean,breaksD),
         bio_04_meanf = cut(bio_04_mean,breaks)) |>
  group_by(bio_15_meanf,bio_04_meanf) |>
  summarise(value = mean(value)) |>
  mutate(bio_15_mean = as.numeric(bio_15_meanf),
         bio_04_mean = as.numeric(bio_04_meanf)) |>
  ggplot(aes(x=bio_04_mean,y=bio_15_mean,fill=value))+
  geom_tile()+scale_fill_viridis(option="inferno",limits=c(50,77),oob=squish)
  
dat1 %>%
  filter(value < 77 & value >= 50) |>
  ggplot(aes(x=bio_04_mean,y=bio_15_mean,z=value))+
  # geom_tile()
  theme_minimal()+
  stat_summary_2d(fun = function(x) {if(length(x[!is.infinite(x)])>=5)(mean(x))else(NA)},bins = 30,na.rm = FALSE)+
  geom_tile(aes(fill=value))+
  scale_fill_viridis(option="inferno",limits=c(58,65),oob=squish)+theme_bw()+
  theme(axis.text = element_text(size = 14,colour = "black"),
        axis.title = element_text(size=16),legend.title =element_blank())+xlab("Temperature seasonality")+
  ylab("Precipitation seasonality")+facet_wrap(~WWF_MHTNAM)

  
phenodat |> 
  dplyr::select(x,y,bio_04_mean,bio_15_mean) |>
  mutate(bio_04_mean = bio_04_mean/1000) |>
  rasterFromXYZ(crs=latlon) |>
  writeRaster(filename = "/home/marco/Desktop/seas.tif",overwrite = TRUE)










# cross-validated R2 ------------------------------------------------------
read_csv("./jeodpp_results/crossR2.csv") %>%
  arrange(it) %>%
  mutate(CV = factor(paste0("Fold ",it),levels = c(paste0("Fold ",it)))) %>%
  ggplot(aes(x=it, y=R2))+geom_point(,size = 5,color="blue")+
  geom_hline(yintercept=0.4315,color="red")+
  geom_linerange(aes(x=it, ymax=R2, ymin=0))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10))+
  ylim(0,1)+theme_bw()+theme(axis.text = element_text(size=15,color = "black"),
                                  axis.title = element_text(size=19,colour = "black"))+
  xlab("Cross-validation fold number")+ylab(expression('R'^2))->R2cvalid
  
ggsave(R2cvalid,filename = "/home/marco/Dropbox/phenology/figures/supplementary/Fig7/Fig7R2.png",
       dpi = 400, height = 8, width = 8)

