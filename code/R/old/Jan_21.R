library(tidyverse);library(raster)


# read files --------------------------------------------------------------

# climate data
readRDS("/media/marco/marcodata19/data1tb_laptop/phenology/rds/clim.RDS") %>%
  dplyr::select(x,y,bio_1,bio_12) %>%
  rasterFromXYZ(crs=CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")) %>%
  projectRaster(.,crs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),res=0.05)->climdt

  readRDS("/media/marco/marcodata19/data1tb_laptop/phenology/rds/pft_prop.RDS")->pft


raster("/home/marco/Desktop/pheno5km_2002.tif") %>%
  projectRaster(.,crs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),res=0.05)->pheno2002
  
# PFT forests > 0.80 
readRDS("/media/marco/marcodata19/data1tb_laptop/phenology/rds/pft_prop.RDS") %>%
  filter(prop > 0.70) %>%
  mutate(PFTn = as.integer(as.factor(PFT))) %>% {.->>tmp }%>%
  dplyr::select(x,y,PFTn) %>%
  rasterFromXYZ(crs=CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")) %>%
  projectRaster(.,crs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),res=0.05)->pftsr

writeRaster(pftsr,filename = "/home/marco/Desktop/pft.tif")

rastertodf(pftsr) %>%
  inner_join(grid5km)


dim(unique(dd[1:2]))
dim(dd[1:2])
resample(pftsr,pheno2002)->pftsr1

mask(pheno2002,pftsr1)->pheno2002a

resample(climdt,pheno2002a)->climdt1
writeRaster(pheno2002a,filename="/home/marco/Desktop/tmpmaps/pheno.tif")
writeRaster(climdt,filename="/home/marco/Desktop/tmpmaps/clim.tif")

as_tibble(rastertodf(pheno2002a)) %>%
  inner_join(as_tibble(rastertodf(climdt1)))->dd

read_csv("/home/marco/Desktop/tmpmaps/stats",col_names = FALSE) %>%
  rename(pheno = X1, temp = X2, prec = X3)->dat

dat %>%
ggplot(aes(x=prec,y=temp,z=pheno))+
                    stat_summary_2d(fun = function(x) {if(length(x[!is.infinite(x)])>=5)(mean(x))else(NA)},bins = 30,na.rm = FALSE)+
                    theme_minimal()+
                    theme(legend.position = "right",legend.title = element_blank(),strip.text.x = element_text(size=17,face="bold"),
                          strip.text.y = element_text(size=14,face="bold"),axis.text=element_text(size=12,colour="black"),
                          axis.title = element_text(size = 15),strip.background = element_rect(colour = "white", fill = "white"),
                          plot.title = element_text(hjust = 0.5,size=30,face="bold"))+xlab("Precipitation")+ylab("Temperature")+
scale_fill_distiller(palette = "RdYlBu")->myplot

ggsave(myplot,filename = "/mnt/data1tb/Dropbox/phenology/figures/jan21/heatmap.png",width=8,height=8,dpi=400)



