library(raster)
library(marcoUtils)
library(tidyverse)
library(sf)
library(phenoutils)

raster("./gregdataset/dif005.tif") %>%
  aggregate(fun = mean,fact = 10)

raster("./gregdataset/dif005.tif") %>%
  aggregate(fun = mean,fact = 10) %>%
  projectRaster(crs=CRS("+proj=robin +lon_0=0 +x_0=0 
                +y_0=0 +ellps=WGS84 +datum=WGS84 
                        +units=m +no_defs"))->dd1

template<-raster("/home/marco/Desktop/template.tif")
resample(dd1,template) %>%
  rastertodf(.) %>%
  mutate(value1=cut(value,breaks=round(unique(quantile(value,
                                                       probs = seq(0, 1, length.out = 7 + 1))),digits = 2))) %>%
  mutate(title ="greg's metric")->tmpdf

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


ggsave(p,filename = "./figures/Sep21/Gregsmetric_changesv2.png",height=9,width = 12,dpi=400)
