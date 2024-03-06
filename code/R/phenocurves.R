library(tidyverse)
library(scales)
library(sf)

res<-NULL

for(i in 1:30){
  sample(seq(from=0.6,to=0.9,by=0.01),1)->ceil
  tibble(ndvi = scales::rescale(rnorm(182),to=c(0,ceil))) %>%
    arrange(ndvi) %>%
    mutate(DOY = 1:length(ndvi)) %>%
    #bind_rows(tibble(ndvi = 0.5,DOY=183)) %>%
    bind_rows(tibble(ndvi = scales::rescale(rnorm(182),to=c(ceil,0))) %>%
                arrange(desc(ndvi)) %>%
                mutate(DOY = 184:365))->dd1
  dd1 %>% mutate(id = i)->dd2
  bind_rows(dd2,res)->res
}

# upper left panel
res %>%
  #ggplot(aes(x=DOY,y=ndvi))+geom_point()
  group_by(id) %>%
  # select only a few "pixels"...
  filter(id %in% c(1:9)) %>%
  mutate(ndvi_cumsum = cumsum(ndvi)) %>%
  arrange(id) %>%
  mutate(Pixel = paste("Pixel", id)) %>%
  mutate(Pixel = factor(Pixel,levels = unique(Pixel))) %>%
  ggplot(data=.)+geom_line(aes(x=DOY,y=ndvi_cumsum),colour ='red',size=0.9)+
  facet_wrap(~Pixel, ncol = 3)+theme_bw()+ylab("Cumulated NDVI")+xlab("Day of the year")+
  xlim(1,365)+ylim(0.1,160)+scale_x_continuous(breaks = c(1,100,200,300))+
  theme(axis.text.y = element_text(size = 10,color ="black"),
        axis.text.x = element_text(size = 10, angle = 90, hjust = 1,
                                   color ="black"),
        strip.text = element_text(size = 10, color = "black"))->p1

ggsave(p1,filename ="./figures/paper/Fig4/components/cumNDVI.png",width = 3, height = 5,dpi = 400)

# old figure from supplementary materials

# CUMULATED NDVI
res %>%
  # select only a few "pixels"...
  filter(id %in% c(1:9)) %>%
  group_by(id) %>%
  mutate(ndvi_cumsum =  cumsum(ndvi)) %>%
  arrange(id) %>%
  mutate(Pixel = paste("Pixel", id)) %>%
  mutate(Pixel = factor(Pixel,levels = unique(Pixel))) %>%
  ggplot(data=.)+geom_line(aes(x=DOY,y=ndvi_cumsum,group=id),colour ='red',size=0.8, alpha = 0.5)+
  ylab("Cumulated NDVI")+theme_minimal()+theme(axis.text = element_text(size=14,colour = "black"),
                                               axis.title =element_text (size=17))+
  xlim(1,365)+ylim(0.1,160)+scale_x_continuous(breaks = c(1,100,200,300))->p2


ggsave(p2,filename = "./figures/paper/Fig4/components/cumNDVI_all.png",width = 3.5, height = 5,dpi = 400, bg = "white")


# NORMALISED CUMULATED NDVI
res %>%
  # select only a few "pixels"...
  filter(id %in% c(1:9)) %>%
  group_by(id) %>%
  mutate(ndvi_cumsum =   rescale(cumsum(ndvi))) %>%
  arrange(id) %>%
  mutate(Pixel = paste("Pixel", id)) %>%
  mutate(Pixel = factor(Pixel,levels = unique(Pixel))) %>%
  ggplot(data=.)+geom_line(aes(x=DOY,y=ndvi_cumsum,group=id),colour ='blue',size=0.2, alpha = 0.4)+
  ylab("Normalised Cumulated NDVI")+theme_minimal()+theme(axis.text = element_text(size=14,colour = "black"),
                                               axis.title =element_text (size=17))+
  xlim(1,365)+ylim(0,1)+scale_x_continuous(breaks = c(1,100,200,300))->p3


ggsave(p3,filename = "./figures/paper/Fig4/components/cumNDVI_all_norm.png",width = 3.5, height = 5,dpi = 400,
       bg = "white")

# DEVIATIONS AT EACH TIME-STEP 
res %>%
  # select only a few "pixels"...
  filter(id %in% c(1:9)) %>%
  group_by(id) %>%
  mutate(ndvi_cumsum_norm =   rescale(cumsum(ndvi))) %>%
  group_by(DOY) %>%
  summarise(norm_mean = mean(ndvi_cumsum_norm),
            norm_sd = sd(ndvi_cumsum_norm))->dfmean 

inner_join(dfmean,tibble (DOY = seq(from=1,to=365,by=8)))->dfpoints


res %>%
  # select only a few "pixels"...
  filter(id %in% c(1:9)) %>%
  group_by(id) %>%
  mutate(ndvi_cumsum =   rescale(cumsum(ndvi))) %>%
  arrange(id) %>%
  mutate(Pixel = paste("Pixel", id)) %>%
  mutate(Pixel = factor(Pixel,levels = unique(Pixel))) %>%
ggplot(data=.)+geom_line(aes(x=DOY,y=ndvi_cumsum,group=id),colour ='grey20',size=0.1)+
  ylab("Normalised Cumulated NDVI")+
  geom_line(data=dfmean,aes(x=DOY,y=norm_mean),color="red",size=0.3)+
  geom_point(data = dfpoints,aes(x=DOY,y=norm_mean),colour="red",size=1)+
  geom_errorbar(data=dfpoints,aes(x=DOY,y=norm_mean,ymin=norm_mean-(norm_sd), 
                                  ymax=norm_mean+(norm_sd)), 
                width=1,color="red",size=0.5)+theme_minimal()+
  theme(axis.text = element_text(size=14,colour = "black"),
        axis.title =element_text (size=17))->p4

ggsave(p4,filename = "./figures/paper/Fig4/components/deviationsNDVI.png",width = 5, height = 5.5,dpi = 400,bg = "white")

laea<-"+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"



st_read("/home/marco/Desktop/bbox_phenology2.gpkg")->bbox
st_bbox(bbox)->bbox1

st_make_grid(bbox1, cellsize = 4000, square = TRUE,crs = laea) %>%
  st_as_sf() %>%
  rename(geom = x) %>%
  mutate(phenodiv = sample(seq(from = 1, to = 90,by = 0.01),length(geom)))->myp

st_write(myp,"/home/marco/Desktop/grid.gpkg")
st_read("/home/marco/Desktop/gridv1.gpkg") %>%
  mutate(phenodiv = round(phenodiv,digits = 1))->myp

st_write(myp,"/home/marco/Desktop/myp.gpkg")

myp %>%  
  ggplot()+geom_sf(aes(fill = phenodiv))+scale_fill_viridis(option = "inferno",alpha = 0.5)+
  theme_minimal()+theme(axis.text = element_blank(),
                        legend.position = "bottom",
                        panel.grid = element_blank())+
  geom_sf_text(aes(label = phenodiv), colour = "black", size = 4)+
  xlab("")+ylab("")->phenodiv



ggsave(phenodiv,filename = "./figures/paper/Fig4/components/phenodiv.png",width = 5, height = 4,dpi = 400,bg = "white")


