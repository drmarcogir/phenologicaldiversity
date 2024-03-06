library(tidyverse)

read_csv("/home/marco/Desktop/stats1",col_names = F) %>%
  rename(cat=X1,stab=X2,ch=X3) %>%
  mutate(stab = scales::rescale(stab,to=c(0,1))) %>%
  mutate(ch = scales::rescale(ch,to=c(0,1)))->dat

         ,ch = scales::rescale(ch,to=(0,1)))->dat



get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

dat %>%
  mutate(ch=log(ch+1),stab=log(stab+1)) %>%
  filter(stab <= 0.6) %>% {.->> tmp } %>%
  mutate(density = get_density(ch, stab, n = 100)) %>%
  ggplot(aes(x=ch,y=stab))+ geom_bin2d(bins=30)+scale_fill_viridis(trans="log")+
  geom_smooth(method = "lm")+theme_minimal()


  #mutate(ch=log(ch+1),stab=log(stab+1)) %>%
  ggplot(aes(x=ch,y=stab,color=density))+geom_point()+scale_color_viridis()
  
  geom_smooth(method = "lm")

+stat_summary_2d()
  
  
# list files  
list.files("/media/marco/marcodata19/height/canopy_height1",full.names = T)->chl
list.files("/media/marco/marcodata19/height/stability_metrics",full.names = T)->stab
list.files("/media/marco/marcodata19/height/stability_metrics2",full.names = T)->test


# raster grid
as_tibble(rastertodf(raster("/mnt/data1tb/Dropbox/phenology/gridGEE/grid5km_more75.tif")))->df
# canopy height data  
map_dfr(chl,read_csv) %>%
  dplyr::select(zone,stdDev) %>%
  rename(sd = stdDev,cat=zone) %>%
  filter(!is.na(sd)) %>% 
  group_by(cat) %>%
  summarise(sd = mean(sd))->ch
  
ch %>%
  inner_join(df %>%
               rename(cat = value)) %>%
  dplyr::select(x,y,sd) %>%
  rasterFromXYZ() ->chr
writeRaster(chr,filename = "/home/marco/Desktop/myr.tif")

# stability data

map_dfr(stab,read_csv) %>%
  dplyr::select(zone,mean) %>%
  rename(cat=zone) %>%
  filter(!is.na(mean)) %>%
  group_by(cat) %>%
  summarise(mean = mean(mean))->stabdf

inner_join(stabdf,ch) %>%
  inner_join(df %>%
               rename(cat = value)) %>%
  dplyr::select(x,y,mean) %>%
  rasterFromXYZ() ->stabr 
writeRaster(stabr,filename = "/home/marco/Desktop/stabr.tif")


map_dfr(test,read_csv) %>%
  dplyr::select(zone,mean) %>%
  rename(cat=zone) %>%
  filter(!is.na(mean)) %>%
  group_by(cat) %>%
  summarise(mean = mean(mean))->stabdf

read_csv("/media/marco/marcodata19/stability_1.csv") %>%
  dplyr::select(mean,zone) %>%
  rename(cat = zone)->cv

read_csv("/media/marco/marcodata19/height/stability_metrics2/stability_1.csv") %>%
  dplyr::select(mean,zone) %>%
  rename(cat = zone,mean1 = mean)->sd
  


inner_join(stabdf,ch)->dat 

  dat %>%
  rename(stab = mean,ch=sd) %>%
  #mutate(ch=log(ch+1),stab=log(stab+1)) %>%
  #filter(stab > 4) %>%
  #mutate(density = get_density(ch, stab, n = 100)) %>%
  ggplot(aes(x=ch,y=stab))+ geom_bin2d(bins=30)+scale_fill_viridis(trans="log")+
  geom_smooth(method = "lm")+theme_minimal()


as.data.frame(dat)->dat1
dat1[3]->X
dat1[2]$mean->y
mod<-gam(mean~s(sd),data=dat1)

## Create an 'ice' object for the predictor "age":
bhd.ice = ice(object = mod, X =X , y =y, predictor = "sd",
              frac_to_build = .1)



    
    