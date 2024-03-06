library(terra)
library(marcoUtils)

# read in phenology data
read_csv("./scripts/gregory/data/phenodat.csv")->phenodat
phenodat %>%
  filter(forest_prop > 0.85) %>%
  dplyr::select(x,y) %>%
  mutate(id = 1) %>%
  rast(type = "xyz",crs = latlon) %>%
  writeRaster(filename = "/home/marco/Desktop/mask5km_for.tif")


names(phenodat)

phenodat %>%
  dplyr::select(x,y) %>%
  mutate(id = 1) %>%
  rast(type = "xyz",crs = latlon) %>%
  writeRaster("/home/marco/Desktop/mask5km.tif")

st_read("/home/marco/Desktop/gapfilled/2003filt.gpkg") %>%
  filter(!is.na(filtdata2003_average)) %>%
  st_write("/home/marco/Desktop/gapfilled/2003filt.gpkg")
  
  plot()
  #filter(filtdata2003_sd_stddev < 0.1) %>%
   #mutate(filtdata2003_average = 1 - filtdata2003_average)  %>%
  ggplot()+geom_sf(aes(fill = filtdata2003_sd_stddev))+theme_bw()+scale_fill_viridis(option = "magma")



myr <- rast("/home/marco/Desktop/mosaic2003_filt.tif")
myr1 <- 1-myr
myr1[myr1 > 0.6]<-NA

myr2 <- aggregate(myr1,fact = 400, fun = mean, na.rm = TRUE)
writeRaster(myr1,filename = "/home/marco/Desktop/test60.tif",overwrite = TRUE)

myr <- rast("/home/marco/Desktop/gapfilled/mosaic_avg_filt.tif")
myr1 <- aggregate(myr,fact = 200, fun = median, data = mean,na.rm = TRUE)
myr2 <- aggregate(myr,fact = 200, fun = sd, data = mean,na.rm = TRUE)

breaks <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6,0.70,1)
labels <- c("0-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", "0.4-0.5", "0.5-0.6", "0.6-0.7","0.7-0.8")

as.data.frame(myr1,xy =TRUE) %>%
  as_tibble() %>%
  #filter(mosaic_avg < 0.85) %>%
  mutate(mosaic_avgf = cut(mosaic_avg, breaks=breaks,labels = labels, include.lowest = TRUE)) %>%
  ggplot(aes(x = x, y = y, fill = mosaic_avg))+geom_tile()+
  theme_minimal()+theme(axis.text = element_blank(),
                        axis.title = element_blank(),
                        axis.ticks = element_blank())+
  #scale_fill_viridis(discrete = TRUE)
  scale_fill_viridis(option = "viridis",oob = squish, limits = c(0,0.8),
                     )

as.data.frame(myr1,xy =TRUE) %>%
  as_tibble() %>%
  #filter(mosaic_avg < 0.85) %>%
  mutate(mosaic_avgf = cut(mosaic_avg, breaks=breaks,labels = labels, include.lowest = TRUE)) %>%
  ggplot()+
  geom_sf(data=world2,color='black',fill=NA,size=0.09)+
  geom_tile(aes(x = x, y = y, fill = mosaic_avg))+
  #scale_fill_viridis(discrete = TRUE)
  scale_fill_viridis(option = "viridis",oob = squish, limits = c(0,0.8))+
  theme_minimal()+theme(axis.text = element_blank(),
                        axis.title = element_blank(),
                        axis.ticks = element_blank(),
                        legend.title = element_blank())+
  guides()
  
v

# read in support GIS files
st_read("./gregory/data/GIS/worldsimple.shp")->world2

myp1 <- st_read("/home/marco/Desktop/gapfilled/mosaic.gpkg") %>%
  filter(!is.na(mosaic_for_average)) %>%
  ggplot()+
  geom_sf(aes(fill = mosaic_for_average))+
  scale_fill_viridis(option = "viridis",oob = squish, limits = c(0,0.7))+
  geom_sf(data=world2,color='black',fill=NA,size=0.09)+
    theme_minimal()+theme(axis.text = element_blank(),
                          axis.title = element_blank(),
                          axis.ticks = element_blank(),
                          legend.title = element_blank())
myp2 <- st_read("/home/marco/Desktop/gapfilled/mosaic.gpkg") %>%
  filter(!is.na(mosaic_for_average)) %>%
  ggplot()+
  geom_sf(aes(fill = mosaic_for_stddev))+
  scale_fill_viridis(option = "viridis",oob = squish, limits = c(0,0.2))+
  geom_sf(data=world2,color='black',fill=NA,size=0.09)+
  theme_minimal()+theme(axis.text = element_blank(),
                        axis.title = element_blank(),
                        axis.ticks = element_blank(),
                        legend.title = element_blank())

ggsave(myp, filename = "./ERC_R1/figures/FigS6.png",dpi = 400, height = 7, width = 15, bg = "white")  
  
