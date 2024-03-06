# load required libraries -------------------------------------------------
library(raster);library(phenoutils)
library(tidyverse);library(sf)
library(data.table);library(scales)
library(marcoUtils);library(tmap)
library(RColorBrewer);library(grid)
library(viridis);library(mgcViz)
library(gridExtra);library(ggridges)
st_read("./worldshp/worlmp.shp")->world2
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


# create climate space figure ---------------------------------------------


phenodat |> 
  filter(bio_12_mean < 4000) |>
  filter(!is.infinite(aridity)) |>
  filter(bio_15_mean < 160) |>
  mutate(bio_04_mean = bio_04_mean/1000) |>
  inner_join(biomes) |>
  dplyr::select(bio_04_mean,bio_15_mean,WWF_MHTNAM_agg) %>%
  as.data.frame() |>
  rename(xdata = bio_04_mean, ydata= bio_15_mean,level = WWF_MHTNAM_agg) |>
  as.data.table() %>% 
  .[, .SD[chull(xdata, ydata)], by = level] %>%
  ggplot(aes(x=xdata,y=ydata,color=level)) +
  geom_polygon(aes(fill=level,alpha = 0.5))+scale_alpha(guide = 'none')+
  theme(axis.text = element_text(size = 15,colour = "black"),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 14))+
  xlab("Temperature seasonality")+ylab("Precipitation seasonality")->p1

ggsave(p1,filename = "./3dplotsforMirco/seasonalityclimatespace.png",height = 8,
       width = 8, dpi = 400)  


phenodat %>%
  as.data.frame %>%
  pull(x) %>%
  mean

  
