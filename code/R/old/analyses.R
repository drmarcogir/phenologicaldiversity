# Analyse phenological diversity data using topography
library(tidyverse)
library(data.table)
# Phenology data
tibble(filename = list.files("/media/marco/marcodata19/phenology_csv",full.names = T,pattern = ".csv")) %>%
  mutate(file_contents = map(filename,read_csv)) %>%
  mutate(metric = str_remove(basename(filename),".csv")) %>%
  mutate(year = as.numeric(str_split_fixed(metric,"_",n=4)[,3]),
         metric = str_split_fixed(metric,"_",n=4)[,1]) %>%
  unnest(cols = file_contents) %>%
  dplyr::select(-c(filename,`system:index`,`.geo`,count)) %>%
  rename(value=zone)->metrics

# metrics %>%
#   group_by(metric,value) %>%
#   summarise(median = )

# inner_join(as_tibble(rastertodf(raster("/mnt/data1tb/phenology/gridGEE/grid_pheno_grass.tif")
                                
metrics %>% 
  filter(year < 2006) %>%
  group_by(metric,value) %>%
  summarise(stdDev1 = median(stdDev))->metrics1

metrics %>%
  filter(year > 2011) %>%
  group_by(metric,value) %>%
  summarise(stdDev2 = median(stdDev)) %>%
  inner_join(metrics1)->metrics2

metrics2 %>%
   mutate(dif = stdDev2-stdDev1)->metrics3
  
# aggregate metrics 
metrics %>%
  group_by(metric,value) %>%
   summarise(median = median(stdDev))->metrics1

# Human impact layer
fread("/home/marco/Desktop/hfl") %>%
  rename(humi=V1,value=V2) %>%
  group_by(value) %>%
  summarise(humi = mean(humi))->hfl


# altitude data
tibble(filename = list.files("/media/marco/marcodata19/topography_phenology",full.names = T,pattern = ".csv")) %>%
  mutate(file_contents = map(filename,read_csv)) %>%
  unnest(cols = file_contents) %>%
  dplyr::select(-c(filename,`system:index`,`.geo`,count)) %>%
  rename(value=zone,sd_elev=stdDev)->top


# slope data
tibble(filename = list.files("/media/marco/marcodata19/topography_phenology",full.names = T,pattern = ".csv")) %>%
  dplyr::filter(str_detect(filename, "sd_slope")) %>%
  mutate(file_contents = map(filename,read_csv)) %>%
  unnest(cols = file_contents) %>%
  dplyr::select(-c(filename,`system:index`,`.geo`,count)) %>%
  rename(value=zone,sd_slope=stdDev)->sd_slope

# mean slope data
tibble(filename = list.files("/media/marco/marcodata19/topography_phenology",full.names = T,pattern = ".csv")) %>%
  dplyr::filter(str_detect(filename, "mean_slope")) %>%
  mutate(file_contents = map(filename,read_csv)) %>%
  unnest(cols = file_contents) %>%
  dplyr::select(-c(filename,`system:index`,`.geo`,count)) %>%
  rename(value=zone,mean_slope=mean)->mean_slope

inner_join(metrics1,sd_slope) %>%
  na.exclude()  %>%
  inner_join(hfl) %>%
  inner_join(top) %>%
  inner_join(mean_slope)->finaldat

mod<-lmer(dif~sd_slope+humi+(1|metric),data=finaldat)
finaldat %>%
  group_by(metric) %>%
  summarise(cor = cor(median,mean_slope))
  
# raster of total change
as_tibble(rastertodf(raster("/mnt/data1tb/phenology/gridGEE/grid_pheno_grass.tif"))) %>%
  inner_join(metrics3 %>%
               group_by(value) %>%
               summarise(dif = mean(dif)))->df
df %>%
  dplyr::select(x,y,dif) %>%
  rasterFromXYZ() %>%
  writeRaster(,filename = "/home/marco/Desktop/diftot.tif")
