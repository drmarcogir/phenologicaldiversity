library(tidyverse)
library(phenoutils)
library(marcoUtils)
library(terra)
library(data.table)

hey<- fread("/mnt/data1tb/alessandro_metric/finaldat.csv") %>%
  group_by(cat) %>%
  summarise(value = median(value)) %>%
  inner_join(grid5km)
  
  
hey1 <-fread("/mnt/data1tb/alessandro_metric/final.csv") %>%
  group_by(cat) %>%
  summarise(value = median(value)) %>%
  inner_join(grid5km)

myr <- hey1 %>%
   dplyr::select(x,y,value) %>%
   rast(type = "xyz", crs = latlon)

writeRaster(myr,filename = "/home/marco/Desktop/pheno_resilience.tif",overwrite = TRUE)
# 
# myr <- cc %>%
#   group_by(cat) %>%
#   summarise(value = median(value)) %>%
#   inner_join(grid5km) %>%
#   dplyr::select(x,y,value) %>%
#   rast(type = "xyz", crs = latlon)
#   
# fread("/home/marco/alessandro_metric/final.csv") %>%
#   #fread("/mnt/data1tb/alessandro_metric/finaldat.csv") %>%
#   group_by(cat) %>%
#   summarise(value = median(value)) %>%
#   inner_join(grid5km) 
# 
# writeRaster(myr,filename = "/home/marco/Desktop/test.tif")
