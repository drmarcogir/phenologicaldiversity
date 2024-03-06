# load required libraries -------------------------------------------------
library(tidyverse)
library(data.table) 
library(phenoutils)
library(viridis)
library(raster) 
library(scales)
library(furrr)
library(marcoUtils)
library(sf)
library(ranger)

# read in data ------------------------------------------------------------

#fread("/mnt/data1tb/alessandro_metric/final.csv")->res
fread("/home/marco/alessandro_metric/final.csv")->res

res %>%
  group_by(cat) %>%
  summarise(count = n()) %>%
  filter(count > 12) %>%
  dplyr::select(cat) %>%
  inner_join(res)->res

#res %>%
#  group_by(cat) %>%
#  summarise(count = n()) %>%
#  filter(count > 12) %>%
#  dplyr::select(cat) %>%
#  inner_join(grid5km) %>%
#  dplyr::select(x,y,cat) %>%
#  rasterFromXYZ() %>%
#  plot()
# read in climate data
#fread("/mnt/data1tb/alessandro_metric/terraclimate_combined.csv")->clim

fread("/home/marco/alessandro_metric/terraclimate_combined.csv")->clim

res %>%
  inner_join(clim) %>%
  inner_join(read_csv("./tmp/forestcount",col_names = FALSE) %>%
               rename(cat = X1, count = X2) %>%
               mutate(forest_prop = count /max(count))) %>%
  filter(forest_prop > 0.75) %>%
  #inner_join(read_csv("tmp/gridstats025degree",col_names = FALSE) %>%
  #             fread("/media/marco/marcodata19/phenologytraining/dat.csv")->dat
  #rename(cat = X1,cat025d=X2) %>%
  inner_join(st_read("./KG/kg_codesv1.shp") %>%
               as_tibble() %>%
               dplyr::select(lvl1_tx,lvl2_tx,lvl3_tx,lvl1_ct,lvl2_ct,lvl3_ct) %>%
               unique()) %>%
  as_tibble() %>%
  inner_join(read_csv("tmp/gridstats025degree",col_names = FALSE) %>%
               rename(cat = X1,cat025d=X2))->dat

dat %>%
  filter(value < 180) %>%
  group_by(cat025d,year) %>%
  summarise(value = median(value))->dat1

dat1 %>%
  mutate(p1_p2=ifelse(year < 2011,"p1","p2")) %>%
  mutate(p1_p2 = factor(p1_p2,levels = c("p1","p2")))->dat2

write_csv(dat2,"./jeodppfiles/dat_025.csv")
