library(tidyverse);

read_csv("./jeodpp_results/dat1_DALEX.csv") %>%
  pull(cat2d)->cat2d

bind_rows(readRDS("./jeodpp_results/dalex_predict_parts.RDS")) %>%
  filter(varname=="bio_01_mean") %>%
  mutate(cat2d = cat2d) %>%
  inner_join(readRDS("./rastergrids/g2d.RDS")) %>%
  dplyr::select(x_2,y_2,contribution) %>%
  rasterFromXYZ(crs=latlon) %>%
  writeRaster(filename = "/home/marco/Desktop/test.tif",overwrite=TRUE)




readRDS("./jeodpp_results/limeres.RDS") %>%
  inner_join(readRDS("./rastergrids/g2d.RDS")) %>%
  unnest(cols = res) %>%
  dplyr::select(x_2,y_2,feature,feature_weight) %>%
  filter(feature=="bio_01_mean") %>%
  dplyr::select(x_2,y_2,feature_weight) %>%
  rasterFromXYZ(crs = latlon) %>%
  writeRaster(filename = "/home/marco/Desktop/limebio01_mean.tif")
