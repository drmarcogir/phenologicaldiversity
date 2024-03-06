library(tidyverse);library(raster)
library(marcoUtils);library(phenoutils)

# year list

# this is a test ----------------------------------------------------------
year.l<-2003

# put together individual tiles
#unlink("./tmpgeotif/Y_2015.tif")
map(year.l,tile_geotif,inpath = "/mnt/data1tb/alessandro_metric/tmpmarcog/",
    outpath="/mnt/data1tb/Dropbox/phenology/tmpgeotif/July2021/")

raster("/mnt/data1tb/Dropbox/phenology/tmpgeotif/2001latestY_2001.tif")->myr
myr[myr==0]<-NA
#writeRaster(myr,"/mnt/data1tb/Dropbox/phenology/tmpgeotif/y2003_cleaned_terraclimate_filter_experiment.tif",overwrite = TRUE)
writeRaster(myr,"/mnt/data1tb/Dropbox/phenology/tmpgeotif/2001latestY_2001.tif",overwrite = TRUE)



# this is the real stuff  ----------------------------------------------------------

# change directory, depending on where files are stored
#list.files("/mnt/data1tb/alessandro_metric/tempfilt",full.names = TRUE) %>%
#  .[str_detect(.,"Y_2003")]->y2003

year.l<-2003

# put together individual tiles
#unlink("./tmpgeotif/Y_2003.tif")
map(year.l,tile_geotif,inpath = "/mnt/data1tb/alessandro_metric/tempfilt/",
    outpath="/mnt/data1tb/Dropbox/phenology/GEE_results_geotiff/")


# import stuff into GRASS GIS and use grid
filel<-list.files("/mnt/data1tb/Dropbox/phenology/GEE_results_geotiff",full.names = TRUE,pattern = ".tif")
map(filel,grass_phenoin) %>%
  reduce(bind_rows) %>%
  mutate(year = paste0("y_",year)) %>%
  pivot_wider(names_from = year,values_from =pheno)->phenodata


# forest cover 5 km grid cells
phenodata %>%
  left_join(read_csv("./GRASSGIS_files/forestprop1",col_names = FALSE) %>%
              rename(cat =X1 , area = X2)) %>%
  # pft info
  left_join(read_csv("./GRASSGIS_files/pft.csv")) %>%
  # climatology (worldclim)
  left_join(read_csv("/mnt/data1tb/Dropbox/phenology/GRASSGIS_files/bio1",col_names = FALSE) %>%
  rename(bio1 = X1, cat = X2) %>%
    group_by(cat) %>%
    summarise(bio1 = mean(bio1))) %>%
  left_join(read_csv("/mnt/data1tb/Dropbox/phenology/GRASSGIS_files/bio12",col_names = FALSE) %>%
              rename(bio12 = X1, cat = X2) %>%
              group_by(cat) %>%
              summarise(bio12 = mean(bio12))) %>%
  inner_join(grid5km) %>%
  write_csv("./data/phenodata.csv")
  



#grid5km %>%
#  inner_join(read_csv("./GRASSGIS_files/forestprop1",col_names = FALSE) %>%
#                         rename(cat =X1 , area = X2)) %>%

#grid5km %>%
#  inner_join(phenodata) %>%
#  dplyr::select(x,y,y_2003) %>%
#  rasterFromXYZ() %>%
#  writeRaster(.,filename = "/home/marco/Desktop/Y_2003v1.tif")

# merge with climatic and pft data
