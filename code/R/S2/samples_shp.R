library(sf);library(tidyverse)

# insert polyID field
st_read("./s2data/sampleareas/samples_Sentinel2.shp") %>%
  mutate(polyID = 1:length(geometry)) %>%
  st_write("./s2data/sampleareas/samples_Sentinel2.shp",delete_layer = TRUE)

# upload 
system("gsutil cp /mnt/data1tb/Dropbox/phenology/s2data/sampleareas/samples_Sentinel2.shp gs://marco_g_assets/")
system("gsutil cp /mnt/data1tb/Dropbox/phenology/s2data/sampleareas/samples_Sentinel2.dbf gs://marco_g_assets/")
system("gsutil cp /mnt/data1tb/Dropbox/phenology/s2data/sampleareas/samples_Sentinel2.prj gs://marco_g_assets/")
system("gsutil cp /mnt/data1tb/Dropbox/phenology/s2data/sampleareas/samples_Sentinel2.shx gs://marco_g_assets/")

# create new asset
system("earthengine upload table --asset_id=users/marcogirardello/exploratoryproject/samples gs://marco_g_assets/samples_Sentinel2.shp")
