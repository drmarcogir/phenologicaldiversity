read_csv("tmp/gridstats025degree",col_names = FALSE) %>%
  rename(cat = X1,cat025d=X2) %>%
  inner_join(grid5km) %>%
  dplyr::select(x,y,cat025d) %>%
  rasterFromXYZ(crs = latlon) %>%
  writeRaster(filename = "/home/marco/Desktop/grid025.tif")


# empty raster with desired resolution
raster(res=0.25)->myr


# create raster grid and write out file
rasterize(world,myr) %>%
  rastertodf() %>%
  mutate(value = 1:length(value)) %>% {.->> tmpgrid} %>%
  rasterFromXYZ(crs=latlon) %>%
  writeRaster(filename = "/home/marco/Desktop/grid025.tif",overwrite = TRUE)
