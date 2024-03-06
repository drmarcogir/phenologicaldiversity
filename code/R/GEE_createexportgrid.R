library(sf);library(marcoUtils)
library(tidyverse)
# create bounding box
st_bbox(c(xmin =-178.2918491139962, xmax = 179.98940088600375, ymax =73.88593845172474, ymin = -61.42493117272831), 
        crs = st_crs(4326))->box
  
st_crop(world,box)->dat2

# create grid (this covers the entire globe)
st_make_grid(dat2, cellsize = 0.05, square = TRUE,crs=latlon,
             what = "polygons") %>%
  st_as_sf() %>%
  rename(geometry = x) %>%
  mutate(polyID = 1:length(geometry))->tmpgrid 

st_intersection(world,tmpgrid,sparse=TRUE) %>%
  pull(polyID) %>%
  unique()->polylookup

tmpgrid  %>%
  filter(polyID %in% polylookup)->tmpgrid1
st_write(tmpgrid1,"/media/marco/marcodata19/phenologylargefiles/gridGEE/grid5km.shp")


# filter grid only by forested areas
#  missing step needs redoing
st_write(tmpgrid,"./gridGEE/tmpgrid.shp")

raster("./GRASSGIS_files/grid_5km_mask_unchanged_500m.tif")->dd

rasterToPolygons(raster("/mnt/data1tb/Dropbox/phenology/GRASSGIS_files/grid_5km_mask_unchanged_500m.tif"))->dd

rastertodf(dd) %>%
  slice(1:2000) %>%
  rasterFromXYZ(crs = latlon)->dd1

rasterToPolygons(dd1)->dd2

st_write(tmpgrid,"/home/marco/Desktop/tmpgrid.shp",delete_layer = TRUE)

st_as_sf(dd)->dd3
dd3 %>%
  rename(polyID = grid_5km_mask_unchanged_500m) %>%
  mutate(polyID = 1:length(polyID)) %>%
  st_write(.,"/home/marco/Desktop/grid5km.shp",delete_layer = TRUE)

# missing tiles from basic grid!
tmpgrid %>%
  filter(polyID %in% c(3719,3575,3429,3569,3570,3714,3427,3571,3715,3859,3716,3287,2713,2714,2570,2571,2572,3160,3159,4296,4297,4298,4152,4153,3001,3002)) %>%
  mutate(polyID = 2040:(2039+26))->missing
  

# join together with old grid dataset
st_read("./gridGEE/fine_gridv1.shp") %>%
  bind_rows(missing) %>%
  st_write(.,"./gridGEE/fine_gridv2.shp",delete_layer = TRUE)

