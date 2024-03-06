library(marcoUtils);library(sf)
library(tidyverse)
st_bbox(c(xmin =-178.29184894, xmax = -148.29230, ymax =73.88593834, ymin = 51.707), 
        crs = st_crs(4326))->box


st_crop(world,box)->dat2

# read in old file
st_read("./gridGEE/gridv1.shp") %>%
  st_set_crs(latlon) %>%
  filter(polyID!=34) %>%
  mutate(polyID = 1:length(geometry)) %>%
# create new smaller grid and bind it to old file
  bind_rows(st_make_grid(dat2, cellsize = 9, square = TRUE,crs=latlon,
               what = "polygons") %>%
  st_as_sf() %>%
  rename(geometry = x) %>%
  mutate(polyID = 1:length(geometry)+41, polyID = as.integer(polyID))) %>%
  # write out result
  st_write("./gridGEE/gridjun21.shp",delete_layer = TRUE)

  
# read in old grid
st_read("/home/marco/Desktop/gridjune/gridjun21.shp") %>%
  filter(polyID < 360) %>%
  bind_rows(st_read("/home/marco/Desktop/gridjune/newgrid.shp") %>%
              filter(id %in% c(43,44,57,58,71,72)) %>%
              dplyr::select(id) %>%
              rename(polyID = id)) %>%
  mutate(polyID = 1:length(geometry), polyID = as.integer(polyID)) %>%
  # write out result
  st_write("/home/marco/Desktop/gridjune/gridjun21.shp",delete_layer = TRUE)

st_read("/home/marco/Desktop/gridjune/missing.shp") %>%
  mutate(polyID = 1:length(geometry), polyID = as.integer(polyID)) %>%
  dplyr::select(polyID) %>%
  # write out result
  st_write("/home/marco/Desktop/gridjune/missing.shp",delete_layer = TRUE)


st_read("./gridGEE/gridjun21v2.shp") %>%
  mutate(polyID = 1:length(geometry), polyID = as.integer(polyID)) %>%
  # write out result
  st_write("./gridGEE/gridjun21v2.shp",delete_layer = TRUE)

st_read("/home/marco/Desktop/gridjune/missing.shp") %>%
  mutate(polyID = 1:length(geometry), polyID = as.integer(polyID)) %>%
  dplyr::select(polyID) %>%
  # write out result
  st_write("/home/marco/Desktop/gridjune/missing.shp",delete_layer = TRUE)



st_read("./gridGEE/gridjun21v2.shp") %>%
  filter(polyID %in% c(304,306,305))->dd

st_bbox(dd)->dd1
st_bbox(c(xmin =-178.29184894, xmax = -148.29230, ymax =73.88593834, ymin = 51.707), 
        crs = st_crs(4326))->box

st_make_grid(dd1, cellsize = 1, square = TRUE,crs=latlon,
             what = "polygons") %>%
  st_write("./gridGEE/tmp.shp",delete_layer = TRUE)

st_read("./gridGEE/gridjun21v2.shp") %>%
  filter(!polyID %in% c(304:313)) %>%
  bind_rows(st_read("./gridGEE/tmp.shp") %>%
              mutate(polyID = 1:length(geometry))) %>%
  mutate(polyID = 1:length(geometry), polyID = as.integer(polyID)) %>%
  dplyr::select(-c(FID)) %>%
  # write out result
  st_write("./gridGEE/gridjun21v5.shp",delete_layer = TRUE)

st_read("./gridGEE/gridjun21v5.shp") %>%
  filter(polyID %in% c(21,273,281,283,287,295,296,297,298)) %>%
  st_write("./gridGEE/problems.shp",delete_layer = TRUE)
  
st_read("./gridGEE/gridjul21.shp") %>%
  rename(polyID = id) %>%
  mutate(polyID = 1:length(polyID)) %>%
  st_write("./gridGEE/testgridjul21.shp",delete_layer = TRUE)

st_read("./gridGEE/gridjul21.shp") %>%
  st_bbox() %>%
  st_make_grid(dd1, cellsize = 1, square = TRUE,crs=latlon,
               what = "polygons")

seq(from=-90,to=90,by=4)->lat
seq(from=-180,to=180,by=25)->lon


for (i in 1:length(lat)-1){  
  tibble(ymin = lat[i],ymax=lat[i+1])->tmp
  for(y in 1:length(lon)-1){
    tmp %>%
    bind_cols(tibble(xmin = lon[y],xmax=lon[y+1]))->tmp1
  }
}


st_bbox(c(xmin = 16.1, xmax = 16.6, ymax = 48.6, ymin = 47.9), crs = st_crs(4326))
st_bbox()

tmp1 %>%
  pivot_longer(names_to = "xyinfo",values_to = "values",cols = everything()) %>%
  dplyr::select(xyinfo,values) %>%
  deframe()->boxcoor

st_bbox(boxcoor, crs = latlon)->mybox
st_cast(mybox, "POLYGON")

st_make_grid(mybox, cellsize = 1, square = TRUE,crs=latlon,
             what = "polygons") %>%
  plot()
