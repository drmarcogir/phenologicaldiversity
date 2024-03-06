library(sf);library(raster)
library(tidyverse)

system("r.in.gdal in=/media/marco/marcodata19/Phenology_original/sd_slices_maps/Peak_slice1_sd.tif out=Peak_slice1")
system("r.in.gdal in=/media/marco/marcodata19/Phenology_original/sd_slices_maps/Peak_slice2_sd.tif out=Peak_slice2")

system("r.in.gdal in=/media/marco/marcodata19/Phenology_original/sd_slices_maps/Peak_slice_dif.tif out=dif")
system("v.in.ogr in=/media/marco/marcodata19/Phenology_original/pristineforests/ifl_2016.shp out=pristine")

system("g.region raster=Peak_slice1")

st_read("/media/marco/marcodata19/Phenology_original/biomes/tnc_terr_ecoregions.shp")->dat


dat %>%
  pull(WWF_MHTNAM) %>%
  .[str_detect(.,"For")] %>%
  unique()->forl

res<-NULL

for (i in 1:length(forl)){
  dat %>%
    filter(WWF_MHTNAM==forl[i]) %>%
    st_write("test.shp",delete_layer=T)
  system("v.in.ogr in=test.shp out=test --o")
  system("v.to.rast in=test out=test use=cat --o")
  system("r.mapcalc 'test1 = test/test' --o")
  system("r.mask raster=test1 maskcats=1")
  tibble(value = as.numeric(system("r.stats -nA in=Peak_slice1",intern = T)),forest=forl[i],period="first")->tmpres
  bind_rows(tmpres,res)->res
  tibble(value = as.numeric(system("r.stats -nA in=Peak_slice2",intern = T)),forest=forl[i],period="second")->tmpres1
  bind_rows(tmpres1,res)->res
  system("r.mask -r")
}


res<-NULL

for (i in 1:length(forl)){
  dat %>%
    filter(WWF_MHTNAM==forl[i]) %>%
    st_write("test.shp",delete_layer=T)
  system("v.in.ogr in=test.shp out=test --o")
  system("v.to.rast in=test out=test use=cat --o")
  system("r.mapcalc 'test1 = test/test' --o")
  system("r.mask raster=test1 maskcats=1")
  tibble(value = mean(as.numeric(system("r.stats -nA in=dif",intern = T))),forest=forl[i])->tmpres
  bind_rows(tmpres,res)->res
  system("r.mask -r")
}



res %>%
  group_by(period,forest) %>%
  mutate(value = scales::rescale(value,to=c(0,1))) %>%
  ggplot()+geom_boxplot(aes(x=period,y=value))+facet_wrap(~forest)+theme_minimal()

system("v.to.rast in=pristine out=tmp use=cat --o")
system("r.mapcalc 'pristine = tmp/tmp' --o")
system("r.mask raster=pristine maskcats=1 -i")
tibble(value = as.numeric(system("r.stats -nA in=Peak_slice1",intern = T)),period="first",type="non-pristine") %>%
  bind_rows(tibble(value = as.numeric(system("r.stats -nA in=Peak_slice2",intern = T)),period="second",type="non-pristine"))->dat
system("r.mask -r")
system("r.mask raster=pristine maskcats=1")
tibble(value = as.numeric(system("r.stats -nA in=Peak_slice1",intern = T)),period="first",type="pristine") %>%
  bind_rows(tibble(value = as.numeric(system("r.stats -nA in=Peak_slice2",intern = T)),period="second",type="pristine")) %>%
  bind_rows(dat)->dat1

dat1 %>%
  ggplot()+geom_boxplot(aes(x=period,y=value))+facet_wrap(~type)+theme_minimal()

dat1 %>%
  filter(type=="non-pristine")->tmp

as_tibble(rastertodf(raster("/home/marco/Desktop/grid_5km.tif")))->griddf



read_delim("/home/marco/Desktop/stats",delim =" ",col_names = F) %>%
  mutate(prop =X3/64) %>%
  filter(prop > 0.75) %>%
  dplyr::select(X2) %>%
  rename(value = X2) %>%
  inner_join(griddf) %>%
  dplyr::select(x,y,value) %>%
  rasterFromXYZ()->myr


