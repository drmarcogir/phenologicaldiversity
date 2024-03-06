# Info on GRASS session ---------------------------------------------------
# Prepare data needed for subsequent analyses
# GRASS: /media/marco/marcodata19/backupfrombigdisk/phenology/grassdata
# location: world
# mapset: PERMANENT


# load libraries ----------------------------------------------------------
library(marcoUtils);library(raster)
library(sf);library(fasterize)
library(tidyverse)

# grid5km -----------------------------------------------------------------


# convert world into raster
raster(extent(world),res=0.05)->tmpr
fasterize(world,tmpr)->tmpr1
tmpr1[is.na(tmpr1)]<-1

as_tibble(rastertodf(tmpr1)) %>%
  mutate(value=1:length(value)) %>%
  rasterFromXYZ(crs=latlon) %>%
  writeRaster(filename = "./grid_grass/grid5km.tif",overwrite=TRUE)

# rasterize world shapefile: resolution 0.05
system("g.region vector=world res=0.05 -pag")
system("v.to.rast in=world out=tmp use=cat --o")
# set to 1 all pixels
system("r.mapcalc 'world = tmp/tmp' --o")
system("r.null map=world null=1")
system("g.region raster=world -pg")
# get coordinates for every pixel and import back into GRASS
system(command = paste0("r.stats -lgn world  | awk '{print $1\"|\"$2\"|\"NR}' | v.in.ascii in=- out=grid5km"))
# rasterize vector point file
system("g.region vector=world res=0.05 -pga")
system("v.to.rast in=grid5km out=grid5km1 use=cat --o")
# mask for unchanged pixels
system("r.mask raster=mask_unchanged_500m maskcats=1")
# create new grid
system("r.mapcalc 'grid_5km_mask_unchanged_500m =  grid5km1' --o")
# save grid
system("r.out.gdal in=grid5km1 out=/mnt/data1tb/Dropbox/phenology/GRASSGIS_files/grid_5km_mask_unchanged_500m.tif createopt='COMPRESS=LZW' --o")

raster("./GRASSGIS_files/grid_5km_mask_unchanged_500m.tif") %>%
  rastertodf() %>%
  as_tibble() %>%
  rename(cat = value)->grid5km 
  save(grid5km,file="/mnt/data1tb/Dropbox/phenoutils/data/grid5km.rda")

load("/mnt/data1tb/Dropbox/phenoutils/data/grid5km.rda")

# import 5km grid
system("r.in.gdal in=/mnt/data1tb/Dropbox/phenology/GRASSGIS_files/grid_5km_mask_unchanged_500m.tif out=grid5km1")

# forest cover 5km grid ---------------------------------------------------


# get forest cover for each 5 km x 5 km 
system("g.region raster=lc_2015_for -pg")
system("r.stats -lna in=lc_2015_for,grid5km1 > /mnt/data1tb/Dropbox/phenology/GRASSGIS_files/propfor")
system("cat /mnt/data1tb/Dropbox/phenology/GRASSGIS_files/propfor | awk '{print $2\",\"$3}' > /mnt/data1tb/Dropbox/phenology/GRASSGIS_files/forestprop1")


# climatology -------------------------------------------------------------


# get climate info
system("r.in.gdal in=/media/marco/marcodata19/data1tb_laptop/phenology/climate/wc2.1_2.5m_bio_1.tif out=bio1")
system("r.in.gdal in=/media/marco/marcodata19/data1tb_laptop/phenology/climate/wc2.1_2.5m_bio_12.tif out=bio12")
system("g.region raster=bio1")
# temperature
system("r.stats -An in=bio1,grid5km1 > /mnt/data1tb/Dropbox/phenology/GRASSGIS_files/tmp")
system("cat /mnt/data1tb/Dropbox/phenology/GRASSGIS_files/tmp | awk '{print $1\",\"$2}' > /mnt/data1tb/Dropbox/phenology/GRASSGIS_files/bio1")
# rainfall
system("r.stats -An in=bio12,grid5km1 > /mnt/data1tb/Dropbox/phenology/GRASSGIS_files/tmp")
system("cat /mnt/data1tb/Dropbox/phenology/GRASSGIS_files/tmp | awk '{print $1\",\"$2}' > /mnt/data1tb/Dropbox/phenology/GRASSGIS_files/bio12")


list.files("/media/marco/marcodata19/chelsa",full.names = T) %>%
  .[str_detect(.,'bio10')]->biovars

# import variables
for (i in 1:length(biovars)){
  print(i)
  system(command = paste0("r.in.gdal in=",biovars[i]," out=",str_remove(basename(biovars[i]),".tif")))
}


system("g.region raster=CHELSA_bio10_01")

# mean
res<-NULL

for (i in str_remove_all(basename(biovars),".tif")){
  print(i)
  system(command = paste0("r.stats -gn in=",i,",grid_5km_mask_unchanged_500m > /mnt/data1tb/grasstmp/tmp"))
  read_delim("/mnt/data1tb/grasstmp/tmp",delim=" ",col_names = FALSE) %>%
    rename(variable = X3,cat = X4) %>%
    dplyr::select(-c(X1,X2)) %>%
    group_by(cat) %>%
    summarise(variable = sd(variable)) %>%
    mutate(varname = i)->tmp
  bind_rows(tmp,res)->res
}

# standard deviation
res<-NULL

for (i in str_remove_all(basename(biovars),".tif")){
  print(i)
  system(command = paste0("r.stats -gn in=",i,",grid_5km_mask_unchanged_500m > /mnt/data1tb/grasstmp/tmp"))
  fread("/mnt/data1tb/grasstmp/tmp") %>%
    rename(variable = V3,cat = V4) %>%
    dplyr::select(-c(V1,V2)) %>%
    .[, by = cat,.(variable = sd(variable))] %>%
    mutate(varname = i)->tmp
  bind_rows(tmp,res)->res
}


res %>%
  pivot_wider(names_from = varname,values_from = variable) %>%
  rename_with(~str_remove(., 'CHELSA_')) %>%
  rename_with(~str_replace(.,'bio10_','bio_')) %>%
  write_csv("./GRASSGIS_files/bioclim_sd.csv")


# pet
list.files("/media/marco/marcodata19/chelsa",full.names = T) %>%
  .[str_detect(.,'pet')]->biovars

# import variables
for (i in 1:length(biovars)){
  print(i)
  system(command = paste0("r.in.gdal in=",biovars[i]," out=",str_remove(basename(biovars[i]),"_1979-2013.tif")," --o"))
}


system("g.list rast | grep pet",intern = TRUE)->mapl
system(command = paste0("r.mapcalc 'pet_mean = (",paste0(mapl,collapse = "+"),")/12'"))
system(command = paste0("r.stats -An in=pet_mean,grid5km1 > /mnt/data1tb/grasstmp/tmp"))
read_delim("/mnt/data1tb/grasstmp/tmp",delim=" ",col_names = FALSE) %>%
  rename(variable = X1,cat = X2) %>%
  group_by(cat) %>%
  summarise(variable = mean(variable)) %>%
  rename(pet_mean = variable) %>%
  write_csv("./GRASSGIS_files/pet_mean.csv")

# PFTs --------------------------------------------------------------------


# list of pfts
pft.l<-c("BrEv","BrDc","NeEv","NeDe")

#pft_prop<-NULL

for (i in 1:4){
  print(pft.l[i])
  # subset pft of interest and create reclass rule file
  pft %>%
    dplyr::filter(PFT==pft.l[i]) %>%
    mutate(rcl=paste0(cat," = ",area_m2)) %>%
    dplyr::select(rcl) %>%
    write.table(.,file="./tmp/tmp",row.names=F,quote=F,col.names = F)
  # set region
  system("g.region raster=lc_2015")
  # create tmp reclassified map
  system("r.reclass in=lc_2015 out=tmp rules=/mnt/data1tb/Dropbox/phenology/tmp/tmp --o")
  # aggregate map to 5km 
  system("g.region raster=grid5km") 
  system("r.resamp.stats in=tmp method=sum out=tmpsum --o")
  # calculate proportions
  system("r.mapcalc 'tmpfrac = tmpsum /30980073' --o")
  # outputs map
  system("r.stats -An in=tmpfrac,grid5km > /mnt/data1tb/Dropbox/phenology/GRASSGIS_files/tmp")
  read_delim("/mnt/data1tb/Dropbox/phenology/GRASSGIS_files/tmp",delim = " ",col_names = F) %>%
    rename(prop = X1,cat = X2) %>%
    mutate(pft = pft.l[i])->tmpres
  bind_rows(tmpres,pft_prop)->pft_prop
}  
 
pft_prop %>%
  pivot_wider(names_from = pft,values_from = prop) %>%
  write_csv("GRASSGIS_files/pft.csv")

# Jetz's product ----------------------------------------------------------------------

# read in 2002 map into GRASS
#system("r.in.gdal in=/mnt/data1tb/Dropbox/phenology/GEE_results_geotiff/Y_2002grass.tif out=y2002 --o")
#system("g.region raster=y2002 res=0.05 -pga")
# set year 2002 to integer
system("r.mapcalc 'general_mask = grid_5km_mask_unchanged_500m/grid_5km_mask_unchanged_500m' --o")
#system("g.region raster=y2002 -p")
system("d.rast general_mask")
#system("r.mapcalc 'jetz1 = jetz' --o")

# set mask
system("r.mask raster=general_mask maskcats=1")
#system("r.stats -lgn jetz1 -ln | wc -l")
system("r.mask -r")

# read Jetz's map into GRASS GIS
system("r.in.gdal in=/media/marco/marcodata19/validationmetrics/std_01_05_5km_uint16.tif out=jetz --o")
system("d.rast jetz")

# get stats out
system("r.stats -nA in=jetz,grid_5km_mask_unchanged_500m > /mnt/data1tb/Dropbox/phenology/results_tmp/jetzvsy2002")


# ESA CCI (land cover) ----------------------------------------------------------------------
# read in land cover data
system("r.mask -r")
system('r.in.gdal in=NETCDF:"/media/marco/marcodata19/backupfrombigdisk/ESS_Drive/Marco/datasets/ESACCI_lcover/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2002-v2.0.7.nc":lccs_class band=1 out=esa_2002 -o')
# read in mask of unchanged forest
#system("r.in.gdal in=/media/marco/marcodata19/validationmetrics/forest_unchanged_300m.tif out=forest_mask_unchanged_300m")
system("g.region raster=mask_unchanged_300m")
system("r.mask raster=mask_unchanged_300m maskcats=1")
system("r.mapcalc 'esa_2002a = esa_2002' --o")

# get stats for land cover (forest types)
system("g.region raster=esa_2002a")
system('r.mask raster=esa_2002a maskcats="50 60 61 61 62 70 71 72 80 81 82 90 100"')
system("r.stats -an in=esa_2002a,grid_5km_mask_unchanged_500m > /media/marco/marcodata19/validationmetrics/pheno_grid_ESAvalues2002")


system("r.mask -r")
###############################################
# aggregate! PHENOLOGY datasets at 1km resolution


list.files("/mnt/data1tb/alessandro_metric/combined500m",full.names = TRUE)->filel

res<-NULL

for (i in 1:length(filel)){
  print(i)
  system(command = paste0("r.in.gdal in=",filel[i]," out=tmp --o"))
  system("g.region raster=tmp")
  system("r.null map=tmp setnull=0")
  # mask out everything outside gridmask
  system("g.region raster=grid5km -pa")
  system("r.mask raster=gridmask maskcats=1")
  # aggregate at 5km resolution
  system("r.resamp.stats in=tmp out=tmp_5km method=average --o")
  # remote old info on statistics
  system("rm  -f /home/marco/Dropbox/phenology/tmp/tmpstats")
  system("r.stats -ng in=tmp_5km,grid5km | awk '{print $3\",\"$4}' > /mnt/data1tb/Dropbox/phenology/tmp/tmpstats")
  # read in data
  read_csv("/mnt/data1tb/Dropbox/phenology/tmp/tmpstats",col_names = FALSE) %>%
    rename(value = X1,cat=X2) %>%
    mutate(year=as.numeric(str_remove_all(basename(filel[i]),"Y_|.tif")))->tmp
  bind_rows(tmp,res)->res
  # remove mask
  system("r.mask -r")
}

res %>%
  write_csv("/mnt/data1tb/alessandro_metric/final.csv")

# grid creation -----------------------------------------------------------


grid_create(x=0.25)->g1d

# flexible grid
grid_create<-function(x){
  # empty raster with desired resolution
  raster(res=x)->myr
  # create path for output file
    paste0("/mnt/data1tb/Dropbox/phenology/gridGEE/",
           str_remove_all(as.character(x),pattern="\\."),
           "degreegrid.tif")->outp
    str_remove_all(as.character(x),pattern="\\.")->resc
  # create raster grid and write out file
  rasterize(world,myr) %>%
    rastertodf() %>%
    mutate(value = 1:length(value)) %>% {.->> tmpgrid} %>%
    rasterFromXYZ(crs=latlon) %>%
    writeRaster(outp,overwrite = TRUE)
  # read newly created files into GRASS GIS
  system(command = paste0("g.region res=",x," -pa"))
  system(command = paste0("r.in.gdal in=",outp," out=tmp --o -o"))
  system(command = paste0("r.mapcalc 'grid",resc,"d =int(tmp)' --o"))
  system("g.region raster=grid5km")
  paste0("/mnt/data1tb/Dropbox/phenology/tmp/gridstats",resc,"degree")->outp1
  system(command = paste0("rm -f ", outp1))
  paste0("grid",resc,"d")->rstgrid
  system(command = paste0("r.stats -ln in=grid5km,",rstgrid," | awk '{print $1\",\"$2}' > ",outp1))
  return(tmpgrid)
}


read_csv("/mnt/data1tb/Dropbox/phenology/tmp/gridstats025degree",col_names = FALSE) %>%
  dplyr::select(X2) %>%
  unique() %>%
  dim()


# forest mask
system("g.list rast | grep mask")
system("g.region raster=mask_unchanged_500m")
system("g.region res=0.05 -pag")
system("r.info map=mask_unchanged_500m")
system("r.resamp.stats in=mask_unchanged_500m out=forestcount_5km method=count")
system("r.in.gdal in=/mnt/data1tb/Dropbox/phenology/GRASSGIS_files/grid_5km_mask_unchanged_500m.tif out=grid5km --o")
system("r.stats -lgn in=grid5km,forestcount_5km | awk '{print $3\",\"$4}' > /mnt/data1tb/Dropbox/phenology/tmp/forestcount")

# topography -------------------------------------------------------------

# population density
system("r.in.gdal in=/mnt/data1tb/Dropbox/phenology/popdensity/popdensity.tif out=popdensity")
system("g.region raster=popdensity -pg")
system("g.region raster=grid5km")
system("r.resamp.stats in=popdensity method=average out=popdensity5km")
system("r.stats -lgn in=grid5km,popdensity| awk '{print $3\",\"$4}' > /mnt/data1tb/Dropbox/phenology/tmp/popdensity")


# human impact index 
system("r.in.gdal in=/home/marco/Desktop/impact/impactindex.tif out=humi -o -l")
system("g.region raster=grid5km")
system("r.resamp.stats in=humi method=average out=humi5km")
system("r.stats -lgn in=grid5km,humi5km| awk '{print $3\",\"$4}' > /mnt/data1tb/Dropbox/phenology/tmp/humi")


# ecoregions
system("v.in.ogr in=/mnt/data1tb/Dropbox/HistFunc/envdata/ecoregionsaugust/tnc_terr_ecoregions.shp out=ecoregions")
system("g.region raster=grid100km -pg")
system("v.to.rast in=ecoregions out=ecoregions use=attr attribute_column=WWF_MHTNUM --o")
system("r.stats -lgn in=grid100km,ecoregions| awk '{print $3\",\"$4}' > /mnt/data1tb/Dropbox/phenology/tmp/ecoregions1degree")

read_csv("./tmp/ecoregions1degree",col_names = F) %>%
  rename(cat1d = X1,WWF_MHTNUM = X2) %>%
  inner_join(st_read("/mnt/data1tb/Dropbox/HistFunc/envdata/ecoregionsaugust/tnc_terr_ecoregions.shp") %>%
              as_tibble() %>%
              dplyr::select(WWF_MHTNUM,WWF_MHTNAM) %>%
              unique()) %>%
  write_csv("./tmp/ecoregions1degree.csv")


  


yearl<-2004:2020
map(yearl,tile_geotif,inpath = "/mnt/data1tb/alessandro_metric/tmpmarcog1v1/",
    outpath="/mnt/data1tb/alessandro_metric/combined500m/")

# pristine forests ---------------------------------------------------------
system("g.region raster=grid5km")
system("v.in.gdal in=/media/marco/marcodata19/Phenology_original/pristineforests/ifl_2016.shp out=pristine")
system("v.to.rast in=pristine out=tmp_5km use=cat --o")
system("r.mapcalc 'pristine_5km = tmp_5km/tmp_5km' --o")
system("r.stats -lgn in=grid5km,pristine_5km| awk '{print $3\",\"$4}' > /mnt/data1tb/Dropbox/phenology/tmp/pristine")
system("r.in.gdal in=/media/marco/marcodata19/lcturnoverphenology/grid1km.tif out=tmp --o")
system("r.mapcalc 'grid1km = int(tmp)' --o")
system("g.region raster=grid1km")

# 1km grid ----------------------------------------------------------------
system("g.region -d res=0.008333333 -pag")
system("v.to.rast in=countries out=countries use=cat --o")
system("r.mapcalc 'countries_1km = countries/countries' --o")
system("r.stats -lgn in=grid5km,countries_1km | awk '{print $1\",\"$2\",\"$3}' > /media/marco/marcodata19/lcturnoverphenology/test1km")
read_csv("/media/marco/marcodata19/lcturnoverphenology/test1km",col_names = F) %>%
  mutate(cat = 1:length(X2))->df

df %>%
  dplyr::select(X1,X2,cat) %>%
  rasterFromXYZ(crs=latlon) %>%
  writeRaster(filename = "/media/marco/marcodata19/lcturnoverphenology/grid1km.tif")



# lc turnover -------------------------------------------------------------

# get stats for land cover (forest types)
#system("g.region raster=mask_unchanged_300m")
#system('r.mask raster=lc_2015 maskcats="50 60 61 61 62 70 71 72 80 81 82 90 100"')
#system("r.stats -an in=lc_2015,grid1km > /media/marco/marcodata19/validationmetrics/pheno_grid_ESAvalues20151km")

calc_dis<-function(x){
  x %>%
    dplyr::select(-c(cat_1km,x,y)) %>%
    replace(is.na(.), 0) %>%
    dist() %>%
    mean()->res
  return(res)
}




list.files("/media/marco/marcodata19/lcturnoverphenology",pattern=".tif",full.names = TRUE)->filel

res<-NULL
for (i in 1:length(filel)){
  # get stats
  system(command = paste0("r.in.gdal in=",filel[i]," out=tmp --o"))
  system("g.region raster=tmp")
  system("r.mapcalc 'grid1kmtmp = int(tmp)' --o")
  system("g.region raster=lc_2015")
  system("r.stats -an in=lc_2015,grid1kmtmp > /media/marco/marcodata19/validationmetrics/pheno_grid_ESAvalues20151km")
  # read file back in
  read_delim("/media/marco/marcodata19/validationmetrics/pheno_grid_ESAvalues20151km",delim = " ",col_names = FALSE) %>%
    rename(esa_cat = X1,cat_1km = X2,area=X3) %>%
    mutate(esa_cat = paste0("lc_",esa_cat)) %>%
    pivot_wider(names_from = esa_cat,values_from = area) %>% 
    inner_join(df %>%
                 rename(x =X1,y = X2,cat_1km = cat,cat = X3))->df1
  # filter  5 km x 5km cells with more than 19 records
  df1 %>%
    group_by(cat) %>%
    summarise(n = n()) %>%
    filter(n > 4) %>%
    inner_join(df1) %>%
    dplyr::select(-c(n)) %>%
    group_by(cat) %>%
    nest() %>%
    ungroup()->df2
  # calculate dissimilarity
  plan(multisession,workers = 7)
  df2 %>%
    ungroup() %>%
    mutate(res = future_map(data,~calc_dis(x=.))) %>%
    unnest(cols = res) %>%
    dplyr::select(-c(data))->df3
  bind_rows(df3,res)->res
  system("rm -f /media/marco/marcodata19/validationmetrics/pheno_grid_ESAvalues20151km")
  system("rm -f /media/marco/marcodata19/lcturnoverphenology/grid1km_v2.tif")
  rm(df1)
  rm(df2)
  rm(df3)
  system("g.remove -f type=raster name=tmp")
  system("g.remove -f type=raster name=grid1kmtmp")
}





res %>%
  inner_join(grid5km) %>%
  dplyr::select(x,y,res) %>%
  st_as_sf(coords=c("x","y"))->dat
  
  %>%
  st_write("/home/marco/Desktop/test.shp")
  
  rasterFromXYZ() %>%
  writeRaster(filename = "/home/marco/Desktop/myr2.tif",overwrite = TRUE)


  rangel




df3 %>%
  dplyr::select(cat,res) %>%
  inner_join(grid5km) %>%
  dplyr::select(x,y,res) %>%
  rasterFromXYZ(crs = latlon) %>%
  writeRaster(filename = "/home/marco/Desktop/diss.tif")


# annual climate data -----------------------------------------------------
list.files("/media/marco/marcodata19/bioclimdynamic",full.names = TRUE) %>%
  .[str_detect(.,"terraclimate")]->fileslglob

yearl<-as.character(2003:2020)

for (y in 1:length(yearl)){
  print(yearl[y])
  fileslglob[str_detect(fileslglob,yearl[y])]->filesl
  res<-NULL
  for (i in 1:length(filesl)){
    system(command = paste0("r.in.gdal in=",filesl[i]," out=tmp --o"))
    #system("g.region raster=tmp -pg")
    system("g.region raster=grid_5km_mask_unchanged_500m -pg")
    system(command = paste0("r.resamp.stats in=tmp out=tmp1 method=average --o"))
    #system(command = "r.stats -gn in=grid_5km_mask_unchanged_500m,tmp1 > /home/marco/Desktop/dat")
    system(command = "r.stats -gn in=grid_5km_mask_unchanged_500m,tmp1 |  awk '{print $3\",\"$4}' > /home/marco/Desktop/dat")
    read_csv("/home/marco/Desktop/dat",col_names = F) %>%
      dplyr::rename(cat = X1, value = X2) %>%
      mutate(var=str_split_fixed(basename(filesl[i]),"_",n=3)[,1],
             year = str_split_fixed(basename(filesl[i]),"_",n=3)[,2]) %>%
      mutate(year = as.numeric(year))->df
    bind_rows(df,res)->res
  }
  res %>%
    pivot_wider(names_from = var,values_from = value)->tmpdf
  fout<-paste0("/media/marco/marcodata19/bioclim_csv/terraclimate/",yearl[y],".csv")
  write_csv(tmpdf,fout)
  system("rm /home/marco/Desktop/dat")
}

list.files("/media/marco/marcodata19/bioclim_csv/terraclimate",full.names = T) %>%
  map_dfr(read_csv)->bioclimdf
  

# Koppen Geiger -----------------------------------------------------------
system("v.in.ogr in=/mnt/data1tb/Dropbox/phenology/KG/kg_codes.shp out=kg --o")
system("g.region raster=grid5km1 -pg")

system("v.to.rast in=kg out=kg_lv1 use=attr attribute_column=lvl1_ct")
system("v.to.rast in=kg out=kg_lv2 use=attr attribute_column=lvl2_ct")
system("v.to.rast in=kg out=kg_lv3 use=attr attribute_column=lvl3_ct")

system("r.stats -ln in=kg_lv1,grid_5km_mask_unchanged_500m | awk '{print $1\",\"$2}' > /mnt/data1tb/Dropbox/phenology/KG/kg_lv1")
system("r.stats -ln in=kg_lv2,grid_5km_mask_unchanged_500m | awk '{print $1\",\"$2}' > /mnt/data1tb/Dropbox/phenology/KG/kg_lv2")
system("r.stats -ln in=kg_lv3,grid_5km_mask_unchanged_500m | awk '{print $1\",\"$2}' > /mnt/data1tb/Dropbox/phenology/KG/kg_lv3")

read_csv("/mnt/data1tb/Dropbox/phenology/KG/kg_lv1",col_names = F) %>%
  rename(lvl1_ct=X1,cat=X2) %>%
  inner_join(read_csv("/mnt/data1tb/Dropbox/phenology/KG/kg_lv2",col_names = F) %>%
               rename(lvl2_ct=X1,cat=X2)) %>%
  inner_join(read_csv("/mnt/data1tb/Dropbox/phenology/KG/kg_lv3",col_names = F) %>%
               rename(lvl3_ct=X1,cat=X2)) %>%
  inner_join(bioclimdf) %>%
  write_csv("/media/marco/marcodata19/bioclim_csv/terraclimate_combined.csv")
  


# difference in NDVI ----------------------------------------------------------------
system("r.in.gdal in=/home/marco/Desktop/test/out.tif out=ndvi_dif")
system("g.region res=0.05")
system("r.resamp.stats in=ndvi_dif out=ndvi_dif5km method=average")
system("g.region -p")

system("r.stats -lgn in=grid5km,ndvi_dif5km | awk '{print $3\",\"$4}' > /media/marco/marcodata19/lcturnoverphenology/ndvi_dif5km")
read_csv("/media/marco/marcodata19/lcturnoverphenology/ndvi_dif5km",col_names = F) %>%
  rename(cat = X1,ndvi_dif=X2)->df



# topographic diversity ---------------------------------------------------
system(command = "r.in.gdal in=/home/marco/Desktop/geom_1KMsha_GMTEDmd.tif out=shannontop")
system("g.region raster=shannontop")
system(command = paste0("r.stats -gn in=shannontop,grid_5km_mask_unchanged_500m > /mnt/data1tb/grasstmp/tmp"))
read_delim("/mnt/data1tb/grasstmp/tmp",delim=" ",col_names = FALSE) %>%
  rename(variable = X3,cat = X4) %>%
  dplyr::select(-c(X1,X2)) %>%
  group_by(cat) %>%
  summarise(variable = mean(variable)) %>%
  write_csv("./tmp/shannontop.csv")
 


# annual climate data 1km -------------------------------------------------
list.files("/media/marco/marcodata19/chelsa_tseries",full.names = TRUE) %>%
  .[str_detect(.,"tas")]->fileslglob


yearl<-as.character(2003:2019)

res<-NULL

# loop through files
for (y in 1:length(yearl)){
  print(yearl[y])
  fileslglob[str_detect(fileslglob,yearl[y])]->filesl
  for (i in 1:length(filesl)){
    system(command = paste0("r.in.gdal in=",filesl[i]," out=tmp_",i, " --o"))
  }
  # aggregate temperature (monthly time series for a given year)
  system("g.region raster=tmp_1")
  system(command = paste0("r.mapcalc 'bio_01 = (",paste0("tmp_",1:12,collapse = "+"),")/12' --o"))
  # rescale
  system(command = paste0("r.mapcalc 'bio_01a = bio_01*0.1' --o"))
  # convert to Celsius degrees
  system(command = paste0("r.mapcalc 'bio_01b = bio_01a-273.15' --o"))
  # sdev spatial data
  system("g.region raster=grid_5km_mask_unchanged_500m -pg")
  system("g.remove type=raster name=bio_01c -f")
  system(command = paste0("r.resamp.stats in=bio_01b out=bio_01c method=stddev --o"))
  # save data
  system(command = "r.stats -gn in=grid_5km_mask_unchanged_500m,bio_01c |  awk '{print $3\",\"$4}' > /home/marco/Desktop/dat")
  # put everything together
  read_csv("/home/marco/Desktop/dat",col_names = F) %>%
    dplyr::rename(cat = X1, value = X2) %>%
    mutate(year = as.numeric(yearl[y]))->tmpres
  bind_rows(tmpres,res)->res
  system("rm /home/marco/Desktop/dat")
}

res %>%
  write_csv(file = "/media/marco/marcodata19/tseries_1km/bio_01.csv")


#df_dif %>%
#  inner_join(grid5km) %>%
#  dplyr::select(x,y,dif) %>%
#  rasterFromXYZ(crs = latlon) %>%
#  writeRaster(filename = "/home/marco/Desktop/dif.tif")

df_dif %>%
  write_csv(file = "/home/marco/Desktop/temp_dif.csv")


# grid025 -----------------------------------------------------------------
system("r.in.gdal in=/home/marco/Desktop/grid025.tif out=grid025 --o")
system("g.region raster=grid025 -pg")
system("v.to.rast in=ecoregions out=ecoregions use=attr attribute_column=WWF_MHTNUM --o")
system("r.stats -lgn in=grid025,ecoregions| awk '{print $3\",\"$4}' > /home/marco/Dropbox/phenology/tmp/ecoregions025degree")

read_csv("./tmp/ecoregions025degree",col_names = F) %>%
  rename(cat1d = X1,WWF_MHTNUM = X2) %>%
  inner_join(st_read("/home/marco/Dropbox/HistFunc/envdata/ecoregionsaugust/tnc_terr_ecoregions.shp") %>%
               as_tibble() %>%
               dplyr::select(WWF_MHTNUM,WWF_MHTNAM) %>%
               unique()) %>%
  write_csv("./tmp/ecoregions025degree.csv")

# mean annual temperature modified ----------------------------------------

g.region raster=bio01_modified
r.stats -gn in=grid_5km_mask_unchanged_500m,bio01_modified > /media/marco/marcodata19/chelsa_tmean/values_mat
read_delim("/media/marco/marcodata19/chelsa_tmean/values_mat",delim=" ",col_names = FALSE) %>%
  rename(variable = X3,cat = X4) %>%
  dplyr::select(-c(X1,X2)) %>%
  group_by(cat) %>%
  summarise(variable = mean(variable))->tmp

res<-NULL

for (i in str_remove_all(basename(biovars),".tif")){
  print(i)
  system(command = paste0("r.stats -gn in=",i,",grid_5km_mask_unchanged_500m > /mnt/data1tb/grasstmp/tmp"))
  read_delim("/mnt/data1tb/grasstmp/tmp",delim=" ",col_names = FALSE) %>%
    rename(variable = X3,cat = X4) %>%
    dplyr::select(-c(X1,X2)) %>%
    group_by(cat) %>%
    summarise(variable = sd(variable)) %>%
    mutate(varname = i)->tmp
  bind_rows(tmp,res)->res
}

# aridity: climatology ----------------------------------------------------

# SEASONALITY IN ARIDITY
# system("r.in.gdal in=/media/marco/marcodata19/chelsa/aridity/Ar_SEAS.tif out=aridity_seas")
# system("g.region raster=aridity_seas")
# 
# system(command = paste0("r.stats -gn in=aridity_seas,grid_5km_mask_unchanged_500m > /home/marco/tmp/aridity"))
# 
# fread("/home/marco/tmp/aridity") %>%
#   rename(variable = V3,cat = V4) %>%
#   dplyr::select(-c(V1,V2)) %>%
#   .[, by = cat,.(variable = sd(variable))] %>%
#   mutate(varname = "aridity_seas_sd") %>%
#   write_csv("./GRASSGIS_files/aridity_seas_sd.csv")
# 
# 
# fread("/home/marco/tmp/aridity") %>%
#   rename(variable = V3,cat = V4) %>%
#   dplyr::select(-c(V1,V2)) %>%
#   .[, by = cat,.(variable = mean(variable))] %>%
#   mutate(varname = "aridity_seas_mean") %>%
#   write_csv("./GRASSGIS_files/aridity_seas_mean.csv")

# MEAN ARIDITY
system("r.in.gdal in=/media/marco/marcodata19/chelsa/aridity/annual_ar/ar.tif out=aridity_average --o")
system("g.region raster=aridity_average")

system(command = paste0("r.stats -gn in=aridity_average,grid_5km_mask_unchanged_500m > /home/marco/tmp/aridity_annualaverage"))

fread("/home/marco/tmp/aridity_annualaverage") %>%
  rename(variable = V3,cat = V4) %>%
  dplyr::select(-c(V1,V2)) %>%
  filter(variable > (quantile(variable,probs=c(0.01))) & variable < (quantile(variable,probs=c(0.99)))) %>%
  .[, by = cat,.(variable = sd(variable))] %>%
  mutate(varname = "aridity_seas_sd") %>%
  write_csv("./GRASSGIS_files/aridity_annualaverage_sd.csv")


fread("/home/marco/tmp/aridity_annualaverage") %>%
  rename(variable = V3,cat = V4) %>%
  dplyr::select(-c(V1,V2)) %>%
  filter(variable > (quantile(variable,probs=c(0.01))) & variable < (quantile(variable,probs=c(0.99)))) %>%
  .[, by = cat,.(variable = mean(variable))] %>%
  mutate(varname = "aridity_seas_mean") %>%
  write_csv("./GRASSGIS_files/aridity_annualaverage_mean.csv")