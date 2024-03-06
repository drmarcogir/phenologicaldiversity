# process ERA 5 - land data
library(tidyverse)
library(lubridate)
library(raster)

for (i in 1:264){
  print(i)
  system(command = paste0("gdal_translate -b ", i, " -a_nodata 9999 /media/marco/marcodata19/ERA_LAND/temperature.grib /media/marco/marcodata19/ERA_LAND/tmp.tif"))
  r<-rotate(raster("/media/marco/marcodata19/ERA_LAND/tmp.tif"))
  r1<-r-273.15
  crs(r1)<-latlon
  # get variable, year and month
  system("gdalinfo /media/marco/marcodata19/ERA_LAND/tmp.tif | grep GRIB_VALID_TIME",intern = TRUE) %>%
  trimws() %>%
  str_remove(.,"GRIB_VALID_TIME=") %>%
  str_remove(.," sec UTC") %>%
  as.numeric()->unixdate
  # filename 
  paste0("/media/marco/marcodata19/ERA_LAND/tif/tmp_",year(as_datetime(unixdate)),"_",month(as_datetime(unixdate)),".tif")->fileout
  writeRaster(r1,fileout,overwrite=T)
  system("rm -f /media/marco/marcodata19/ERA_LAND/tmp.tif")
}

for (i in 1:264){
  print(i)
  system(command = paste0("gdal_translate -b ", i, " -a_nodata 9999 /media/marco/marcodata19/ERA_LAND/precipitation.grib /media/marco/marcodata19/ERA_LAND/tmp.tif"))
  r1<-rotate(raster("/media/marco/marcodata19/ERA_LAND/tmp.tif"))
  crs(r1)<-latlon
  # get variable, year and month
  system("gdalinfo /media/marco/marcodata19/ERA_LAND/tmp.tif | grep GRIB_VALID_TIME",intern = TRUE) %>%
    trimws() %>%
    str_remove(.,"GRIB_VALID_TIME=") %>%
    str_remove(.," sec UTC") %>%
    as.numeric()->unixdate
  # filename 
  paste0("/media/marco/marcodata19/ERA_LAND/tif/prec_",year(as_datetime(unixdate)),"_",month(as_datetime(unixdate)),".tif")->fileout
  writeRaster(r1,fileout,overwrite=T)
  system("rm -f /media/marco/marcodata19/ERA_LAND/tmp.tif")
}



# evapotranspiration 
for (i in 1:264){
  print(i)
  system(command = paste0("gdal_translate -b ", i, " -a_nodata 9999 /media/marco/marcodata19/ERA_LAND/evaporation.grib /media/marco/marcodata19/ERA_LAND/tmp.tif"))
  r<-rotate(raster("/media/marco/marcodata19/ERA_LAND/tmp.tif"))
  #r1<-r-273.15
  crs(r1)<-latlon
  # get variable, year and month
  system("gdalinfo /media/marco/marcodata19/ERA_LAND/tmp.tif | grep GRIB_VALID_TIME",intern = TRUE) %>%
    trimws() %>%

        str_remove(.," sec UTC") %>%
    as.numeric()->unixdate
  # filename 
  paste0("/media/marco/marcodata19/ERA_LAND/tif/evap_",year(as_datetime(unixdate)),"_",month(as_datetime(unixdate)),".tif")->fileout
  writeRaster(r1,fileout,overwrite=T)
  system("rm -f /media/marco/marcodata19/ERA_LAND/tmp.tif")
}

r<-raster("/media/marco/marcodata19/ERA_LAND/tif/prec_1995_6.tif")

# create drought index
yearl<-1995:2016
filesl<-list.files("/media/marco/marcodata19/ERA_LAND/tif",full.names = TRUE)
monthl<-1:12

for (i in 1:length(yearl)){
  print(yearl[i])
  filesl %>%
    .[!str_detect(.,"tmp")]->tmpfiles
  tmpfiles %>%
    .[str_detect(.,as.character(yearl[i]))]->tmpyear
  # subset by month
  for (y in 1:length(monthl)){
    tmpyear %>%
      .[str_detect(.,paste0("_",monthl[y],".tif"))]->tmpfiles1  
    # get raster files
    raster(tmpfiles1[str_detect(tmpfiles1,"evap")])->evap
    raster(tmpfiles1[str_detect(tmpfiles1,"prec")])->prec
    # drought index
    prec-evap->drought
    # write out raster files
    paste0("/media/marco/marcodata19/ERA_LAND/tif/drought_",yearl[i],"_",monthl[y],".tif")->out
    writeRaster(drought,out,overwrite = TRUE)
  }
}


# create annual rasters
yearl<-1995:2016
varl<-c("tmp","prec","evap")
filesl<-list.files("/media/marco/marcodata19/ERA_LAND/tif",full.names = TRUE)

for (i in 1:length(varl)){
  print(varl[i])
  # subset variable of interest
  filesl %>%
    .[str_detect(.,varl[i])]->tmpfiles
  for (y in 1:length(yearl)){
    print(yearl[y])
    # subset given year
    tmpfiles %>%
      .[str_detect(.,as.character(yearl[y]))]->tmpyear
    # get raster stack
    stack(tmpyear)->tmpr
    if(varl[i]=="tmp"){
      # get mean
      mean(tmpr)->tmpr1
      # get standard deviation
      beginCluster(7)
      clusterR(tmpr, calc, args=list(sd, na.rm=T))->tmpr2
      endCluster()
      # mean 
      paste0("/media/marco/marcodata19/ERA_LAND/annual_rasters/",varl[i],"_mean_",yearl[y],".tif")->out1
      writeRaster(tmpr1,out1)
      # standard deviation
      paste0("/media/marco/marcodata19/ERA_LAND/annual_rasters/",varl[i],"_sd_",yearl[y],".tif")->out1
      writeRaster(tmpr2,out1)
    } else {
      # get sum
      sum(tmpr)->tmpr1
      # standard deviation
      beginCluster(7)
      clusterR(tmpr, calc, args=list(sd, na.rm=T))->tmpr2
      endCluster()
      # sum
      paste0("/media/marco/marcodata19/ERA_LAND/annual_rasters/",varl[i],"_sum_",yearl[y],".tif")->out1
      writeRaster(tmpr1,out1) 
      # standard deviation
      paste0("/media/marco/marcodata19/ERA_LAND/annual_rasters/",varl[i],"_sd_",yearl[y],".tif")->out1
      writeRaster(tmpr2,out1)
      
    }
  }
}

# Drought
# create annual rasters
yearl<-1995:2016
varl<-c("drought")
filesl<-list.files("/media/marco/marcodata19/ERA_LAND/tif",full.names = TRUE)

for (i in 1:length(varl)){
  print(varl[i])
  # subset variable of interest
  filesl %>%
    .[str_detect(.,varl[i])]->tmpfiles
  for (y in 1:length(yearl)){
    print(yearl[y])
    # subset given year
    tmpfiles %>%
      .[str_detect(.,as.character(yearl[y]))]->tmpyear
    # get raster stack
    stack(tmpyear)->tmpr
      # get mean
      mean(tmpr)->tmpr1
      # get standard deviation
      beginCluster(7)
      clusterR(tmpr, calc, args=list(sd, na.rm=T))->tmpr2
      endCluster()
      # mean 
      paste0("/media/marco/marcodata19/ERA_LAND/annual_rasters/",varl[i],"_mean_",yearl[y],".tif")->out1
      writeRaster(tmpr1,out1)
      # standard deviation
      paste0("/media/marco/marcodata19/ERA_LAND/annual_rasters/",varl[i],"_sd_",yearl[y],".tif")->out1
      writeRaster(tmpr2,out1)
  }
}
x-special/nautilus-clipboard
copy
file://

# import maps into GRASS
filesl<-list.files("/media/marco/marcodata19/ERA_LAND/annual_rasters",full.names = TRUE)

res<-NULL

for (i in 1:length(filesl)){
  print(i)
  system("g.region res=0.1")
  system(paste0("r.in.gdal in=",filesl[i]," out=tmp -l -o --o"))
  system("g.region raster=grid5km")
  system("r.stats -nA in=grid5km,tmp > /media/marco/marcodata19/Phenology_original/tmp_stats/stats")
  read_delim("/media/marco/marcodata19/Phenology_original/tmp_stats/stats",delim = " ",col_names = F) %>%
    rename(cat = X1, value=X2) %>%
    mutate(variable = str_remove(basename(filesl[i]),".tif"))->tmpres
  bind_rows(tmpres,res)->res
}

write_csv(res,"/media/marco/marcodata19/Phenology_original/ERA_LAND/clim.csv")
