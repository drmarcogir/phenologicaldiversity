

list.files("/media/marco/marcodata19/chelsa_tmean",pattern = ".tif",full.names = TRUE)->filesl

for (i in 1:length(months)){
  print(i)
  # loop through files
  filesl[str_detect(filesl,months[i])]->focalfiles
  # rasters and averages
  (rast(focalfiles[1])+rast(focalfiles[2]))/2->tmean
  # replace values smaller than 0 with 0
  tmean[tmean < 0] <- 0
  # fileout
  paste0("/media/marco/marcodata19/chelsa_tmean/means/tmean_",str_replace_all(months[i],"_",""),'.tif')->fout
  terra::writeRaster(tmean,fout)
}         
  
rast(list.files("/media/marco/marcodata19/chelsa_tmean/means/",full.names = TRUE))->montlyfiles
mean(montlyfiles)->MAT
terra::writeRaster(MAT,"/media/marco/marcodata19/chelsa_tmean/annual_mean.tif")