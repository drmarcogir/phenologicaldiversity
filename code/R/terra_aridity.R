library(marcoUtils)
library(terra)
library(tidyverse)

list.files("/media/marco/marcodata19/chelsa/aridity",full.names = TRUE,pattern=".tif")->filesl
           
rast(filesl[str_detect(filesl,"_pet_")])->pet
sum(pet/100)->pet_sum

rast(filesl[str_detect(filesl,"_pr_")])->pr
sum(pr/10)->pr_sum

pet_sum/pr_sum->ar

writeRaster(ar,filename = "/media/marco/marcodata19/chelsa/aridity/annual_ar/ar.tif",overwrite = TRUE)

