library(googledrive)
drive_ls(path = "curruspito_phenology", recursive = TRUE)->file.l

for (i in 1:dim(newfiles)[1]){
  print(i)
  drive_download(file=newfiles[i,],path =paste0("/mnt/data1tb/phenology/dataApril/",newfiles[i,]$name),overwrite = TRUE)
}

setwd("/mnt/data1tb/phenology/dataApril/")
list.files(pattern = ".tif")

file.l %>%
  anti_join(tibble(name=list.files()))->newfiles



