library(tidyverse);library(lubridate)
setwd("/home/marco/Desktop/phenology_8day/")

system("r.in.gdal in=file_1009843200000.tif out=test --o")
system("g.region raster=test")
system("d.mon start=wx1")

filel<-list.files()

res<-NULL

for (i in 1:length(filel)){
  # get unix timestamp
  str_remove_all(filel[i],"file_|.tif") %>% {.->>unixdate} %>%
    as.numeric()/1000->tmp 
  # convert unix timestamp into date
  as_date(as_datetime(tmp))->dt
  # store in tibble
  tibble(dt,filename=filel[i]) -> tmpres
  bind_rows(tmpres,res)->res
}

res %>%
  arrange(dt) %>%
  mutate(start=dt,end=lead(dt)-1) %>%
  slice(1:45)->grassinfo

for (i in 1:dim(grassinfo)[1]){
  print(i)
  # read in files
  system(command=paste0("r.in.gdal in=",grassinfo[i,]$filename," out=map",i," --o"))
  # register map
  system(command=paste0("t.register type=raster maps=map",i," start=",grassinfo[i,]$start," end=",grassinfo[i,]$end," --o"))
}


#grassinfo %>%
#  mutate(mapname = paste0("map",1:45))->grassinfo1

# create space-time rastermap
system(command='t.create type=strds temporaltype=absolute output=modis_8days title="MODIS 8 days" description="MODIS daily NDVI dataset" --o')
# register maps
system(command=paste0("t.register type=raster input=modis_8days maps=",paste0("map",1:10,collapse = ",")," --o"))
# fill in gaps
system(command="t.rast.gapfill input=modis_8days basename=gap --o")
system("g.list rast | grep gap")

system("d.erase")
system("d.rast gap_2002_02_01")       


system("g.list rast | grep gap",intern = T)->maps

for (i in 1:length(maps)){
  system(command=paste0("g.remove type=raster -f name=",maps[i]))
}


system("d.erase")
system("d.rast gap_2002_02_01")

system(command="t.rast.list input=modis_8days method=deltagaps")


# missing maps?
system(command="t.rast.gapfill input=modis_8days basename=gap --o")



# map1 
system(command="r.mapcalc 'map1a = int(map1/map1)'")
system(command="r.mapcalc 'map2a = int(map2/map2)'")
system(command="r.mapcalc 'map3a = int(map3/map3)'")

system("r.colors map=map1a color=rainbow")
system("r.colors map=map2a color=random")
system("r.colors map=map3a color=blues")

system("d.erase")
system("d.rast map1a")
system("d.rast map2a")
system("d.rast map3a")

system("d.rast gap_2002_01_08")
system("d.rast gap_2002_01_16")

system(command = "r.series input=map1a,map2a,map3a method=sum output=sum_maps")
gap_2002_01_16



seq(as.Date('1982-01-15'), as.Date('2010-12-15'), 'months')


stack(c("file_1009843200000.tif","file_1010534400000.tif","file_1011225600000.tif"))->myr
rastertodf(myr)->dd
brick(rasterFromXYZ(dd[,c(1,2,3)]),rasterFromXYZ(dd[,c(1,2,4)]),rasterFromXYZ(dd[,c(1,2,5)]))->myr1

rts(myr1,grassinfo$start[1:3])->myr2
rasterts_linear <- rtsa.gapfill(x=myr2, method="linear",cores=6)

rasterts_linear[[1]]

x <- 1:10
y <- rnorm(10)
par(mfrow = c(2,1))
plot(x, y, main = "approx(.) and approxfun(.)")
points(approx(x, y), col = 2, pch = "*")
points(approx(x, y, method = "constant"), col = 4, pch = "*")

grassinfo %>%
  mutate(value = sample(c(rep(NA,10),rnorm(30)))) %>% {.->>tmp}  %>%
  mutate(cc = na.approx(value$tmp,rule =2))

na.approx(tmp$value,rule =2 )

na.approx(tmp$value)

mutate(ip.value = na.approx(value, maxgap = 4, rule = 2))

v<-sample(1:100,8)
c(21,6,18,1,51,34,82,3)

ndvi<-c(4,20,56,80,67,50,23,2)

ndvi<-c(16,20,23,24,30,32,32,33)
na.approx(c(16,NA,23,NA,NA,32,NA,NA))


c(21,6,18,1,51,34,82,3)


na.approx(v,rule =2 )

mydf <- data.frame(date = as.Date(c("2015-10-05","2015-10-08","2015-10-09",
                                    "2015-10-12","2015-10-14")),       
                   value = c(8,3,9,NA,5))

library(dplyr)
library(zoo)

data.frame(date = seq(mydf$date[1], mydf$date[nrow(mydf)], by = 1)) %>%
  full_join(mydf, by = "date") %>%
  mutate(approx = na.approx(value))
