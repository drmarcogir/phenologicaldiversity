
library(terra)

rast("/home/marco/Desktop/ciao1/chelsa-w5e5v1.0_obsclim_pr_30arcsec_global_daily_201012.nc")->myr

myr[[1]]->tmp
myr[[2]]->tmp1

e <- ext(-180, 180, -55.9, 79)
system.time(rc <- terra::crop(tmp, e))

terra::writeRaster(tmp,filename = "/home/marco/Desktop/test.tif",overwrite=TRUE)


mean(rast(list(rc,rc,rc,rc,rc,rc,rc,rc,rc,rc,rc,rc,
               rc,rc,rc,rc,rc,rc,rc,rc,rc,rc,rc,rc,
               rc,rc,rc,rc,rc,rc,rc)))->dd

length(list(rc,rc,rc,rc,rc,rc,rc,rc,rc,rc,rc,rc))

r <- raster(ncol=2, nrow=2)
vals <- sample(1:100,4)
r <- setValues(r, vals)
# equivalent to
values(r) <- vals
r6<-r
stack(r1,r2,r3,r4,r5,r6)->mystack

writeRaster(mystack,filename = "/home/marco/Desktop/myr.tif",overwrite=TRUE)
rast("/home/marco/Desktop/myr.tif")->myr
fun1=function(a,b){return(a+b)}

lapp(myr[[1:2]],fun=fun1,cores=6)->dd


nc_open("/home/marco/Desktop/ciao1/chelsa-w5e5v1.0_obsclim_pr_30arcsec_global_daily_201012.nc")->nc_data
names(nc_data[['var']])


lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")
pr.array <- ncvar_get(nc_data, "pr",start=c(4294967295,0,0),cunt=c(2,2,0))


read_ncdf("/home/marco/Desktop/ciao1/chelsa-w5e5v1.0_obsclim_pr_30arcsec_global_daily_201012.nc",
          var = )->dd
"/home/marco/Desktop/ciao1/chelsa-w5e5v1.0_obsclim_pr_30arcsec_global_daily_201012.nc"->f
read_ncdf(f, ncsub = cbind(start = c(1, 1, 1, 0), count = c(10, 12, 1, 0)))
ncmeta::nc_coord_var(f)->dd1
var <- ncmeta::get_vars(var, meta)
rep_var <- var[1L]