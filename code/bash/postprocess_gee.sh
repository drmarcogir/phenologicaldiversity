grass78
# import map creating by filling in minimum 
r.in.gdal in=/mnt/data1tb/Dropbox/phenology/alessandro_metric/y2002_minNDVI_times3_1.tif out=y2002_minNDVI_times3
g.region raster=y2002_minNDVI_times3
d.mon start=wx1
d.rast y2002_minNDVI_times3

# import grid
r.in.gdal in=/mnt/data1tb/Dropbox/phenology/gridGEE/grid_pheno_grass.tif out=grid1 --o
r.mapcalc 'grid2 = int(grid1/grid1)'

# create mask using grid info
r.mask raster=grid2 maskcats=1
d.erase
d.rast y2002_minNDVI_times3

# export unconstrained forest diversity map i.e. the one created using the NDVI minimum rule
r.out.gdal in=y2002_minNDVI_times3 out=/mnt/data1tb/Dropbox/phenology/alessandro_metric/y2002_minNDVI_times3_clipped.tif createopt="COMPRESS=LZW"

# import map created not using the NDVI minimum rule
r.in.gdal in=/mnt/data1tb/Dropbox/phenology/alessandro_metric/y2002_23imgs.tif out=y2002_23imgs

# set to 1 y2002_23imgs
r.mapcalc 'y2002_23imgsv1 = int(y2002_23imgs/y2002_23imgs)' 

# set to y2002_minNDVI_times3
r.mapcalc 'y2002_minNDVI_times3v1 = int(y2002_minNDVI_times3/y2002_minNDVI_times3)' 


r.series in=y2002_23imgsv1,y2002_minNDVI_times3v1 method=sum out=tmp --o
r.mapcalc 'tmp1 = int(tmp)' --o

r.out.gdal in=tmp1 out=/mnt/data1tb/Dropbox/phenology/alessandro_metric/dif.tif createopt="COMPRESS=LZW"
