# main map
g.region res=0.05 -ap
r.in.gdal in=/home/marco/Desktop/Y_2003.tif out=complete2003 --o


r.null map=complete2003 setnull=0

# 1km map 
r.in.gdal in=Y_2003_poly_14_1km.tif out=test1km --o

# 463m map
r.in.gdal in=Y_2003_poly_14_463m.tif out=test463m --o



# try and resample 1km map! (need mask first)
g.region raster=complete2003 -ap
r.mapcalc 'mask2003 = int(complete2003/complete2003)' --o
r.mask raster=mask2003 maskcats=1

r.resamp.stats in=test1km out=test1kma method=average
r.out.gdal in=test1kma out=test1kma.tif createopt="COMPRESS=LWZ"


r.out.gdal in=complete2003 out=complete2003.tif createopt="COMPRESS=LWZ"

# try and resample 463m map! (need mask first)
g.region raster=complete2003 -ap


r.resamp.stats in=test463m out=test463ma method=average
r.out.gdal in=test463ma out=test463m.tif createopt="COMPRESS=LWZ"

r.in.gdal in=/mnt/data1tb/Dropbox/phenology/testprojections/test.tif out=test2003 --o

g.region raster=test2003 -ap
r.stats test2003

r.resamp.stats in=test2003 out=test20035km method=average


r.in.gdal in=/mnt/data1tb/Dropbox/phenology/GRASSGIS_files/grid_5km_mask_unchanged_500m.tif out=grid5km --o
g.region raster=grid5km -pa
r.mapcalc 'gridmask = grid5km/grid5km'
r.mask raster=gridmask maskcats=1
r.resamp.stats in=test2003 out=test20035km method=average --o
r.out.gdal in=test20035km out=/mnt/data1tb/Dropbox/phenology/testprojections/test20035km.tif createopt="COMPRESS=DEFLATE" --o

r.stats -ng in=test20035km,grid5km | awk '{print $3","$4}' > /mnt/data1tb/Dropbox/phenology/testprojections/20031km
r.stats -ng in=test20035km,grid5km | awk '{print $3","$4}' > /mnt/data1tb/Dropbox/phenology/testprojections/20031km


rm /mnt/data1tb/Dropbox/phenology/testprojections/complete2003


head /mnt/data1tb/Dropbox/phenology/testprojections/20031km

r.stats -ln in=test20035km,grid5km

