
#--- 18 images
# build virtual file
gdalbuildvrt outfile.vrt $(ls y2002_18imgs/*.tif)  

# create geotiff
gdal_translate -of GTiff -co "COMPRESS=LZW" outfile.vrt /mnt/data1tb/Dropbox/phenology/alessandro_metric/y2002_18imgs.tif

#--- 20 images
# build virtual file
gdalbuildvrt outfile.vrt $(ls y2002_20imgs/*.tif)  

# create geotiff
gdal_translate -of GTiff -co "COMPRESS=LZW" outfile.vrt /mnt/data1tb/Dropbox/phenology/alessandro_metric/y2002_20imgs.tif



#--- 23 images
# build virtual file
gdalbuildvrt outfile.vrt $(ls y2002_23imgs/*.tif)  

# create geotiff
gdal_translate -of GTiff -co "COMPRESS=LZW" outfile.vrt /mnt/data1tb/Dropbox/phenology/alessandro_metric/y2002_23imgs.tif

#--- minimum rule window 3
# build virtual file
gdalbuildvrt outfile.vrt $(ls y2002_minNDVI_times3/*.tif)  

# create geotiff
gdal_translate -of GTiff -co "COMPRESS=LZW" outfile.vrt /mnt/data1tb/Dropbox/phenology/alessandro_metric/y2002_minNDVI_times3.tif


#--- minimum rule window 5
# build virtual file
gdalbuildvrt outfile.vrt $(ls /mnt/data1tb/alessandro_metric/y2002_minNDVI_times5/*.tif)  

# create geotiff
gdal_translate -of GTiff -co "COMPRESS=LZW" outfile.vrt /mnt/data1tb/Dropbox/phenology/alessandro_metric/y2002_minNDVI_times5.tif


# e.g for moving files
ls | grep times_5.tif | xargs mv -t y2002_minNDVI_times5/



#--- zero version 15 images
# build virtual file
gdalbuildvrt outfile.vrt $(ls /mnt/data1tb/alessandro_metric/y2002_15imgs0/*.tif)  

# create geotiff
gdal_translate -of GTiff -co "COMPRESS=LZW" outfile.vrt /mnt/data1tb/Dropbox/phenology/alessandro_metric/y2002_zeroNDVI_15imgs.tif


# e.g for moving files
ls | grep times_5.tif | xargs mv -t y2002_minNDVI_times5/

qgis /mnt/data1tb/Dropbox/phenology/alessandro_metric/y2002_zeroNDVI_15imgs.tif

gdalbuildvrt outfile.vrt $(ls geotifs/*.tif | grep ) 





