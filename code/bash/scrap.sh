# delete all files within bucket
gsutil rm "gs://marco_g_assets/*"

# gsutil
gsutil cp /mnt/data1tb/Dropbox/phenology/gridGEE/fine_gridv1.dbf "gs://marco_g_assets/"
gsutil cp /mnt/data1tb/Dropbox/phenology/gridGEE/fine_gridv1.prj "gs://marco_g_assets/"
gsutil cp /mnt/data1tb/Dropbox/phenology/gridGEE/fine_gridv1.shp "gs://marco_g_assets/"
gsutil cp /mnt/data1tb/Dropbox/phenology/gridGEE/fine_gridv1.shx "gs://marco_g_assets/"

# use earthengine command to upload to assets
earthengine ls users/marcogirardello/phenoutils

earthengine upload table --asset_id=users/marcogirardello/phenoutils/grid_export_phenology3 gs://marco_g_assets/fine_gridv1.shp
earthengine acl set public users/marcogirardello/phenoutils/grid_export_phenology3
