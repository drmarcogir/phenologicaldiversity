earthengine ls users/marcogirardello/phenofixfailedexports > info

for i in $(cat info| grep projects);do
     tmpasset=$(echo $i | sed "s/projects\/earthengine-legacy\/assets\///")
     #echo $tmpasset
     earthengine rm $tmpasset
done


earthengine ls users/marcogirardello/phenofixfailedexports > info


for i in $(cat info| grep projects);do
     tmpasset=$(echo $i | sed "s/projects\/earthengine-legacy\/assets\///")
     #echo $tmpasset
     earthengine acl set public $tmpasset
done

gsutil -m rm gs://marco_g_assets/*

gsutil cp /mnt/data1tb/Dropbox/phenology/tmp/missing_2003.shp gs://marco_g_assets/
gsutil cp /mnt/data1tb/Dropbox/phenology/tmp/missing_2003.shx gs://marco_g_assets/
gsutil cp /mnt/data1tb/Dropbox/phenology/tmp/missing_2003.prj gs://marco_g_assets/
gsutil cp /mnt/data1tb/Dropbox/phenology/tmp/missing_2003.dbf gs://marco_g_assets/

earthengine upload table --asset_id=users/marcogirardello/phenofixfailedexports/missing2003 gs://marco_g_assets/missing_2003.shp

earthengine acl set public users/marcogirardello/phenofixfailedexports/missing2003







