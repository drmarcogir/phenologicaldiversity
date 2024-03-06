# export bioclim variables calculated from ERA5land data

for i in $(docker container ls -aq);do
   docker start $i
done


yearl=$(seq 2004 2020)

for i in $yearl;do
echo $i
# pass environment year environment variable
docker exec --env year=$i --env biomin=0 --env biomax=3 marco_g bash -c "python3 /home/scripts/ERA5_landnew.py"
# pass environment year environment variable
docker exec --env year=$i --env biomin=3 --env biomax=7 marco_g12 bash -c "python3 /home/scripts/ERA5_landnew.py"
# pass environment year environment variable
docker exec --env year=$i --env biomin=7 --env biomax=11 papilio bash -c "python3 /home/scripts/ERA5_landnew.py"
# pass environment year environment variable
docker exec --env year=$i --env biomin=11 --env biomax=14 dawn bash -c "python3 /home/scripts/ERA5_landnew.py"
# pass environment year environment variable
docker exec --env year=$i --env biomin=14 --env biomax=17 dusk bash -c "python3 /home/scripts/ERA5_landnew.py"
# pass environment year environment variable
docker exec --env year=$i --env biomin=17 --env biomax=19 roller bash -c "python3 /home/scripts/ERA5_landnew.py"
done


