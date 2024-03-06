# export bioclim variables calculated from ERA5land data

for i in $(docker container ls -aq);do
   docker start $i
done

docker exec --env year=2007 marco_g bash -c "python3 /home/scripts/terraclimate_bioclim.py"

# pass environment year environment variable
docker exec --env year=2004 marco_g bash -c "python3 /home/scripts/terraclimate_bioclim.py"
# pass environment year environment variable
docker exec --env year=2005 marco_g12 bash -c "python3 /home/scripts/terraclimate_bioclim.py"
# pass environment year environment variable
docker exec --env year=2006 papilio bash -c "python3 /home/scripts/terraclimate_bioclim.py"
# pass environment year environment variable
docker exec --env year=2007 dawn bash -c "python3 /home/scripts/terraclimate_bioclim.py"
# pass environment year environment variable
docker exec --env year=2008 dusk bash -c "python3 /home/scripts/terraclimate_bioclim.py"
# pass environment year environment variable
docker exec --env year=2009 roller bash -c "python3 /home/scripts/terraclimate_bioclim.py"
# pass environment year environment variable
docker exec --env year=2010 marco_g bash -c "python3 /home/scripts/terraclimate_bioclim.py"
# pass environment year environment variable
docker exec --env year=2011 marco_g12 bash -c "python3 /home/scripts/terraclimate_bioclim.py"
# pass environment year environment variable
docker exec --env year=2012 papilio bash -c "python3 /home/scripts/terraclimate_bioclim.py"
# pass environment year environment variable
docker exec --env year=2013 dawn bash -c "python3 /home/scripts/terraclimate_bioclim.py"
# pass environment year environment variable
docker exec --env year=2014 dusk bash -c "python3 /home/scripts/terraclimate_bioclim.py"
# pass environment year environment variable
docker exec --env year=2015 roller bash -c "python3 /home/scripts/terraclimate_bioclim.py"
# pass environment year environment variable
docker exec --env year=2016 marco_g bash -c "python3 /home/scripts/terraclimate_bioclim.py"
# pass environment year environment variable
docker exec --env year=2017 marco_g12 bash -c "python3 /home/scripts/terraclimate_bioclim.py"
# pass environment year environment variable
docker exec --env year=2018 papilio bash -c "python3 /home/scripts/terraclimate_bioclim.py"
# pass environment year environment variable
docker exec --env year=2019 dawn bash -c "python3 /home/scripts/terraclimate_bioclim.py"
# pass environment year environment variable
docker exec --env year=2020 dusk bash -c "python3 /home/scripts/terraclimate_bioclim.py"


docker exec --env year=2013 marco_g bash -c "python3 /home/scripts/terraclimate_bioclim.py"
docker exec --env year=2019 marco_g12 bash -c "python3 /home/scripts/terraclimate_bioclim.py"


