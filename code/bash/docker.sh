# after install docker add user (marco in this case)
# give all users permission to run the docker sock
sudo chmod 666 /var/run/docker.sock

# pull official ubuntu image
docker pull ubuntu

# list containers and the time they were used
docker ps  -a

# start up a bash
docker run -it ubunpapilio@gmail.comtu bash

#docker run ubuntu bash -c "apt-get -y update" 
#docker run -it ubuntu bash

docker run -it ubuntu bash
apt install nginx

# docker image is something immutable, like a template a virtual environment. what you do is that you build a container on top of the image to run applications. a layer is written on top of the image, it's writeable, it is possible to install stuff etc. Explanation here: https://phoenixnap.com/kb/docker-image-vs-container

# remove all containers
docker container rm $(docker container ls -aq)
docker container rm de9034929c41


# start container interactively (once created)
docker start -ai ae106acb4599 
docker rename brave_thompson marco_g

# new container
docker run -it ubuntu bash
docker rename nifty_mendel marco_g12


docker start marco_g

##################################
# Step 1: pull official ubuntu image
###################################

docker pull ubuntu

############################
# Step 2: create new 
# containers with a shared 
# directory between containers
# and hostmissed
############################

# create container and share directory with host system (marco.girardello@gmail.com)
docker run -it --name marco_g -v /home/marco/dockerdata:/home/scripts ubuntu:latest
# create container and share directory with host system (marco.girardello12@gmail.com)
docker run -it --name marco_g12 -v /home/marco/dockerdata:/home/scripts ubuntu:latest
# create container and share directory with host system (curruspito.papilio@gmail.com)
docker run -it --name papilio -v /home/marco/dockerdata:/home/scripts ubuntu:latest
# create container and share directory with host system (simone.ruzza12@gmail.com)
docker run -it --name dawn -v /home/marco/dockerdata:/home/scripts ubuntu:latest
# create container and share directory with host system (curruspito.papilio1@gmail.com)
docker run -it --name dusk -v /home/marco/dockerdata:/home/scripts ubuntu:focal
# create container and share directory with host system (adharapv@gmail.com)
docker run -it --name roller -v /home/marco/dockerdata:/home/scripts ubuntu:focal

# create new containers after images were saved and copied over somewhere else
# create container and share directory with host system (marco.girardello@gmail.com)
#docker run -it --name marco_g -v /home/marco/dockerdata:/home/scripts ubuntu
# create container and share directory with host system (marco.girardello12@gmail.com)
#docker run -it --name marco_g12 -v /home/marco/dockerdata:/home/scripts ubuntu
# create container and share directory with host system (curruspito.papilio@gmail.com)
#docker run -it --name papilio -v /home/marco/dockerdata:/home/scripts geeimg_papilio
# create container and share directory with host system (simone.ruzza12@gmail.com)
#docker run -it --name dawn -v /home/marco/dockerdata:/home/scripts geeimg_dawn
# create container and share directory with host system (curruspito.papilio1@gmail.com)
#docker run -it --name dusk -v /home/marco/dockerdata:/home/scripts geeimg_dusk
# create container and share directory with host system (adharapv@gmail.com)
#docker run -it --name roller -v /home/marco/dockerdata:/home/scripts geeimg_roller

docker image pull ubuntu:foss

docker run -it --name testcontainer -v /home/marco/dockerdata:/home/scripts ubuntu:latest


###############################
# Step 3: set up GEE Python API
###############################

# within each docker environment: authentication needs to be 
# via a browser using the URL produced during the earthengine authenticate
#docker start -ai 23780635557e
apt update --yes
apt install python3 --yes
apt-get install python3-pip --yes
#pip3 install ee
pip3 install google-auth
pip3 install earthengine-api 
#pip3 install pandas 
pip3 install earthengine-api 
pip3 install earthengine-api --upgrade

earthengine authenticate 


# install jupyter lab
pip3 install jupyter
pip3 install jupyterlab


####################################################
# Step 4: start all the containers (non-interactively)
####################################################
for i in $(docker container ls -aq);do
   docker start $i
done


docker start -ai dawn


############################################################ 
# install gcloud in order to be able to authenticate on gee
###########################################################

apt-get install apt-transport-https ca-certificates gnupg --yes

echo "deb [signed-by=/usr/share/keyrings/cloud.google.gpg] https://packages.cloud.google.com/apt cloud-sdk main" | tee -a /etc/apt/sources.list.d/google-cloud-sdk.list

apt-get install curl --yes

curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key --keyring /usr/share/keyrings/cloud.google.gpg add -


curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key add -

curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | tee /usr/share/keyrings/cloud.google.gpg

apt-get update && apt-get install google-cloud-cli




######################################################
# Save a docker image
######################################################

docker commit 23780635557e geeimg_marco_g
docker save geeimg_marco_g > geeimg_marco_g.tar

docker commit f4a6a5752027 geeimg_marco_g12
docker save geeimg_marco_g12 > geeimg_marco_g12.tar

docker commit 761d61f13a18 geeimg_papilio
docker save geeimg_papilio > geeimg_papilio.tar

docker commit 224960866d3d geeimg_dawn
docker save geeimg_dawn > geeimg_dawn.tar

docker commit dbdcc08b18a4 geeimg_dusk
docker save geeimg_dusk > geeimg_dusk.tar

docker commit 6a5bd44f46d2 geeimg_roller
docker save geeimg_roller > geeimg_roller.tar


######################################################
# Fire up a jupyter notebook from a docker container
######################################################

# create container (specify port)
#docker run -it -p 8888:8888  --name testjupyter1 ubuntu


docker run -it -p 8888:8888 --name phenology -v /mnt/data1tb/Dropbox/phenology/scripts/notebooks:/home/scripts ubuntu

# fire up jupyter
jupyter lab --ip 0.0.0.0 --port 8888 --allow-root

#RUN useradd -s /bin/bash user
docker run -it --user marco -p 8888:8888 --name phenology1 -v /mnt/data1tb/Dropbox/phenology/scripts/notebooks:/home/scripts ubuntu



#########################################################
# Step 5: copy GEE code to shared folder (Guido's script)
#######################################################

#cp /mnt/data1tb/Dropbox/phenology/scripts/phenodiv.py /home/marco/dockerdata/


####################################################
# Step 6: run script and save results to gcloud 
# phenology project
####################################################
  
# pass year and  tile numbers 
# docker exec --env year=2002 --env minpoly=1 --env maxpoly=506 marco_g bash -c "python3 /home/scripts/phenodiv.py"
# pass environment year environment variable
# docker exec --env year=2002 --env minpoly=507 --env maxpoly=1013 marco_g12 bash -c "python3 /home/scripts/phenodiv.py"
# pass environment year environment variable
# docker exec --env year=2002 --env minpoly=1014 --env maxpoly=1520 papilio bash -c "python3 /home/scripts/phenodiv.py"
# pass environment year environment variable
# docker exec --env year=2002 --env minpoly=1521 --env maxpoly=2026 dawn bash -c "python3 /home/scripts/phenodiv.py"

# pass environment year environment variable
#docker exec --env year=2002 --env minpoly=1521 --env maxpoly=2026 dawn bash -c "python3 /home/scripts/phenodiv.py"


# delete running tasks
#docker exec marco_g /bin/bash -c "sh /home/scripts/deletetasks.sh"
# docker exec marco_g /bin/bash -c "sh /home/scripts/deletetasks.sh"
# docker exec papilio /bin/bash -c "sh /home/scripts/deletetasks.sh"
# docker exec dawn /bin/bash -c "sh /home/scripts/deletetasks.sh"

# --- use script with minimum


docker exec --env year=2003 --env minpoly=1 --env maxpoly=49 marco_g bash -c "python3 /home/scripts/8daymetric_terraclimateJun21.py"
# pass environment year environment variable
docker exec --env year=2003 --env minpoly=50 --env maxpoly=99 marco_g12 bash -c "python3 /home/scripts/8daymetric_terraclimateJun21.py"
# pass environment year environment variable
docker exec --env year=2003 --env minpoly=100 --env maxpoly=149 papilio bash -c "python3 /home/scripts/8daymetric_terraclimateJun21.py"
# pass environment year environment variable
docker exec --env year=2003 --env minpoly=150 --env maxpoly=199 dawn bash -c "python3 /home/scripts/8daymetric_terraclimateJun21.py"
# pass environment year environment variable
docker exec --env year=2003 --env minpoly=200 --env maxpoly=249 dusk bash -c "python3 /home/scripts/8daymetric_terraclimateJun21.py"
# pass environment year environment variable
docker exec --env year=2003 --env minpoly=249 --env maxpoly=304 roller bash -c "python3 /home/scripts/8daymetric_terraclimateJun21.py"



# pass environment year environment variable
docker exec marco_g12 bash -c "python3 /home/scripts/topography.py"
docker exec papilio bash -c "python3 /home/scripts/topography.py"


############################################


# pass environment year environment variable
docker exec --env year=2019 --env minpoly=1 --env maxpoly=30 marco_g bash -c "python3 /home/scripts/8daymetric_Aug21_median.py"
# pass environment year environment variable
docker exec --env year=2019 --env minpoly=31 --env maxpoly=61 marco_g12 bash -c "python3 /home/scripts/8daymetric_Aug21_median.py"
# pass environment year environment variable
docker exec --env year=2019 --env minpoly=62 --env maxpoly=92 papilio bash -c "python3 /home/scripts/8daymetric_Aug21_median.py"
# pass environment year environment variable
docker exec --env year=2019 --env minpoly=93 --env maxpoly=123 dawn bash -c "python3 /home/scripts/8daymetric_Aug21_median.py"
# pass environment year environment variable
docker exec --env year=2019 --env minpoly=124 --env maxpoly=154 dusk bash -c "python3 /home/scripts/8daymetric_Aug21_median.py"
# pass environment year environment variable
docker exec --env year=2019 --env minpoly=155 --env maxpoly=184 roller bash -c "python3 /home/scripts/8daymetric_Aug21_median.py"



# pass environment year environment variable
docker exec --env year=2020 --env minpoly=155 --env maxpoly=184 roller bash -c "python3 /home/scripts/8daymetric_Aug21_median.py"


#----------------------------------------------------

# pass environment year environment variable
docker exec --env mint=1 --env maxt=4 marco_g bash -c "python3 /home/scripts/functionalproperties.py"
# pass environment year environment variable
docker exec --env mint=5 --env maxt=9 marco_g12 bash -c "python3 /home/scripts/functionalproperties.py"
# pass environment year environment variable
docker exec --env mint=10 --env maxt=14 papilio bash -c "python3 /home/scripts/functionalproperties.py"
# pass environment year environment variable
docker exec --env mint=15 --env maxt=19 dawn bash -c "python3 /home/scripts/functionalproperties.py"
# pass environment year environment variable
docker exec --env mint=20 --env maxt=23 dusk bash -c "python3 /home/scripts/functionalproperties.py"


