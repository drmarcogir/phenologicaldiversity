# Google Cloud stuff. Uses bash tool gcloud to add users to my project called phenology_5km

# login with browser
gcloud auth login

# add GEE USERS (e.g. marco.girardello12@gmail.com) 4 in total
gcloud projects add-iam-policy-binding phenology-303215 --member="user:marco.girardello12@gmail.com" --role="roles/editor"


##################################
# Step 1: pull official ubuntu image
###################################

docker pull ubuntu

############################
# Step 2: create new 
# containers with a shared 
# directory between containers
# and host
############################

# create container and share directory with host system 
docker run -it --name marco_g -v /home/marco/dockerdata:/home/scripts ubuntu
# create container and share directory with host system 
docker run -it --name marco_g12 -v /home/marco/dockerdata:/home/scripts ubuntu
# create container and share directory with host system 
docker run -it --name papilio -v /home/marco/dockerdata:/home/scripts ubuntu
# create container and share directory with host system 
docker run -it --name dawn -v /home/marco/dockerdata:/home/scripts ubuntu


###############################
# Step 3: set up GEE Python API
###############################

# within each docker environment: authentication needs to be 
# via a browser using the URL produced when doing earthengine authenticate
docker start -ai 224960866d3d
apt update
apt install python3
apt-get install python3-pip
pip3 install ee
pip3 install earthengine-api
earthengine authenticate 

####################################################
# Step 4: start all the containers (non-interactively)
####################################################
for i in $(docker container ls -aq);do
   docker start $i
done


#########################################################
# Step 5: copy GEE code to shared folder (Guido's script)
#######################################################


cp /mnt/data1tb/Dropbox/phenology/scripts/phenodiv.py /home/marco/dockerdata/


####################################################
# Step 6: run script and save results to gcloud 
# phenology project
####################################################

# pass year and  tile numbers 
docker exec --env year=2001 --env minpoly=1 --env maxpoly=506 marco_g bash -c "python3 /home/scripts/phenodiv.py"
# pass environment year environment variable
docker exec --env year=2001 --env minpoly=507 --env maxpoly=1013 marco_g12 bash -c "python3 /home/scripts/phenodiv.py"
# pass environment year environment variable
docker exec --env year=2001 --env minpoly=1014 --env maxpoly=1520 papilio bash -c "python3 /home/scripts/phenodiv.py"
# pass environment year environment variable
docker exec --env year=2001 --env minpoly=1521 --env maxpoly=2026 dawn bash -c "python3 /home/scripts/phenodiv.py"

                
