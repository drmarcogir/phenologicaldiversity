#docker run -it --name phenoml -v /home/marco/dockerdata:/home/scripts ubuntu

mkdir phenodat
cd phenodat
sudo apt update

# install xz utils
#sudo apt install xz-utils
# install wget
#sudo apt install wget

# update indices
chmod 400 phenoml.pem
#click on instance with mouse and check for connection details!
ssh -i "phenoml.pem" ubuntu@ec2-18-197-1-171.eu-central-1.compute.amazonaws.com

# install gcloud
echo "deb [signed-by=/usr/share/keyrings/cloud.google.gpg] https://packages.cloud.google.com/apt cloud-sdk main" | sudo tee -a /etc/apt/sources.list.d/google-cloud-sdk.list
sudo apt-get install apt-transport-https ca-certificates gnupg
curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | sudo apt-key --keyring /usr/share/keyrings/cloud.google.gpg add -
sudo apt-get update && sudo apt-get install google-cloud-sdk
gcloud init

#apt update -qq
# install two helper packages we need
#sudo apt install --no-install-recommends software-properties-common dirmngr
# add the signing key (by Michael Rutter) for these repos
# To verify key, run gpg --show-keys /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc 
# Fingerprint: 298A3A825C0D65DFD57CBB651716619E084DAB9
wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
# add the R 4.0 repo from CRAN -- adjust 'focal' to 'groovy' or 'bionic' as needed
sudo add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"

# install base R
sudo apt install --no-install-recommends r-base

# add repo for packages
sudo add-apt-repository ppa:c2d4u.team/c2d4u4.0+
# install tidyverse and ranger
sudo apt install --no-install-recommends r-cran-tidyverse
sudo apt install --no-install-recommends r-cran-ranger


# download data
gsutil -m cp gs://phenologyml/dat.tar.xz .
# unzip data
tar -xf dat.tar.xz


