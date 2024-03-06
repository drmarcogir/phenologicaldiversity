# google cloud add users using gcloud (IAM)

# Project created via web browser: phenology_5km

# login with browser
gcloud auth login
gcloud config set account marco.girardello@gmail.com
gcloud config set project phenology-303215

# list projects by ids
gcloud projects list --sort-by=projectId --limit=5

# use project id to get files
gsutil ls gs://bucket/curruspito/

#gsutil kms authorize


# list grantable roles
gcloud iam list-grantable-roles //cloudresourcemanager.googleapis.com/projects/$phenology-303215

# add GEE USERS (e.g. marco.girardello12@gmail.com): other GEE/gmail accounts
gcloud projects add-iam-policy-binding phenology-303215 --member="user:curruspito.papilio@gmail.com" --role="roles/editor"
 

# copy entire folder !  
# gsutil -m cp \
#  "gs://phenology_5km/*.tif" \
#  .
  
gsutil -m cp  "gs://phenology_5km/*.tif" .
  

gsutil -m rm "gs://phenology_5km/*.tif" 
  
  
rm *latest*
ls | grep zeroversion | grep 1530
ls | grep zeroversion | grep 1531

rm Y_2002_1530_zeroversion.tif
rm Y_2002_1531_zeroversion_test1531_simplfied.tif
rm Y_2002_1531_zeroversion_test1531.tif
