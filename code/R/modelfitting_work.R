# load required libraries
library(tidymodels);library(ranger)
library(DALEXtra);library(vip)
library(blockCV);library(data.table)
#  read in data time series data  -------------------------------------------------------------------------
#read_csv("/mnt/data1tb/alessandro_metric/finaldat.csv")->phenodat
# aggregate data at 0.25 degrees
#phenodat %>%
#  inner_join(read_csv("tmp/gridstats025degree",col_names = FALSE) %>%
#                           rename(cat = X1,cat025d=X2))->dd

#dd %>%
#  mutate(lvl2_tx = paste(lvl1_tx,lvl2_tx)) %>%
#  dplyr::select(-c(cat,lvl1_tx,lvl3_tx,lvl1_ct,lvl2_ct,forest_prop,count)) %>%
  
  
  
#  pivot_longer(cols=-c(cat025d),values_to = "values",names_to = "variables") %>%
#  group_by(cat025d,variables) %>%
#  summarise(values = mean(values)) %>%
#  pivot_wider(names_from = "variables",values_from = "values",id_cols = c("cat025d"))->phenodat1 



phenodat1[sample(1:dim(phenodat1)[1],5000),] %>%
  ungroup() %>%
 dplyr::select(!!c(paste0('bio_0',1:9),paste0('bio_',10:19),"y_2004","std_elev",
                                    "humi","aridity","cat025d"))->phenodat2

# read in spatial data ----------------------------------------------------
fread("/mnt/data1tb/alessandro_metric/final.csv") %>%
#fread("/mnt/data1tb/alessandro_metric/finaldat.csv") %>%
  group_by(cat) %>%
  summarise(value = median(value)) %>%
  inner_join(grid5km) %>%
  inner_join(read_csv("./tmp/forestcount",col_names = FALSE) %>%
               rename(cat = X1, count = X2) %>%
               mutate(forest_prop = count /max(count))) %>%
  # add topography data
  inner_join(read_csv("./tmp/slope",col_names = F) %>%
               rename(cat = X1,mean_slope = X2)) %>%
  inner_join(read_csv("./tmp/elev",col_names = F) %>%
               rename(cat = X1,mean_elev = X2)) %>%
  inner_join(read_csv("./tmp/std_elev",col_names = F) %>%
               rename(cat = X1,std_elev = X2)) %>%
  inner_join(read_csv("./tmp/std_slope",col_names = F) %>%
               rename(cat = X1,std_slope = X2)) %>%
  # add climate data
  inner_join(read_csv("./GRASSGIS_files/bioclim_sd.csv")) %>%
  #inner_join(read_csv("./GRASSGIS_files/pet_mean.csv")) %>%
  # add info for resampling in geographic space
  inner_join(read_csv("tmp/gridstats1degree",col_names = FALSE) %>%
               rename(cat = X1,cat1d=X2)) %>%
  inner_join(read_csv("tmp/gridstats025degree",col_names = FALSE) %>%
               rename(cat = X1,cat025d=X2)) %>%
  inner_join(read_csv("tmp/gridstats3degree",col_names = FALSE) %>%
               rename(cat = X1,cat3d=X2)) %>%
  inner_join(read_csv("tmp/gridstats2degree",col_names = FALSE) %>%
               rename(cat = X1,cat2d=X2)) %>%
  # calculate aridity and rescale temp seasonality
  #mutate(aridity = pet_mean/bio_12,bio_04 = bio_04/1000) %>%
  #mutate(bio_01 = bio_01/10) %>%
  # add population density
  inner_join(read_csv("tmp/humi",col_names = FALSE) %>%
               rename(cat = X1,humi=X2)) %>%
  # ecoregions
  inner_join(read_csv("./tmp/ecoregions.csv"))->phenodat

# create raster grid
raster(res=0.25)->myr
rasterize(world,myr) %>%
  rastertodf() %>%
  mutate(value = 1:length(value)) %>%
  rename(cat025d = value) %>% {.->>tmp} %>%
  mutate(x1 = x, y1= y) %>%
  st_as_sf(coords =c("x1","y1"),crs=latlon) %>%
  st_transform(crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs") %>% {.->>tmp} %>%
  st_coordinates() %>%
  as_tibble() %>%
  bind_cols(as_tibble(tmp) %>% rename(lon = x, lat =y) %>% dplyr::select(-c(geometry)))->g025
  
phenodat %>%
  filter(forest_prop > 0.85) %>%
  dplyr::select(-c(cat1d,forest_prop,count,cat3d,cat2d,WWF_MHTNUM,WWF_MHTNAM,x,y,cat)) %>%
  pivot_longer(cols=-c(cat025d),values_to = "values",names_to = "variables") %>%
  group_by(cat025d,variables) %>%
  summarise(values = mean(values)) %>%
  pivot_wider(names_from = "variables",values_from = "values",id_cols = c("cat025d")) %>%
  inner_join(g025)->phenodat1
  

# fit rf model and remove irrelevant features -------------------------------
form<-as.formula(c("value~",paste0(c(paste0('bio_0',1:9),paste0('bio_',10:19),
                   "std_elev","humi","aridity"),collapse="+")))
                 

       
mod<-ranger(form,data=phenodat1,importance = 'permutation')

# variable importance
tibble(variables = names(mod$variable.importance),imp = mod$variable.importance)->varimp
# correlation matrix
cor(phenodat2 %>% ungroup %>% dplyr::select(!!names(mod$variable.importance)))->cormat
cormat[upper.tri(cormat,diag = TRUE)]<- NA
# correlation matrix in long format
cormat %>%
  as.data.frame() %>%
  mutate(rows = row.names(.)) %>%
  pivot_longer(values_to = "value",names_to = "cols",cols = -c(rows)) %>%
  as_tibble() %>%
  drop_na() %>%
  mutate(value = abs(value))->cormat1
# loop through each variable combination and retain most important one of the
# highly correlated ones
res<-NULL

for(i in 1:dim(cormat1)[1]){
  print(i)
  if(cormat1[i,]$value < 0.7){
    next
  } else {
    cormat1[i,] %>%
      dplyr::select(1,3) %>%
      rename(value= value,variables = rows) %>%
      bind_rows(cormat1[i,] %>%
                  dplyr::select(2,3) %>%
                  rename(value = value,variables = cols)) %>%
      inner_join(varimp) %>%
      filter(imp==max(imp)) %>%
      dplyr::select(variables)->tmpres
    bind_rows(tmpres,res)->res
  }
} 

# get final dataset with selected predictors and response
phenodat1 %>% 
  ungroup() %>% 
  dplyr::select(!!c(res %>% unique() %>% pull(variables),"value","cat025d"))->phenodat2



# create spatial folds for cross-validation------------------------------------------
# convert tibble into sf object
phenodat4 %>%
  st_as_sf(coords=c("X","Y"))->phenodat5

# create indices for spatial cross-validation
sb <- spatialBlock(speciesData = phenodat5,
                   theRange = 1000000, # size of the blocks
                   k = 10,
                   selection = "random",
                   iteration = 100, # find evenly dispersed folds
                   xOffset = 0, # shift the blocks horizontally
                   yOffset = 0)
# convert sf object back into tibble
phenodat5 %>%
  as_tibble() %>%
  dplyr::select(-c(geometry))->phenodat6
# assign names to tibble list, analysis, assessment
for (i in 1:length(sb$folds)){
  names(sb$folds[[i]])<-c("analysis","assessment")
}
# create dataframe splits
splits <- lapply(sb$folds, make_splits, data = phenodat4)
# tibble containing cross-validation info
manual_rset(splits, c("Split 1", "Split 2","Split 3","Split 4",
                      "Split 5","Split 6","Split 7",
                      "Split 8","Split 9","Split 10"))->trees_folds


# model fine tuning -------------------------------------------------------
# create model recipe
mod<-ranger(value~bio_01+bio_12+bio_04+bio_15+humi+std_slope+PC1+PC2,data=phenodat4)


phenodat4 %>% 
  ungroup() %>% 
  dplyr::select(-c(cat025d)) %>%
  recipe(value~bio_01+bio_12+bio_04+bio_15+humi+std_slope+PC1+PC2)->phenodat1_rec
# estimate the required parameters from the training set that can
# be later applied to other datasets
phenodat1_prep <- prep(phenodat1_rec)
# applied operation specificied in recipe
juiced <- juice(phenodat1_prep)
# specifcy model type, engine etc.
tune_spec <- rand_forest(mtry = tune(),trees = 1000,min_n = tune()) %>%
  #set_args(importance = 'permutation') %>%
  set_mode("regression") %>%
  set_engine("ranger")
# create model workflow
tune_wf <- workflow() %>%
  add_recipe(phenodat1_rec) %>%
  add_model(tune_spec)


# model training (uses grid search): only tunes for hyperparameters
doParallel::registerDoParallel(cores = 7)
tune_res <- tune_grid(tune_wf,resamples = trees_folds,grid = 2)

rf_param <-
  tune_wf  %>%
  parameters() %>%
  update(mtry = mtry(range = c(1L, 10L)))

