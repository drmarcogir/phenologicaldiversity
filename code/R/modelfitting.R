# load required libraries -------------------------------------------------------------------------
library(tidymodels);library(ranger)
library(DALEXtra);library(vip)
library(blockCV);library(data.table)
library(phenoutils);library(marcoUtils)
library(tidyverse);library(sf)
library(raster);library(pdp)
library(ggcorrplot);library(vegan)
#  read in data spatial data  -------------------------------------------------------------------------
 #fread("/mnt/data1tb/alessandro_metric/final.csv") %>%
  fread("/home/marco/alessandro_metric/finaldat.csv") %>%
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
  inner_join(read_csv("./tmp/ecoregions.csv")) %>%
  inner_join(read_csv("./tmp/shannontop.csv") %>%
               rename(Htop= variable)) %>%
  # bioclim background climate
  inner_join(read_csv("./GRASSGIS_files/bioclim.csv") %>%
               dplyr::select(c(cat,bio_01,bio_12,bio_15,bio_04)) %>%
               rename(bio_01_mean = bio_01,bio_12_mean = bio_12,
                      bio_15_mean = bio_15,
                      bio_04_mean = bio_04)) %>%
  inner_join(read_csv("./GRASSGIS_files/pet_mean.csv")) %>%
  # calculate aridity and rescale temp seasonality
  mutate(aridity = pet_mean/bio_12,bio_04 = bio_04/1000) %>%
  mutate(bio_01 = bio_01/10)->phenodat


phenodat %>%
  filter(forest_prop > 0.75) %>%
  filter(value  < 180) %>%
  write_csv("./data/phenodat5_5km.csv")



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
  filter(forest_prop > 0.75) %>%
  dplyr::select(-c(cat1d,forest_prop,count,cat3d,cat2d,WWF_MHTNUM,WWF_MHTNAM,x,y,cat)) %>%
  pivot_longer(cols=-c(cat025d),values_to = "values",names_to = "variables") %>%
  group_by(cat025d,variables) %>%
  summarise(values = mean(values)) %>%
  pivot_wider(names_from = "variables",values_from = "values",id_cols = c("cat025d")) %>%
  inner_join(g025)->phenodat1


thin<-function(x){
  prop <- ceiling(dim(x)[1]*0.6)
  x %>%
    sample_n(size =prop)->tmpres
  return(tmpres)
}

phenodat1 %>%
  filter(value < 180)->phenodat3



# dissimilarity in forest types -------------------------------------------
disf<-function(x){
  x %>%
    dplyr::select(-c(cat)) %>%
    vegdist(method="bray")->vdist
  mean(vdist)
}

read_delim("/media/marco/marcodata19/validationmetrics/pheno_grid_ESAvalues2002",delim = " ",col_names = F) %>%
  rename(lc=X1,cat=X2,area=X3) %>%
  mutate(lc = paste0("lc_",lc)) %>%
  pivot_wider(names_from = lc,values_from=area,values_fill=0) %>%
  inner_join(read_csv("tmp/gridstats025degree",col_names = FALSE) %>%
               rename(cat = X1,cat025d=X2)) %>%
  inner_join(phenodat %>% filter(forest_prop > 0.75) %>% 
               filter(value < 180) %>% dplyr::select(cat)) %>% {.->>tmplc}%>%
  group_by(cat025d) %>%
  summarise(count = n())->lcdat


lcdat %>%
  filter(count > 1) %>%
  dplyr::select(-c(count)) %>%
  inner_join(tmplc) %>%
  group_by(cat025d) %>%
  nest() %>%
  mutate(res = map(data,disf)) %>%
  unnest(res) %>%
  rename(dis = res) %>%
  dplyr::select(cat025d,dis) %>%
  write_csv("/home/marco/Desktop/diss.csv")

read_csv("/home/marco/Desktop/diss.csv") %>%
  inner_join(phenodat3)->phenodat5

phenodat5 %>%
  rename(disb=dis) %>%
  #inner_join(phenodatg  %>%
  #             rename(disg=dis) %>%
  #             dplyr::select(cat025d,disg)) %>%
  # this contains both bray curtis and gower indices!
  write_csv(file = "./data/phenodat5.csv")

# correlation matrix ------------------------------------------------------
phenodat5 %>%
  ungroup() %>%
  dplyr::select("bio_01","bio_12","bio_04","bio_15","humi","disb") %>%
  cor()-> cormat
 
# do it with cart

#to_remove<- findCorrelation(cormat, cutoff = 0.7)
#corr.matrix_2 <- cor(tmp[,to_remove], method = "pearson",  use = "pairwise.complete.obs")


ggcorrplot(cormat,
           hc.order = TRUE,
           type = "lower",
           lab = TRUE)+theme(axis.text = element_text(colour = "black"))->corp
  
ggsave(corp,filename = "./figures/supplementary/cormatrix.png",
       width = 8,height = 8,dpi = 400,bg = "white")
# This figure will have to be recreated, without bio_07 and with proper labels!


# full rf model -----------------------------------------------------------
read_csv("./data/phenodat5.csv") %>%
  mutate(bio_04 = bio_04/10)->phenodat5


mod<-ranger(value~bio_01+bio_12+bio_04+bio_15+humi+Htop+bio_01_mean+bio_12_mean+bio_04_mean+bio_15_mean,
            data=phenodat5,importance = 'permutation')

#p1 <- partial(mod, pred.var = c("bio_01", "bio_12"), plot = TRUE, chull = TRUE)

# variable importance
tibble(value = mod$variable.importance,variable = names(mod$variable.importance)) %>%
  arrange(desc(value)) %>%
  write_csv("./rfresults/rf_importance.csv")


# partial dependence plots ------------------------------------------------

varl<-c("bio_01","bio_12","bio_04","bio_15","humi","disb","Htop")

res<-NULL
  
for(i in 1:length(varl)){
  print(i)
  p <- pdp::partial(mod, pred.var = varl[i], plot = TRUE,
              plot.engine = "ggplot2")
  as_tibble(p$data) %>%
    rename(pred = 1) %>%
    mutate(variable = varl[i])->tmpres
  bind_rows(tmpres,res)->res
}


res %>%
  ggplot(aes(x=pred,y=yhat))+theme_bw()+facet_wrap(~variable,scales = "free")+geom_line()->pdp1 

ggsave(pdp1,filename = "./figures/supplementary/pdp.png",width = 8,height = 7,dpi = 400)

# partial dependence plots for interactions
p1 <- partial(mod, pred.var = c("bio_01", "bio_12"),chull = TRUE)
write_csv(p1,file = "./pdp/p1.csv")
p2 <- partial(mod, pred.var = c("bio_15", "bio_04"),chull = TRUE)
write_csv(p2,file = "./pdp/p2.csv")
p3 <- partial(mod, pred.var = c("disb", "Htop"),chull = TRUE)
write_csv(p3,file = "./pdp/p3.csv")
p4 <- partial(mod, pred.var = c("disb", "bio_01"),chull = TRUE)
write_csv(p4,file = "./pdp/p4.csv")
p5 <- partial(mod, pred.var = c("disb", "bio_12"),chull = TRUE)
write_csv(p5,file = "./pdp/p5.csv")
p6 <- partial(mod, pred.var = c("Htop", "bio_01"),chull = TRUE)
write_csv(p6,file = "./pdp/p6.csv")
p7 <- partial(mod, pred.var = c("Htop", "bio_12"),chull = TRUE)
write_csv(p7,file = "./pdp/p7.csv")


# scatterplots ------------------------------------------------------------
phenodat4 %>%
  #filter(bio_04 > 25) %>%
  #mutate(value = log(value+1),bio_04 = log(bio_04+1)) %>%
  #filter(value > 2.5) %>%
  #filter(bio_04 < 150) %>%
  ggplot(aes(x=PC2,y=value))+
  #geom_smooth(method="gam",se=FALSE,color="red")
  geom_bin2d(bins=60)+geom_smooth(method="gam",se=FALSE,color="red")+
  scale_fill_viridis(option = "B",trans ="log")
#scale_fill_gradient(low="lightblue1",high="darkblue",trans="log10")+
  
  
# spatial autocorrelation -------------------------------------------------


phenodat5 %>%
  mutate(residuals = value-predvalues$predictions) %>%
  dplyr::select(lon,lat,residuals) %>%
  rasterFromXYZ(crs =latlon) %>%
  projectRaster(crs = ml,res = 25000)->resrast

#coordinates(df)<-~X+Y
#variogram(residuals~1, df)->dfvar

# block size for spatial CV -------------------------------------------------------------------
# run the model in parallel
resrast<-raster("./jeodpp_results/resrast.tif")
range1 <- spatialAutoRange(rasterLayer = resrast,
                           sampleNumber = 40000, # number of cells to be used
                           doParallel = TRUE,
                           nCores = 7, # if NULL, it uses half of the CPU cores
                           plotVariograms = FALSE,
                           showPlots = TRUE)
# correct range 1511865

# create spatial folds for cross-validation------------------------------------------
# convert tibble into sf object
phenodat5 %>%
  dplyr::select(-c(X,Y)) %>%
  st_as_sf(coords=c("lon","lat"),crs=latlon) %>%
  st_transform(crs=ml)->phenodat5sf
  

  #st_as_sf(coords=c("X","Y"))->phenodat5

# create indices for spatial cross-validation
sb <- spatialBlock(speciesData = phenodat5sf,
                   theRange = 1511865, # size of the blocks
                   k = 10,
                   selection = "random",
                   iteration = 100, # find evenly dispersed folds
                   xOffset = 0, # shift the blocks horizontally
                   yOffset = 0)


# fit random forest models, get importance and R2


R2<-NULL
imp<-NULL


for(i in 1:10){
  print(i)
  phenodat5[sb$folds[[i]][[1]],]->train
  phenodat5[sb$folds[[i]][[2]],]->test
  mod<-ranger(value~bio_01+bio_12+bio_04+bio_15+humi+Htop+disb,data=train,
              importance = 'permutation')
  tibble(value = mod$variable.importance,variable = names(mod$variable.importance)) %>%
    arrange(desc(value))->tmpimp
  bind_rows(tmpimp,imp)->imp
  predict(mod,test)->pred
  tibble(R2 = cor(pred$predictions,test$value),it=i)->tmpR2
  bind_rows(tmpR2,R2)->R2
}

myres %>%
  pull(R2) %>%
  mean()

imp %>%
  group_by(variable) %>%
  summarise(value = mean(value)) %>%
  arrange(desc(value))



# convert sf object back into tibble
#phenodat5sf %>%
#  as_tibble() %>%
#  dplyr::select(-c(geometry))->phenodat6

# assign names to tibble list, analysis, assessment
for (i in 1:length(sb$folds)){
  names(sb$folds[[i]])<-c("analysis","assessment")
}







length(sb$folds[[1]][[1]])
length(sb$folds[[1]][[2]])

# create dataframe splits
splits <- lapply(sb$folds, make_splits, data = phenodat4)
# tibble containing cross-validation info
manual_rset(splits, c("Split 1", "Split 2","Split 3","Split 4",
                      "Split 5","Split 6","Split 7",
                      "Split 8","Split 9","Split 10"))->trees_folds




# model fine tuning -------------------------------------------------------
# create model recipe
phenodat4 %>% 
  ungroup() %>% 
  dplyr::select(-c(cat025d)) %>%
  recipe(value~bio_01+bio_12+bio_04+bio_15+bio_03+
           bio_05+bio_06+bio_07+bio_08+bio_09+bio_10+
           bio_11+bio_13+humi+Htop+PC1+PC2) %>%
  prep()->phenodat1_rec
# step 1: recipe, specify model formula, standardise variables etc.
# step 2: see prep function, estimates the required quantities 
# and statistics needed by any operations (e.g. data normalisation)

# baking the recipe. This applies the preps to new data. If new data 
# not available one can specify new_data = NULL, as below
#bake(phenodat1_rec,new_data = NULL)

# applied operation specificied in recipe
juiced <- juice(phenodat1_rec)
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
doParallel::registerDoParallel(cores = 6)
system.time(tune_res <- tune_grid(tune_wf,resamples = trees_folds,grid = 2))

tune_res1<-tune_res %>%
  dplyr::select(-c(splits))

saveRDS(tune_res1,file ="./results_tmp/tune_res.RDS")



readRDS("./results_tmp/tune_res.RDS")

tune_res %>%
  collect_metrics() 

tune_res %>%
  unnest(.metrics) %>%
  filter(.metric=="rsq")

best_rsq <- select_best(tune_res, "rsq")
final_rf <- finalize_model(
  tune_spec,
  best_rsq
)

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(value ~ .,
      data = juiced
  ) %>%
  vip(geom = "point")




rf_param <-
  tune_wf  %>%
  parameters() %>%
  update(mtry = mtry(range = c(1L, 10L)))


# plot final results -------------------------------------------------------
# plot model performance. this plot gives an idea of the best hyperparameters 
tune_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  dplyr::select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "paramseter") %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y ="RSME")

# variable importance plot
imp$fit$variable.importance %>%
  as_tibble() %>%
  mutate(variables = names(imp$fit$variable.importance)) %>%
  inner_join(labs) %>%
  arrange(value) %>%
  mutate(variables_renamed = forcats::as_factor(variables_renamed)) %>% {.->>tmp} %>%
  ggplot()+geom_bar(aes(x=variables_renamed,y=value,fill=group),stat="identity")+coord_flip()+theme_minimal()+
  xlab("Variable")+ylab("Increase in MSE")+theme(axis.text=element_text(colour = "black",size=12),
                                                 axis.title = element_text(size=19,colour="black"),
  )+ylim(0,600)+
  scale_fill_manual(values=c("black","deepskyblue3","brown3","dark green"))->varimprf
ggsave(varimprf,filename = "./figures/July21/varimprf_tidymodels.png",width = 12,height=9)






doParallel::registerDoParallel(cores = 7)
tune_res <- tune_grid(tune_wf,resamples = trees_folds,grid = 2)

# fit rf model and remove irrelevant features -------------------------------
form<-as.formula(c("y_2004~",paste0(c(paste0('bio_0',1:9),paste0('bio_',10:19),
                                      "std_elev","humi","aridity"),collapse="+")))

mod<-ranger(form,data=phenodat2,importance = 'permutation')

# variable importance
tibble(variables = names(mod$variable.importance),imp = mod$variable.importance)->varimp
# correlation matrix
cor(phenodat2 %>% dplyr::select(-c(y_2004)))->cormat
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
phenodat2 %>% 
  ungroup() %>% 
  dplyr::select(!!c(res %>% unique() %>% pull(variables),"y_2004","cat025d"))->phenodat3




# test linear model -------------------------------------------------------



# ice curves --------------------------------------------------------------

library(doParallel)
registerDoParallel(cores=4)
ice_bio_01<-pdp::partial(mod,pred.var="bio_01",ice=TRUE,parallel=TRUE)
ice_bio_12<-pdp::partial(mod,pred.var="bio_12",ice=TRUE,parallel=TRUE)
ice_bio_04<-pdp::partial(mod,pred.var="bio_04",ice=TRUE,parallel=TRUE)
ice_bio_15<-pdp::partial(mod,pred.var="bio_15",ice=TRUE,parallel=TRUE)
ice_humi<-pdp::partial(mod,pred.var="humi",ice=TRUE,parallel=TRUE)
ice_Htop<-pdp::partial(mod,pred.var="Htop",ice=TRUE,parallel=TRUE)
ice_disb<-pdp::partial(mod,pred.var="disb",ice=TRUE,parallel=TRUE)


ice_bio_01 %>%
  as_tibble() %>%
  rename(variable = bio_01) %>%
  mutate(varname = "bio_01") %>%
  bind_rows(ice_bio_12 %>%
              as_tibble() %>%
              rename(variable = bio_12) %>%
              mutate(varname = "bio_12")) %>%
  bind_rows(ice_bio_04 %>%
              as_tibble() %>%
              rename(variable = bio_04) %>%
              mutate(varname = "bio_04")) %>%
  bind_rows(ice_bio_15 %>%
              as_tibble() %>%
              rename(variable = bio_15) %>%
              mutate(varname = "bio_15")) %>%
  bind_rows(ice_humi %>%
              as_tibble() %>%
              rename(variable = humi) %>%
              mutate(varname = "humi")) %>%
  bind_rows(ice_Htop %>%
              as_tibble() %>%
              rename(variable = Htop) %>%
              mutate(varname = "Htop")) %>%
  bind_rows(ice_disb %>%
              as_tibble() %>%
              rename(variable = disb) %>%
              mutate(varname = "disb")) %>%
  write_csv("./jeodppfiles/ice_curves.csv")

