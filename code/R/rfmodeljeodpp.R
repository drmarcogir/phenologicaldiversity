# load required libraries -------------------------------------------------
library(ranger);library(tidyverse)
library(pdp);library(raster)

latlon<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
ml<-"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

# read in data ------------------------------------------------------------
read_csv("./data/phenodat5_5km.csv")->dat
# fit full model ---------------------------------------------------------
mod<-ranger(value~bio_01+bio_12+bio_04+bio_15+humi+Htop
            +bio_01_mean+bio_12_mean+bio_04_mean+bio_15_mean,data = dat,importance = 'permutation')

# post-processing of model ------------------------------------------------
# get variable importance
tibble(value = mod$variable.importance,varname = names(mod$variable.importance)) %>%
  arrange(desc(value)) %>%
  write_csv("./results/varimp_5km.csv")

# get residuals
predict(mod,dat)->fitted

dat %>%
  mutate(residuals = value-fitted$predictions) %>%
  dplyr::select(x,y,residuals) %>%
  rasterFromXYZ(crs =latlon) %>%
  projectRaster(crs = ml,res = 5000) %>%
  writeRaster("./results/resrast.tif")


# save model object
saveRDS(mod,file = "./results/mod_5km.RDS")


# univariate partial dependence plots -------------------------------------

varl<-c("bio_01","bio_12","bio_04","bio_15","humi","Htop","bio_01_mean","bio_12_mean","bio_15_mean","bio_04_mean")

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

write_csv(res,file = "./results/univariate_pdp.csv")


# two-way interactions partial dependence plots ---------------------------
library(doParallel)
registerDoParallel(cores=15)

tibble(var1 = c("bio_01","bio_15","Htop","bio_01_mean","bio_04_mean"),
       var2=c("bio_12","bio_04","humi","bio_12_mean","bio_15_mean"))->intl

pd2dres<-NULL

for(i in 1:5){
  intl[i,] %>% unlist(., use.names=FALSE)->tmpv
  pd2d <- partial(mod, pred.var = tmpv,chull = TRUE,parallel = TRUE)
  intl[i,] %>% mutate(pd2d = list(pd2d))-> tmpres
  bind_rows(tmpres,pd2dres)->pd2dres
}

saveRDS(pd2dres,file = "./results/pd2dres.RDS")



# spatial cross validation ------------------------------------------------
readRDS("./data/spatial_folds.RDS")->sb

# fit random forest models, get importance and R2

R2<-NULL
imp<-NULL

as.data.frame(dat)->dat


for(i in 1:10){
  print(i)
  dat[sb[[i]][[1]],]->train
  dat[sb[[i]][[2]],]->test
  mod<-ranger(value~bio_01+bio_12+bio_04+bio_15+humi+Htop+bio_01_mean+
                bio_12_mean+bio_04_mean+bio_15_mean,data=train,
              importance = 'permutation')
  tibble(value = mod$variable.importance,variable = names(mod$variable.importance)) %>%
    arrange(desc(value))->tmpimp
  bind_rows(tmpimp,imp)->imp
  predict(mod,test)->pred
  tibble(R2 = cor(pred$predictions,test$value),it=i)->tmpR2
  bind_rows(tmpR2,R2)->R2
}

dat[sb[[i]][[1]],]->train


sb[[i]][[1]]->rows

dat[sample(1:20,3),]

# individual conditional expectations -------------------------------------
library(doParallel)
registerDoParallel(cores=10)

varl<-c("bio_01","bio_12","bio_04","bio_15","humi","Htop","bio_01_mean","bio_12_mean","bio_15_mean","bio_04_mean")

iceres<-NULL

for(i in 1:length(varl)){
  print(i)
  icetmp <- pdp::partial(mod, pred.var = varl[i], plot =FALSE ,ice=TRUE,parallel=TRUE)
  icetmp <- icetmp %>% as_tibble() %>% rename(predictor = 1)
  tibble(variable=varl[i],ice = list(icetmp))->tmpres
  bind_rows(tmpres,iceres)->iceres
}


saveRDS(iceres,file='./results/iceres.RDS')

#write_csv(iceres,file = "./results/iceres.csv")

mod_fit<-function(x){
  mod<-lm(yhat~predictor,data=x)
  broom::tidy(mod) %>%
    filter(term=="predictor") %>%
    dplyr::select(estimate)->tmpres
  return(tmpres)
}

iceres %>%
  unnest(cols=c(ice)) %>%
  group_by(variable,yhat.id) %>%
  nest()->iceres1


plan(multisession, workers = 20)

iceres1 %>%
  ungroup() %>%
  mutate(res = future_map(data,mod_fit))->iceres2

