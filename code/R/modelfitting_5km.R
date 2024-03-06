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
  # calculate aridity and rescale temp seasonality (mean!)
  mutate(aridity = pet_mean/bio_12_mean,bio_04 = bio_04/1000) %>%
  mutate(bio_01 = bio_01/10) %>%
  # # add seasonality in aridity (mean values)
  # inner_join(read_csv("./GRASSGIS_files/aridity_seas_mean.csv") %>%
  # dplyr::select(-c(varname)) %>%
  # rename(aridity_seas_mean = variable)) %>%
  # add seasonality in aridity (standard deviation)
  # inner_join(read_csv("./GRASSGIS_files/aridity_seas_sd.csv") %>%
  #            dplyr::select(-c(varname)) %>%
  #            rename(aridity_seas_sd = variable)) %>%
# add seasonality in aridity (mean values)
inner_join(read_csv("./GRASSGIS_files/aridity_annualaverage_sd.csv") %>%
             dplyr::select(-c(varname)) %>%
             rename(aridity_sd = variable)) %>%
  # add seasonality in aridity (standard deviation)
  inner_join(read_csv("./GRASSGIS_files/aridity_annualaverage_mean.csv") %>%
               dplyr::select(-c(varname)) %>%
               rename(aridity_mean = variable))->phenodat


phenodat %>%
  filter(aridity_mean > (quantile(aridity_mean,probs=c(0.01))) & aridity_mean < (quantile(aridity_mean,probs=c(0.99)))) %>%
  filter(aridity_sd > (quantile(aridity_sd,probs=c(0.01))) & aridity_sd < (quantile(aridity_sd,probs=c(0.99)))) %>%
  filter(forest_prop > 0.75) %>%
  filter(value  < 180) %>%
  write_csv("./data/phenodat5_5km.csv")


# full rf model -----------------------------------------------------------
mod<-ranger(value~bio_01+bio_12+bio_04+bio_15+humi+Htop
            +bio_01_mean+bio_12_mean+bio_04_mean+bio_15_mean,data = dat,importance = 'permutation')


# get variable importance -------------------------------------------------------------------------
tibble(value = mod$variable.importance,varname = names(mod$variable.importance)) %>%
  arrange(desc(value)) %>%
  write_csv("./results/varimp_5km.csv")


# get residuals -------------------------------------------------------------------------
predict(mod,dat)->fitted
dat %>%
  mutate(residuals = value-fitted$predictions) %>%
  dplyr::select(x,y,residuals) %>%
  rasterFromXYZ(crs =latlon) %>%
  projectRaster(crs = ml,res = 5000) %>%
  writeRaster("./results/resrast.tif")

# univariate partial dependence plots -------------------------------------------------------------------------
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


# partial dependence plots for interactions -----------------------------------------------------------------------
p1 <- partial(mod, pred.var = c("bio_01", "bio_12"),chull = TRUE)
write_csv(p1,file = "./pdp/p1.csv")
p2 <- partial(mod, pred.var = c("bio_15", "bio_04"),chull = TRUE)
write_csv(p2,file = "./pdp/p2.csv")
#p3 <- partial(mod, pred.var = c("disb", "Htop"),chull = TRUE)
#write_csv(p3,file = "./pdp/p3.csv")
#p4 <- partial(mod, pred.var = c("disb", "bio_01"),chull = TRUE)
#write_csv(p4,file = "./pdp/p4.csv")
#p5 <- partial(mod, pred.var = c("disb", "bio_12"),chull = TRUE)
#write_csv(p5,file = "./pdp/p5.csv")
p6 <- partial(mod, pred.var = c("Htop", "bio_01"),chull = TRUE)
write_csv(p6,file = "./pdp/p6.csv")
p7 <- partial(mod, pred.var = c("Htop", "bio_12"),chull = TRUE)
write_csv(p7,file = "./pdp/p7.csv")
p8 <- partial(mod, pred.var = c("bio_01_mean", "bio_12_mean"),chull = TRUE)
write_csv(p8,file = "./pdp/p8.csv")
p9 <- partial(mod, pred.var = c("bio_04_mean", "bio_15_mean"),chull = TRUE)
write_csv(p9,file = "./pdp/p9.csv")




# block size for spatial CV -------------------------------------------------------------------
# chooses optimal bin size
resrast<-raster("./jeodpp_results/resrast.tif")
range1 <- spatialAutoRange(rasterLayer = resrast,
                           sampleNumber = 300000, # number of cells to be used
                           doParallel = TRUE,
                           nCores = 7, # if NULL, it uses half of the CPU cores
                           plotVariograms = FALSE,
                           showPlots = TRUE)

# convert tibble into sf object
phenodat %>%
  mutate(x1=x,y1=y) %>%
  st_as_sf(coords=c("x1","y1"),crs=latlon) %>%
  st_transform(crs=ml)->phenodatsf


#st_as_sf(coords=c("X","Y"))->phenodat5

# create indices for spatial cross-validation
sb <- spatialBlock(speciesData = phenodatsf,
                   theRange = 1289205, # size of the blocks
                   k = 10,
                   selection = "random",
                   iteration = 100, # find evenly dispersed folds
                   xOffset = 0, # shift the blocks horizontally
                   yOffset = 0)


saveRDS(sb$folds,file="./jeodppfiles/spatial_folds.RDS")

readRDS("./jeodppfiles/foldsJune22.RDS")->sb
# read in data
read_csv("./jeodppfiles/phenodat5_5km.csv") %>%
  filter(bio_12_mean < 4000) %>%
  filter(!is.infinite(aridity))->phenodat
  
# fit random forest models, get importance and R2

R2<-NULL
imp<-NULL


for(i in 1:10){
  print(i)
  phenodat[sb[[i]][[1]],]->train
  phenodat[sb[[i]][[2]],]->test
  mod<-ranger(value~bio_01+bio_12+bio_04+bio_15+humi+Htop+aridity
              +bio_01_mean+bio_04_mean+bio_15_mean,data=train,
              importance = "permutation",num.threads = 15)
  tibble(value = mod$variable.importance,variable = names(mod$variable.importance)) %>%
    arrange(desc(value))->tmpimp
  bind_rows(tmpimp,imp)->imp
  predict(mod,test)->pred
  tibble(R2 = cor(pred$predictions,test$value),it=i)->tmpR2
  bind_rows(tmpR2,R2)->R2
}

phenodat5[sb$folds[[i]][[1]],]->train


# correlation matrix ------------------------------------------------------

read_csv("./data/phenodat5_5km.csv")->phenodat

phenodat %>%
  ungroup() %>%
  dplyr::select("bio_01","bio_12","bio_04","bio_15","humi","Htop",
                "bio_01_mean","bio_04_mean","bio_15_mean"
                ,"aridity","cat") %>%
  pivot_longer(names_to = "varname",values_to = "values",cols=-c(cat)) %>%
  inner_join(predictors) %>%
  dplyr::select(-c(varname,col)) %>%
  pivot_wider(names_from = label,values_from = values) %>%
  dplyr::select(-c(cat)) %>%
  cor()-> cormat




# do it with cart

#to_remove<- findCorrelation(cormat, cutoff = 0.7)
#corr.matrix_2 <- cor(tmp[,to_remove], method = "pearson",  use = "pairwise.complete.obs")


ggcorrplot(cormat,
           hc.order = TRUE,
           type = "lower",
           lab = TRUE)+theme(axis.text = element_text(colour = "black"))->corp

ggsave(corp,filename = "./figures/supplementary/Fig3/cormatrix_aridity_Mar22.png",
       width = 8,height = 8,dpi = 400,bg = "white")




phenodat %>%
  dplyr::select(value,aridity) %>%
  filter(aridity < 0.3)->dd1

mod<-gam(value~s(aridity),data=dd1)

read_csv("./data/phenodat5_5km.csv") %>%
  ggplot(aes(x=bio_01_mean,y=value))+theme_minimal()+geom_bin2d(bins=60)+geom_smooth(method="lm",se=FALSE,color="red")+
  scale_fill_gradient(low="lightblue1",high="darkblue",trans="log10")


phenodat1 %>%

  ggplot(aes(x=values,y=y_2004))+theme_minimal()+geom_bin2d(bins=60)+geom_smooth(method="lm",se=FALSE,color="red")+
  scale_fill_gradient(low="lightblue1",high="darkblue",trans="log10")+xlab("Predictor")+ylab("Pheno diversity")+
  theme(legend.position = "none",axis.text = element_text(size=12,colour="black"),axis.title = element_text(size=20),
        plot.title = element_text(hjust = 0.5,size=20))+facet_wrap(~variables_renamed)+theme(axis.text.x = element_text(angle = 30,hjust = 1))->uniscatter


read_csv("./data/phenodat5_5km.csv") %>%
  filter(WWF_MHTNAM=="Boreal Forests/Taiga")->dat1

mod<-bam(value~s(bio_01_mean),data=dat1)


mod<-lm(crim~.,data=Boston)
explain_1 <- broken(mod, Boston[11,])


