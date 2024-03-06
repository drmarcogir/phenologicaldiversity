library(tidyverse);library(ranger)
library(pdp);library(parallel)
library(doParallel)

read_csv("/home/marco/Dropbox/phenology/data/phenodat5_5km.csv") %>%
  slice(sample(1:427000,50000))->dat

mod<-ranger(value~bio_01+bio_12+bio_04+bio_15+humi+Htop+aridity
            +bio_01_mean+bio_04_mean+bio_15_mean,data = dat,
            importance = 'permutation',
            num.threads = 3)

predict(mod,dat,num.threads = 1)->test


cluster <- makeCluster(3) # convention to leave 1 core for OS
registerDoParallel(cluster)

p <- pdp::partial(mod, pred.var = c("bio_01","bio_12"), plot = FALSE,
                  parallel=FALSE,paropts = list(.packages = "ranger"),
                  trim.outliers = TRUE,chull = TRUE,num.threads = 3)

partial(cforest_adjusted, 
        pred.var = c("avg_mtg_duration", "avg_mtg_attd"), 
        trim.outliers = TRUE, chull = TRUE, parallel = TRUE,
        grid.resolution = 30,  paropts = list(.packages = "ranger",
                                              num.threads = 1))