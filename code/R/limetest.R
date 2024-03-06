library(lime)
library(tidyverse)
library(furrr)
library(ranger)

read_csv("./data/phenodat5_5km.csv") %>%
  sample_n(50000)->dat

mod<-ranger(value~bio_01+bio_12+bio_04+bio_15+humi+Htop+aridity
            +bio_01_mean+bio_04_mean+bio_15_mean,data = dat,importance = 'permutation')


dat %>%
  pivot_longer(values_to = "pred_value",names_to = "variable",-c(cat,WWF_MHTNAM,WWF_MHTNUM,
                                                                 x,y,cat025d,cat2d,cat3d,cat1d,value,
                                                                 count,forest_prop)) %>%
  group_by(cat2d,variable) %>%
  summarise(pred_value = mean(pred_value)) %>%
  pivot_wider(names_from = variable,values_from = pred_value) %>%
  ungroup() %>%
  dplyr::select(cat2d,bio_01,bio_12,bio_04,bio_15,humi,Htop,aridity,
                bio_01_mean,bio_04_mean,bio_15_mean)->dat1


dat %>% 
  dplyr::select(bio_01,bio_12,bio_04,bio_15,humi,Htop,aridity,
                bio_01_mean,bio_04_mean,bio_15_mean)->dat2

modexpl <- lime(dat2, mod, n_bins = 5)



lime_wrap<-function(inobs){
  explained <- lime::explain(
    x = inobs, 
    explainer = modexpl, 
    n_permutations = 5000,
    dist_fun = "gower",
    kernel_width = .75,
    n_features = 10,
    feature_select = "highest_weights")
  explained %>%
    dplyr::select(feature,feature_value,feature_weight)->res
  return(res)
}

dat1 %>%
  group_by(cat2d) %>%
  nest() %>%
  ungroup()->dd

plan(multisession, workers = 6)

dd %>%
  mutate(res = future_map(data,lime_wrap,
                          .options = furrr_options(seed = TRUE,
                                                   packages = c("lime", "dplyr", "furrr")
                          )))->limeres

dd %>%
  mutate(res = map(data,lime_wrap))->limeres
saveRDS(limeres,file='/eos/jeodpp/home/users/giramar/trends/results/limeres_Apr22.RDS')

