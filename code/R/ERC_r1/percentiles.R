# load required libraries -------------------------------------------------
library(tidyverse);library(sf)
library(scales);library(patchwork)
library(RColorBrewer);library(viridis)

# read in phenology data
read_csv("./scripts/gregory/data/phenodat.csv")->phenodat


phenodat %>%
  #filter(forest_prop > 0.75) %>%
  #filter(value < 180) %>%
  dplyr::select(x,y,value,WWF_MHTNAM) %>%
  inner_join(tibble(WWF_MHTNAM = c("Tundra","Tropical and Subtropical Dry Broadleaf Forests",
                                   "Tropical and Subtropical Grasslands, Savannas and Shrublands",
                                   "Temperate Broadleaf and Mixed Forests",
                                   "Boreal Forests/Taiga",
                                   "Tropical and Subtropical Moist Broadleaf Forests",
                                   "Temperate Conifer Forests") ,
                    WWFMHTNAM1= c("Boreal","Arid","Arid",
                                  "Temperate","Boreal","Tropical","Temperate")) %>%
               mutate(WWFMHTNAM1= factor(WWFMHTNAM1,levels=c("Tropical","Arid","Temperate","Boreal"))))->res

res %>%
  group_by(WWFMHTNAM1) %>%
  summarise(p25 = quantile(value,probs = c(0.25)),
            )



  

  