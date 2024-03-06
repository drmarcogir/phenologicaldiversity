library(tidyverse)
library(raster)
library(marcoUtils)
library(data.table)
library(progress)

### SS gamma and alpha ----
sum_squares <- function(Y) {
  n <- nrow(Y)
  Y.cent <- scale(Y, center = T, scale = F)
  sij <- Y.cent^2
  SS.total <- sum(sij)
  SS.row <- rowSums(sij)
  SS.col <- colSums(sij)
  fcsd <- SS.col / SS.total
  lcsd <- SS.row / SS.total
  sdiv <- SS.total / (n - 1)
  out <- list(ss = SS.total, sdiv = sdiv, lcsd = lcsd, fcsd = fcsd)
  return(out)
}

### SS beta ----
sum_squares_beta <- function(Y, m) {
  n <- nrow(Y)
  Y.cent <- bind_cols(dplyr::select(Y, group), as.data.frame(scale(dplyr::select(Y, -group), scale = F)))
  mskj <- Y.cent %>% 
    group_by(group) %>% 
    mutate_at(vars(-group_cols()), function(x) (mean(x))^2) %>% 
    summarise_at(vars(-group_cols()), sum) %>% 
    ungroup() %>% 
    dplyr::select(-group)
  SSbk <- rowSums(mskj)
  SSbj <- colSums(mskj)
  SSb <- sum(SSbk)
  sdiv <- SSb / (n - 1)
  fcsd <- SSbj / SSb
  lcsd <- SSbk / SSb
  out <- list(ss = SSb, sdiv = sdiv, lcss = SSbk, lcsd = lcsd, fcsd = fcsd)
  return(out)
}


raster("/mnt/data1tb/phenology/gridGEE/grid_pheno_grass.tif")->gridr
as_tibble(rastertodf(gridr)) %>%
  rename(cat = value)->griddf




# read data from stats extractions
fread("/media/marco/marcodata19/Phenology_original/tmp/stats_2005") %>%
  dplyr::rename(GUP = V1,Peak = V2,SL = V3,Dormancy=V4,Maturity=V5,
                Senescence = V6,MidGUP = V7,MidGOD = V8,cat=V9)->dat

# cat number
dat %>%
  group_by(cat) %>%
  summarise(count=n()) %>%
  filter(count > 78) %>%
  dplyr::select(-c(count)) %>%
  inner_join(dat)->dat1


#dat1 %>%
#  dplyr::select(cat) %>%
# unique() %>%
# inner_join(griddf) %>%
# dplyr::select(x,y,cat) %>%
#  rasterFromXYZ()->myr


# set key for inner join
# setkey(dat,cat)
# setkey(catlookup,cat)

# join two tables
# dat[catlookup, nomatch=0] %>%
#   dplyr::select(-c(count))->datfilt

# --- alpha diversity
alpha_wrap<-function(x){
  pb$tick()
  tibble(div=sum_squares(x)$sdiv)->tmpres
  return(tmpres)
}

pb <- progress::progress_bar$new(total = 469838)
  
datfilt %>%
  group_by(cat) %>%
  nest() %>%
  mutate(res=map(data,alpha_wrap)) %>%
  dplyr::select(-c(data)) %>%
  unnest(cols = res) %>%
  dplyr::filter(!is.na(div)) %>%
  inner_join(griddf) %>%
  ungroup() %>%
  dplyr::select(cat,x,y,div) %>%
  mutate(div = scales::rescale(div,to=c(0,1)))->phenodiv

phenodiv %>%
  dplyr::select(x,y,div) %>%
  rasterFromXYZ() %>%
  writeRaster(.,filename = "/home/marco/Desktop/alphaglobal.tif")

# beta diversity
dat1 %>%
  rename(group = cat)->betain

sum_squares_beta(betain)->betaout

dat1 %>%
  dplyr::select(cat) %>%
  unique() %>%
  mutate(betadiv = betaout$lcsd)->betares

betares %>%
  inner_join(griddf) %>%
  dplyr::select(x,y,betadiv) %>%
  rasterFromXYZ() %>%
  writeRaster(.,filename = "/media/marco/marcodata19/Phenology_original/divfiles/betadiv_2001.tif",overwrite=T)


brayc<-function(x){
  pb$tick() 
  mean(vegdist(x, method="bray",diag = F,upper=F))->tmpres
  return(tmpres)
}
pb <- progress::progress_bar$new(total = 469838)


datfilt %>%
  group_by(cat) %>%
  nest() %>%
  mutate(res=map(data,brayc)) %>%
  dplyr::select(-c(data)) %>%
  unnest(cols = res) %>%
  dplyr::filter(!is.na(res)) %>%
  inner_join(griddf) %>%
  ungroup() %>%
  dplyr::select(cat,x,y,res)->phenodiv

phenodiv %>%
  dplyr::select(x,y,res) %>%
  rasterFromXYZ() %>%
  writeRaster(.,filename = "/home/marco/Desktop/bray.tif")
