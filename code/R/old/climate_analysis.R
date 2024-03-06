# Analyses of correlation between heterogeneity and climate  
library(tidyverse);library(data.table)

# read phenometrics
tibble(filename = list.files("/media/marco/marcodata19/phenology_csv",full.names = T,pattern = ".csv")) %>%
  mutate(file_contents = map(filename,read_csv)) %>%
  mutate(metric = str_remove(basename(filename),".csv")) %>%
  mutate(year = as.numeric(str_split_fixed(metric,"_",n=4)[,3]),
         metric = str_split_fixed(metric,"_",n=4)[,1]) %>%
  unnest(cols = file_contents) %>%
  dplyr::select(-c(filename,`system:index`,`.geo`,count)) %>%
  rename(value=zone)->metrics

fwrite(metrics,"/media/marco/marcodata19/Phenology_original/ERA_LAND/pheno.csv")

fread("/media/marco/marcodata19/Phenology_original/ERA_LAND/pheno.csv")->metrics

setkey(metrics, metric,cat,year)
metrics[,list(stdDev=mean(stdDev)),by=list(metric,cat,year)] %>%
  spread(key="metric",value="stdDev")->metrics1

fwrite(metrics1,"/media/marco/marcodata19/Phenology_original/ERA_LAND/pheno_wide.csv")



# filter time series with at least 10 years of data
metrics %>%
  group_by(value,metric) %>%
  summarise(count = n())->tcover

metrics %>%
  rename(cat = value)->metrics

as.data.table(metrics)->metrics

# climate data
fread("/media/marco/marcodata19/Phenology_original/ERA_LAND/clim.csv") %>%
  rename(clim_value = value) %>%
mutate(year = as.numeric(str_split_fixed(variable,"_",n=3)[,3]),
       clim_variable=paste0(str_split_fixed(variable,"_",n=3)[,1],"_",str_split_fixed(variable,"_",n=3)[,2])) %>%
  dplyr::select(-c(variable))->clim

fwrite(clim,"/media/marco/marcodata19/Phenology_original/ERA_LAND/clim.csv")

fread("/media/marco/marcodata19/Phenology_original/ERA_LAND/clim.csv")->clim


dat2 %>%
  filter(!is.na(Peak)) %>%
  group_by(cat_50km) %>%
  summarise(count = n()) %>%
  filter(count > 10) %>%
  dplyr::select(cat_50km) %>%
  inner_join(dat2) %>%
  group_by(cat_50km) %>%
  mutate(drought_mean = as.numeric(scale(drought_mean)),year= as.numeric(scale(year)),
         drought_sd = as.numeric(scale(drought_sd))) %>%
  group_by(cat_50km) %>%
  nest() %>%
  mutate(mod = map(data,~lm(Peak~drought_sd,data=.x)))->res

res %>%
  mutate(par = map(mod,broom::tidy)) %>%
  unnest(cols = par) %>%
  dplyr::select(-c(data,mod)) %>%
  inner_join(as_tibble(rastertodf(grid50km)) %>%
               rename(cat_50km = value))->res1

res1 %>%
  filter(term=="drought_sd") %>%
  #filter(p.value < 0.05) %>%
  ungroup() %>%
  dplyr::select(x,y,estimate) %>%
  rasterFromXYZ() ->myr

writeRaster(myr,filename = "/home/marco/Desktop/Peak1.tif",overwrite=TRUE)
#######################
library(tidyverse)
library(raster)

read_delim("/media/marco/marcodata19/Phenology_original/tmp_stats/gridgroup",delim = " ", col_names =F) %>%
  rename(cat = X1, cat_50km = X2)->griddf

rasterFromXYZ(rastertodf(raster("/mnt/data1tb/Dropbox/phenology/grid50km/grid50km.tif")))->grid50km

inner_join(dat,griddf)->dat1

dat1 %>%
  filter(!is.na(GUP)) %>%
  group_by(cat_50km,year) %>%
  summarise(drought_mean = mean(drought_mean),drought_sd = mean(drought_sd),
            prec_mean =mean(prec_mean) ,prec_sd = mean(prec_sd),
            tmp_mean = mean(tmp_mean),tmp_sd = mean(tmp_sd),GUP = mean(GUP),Peak = mean(Peak),
            SL = mean(SL))->dat2


dat2 %>%
  mutate(GUP = scales::rescale(GUP,to=c(0,1))) %>%
  ggplot()+geom_smooth(aes(x=prec_sd,y=GUP),method="lm")+theme_minimal()+ geom_point(aes(x=drought_mean,y=GUP))

geom_point(aes(x=drought_mean,y=GUP))+theme_minimal()


res2 %>%
  filter(term=="drought_mean") %>%
  dplyr::select(cat_50km,estimate) %>%
  rename(drought = estimate) %>%
  inner_join(
    res1 %>%
      filter(term=="year") %>%
      dplyr::select(cat_50km,estimate) %>%
      rename(year = estimate)) %>%
  filter(year > -25 & year < 25) %>%
  filter(drought > -20 & drought < 40) %>%
  ggplot()+geom_point(aes(x=year,y=drought))+theme_minimal()+geom_smooth(aes(x=year,y=drought),method = "gam")

