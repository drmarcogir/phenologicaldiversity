# load required libraries
library(tidyverse)
library(ranger)

# wrapper function for model fitting
fit_mod<-function(x,option){
  if(option=="two"){
    trainy <- 2003:2011
  } else if(option=="alternate") {
    trainy <- seq(from=2003,to=2020,by=2)
  } else if(option=="last") {
    trainy <- 2003:2016
  }
  x %>%
    filter(year %in% trainy)->train
  x %>%
    filter(!year %in% trainy)->test
  mod<-ranger(value~bio1+bio2+bio3+bio4+bio5+bio6+bio7+
                bio8+bio9+bio10+bio11+bio12+bio13+
                bio14+bio15+bio16+bio17+bio18+bio19,data=train,num.trees = 500)
  predict(mod,test)->predv
  cor(predv$predictions,test$value)->r2
  return(r2)
}


#-------------------------------- Level 2 analyses
read_csv("finaldat.csv") %>%
  group_by(lvl1_tx,lvl2_tx) %>% 
  nest()->dat2


# two equal periods
dat2 %>%
  mutate(res = map(data,~fit_mod(x=.x,option="two")))->res

res %>%
  dplyr::select(-c(data)) %>%
  unnest(cols = res) %>%
  write_csv("two_lvl2.csv")

# alternate years
dat2 %>%
  mutate(res = map(data,~fit_mod(x=.x,option="alternate")))->res

res %>%
  dplyr::select(-c(data)) %>%
  unnest(cols = res) %>%
  write_csv("alternate_lvl2.csv")


# last period
dat2 %>%
  mutate(res = map(data,~fit_mod(x=.x,option="last")))->res

res %>%
  dplyr::select(-c(data)) %>%
  unnest(cols = res) %>%
  write_csv("last_lvl2.csv")

#-------------------------------- Level 3 analyses
read_csv("finaldat.csv") %>%
  group_by(lvl1_tx,lvl2_tx,lvl3_tx) %>% 
  nest()->dat3

# two equal periods
dat2 %>%
  mutate(res = map(data,~fit_mod(x=.x,option="two")))->res

res %>%
  dplyr::select(-c(data)) %>%
  unnest(cols = res) %>%
  write_csv("two_lvl3.csv")

# alternate years
dat2 %>%
  mutate(res = map(data,~fit_mod(x=.x,option="alternate")))->res

res %>%
  dplyr::select(-c(data)) %>%
  unnest(cols = res) %>%
  write_csv("alternate_lvl3.csv")


# last period
dat2[1,] %>%
  mutate(res = map(data,~fit_mod(x=.x,option="last")))->res

res %>%
  dplyr::select(-c(data)) %>%
  unnest(cols = res) %>%
  write_csv("last_lvl3.csv")
