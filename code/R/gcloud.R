library(tidyverse)
library(ranger)

read_csv("dat.csv")->dat

dat %>%
  filter(value < 183) %>%
  group_by(lvl1_tx,lvl2_tx,lvl3_tx) %>%
  summarise(n = n()) %>%
  filter(n > 1000) %>%
  dplyr::select(-c(n)) %>%
  ungroup() %>%
  inner_join(dat) %>%
  group_by(lvl1_tx,lvl2_tx,lvl3_tx) %>% 
  nest()->dat2

dat2 %>%
  mutate(res = map(data,~ranger(value~bio1+bio2+bio3+bio4+bio5+bio6+bio7+
                                  bio8+bio9+bio10+bio11+bio12+bio13+
                                  bio14+bio15+bio16+bio17+bio18+bio19,data=.x,num.trees = 500)$r.squared))->res

res %>%
  unnest(cols = res) %>%
  dplyr::select(-c(data)) %>%
  write_csv("R2results.csv")
  
gsutil cp R2results.csv gs://phenologyml/

  
  
model <- ranger(value~bio1+bio2+bio3+bio4+bio5+bio6+bio7+
                  bio8+bio9+bio10+bio11+bio12+bio13+
                  bio14+bio15+bio16+bio17+bio18+bio19,data = dat, num.trees =  500,
                importance = 'permutation')
                
tibble(variable = names(model$variable.importance),value = model$variable.importance)->imp
write_csv(imp,"importance.csv")

aws s3 cp dat.csv s3://phenodat/

