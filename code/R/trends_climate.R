library(tidyverse)
library(furrr)

read_csv("dat1.csv")->dat1

dat1 %>%
  group_by(cat) %>%
  nest()->dat2

dat2 %>%
  mutate(modres = map(data,mod_fit))->mod_clim
