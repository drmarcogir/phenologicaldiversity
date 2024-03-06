library(tidyverse)

tibble(tmp=1:2026,check=1:2026) %>%
anti_join(tibble(files=list.files("/mnt/data1tb/alessandro_metric/y2002_15imgs0")) %>%
  mutate(tmp=as.numeric(str_split_fixed(files,"_",n=4)[,3])) %>%
  arrange(tmp))%>%
  write_csv(.,"/home/marco/dockerdata/missingy2002.csv")

dd %>%
  dplyr::select(-c(check)) %>%
  slice(4:dim(.)[1]) %>%
  arrange(tmp) %>%
  dim()
  mutate(test = 1707:2026)
  

v="Y_2002_1_zeroversion.tif"
str_split_fixed(v,"_",n=4)[,3]
