library(tidyverse);library(raster)

list.files("/mnt/data1tb/alessandro_metric/y2002_15imgs0/",full.names = T) %>%
  .[!str_detect(.,"splitg")] %>%
  as_tibble() %>%
  mutate(tilen = as.numeric(str_split_fixed(value,"_",n=4)[,3])) %>%
  #write_csv(.,"/home/marco/Desktop/tiles.csv")
  group_by(tilen) %>%
  summarise(tilecount=n()) %>%
  arrange(desc(tilecount))->dd1


  list.files("/mnt/data1tb/alessandro_metric/y2002_15imgs0/") %>%
    .[str_detect(.,"splitg")] %>%
    .[!str_detect(.,"zeroversion")] %>%
    as_tibble() %>%
    mutate(tilen = as.numeric(str_split_fixed(value,"_",n=4)[,3])) %>%
    dplyr::select(tilen) %>%
    unique()->dd
    
dd1 %>%
  filter(!tilen %in% c(1530,1531)) %>%
  bind_rows(dd) %>%
  dim()
  
      
    pull(tilen) %>%
    unique() %>%
    length()
    
    
    list.files("/mnt/data1tb/alessandro_metric/y2002_15imgs0/") %>%
      .[!str_detect(.,"splitg")]
    
    list.files("/mnt/data1tb/alessandro_metric/y2002_15imgs0",full.names = T) %>%
      .[!str_detect(.,"splitg")]
    