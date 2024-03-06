library(marcoUtils);library(sf)
library(tidyverse)

# read in shapefile
st_read("./KG/1976-2000.shp") %>%
  # match it with codes
  inner_join(read_delim("./KG/legend.txt",delim = "...",col_names = F) %>%
               mutate(X1 = as.numeric(trimws(X1)),X2 = trimws(X2)) %>%
               dplyr::rename(GRIDCODE = X1,NAME=X2)) %>%
  # match with description
  inner_join(read_csv("./KG/newtable.csv") %>%
  unite(NAME, c(level1, level2,level3), sep = "", remove = FALSE,na.rm = TRUE))->dat

# level 1 categories
dat %>%
  inner_join(dat %>%
               pull(level1) %>%
               unique() %>%
               as_tibble() %>%
               arrange(value) %>%
               mutate(level1_cat = 1:length(value)) %>%
               rename(level1 = value)) %>%
  # level 2 categories
  inner_join(dat %>%
               as_tibble() %>%
               dplyr::select(level1,level2) %>%
               unique() %>%
               mutate(level2_cat = 1:length(level2))) %>%
  # level 3 categories
  inner_join(dat %>%
               as_tibble() %>%
               dplyr::select(level1,level2,level3) %>%
               unique() %>%
               mutate(level3_cat = 1:length(level3))) %>%
  st_transform(latlon) %>%
  st_write("./KG/kg_codes.shp",delete_layer = TRUE)



# further filtering of the dataset
st_read("./KG/kg_codes.shp") %>%
  filter(lvl3_ct!=24) %>%
  filter(lvl3_ct!=2) %>%
  mutate(lvl3_tx1 = case_when((lvl1_tx=="Continental" & lvl2_tx=="Dry summer" & 
                                 lvl3_tx=="Mediterranean-influenced subarctic climate")~"Subarctic climate",
                              TRUE~lvl3_tx)) %>%
  mutate(lvl1_tx1 = case_when((lvl1_tx=="Continental" & lvl2_tx=="Dry summer" & 
                                 lvl3_tx=="Mediterranean-influenced subarctic climate")~"Continental",
                              TRUE~lvl1_tx)) %>%
  mutate(lvl2_tx1 = case_when((lvl1_tx=="Continental" & lvl2_tx=="Dry summer" & 
                                 lvl3_tx=="Mediterranean-influenced subarctic climate")~"No dry season",
                              TRUE~lvl2_tx)) %>%
  dplyr::select(-c(lvl3_tx,lvl2_tx,lvl1_tx)) %>%
  rename(lvl3_tx = lvl3_tx1,lvl2_tx=lvl2_tx1,lvl1_tx =lvl1_tx1) %>%
  st_write("./KG/kg_codesv1.shp",delete_layer = TRUE)


