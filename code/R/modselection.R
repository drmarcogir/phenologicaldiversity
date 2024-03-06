fread("/home/marco/Desktop/finaldat.csv")->dat

dat %>%
  mutate(lvl2_tx = paste0(lvl1_tx,lvl2_tx)) %>%
  group_by(lvl2_tx) %>%
  nest()->dat1

dat1[1,]$data[[1]] %>% dplyr::select(-c(lvl1_tx,lvl3_tx,cat,lvl1_ct,lvl2_ct,lvl3_ct,count,forest_prop,year))->dd

phenodat1 %>% ungroup %>% dplyr::select(!!c(paste0('bio_0',1:9),paste0('bio_',10:19),"y_2004","std_elev",
                                            "humi","aridity"))->dd
  


mod<-ranger(y_2004~.,data=dd,importance = 'permutation')



# variable importance
tibble(variables = names(mod$variable.importance),imp = mod$variable.importance)->varimp
# correlation matrix
cor(dd %>% dplyr::select(-c(y_2004)))->cormat
cormat[upper.tri(cormat,diag = TRUE)]<- NA
#cormat[abs(cormat) < 0.7]<-NA
cormat %>%
  as.data.frame() %>%
  mutate(rows = row.names(.)) %>%
  pivot_longer(values_to = "value",names_to = "cols",cols = -c(rows)) %>%
  as_tibble() %>%
  drop_na() %>%
  mutate(value = abs(value))->cormat1


res<-NULL

for(i in 1:dim(cormat1)[1]){
  print(i)
  if(cormat1[i,]$value < 0.7){
    next
  } else {
    cormat1[i,] %>%
      dplyr::select(1,3) %>%
      rename(value= value,variables = rows) %>%
      bind_rows(cormat1[i,] %>%
                  dplyr::select(2,3) %>%
                  rename(value = value,variables = cols)) %>%
      inner_join(varimp) %>%
      filter(imp==max(imp)) %>%
      dplyr::select(variables)->tmpres
    bind_rows(tmpres,res)->res
  }
} 


res %>% unique() -> res1

phenodat1 %>% ungroup %>% dplyr::select(!!c(res1 %>% pull(variables),"y_2004"))->dd

# select variables that not too intercorrelated
mod<-ranger(y_2004~.,data=dd,importance = 'permutation')
st_as_sf(coords=c("X","Y"))
tibble(variables = names(mod$variable.importance), imp = mod$variable.importance) %>%
  arrange(desc(imp))

