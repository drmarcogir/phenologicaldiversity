
# pca and binning ---------------------------------------------------------


phenodat4 %>%
  dplyr::select(bio_01,bio_12,bio_04,bio_15,bio_07,humi,Htop) %>%
  decostand(method="standardize") %>%
  prcomp()->pcatest

as_tibble(pcatest$x) %>%
  bind_cols(phenodat4 %>% dplyr::select(cat025d)) %>%
  mutate(PC1_bin = cut(PC1,breaks=20),
         PC2_bin = cut(PC2,breaks=20),
         PC3_bin = cut(PC3,breaks=20),
         PC4_bin = cut(PC4,breaks=20)) %>% {.->> tmpbin} %>%
  group_by(PC1_bin,PC2_bin,PC3_bin,PC4_bin) %>%
  count() %>%
  inner_join(tmpbin) %>%
  inner_join(phenodat4 %>%
               rename(PC1_for = PC1,PC2_for = PC2,PC3_for = PC3,
                      PC4_for = PC4)) %>% 
  group_by(PC1_bin,PC2_bin,PC3_bin,PC4_bin) %>%
  nest()->dd

dd[1,]$data[[1]] %>%
  dplyr::select(lon,lat) %>%
  st_as_sf(coords=c("lon","lat"),crs=latlon) %>%
 #st_union() %>%
 #st_convex_hull() %>%
  st_write("/home/marco/Desktop/test1.shp")


  
  
  
#%>%
  st_write("/home/marco/Desktop/test.shp",delete_layer=T)

  writeRaster(filename = "/home/marco/Desktop/minor.tif")


phenodat %>%
    dplyr::select(bio_01,bio_12,bio_04,bio_15,bio_07,humi,Htop) %>%
    #decostand(method="standardize") %>%
    prcomp(center = TRUE,scale. = TRUE)->pcatest

  as_tibble(pcatest$x) %>%
  bind_cols(phenodat %>% dplyr::select(cat)) %>%
  mutate(PC1_bin = cut(PC1,breaks=80),
         PC2_bin = cut(PC2,breaks=80),
         PC3_bin = cut(PC3,breaks=80),
         PC4_bin = cut(PC4,breaks=80)) %>% {.->> tmpbin} %>%
  group_by(PC1_bin,PC2_bin,PC3_bin) %>%
  count() %>%
  inner_join(tmpbin) %>%
  inner_join(phenodat) %>%
  
  
  
  group_by(PC1_bin,PC2_bin,PC3_bin) %>%
  summarise(bio_01 = mean(bio_01),value = mean(value),bio_12 = mean(bio_12),
            bio_04 = mean(bio_04),x=median(x),y=median(y)) %>%
  mutate(value = log(value+1),bio_01 = log(bio_01+1),bio_12 = log(bio_12+1),
         bio_04 = log(bio_04+1))->dd1
  
dd1 %>%
  group_by(PC1,PC2_bin,PC3_bin) %>%
  filter(n==50) %>%
  group_by(PC1_bin,PC2_bin,PC3_bin) %>%
  nest()->dd2

dd1 %>%
  dplyr::select(x,y,value) %>%
  st_as_sf(coords=c("x","y"),crs=latlon)->dd2

dd2
st_write("/home/marco/Desktop/test.shp",delete_layer=TRUE)

quantile(cc2$value, probs = seq(0.01,0.2,0.9))  


# count -------------------------------------------------------------------

phenodat %>%
  filter(forest_prop > 0.75) %>%
  group_by(cat025d) %>%
  count() %>%
  arrange(desc(n))



# plot --------------------------------------------------------------------




         
cc2 %>%
 ggplot(aes(x=bio_04,y=value))+theme_minimal()+geom_bin2d(bins=40)+geom_smooth(method="lm",se=FALSE,color="red")+
  scale_fill_gradient(low="lightblue1",high="darkblue",trans="log10")+xlab("Predictor")+ylab("Pheno diversity")+
  theme(legend.position = "none",axis.text = element_text(size=12,colour="black"),axis.title = element_text(size=20),
        plot.title = element_text(hjust = 0.5,size=20))+theme(axis.text.x = element_text(angle = 30,hjust = 1))

phenodat4 %>%
  filter(value < 180) %>%
  mutate(bio_01 =log(bio_01+1),value = log(value+1)) %>%
  filter(value > 2.5) %>%
  ggplot(aes(x=bio_01,y=value))+
  theme_minimal()+geom_bin2d(bins = 30) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()+geom_smooth(method="lm",color ="red")


phenodat %>%
  filter(forest_prop > 0.75) %>%
  filter(value < 180) %>%
  inner_join(read_csv("./GRASSGIS_files/bioclim.csv") %>%
  dplyr::select(bio_01,bio_12,cat) %>%
  rename(bio_01_mean = bio_01,bio_12_mean = bio_12)) %>%
  mutate(bio_01_bin = cut(bio_01_mean,breaks=100),
         bio_12_bin = cut(bio_12_mean,breaks=100))->ll

ll %>%
  group_by(bio_01_bin,bio_12_bin)

ll %>%
  dplyr::select(bio_01_bin,bio_12_bin) %>%
  unique() %>%
  mutate(bin_id = 1:length(bio_01_bin)) %>%
  inner_join(ll) %>%
  filter(bin_id==307) %>%
  dplyr::select(x,y,bin_id) %>%
  st_as_sf(coords=c("x","y"),crs=latlon) %>% 
  st_write("/home/marco/Desktop/test3.shp",delete_layer = TRUE)


%>%
  group_by(bio_01_bin,bio_12_bin) %>%
  nest()->dd

dd[1,]$data[[1]] %>%
  dplyr::select(x,y) %>%
  st_as_sf(coords=c("x","y"),crs=latlon) %>%
  st_union() %>%
  st_convex_hull() %>%
  st_write("/home/marco/Desktop/test2.shp",delete_layer = TRUE)
  




