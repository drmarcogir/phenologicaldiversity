# ecoregions labels -------------------------------------------------------


st_read("./ecoregions/tnc_terr_ecoregions.shp") %>%
  dplyr::filter(!WWF_MHTNAM %in% c("Temperate Grasslands, Savannas and Shrublands",
                                   "Flooded Grasslands and Savannas",
                                   "Rock and Ice",
                                   "Mangroves",
                                   "Inland Water"))->polys

polys %>%
  pull(WWF_MHTNAM) %>%
  unique()->ecol

g025 %>%
  dplyr::select(lon,lat,cat025d) %>%
  rasterFromXYZ()->myr
crs(myr)<-latlon

res<-NULL

for(i in 1:length(ecol)){
  print(i)
  polys %>%
    filter(WWF_MHTNAM==ecol[i])->tmpv
  raster::extract(myr,tmpv) %>%
    unlist() %>%
    unique() %>%
    as_tibble() %>%
    rename(cat025d = value) %>%
    na.exclude() %>%
    mutate(ecoreg = ecol[i])->tmpres
  bind_rows(tmpres,res)->res
}



# pca ---------------------------------------------------------------------


read_csv("/home/marco/Desktop/testumap.csv") %>%
  dplyr::select(-c(cat025d)) %>%
  mutate_all(list(~ log(.+1))) %>%
  prcomp(.)->pcares

# get first two components
as_tibble(pcares$x[,1:4]) %>%
  bind_cols(read_csv("/home/marco/Desktop/testumap.csv")%>%
              dplyr::select(cat025d))->pc1_2



# plots -------------------------------------------------------------------
# PCA
as_tibble(pcares$x[,3:4]) %>%
#as_tibble(dcasum$site.scores[,1:2]) %>%
#  rename(PC1 = DCA1,PC2 = DCA2) %>%
  bind_cols(dd %>%
              dplyr::select(cat025d)) %>%
  inner_join(read_csv("/home/marco/Desktop/ecoreg.csv")) %>%
  #filter(ecoreg!="Temperate Conifer Forests") %>%
  #filter(ecoreg!="Temperate Broadleaf and Mixed Forests") %>%
  ggplot()+geom_point(aes(x=PC3,y=PC4,colour=ecoreg),size=3,alpha = 0.5)+theme_minimal()+
  scale_colour_brewer(palette = "Spectral")

# UMAP
read_csv("/home/marco/Desktop/embedding.csv",col_names = FALSE) %>%
  bind_cols(read_csv("/home/marco/Desktop/testumap.csv") %>%
              dplyr::select(cat025d)) %>%
  inner_join(read_csv("/home/marco/Desktop/ecoreg.csv")) %>%
  #filter(ecoreg=="Tropical and Subtropical Moist Broadleaf Forests") %>%
  ggplot()+geom_point(aes(x=X1,y=X2,colour=ecoreg),size=3,alpha = 0.5)+theme_minimal()+
  scale_colour_brewer(palette = "Spectral")
  



read_csv("/home/marco/Desktop/embedding.csv",col_names = FALSE) %>%
  bind_cols(read_csv("/home/marco/Desktop/testumap.csv") %>%
              dplyr::select(cat025d)) %>%
  inner_join(as_tibble(pcares$x[,1:2]) %>%
               #as_tibble(dcasum$site.scores[,1:2]) %>%
               #  rename(PC1 = DCA1,PC2 = DCA2) %>%
               bind_cols(dd %>%
                           dplyr::select(cat025d))) %>%
  inner_join(g025) %>%
  dplyr::select(lon,lat,PC1,PC2) %>%
  rasterFromXYZ(crs =latlon) %>%
  writeRaster(filename = "/home/marco/Desktop/pca.tif")
  
# shapefile ---------------------------------------------------------------
read_csv("/home/marco/Desktop/embedding.csv",col_names = FALSE) %>%
  bind_cols(read_csv("/home/marco/Desktop/testumap.csv") %>%
              dplyr::select(cat025d)) %>%
  inner_join(as_tibble(pcares$x[,1:2]) %>%
               #as_tibble(dcasum$site.scores[,1:2]) %>%
               #  rename(PC1 = DCA1,PC2 = DCA2) %>%
               bind_cols(dd %>%
                           dplyr::select(cat025d))) %>%
  inner_join(g025) %>%
  dplyr::select(lon,lat,PC2) %>%
  rasterFromXYZ(crs =latlon) %>%
  writeRaster(filename = "/home/marco/Desktop/PC2.tif",overwrite =TRUE)
  
  inner_join(read_csv("/home/marco/Desktop/ecoreg.csv")) %>%
  dplyr::select(lon,lat,ecoreg) %>%
  st_as_sf(coords=c("lon","lat")) %>%
  st_write("/home/marco/Desktop/pca.shp",delete_layer = TRUE)

  