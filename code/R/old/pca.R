library(tidyverse);library(data.table)
fread("/media/marco/marcodata19/Phenology_original/clim_analyses/dat.csv") %>%
  dplyr::select(cat,year,GUP,Dormancy,Peak,SL) %>%
  na.exclude() %>%
  group_by(cat) %>%
  summarise(GUP = median(GUP),Dormancy = median(Dormancy), 
            Peak = median(Peak),SL = median(SL))->dat

dat %>%
  dplyr::select(-c(cat)) %>%
  prcomp(.,scale. = T,center = T)->pca1


as_tibble(rastertodf(raster("/mnt/data1tb/Dropbox/phenology/gridGEE/grid_pheno_grass_rob.tif")))->griddf



as_tibble(pca1$x[,1:3]) %>%
  bind_cols(dat[1]) %>%
  inner_join(griddf %>%
               rename(cat =value)) %>%
  dplyr::select(x,y,PC1)->PC1

  rasterFromXYZ(crs=latlon) %>%
  projectRaster(,crs="+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs") %>%
  rastertodf(.) %>%
  as_tibble() %>%
  rename(PC1 = value)->PC1


as_tibble(pca1$x[,1:3]) %>%
  bind_cols(dat[1]) %>%
  inner_join(griddf %>%
               rename(cat =value)) %>%
  dplyr::select(x,y,PC2)->PC2



as_tibble(pca1$x[,1:3]) %>%
  bind_cols(dat[1]) %>%
  inner_join(griddf %>%
               rename(cat =value)) %>%
  dplyr::select(x,y,PC3) ->PC3



inner_join(PC1,PC2) %>%
  inner_join(PC3) %>%
  mutate(PC1 = scales::rescale(PC1,to=c(0,255)),PC2 = scales::rescale(PC2,to=c(0,255)),
         PC3 = scales::rescale(PC3,to=c(0,255))) %>%
  
ggplot()+geom_sf(data=bbox_df_robin, colour='black',fill='grey')+theme_classic()+
    geom_sf(data=wmap_df, fill='grey70',size=0)+
    geom_sf(data=ocean,fill='grey20',size=0)+
    geom_sf(data=wmap_df, linetype="dotted", color="grey70",size=0.2)+
  geom_tile(aes(x=x, y=y, fill=rgb(PC1,PC2,PC3, maxColorValue = 255)))+
  scale_fill_identity()+xlab("")+ylab("")->pca

ggsave(filename="/mnt/data1tb/Dropbox/phenology/figures/maps/pca.tiff",plot=pca,,width = 12,height=8)

rev(brewer.pal(7,"Spectral"))->mypal

PC1 %>%
  ggplot()+geom_sf(data=bbox_df_robin, colour='black',fill='black')+theme_classic()+
  geom_sf(data=wmap_df, fill='grey70',size=0)+
  geom_sf(data=ocean,fill='grey20',size=0)+
  geom_sf(data=wmap_df, linetype="dotted", color="grey70",size=0.2)+
  geom_tile(aes(x=x, y=y, fill=PC1))+xlab("")+ylab("")+
  theme(legend.position="bottom",legend.title = element_blank(),
        legend.justification=c(0.5),legend.key.width=unit(2, "mm"),
        legend.text = element_text(size = rel(1.5)),
        strip.background = element_rect(colour = "white", fill = "white"),
        strip.text = element_text(size = rel(2.5), face = "bold"))+
  scale_fill_distiller(palette = "Accent",direction=-1,guide_legend(direction = "horizontal",byrow = T,reverse = F,
  label.position = "bottom",keyheight = unit(3, units = "mm"),keywidth = unit(180,units = "mm")))
  
guide = guide_legend(direction = "horizontal",
                     keyheight = unit(3, units = "mm"),keywidth = unit(180 / length(unique(tmp$value1)), 
                                                                       units = "mm"),title.position = 'top',title.hjust = 0.5,label.hjust = 1,
                     nrow = 1,byrow = T,reverse = F,label.position = "bottom")
  
  scale_fill_gradientn(colours = terrain.colors(10),guide = guide_legend(direction = "horizontal",
                keyheight = unit(4, units = "mm"),keywidth = 5), 
                                  units = "mm"),title.position = 'top',title.hjust = 0.5,label.hjust = 1,
                                         nrow = 1,byrow = T,reverse = F,label.position = "bottom"))+xlab("")+ylab("")


    scale_fill_brewer(values = mypal)->PC1p


writeRaster(myr1,filename = "/home/marco/Desktop/multiband.tif",overwrite=T)
         