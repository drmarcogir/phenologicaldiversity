library(snow)
library()

st_read("/home/marco/Desktop/terr-ecoregions-TNC/tnc_terr_ecoregions.shp")->dat

phenochange<-raster("/home/marco/Desktop/dif8.tif")

unique(dat$WWF_MHTNAM) %>%
  .[str_detect(.,"For|for")]->forlist



dat %>%
  filter(WWF_MHTNAM %in% forlist) %>%
  st_write(.,"tmp.shp",delete_layer=T)


###########################################################
# ---Mean results (difference between two time slices)
###########################################################

res<-NULL

for (i in 1:length(forlist)){
  dat %>%
    dplyr::filter(WWF_MHTNAM==forlist[i])->tmpbiome
  st_write(tmpbiome,"tmp.shp",delete_layer = T)
  system("v.in.ogr in=tmp.shp out=tmp --o")
  system("g.region vector=tmp res=0.01")
  system("v.to.rast in=tmp out=tmp use=cat --o")
  system("r.mapcalc 'tmp1 = int(tmp/tmp)' --o")
  system("r.mask raster=tmp1 maskcats=1")
  system("g.region raster=dif8")
  system("r.stats -nA dif8 > stats")
  read.delim("stats",col.names = "value") %>%
    pull(value) %>%
    mean()->meanres
  tibble(mean = meanres,biome = forlist[i])->tmp1
  bind_rows(tmp1,res)->res
  system("r.mask -r")
}


res %>%
  arrange(mean) %>%
  mutate(biome = factor(biome,levels = biome)) %>%
  ggplot()+geom_bar(aes(x=mean,y=biome),stat = "identity",fill="brown3",color="black")+
  theme_minimal()+theme(axis.title = element_text(size=20,face="bold"),
                        axis.text = element_text(size=15,colour = "black"))+
  ylab("Biome")+xlab("Mean change")

###########################################################
# ---Original results for period 1 and period (ANOVA style)
###########################################################
res<-NULL


for (i in 1:length(forlist)){
  system("rm -f stats")
  dat %>%
    dplyr::filter(WWF_MHTNAM==forlist[i])->tmpbiome
  st_write(tmpbiome,"tmp.shp",delete_layer = T)
  system("v.in.ogr in=tmp.shp out=tmp --o")
  system("g.region vector=tmp res=0.01")
  system("v.to.rast in=tmp out=tmp use=cat --o")
  system("r.mapcalc 'tmp1 = int(tmp/tmp)' --o")
  system("r.mask raster=tmp1 maskcats=1")
  system("g.region raster=dif8")
  # statistics for first period
  system("r.stats -nA GUP_slice_8_sd > stats")
  read.delim("stats",col.names = "value") %>%
    mutate(period = "2001-2008",biome=forlist[i])->tmpres1
  # statistics for second period
  system("rm -f stats")
  system("r.stats -nA GUP_slice2_8_sd2 > stats")
  read.delim("stats",col.names = "value") %>%
    mutate(period = "2009-2016",biome=forlist[i])->tmpres2
  # store results
  bind_rows(tmpres1,res)->res
  bind_rows(tmpres2,res)->res
  system("r.mask -r")
  system("rm -f stats")
}

res %>%
  ggplot()+geom_boxplot(aes(x=period,y=value))+facet_wrap(~biome)+
  theme_minimal()+theme(axis.title = element_text(size=20,face="bold"),
                        axis.text = element_text(size=15,colour = "black"))

res %>%
  as_tibble() %>%
  mutate(period = factor(period,levels=c("2001-2008","2009-2016"))) %>% {.->>res1} %>%
  group_by(biome) %>%
  nest() %>%
  mutate(mod = map(data,~lm(value~period,data=.)),tidied = map(mod, tidy)) %>%
  unnest(tidied) %>%
  filter(p.value < 0.05)


########################
# ---Everything together
########################

# statistics for first period
system("rm -f stats")
system("r.mask raster=natural_habs maskcats=4")
system("g.region raster=GUP_slice2_8_sd2")
system("r.stats -ngA GUP_slice2_8_sd2,GUP_slice_8_sd > stats")
read_delim("stats",col_names =FALSE,delim = " ") %>%
  dplyr::select(X3,X4) %>%
  rename(p2 = X3,p1=X4) %>%
  gather(key="name",value="value") %>%
  mutate(period = factor(name,levels = c("p1","p2")))->dat1
system("r.mask -r")
mod<-lm(value~period,data=dat1)


bind_rows(tmpres1,tmpres2)->dat1


system("r.in.gdal in=/home/marco/Desktop/WCMC_natural_modified_habitat_screening_layer/natural_modified_habitat_screening_layer.tif out=natural_habs")
system("r.mask raster=natural_habs maskcats=4")

read_delim("/media/marco/marcodata19/Phenology_original/tmp1/forcover1",delim=",",col_names = F) %>%
  rename(cat=X1,area=X2) %>%
  group_by(cat) %>%
  summarise(area = sum(area))

as_tibble(rastertodf(raster("/mnt/data1tb/phenology/gridGEE/grid_pheno_grass.tif")))->dd

read_delim("/media/marco/marcodata19/Phenology_original/tmp1/forcover2",delim=",",col_names = F) 
