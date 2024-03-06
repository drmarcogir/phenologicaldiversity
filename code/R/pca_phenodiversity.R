
# read in spatial data ----------------------------------------------------
fread("/mnt/data1tb/alessandro_metric/final.csv") %>%
  #fread("/mnt/data1tb/alessandro_metric/finaldat.csv") %>%
  group_by(cat) %>%
  summarise(value = median(value)) %>%
  inner_join(grid5km) %>%
  inner_join(read_csv("./tmp/forestcount",col_names = FALSE) %>%
               rename(cat = X1, count = X2) %>%
               mutate(forest_prop = count /max(count))) %>%
  # add topography data
  inner_join(read_csv("./tmp/slope",col_names = F) %>%
               rename(cat = X1,mean_slope = X2)) %>%
  inner_join(read_csv("./tmp/elev",col_names = F) %>%
               rename(cat = X1,mean_elev = X2)) %>%
  inner_join(read_csv("./tmp/std_elev",col_names = F) %>%
               rename(cat = X1,std_elev = X2)) %>%
  inner_join(read_csv("./tmp/std_slope",col_names = F) %>%
               rename(cat = X1,std_slope = X2)) %>%
  # add climate data
  inner_join(read_csv("./GRASSGIS_files/bioclim_sd.csv")) %>%
  #inner_join(read_csv("./GRASSGIS_files/pet_mean.csv")) %>%
  # add info for resampling in geographic space
  inner_join(read_csv("tmp/gridstats1degree",col_names = FALSE) %>%
               rename(cat = X1,cat1d=X2)) %>%
  inner_join(read_csv("tmp/gridstats025degree",col_names = FALSE) %>%
               rename(cat = X1,cat025d=X2)) %>%
  inner_join(read_csv("tmp/gridstats3degree",col_names = FALSE) %>%
               rename(cat = X1,cat3d=X2)) %>%
  inner_join(read_csv("tmp/gridstats2degree",col_names = FALSE) %>%
               rename(cat = X1,cat2d=X2)) %>%
  # calculate aridity and rescale temp seasonality
  #mutate(aridity = pet_mean/bio_12,bio_04 = bio_04/1000) %>%
  #mutate(bio_01 = bio_01/10) %>%
  # add population density
  inner_join(read_csv("tmp/humi",col_names = FALSE) %>%
               rename(cat = X1,humi=X2)) %>%
  # ecoregions
  inner_join(read_csv("./tmp/ecoregions.csv")) %>%
  inner_join(read_csv("./tmp/shannontop.csv") %>%
               rename(Htop= variable))->phenodat




# create raster grid
raster(res=0.25)->myr
rasterize(world,myr) %>%
  rastertodf() %>%
  mutate(value = 1:length(value)) %>%
  rename(cat025d = value) %>% {.->>tmp} %>%
  mutate(x1 = x, y1= y) %>%
  st_as_sf(coords =c("x1","y1"),crs=latlon) %>%
  st_transform(crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs") %>% {.->>tmp} %>%
  st_coordinates() %>%
  as_tibble() %>%
  bind_cols(as_tibble(tmp) %>% rename(lon = x, lat =y) %>% dplyr::select(-c(geometry)))->g025

phenodat %>%
  filter(forest_prop > 0.75) %>%
  dplyr::select(-c(cat1d,forest_prop,count,cat3d,cat2d,WWF_MHTNUM,WWF_MHTNAM,x,y,cat)) %>%
  pivot_longer(cols=-c(cat025d),values_to = "values",names_to = "variables") %>%
  group_by(cat025d,variables) %>%
  summarise(values = mean(values)) %>%
  pivot_wider(names_from = "variables",values_from = "values",id_cols = c("cat025d")) %>%
  inner_join(g025)->phenodat1


phenodat1 %>%
  filter(value < 180)->phenodat3


phenodat3 %>%
  inner_join(pc1_2)->phenodat4


mod<-ranger(value~bio_01+bio_12+bio_04+bio_15+bio_03+
              bio_05+bio_06+bio_07+bio_08+bio_09+bio_10+
              bio_11+bio_13+humi+Htop+PC1+PC2,data=phenodat4,importance = 'permutation')


tibble(value = mod$variable.importance,variable = names(mod$variable.importance)) %>%
  arrange(desc(value)) %>%
  print(n=200)


phenodat4 %>%
  ungroup() %>%
  dplyr::select(cat025d,value,bio_01,bio_12,bio_04,bio_15,humi,std_slope,X1,X2) %>%
  pivot_longer(names_to = "variables",values_to = "pred",cols = -c(cat025d,value)) %>%
  group_by(variables) %>%
  #mutate(pred = scales::rescale(pred,to=c(0,1))) %>%
  filter(variables=="X1") %>%
  #mutate(value = log(value),pred = log(pred)) %>%
  filter(value > 2.5) %>%
  ggplot(aes(x=pred,y=value))+geom_point(size=2,colour="red",alpha=0.3)+theme_minimal()+
  facet_wrap(~variables,scales = "free")


phenodat4 %>%
  ggplot(aes(x=X1,y=X2,z=value))+
  theme_minimal()+
  stat_summary_2d(fun = function(x) {if(length(x[!is.infinite(x)])>=5)(mean(x))else(NA)},bins = 30,na.rm = FALSE)+
  scale_fill_viridis(option = "turbo",limits=c(20,100),oob=squish)+
  xlab("DIM1")+ylab("DIM2")+theme(axis.text=element_text(size=15),axis.title=element_text(size=20),
                                                  plot.title=element_text(size=30,hjust=0.5))


raster("/home/marco/Desktop/DIM1.tif") %>%
  rastertodf() %>%
  filter(value >)

# climate euclidean distance dissimilarity ----------------------------------------

phenodat %>%
  ungroup() %>%
  dplyr::select(matches("cat025d|bio")) %>%
  group_by(cat025d) %>%
  summarise(count = n()) %>%
  filter(count > 2) %>%
  ungroup() %>%
  inner_join(phenodat %>% dplyr::select(matches("cat025d|bio"))) %>% 
  dplyr::select(-c(count)) %>% {.->> tmp} %>%
  dplyr::select(-c('cat025d')) %>%
  vegan::decostand(method = "standardize") %>%
  as_tibble() %>%
  bind_cols(tmp %>% dplyr::select(c('cat025d')))->tmp1

tmp1 %>%
  group_by(cat025d) %>%
  nest() %>%
  mutate(res= map(data,dist)) %>%
  inner_join(phenodat3 %>% dplyr::select(cat025d)) %>%
  mutate(res1 = map(res,mean)) %>%
  dplyr::select(cat025d,res1) %>%
  unnest(cols = c(res1)) %>%
  inner_join(phenodat3)->disdf

disdf %>%
  mutate(value = log(value),res1=log(res1)) %>%
  dplyr::filter(value > 2.5) %>% {.->>dat} %>%
  ggplot(aes(x=res1,y=value))+theme_minimal()+geom_bin2d(bins=30)+geom_smooth(method="lm",se=FALSE,color="red")+
  scale_fill_gradient(low="lightblue1",high="darkblue",trans="log10")+xlab("Predictor")+ylab("Pheno diversity")+
  theme(legend.position = "none",axis.text = element_text(size=12,colour="black"),axis.title = element_text(size=20),
        plot.title = element_text(hjust = 0.5,size=20))+theme(axis.text.x = element_text(angle = 30,hjust = 1))


  ggplot(aes(x=res1,y=value))+geom_point()+geom_smooth(method = "lm")

options(na.action = "na.fail")
mod<-lm(value~res1+humi+std_slope+bio_01+bio_04,data=dat)$
dredge(mod)

mod<-gam(value~res1+humi+s(std_slope)+s(bio_01)+s(bio_04),data=dat)



mod<-lm(log(value)~bio_01+bio_12+bio_04+bio_15+humi+std_slope,data=phenodat3)

mod<-ranger(value~bio_01+bio_12+bio_04+bio_15+humi,data=phenodat3,importance='permutation')


# land cover turnover -----------------------------------------------------
# filter by land cover type (at least two land cover types)
read_delim("/media/marco/marcodata19/validationmetrics/pheno_grid_ESAvalues2002",delim = " ",col_names = F) %>%
  rename(lc=X1,cat=X2,area=X3) %>%
  mutate(lc = paste0("lc_",lc)) %>%
  mutate(area = 1) %>%
  pivot_wider(names_from = lc,values_from=area,values_fill=0) %>%
  mutate(rich = rowSums(.[2:13])) %>%
  filter(rich > 2) %>% 
  dplyr::select(cat,rich)->tmprich
  
  dplyr::select(cat,rich) %>%
  inner_join(read_csv("tmp/gridstats025degree",col_names = FALSE) %>%
               rename(cat = X1,cat025d=X2)) %>%
  group_by(cat025d) %>%
  summarise(rich = sum(rich)) %>%
  inner_join(phenodat3 %>% dplyr::select(cat025d))->tmprich

# add land cover product 
read_delim("/media/marco/marcodata19/validationmetrics/pheno_grid_ESAvalues2002",delim = " ",col_names = F) %>%
             rename(lc=X1,cat=X2,area=X3) %>%
             mutate(lc = paste0("lc_",lc)) %>%
             pivot_wider(names_from = lc,values_from=area,values_fill=0) %>%
             inner_join(tmprich) %>%
             inner_join(read_csv("tmp/gridstats025degree",col_names = FALSE) %>%
                          rename(cat = X1,cat025d=X2)) %>%
  dplyr::select(-c(cat,rich)) %>%  {.->>tmp} %>%
  group_by(cat025d) %>%
  summarise(count = n()) %>%
  filter(count > 2) %>%
  dplyr::select(-c(count)) %>%
  inner_join(tmp) %>%
  mutate_each(funs(log(1 + .)), lc_50:lc_100) %>%
  group_by(cat025d) %>%
  nest() %>%
  mutate(res= map(data,dist)) %>%
  mutate(res2 = map(res,mean)) %>%
  dplyr::select(cat025d,res2) %>%
  unnest(cols = c(res2))->disdf1


tmp %>%
  pivot_longer(names_to = 'variables',values_to ='values',cols=-c(cat025d)) %>%
tmp %>%  
  mutate(H = vegan::diversity(.[2:13],index="shannon"),
         S = vegan::diversity(.[2:13],index="simpson"),
         IS = vegan::diversity(.[2:13],index="invsimpson")) %>%
  group_by(cat025d) %>%
  summarise(H=median(H),S=median(S),IS=median(IS)) %>%
  inner_join(disdf1)->disdf2

  
tmprich %>%
  inner_join(disdf1)->disdf1



tmp %>%
  relocate(cat025d) %>%
  mutate(rsum = rowSums(.[2:13])) %>%
  arrange(rsum)


mod<-lm(log(value)~bio_01+bio_12+bio_04+bio_15+humi+std_slope,data=disdf2)

mod<-ranger(value~bio_01_sd+bio_12_sd+bio_04_sd+bio_15_sd+humi+S+std_slope,data=disdf2,importance='permutation')


disdf2 %>%
  mutate(value = log(value),S=log(S+1)) %>%
  dplyr::filter(value > 3) %>% {.->>dat} %>%
  ggplot(aes(x=S,y=value))+theme_minimal()+geom_bin2d(bins=30)+geom_smooth(method="lm",se=FALSE,color="red")+
  scale_fill_gradient(low="lightblue1",high="darkblue",trans="log10")+xlab("Predictor")+ylab("Pheno diversity")+
  theme(legend.position = "none",axis.text = element_text(size=12,colour="black"),axis.title = element_text(size=20),
        plot.title = element_text(hjust = 0.5,size=20))+theme(axis.text.x = element_text(angle = 30,hjust = 1))

hist(disdf1$res2)



  prcomp() %>%
  .$x %>%
  as_tibble() %>%
  dplyr::select(PC1,PC2,PC3,PC4,PC5,PC6) %>%
  bind_cols(phenodat3)->dd1


mod<-ranger(value~PC1+PC2+PC3+PC4+PC5+PC6,
            data=dd1,importance = 'permutation')

# fit rf model and remove irrelevant features -------------------------------
form<-as.formula(c("value~",paste0(c(paste0('bio_0',1:9),paste0('bio_',10:19),
                                     "std_elev","humi","aridity"),collapse="+")))

mod<-ranger(form,data=dd1)

mod<-ranger(value~bio_01_sd+bio_12_sd+
              bio_14_sd+bio_12+bio_15+bio_04+humi,
              data=phenodat3,importance = 'permutation')

mod$r.squared




phenodat4 %>%
  mutate(Htop = log(Htop+1),value = log(value+1)) %>%
  filter(value > 2.5) %>%
  ggplot(aes(x=Htop,y=value))+geom_smooth()

phenodat3 %>%
  ungroup() %>%
  dplyr::select(bio_01_sd,bio_12_sd,
                  bio_14_sd,bio_01,bio_12,bio_15,bio_04,humi,std_elev) %>%
  cor() %>%
ggcorrplot(.,
           hc.order = TRUE,
           type = "lower",
           outline.color = "white",lab = TRUE)



phenodat3 %>%
  ungroup() %>%
  filter(bio_01_sd < 1.5) %>%
  dplyr::select(cat025d,value,bio_04,bio_12,bio_12_sd,bio_15,bio_14_sd,bio_01_sd,humi,bio_01) %>%
  pivot_longer(values_to = "pred_values",names_to = "variables",cols = -c(cat025d,value)) %>%
  group_by(variables) %>%
  #mutate(pred_values = scales::rescale(pred_values,to=c(0,1))) %>%
  mutate(value = log(value+1)) %>%
  filter(value > 2.5) %>%
  ggplot(aes(x=pred_values,y=value))+theme_minimal()+geom_bin2d(bins=30)+geom_smooth(method="lm",se=FALSE,color="red")+
  scale_fill_gradient(low="lightblue1",high="darkblue",trans="log10")+xlab("Predictor")+ylab("Pheno diversity")+
  theme(legend.position = "none",axis.text = element_text(size=12,colour="black"),axis.title = element_text(size=20),
        plot.title = element_text(hjust = 0.5,size=20))+facet_wrap(~variables,scales = "free")+theme(axis.text.x = element_text(angle = 30,hjust = 1))


#phenodat2 %>%
#  filter(value < 180) %>%
#  ungroup() %>%
#  mutate(rown = 1:length(value)) %>%
#  ggplot(aes(x=rown,y=value))+geom_point()

  

  mutate(perc = cut(value,quantile(value,c(0,0.9))))
  
  mutate(percentile_rank = ntile(value,100)) %>%
  relocate(percentile_rank) %>%
  group_by(percentile_rank) %>%
  summarise(count = n()) %>%
  print(n=200)
 
  #mutate(perc = cut(value,quantile(value,c(0,0.9))))

# umap land cover ---------------------------------------------------------
# filter by land cover type (at least two land cover types)
  read_delim("/media/marco/marcodata19/validationmetrics/pheno_grid_ESAvalues2002",delim = " ",col_names = F) %>%
    rename(lc=X1,cat=X2,area=X3) %>%
    mutate(lc = paste0("lc_",lc)) %>%
    pivot_wider(names_from = lc,values_from=area,values_fill=0)->tmp
  

tmp %>%
  inner_join(read_csv("./tmp/forestcount",col_names = FALSE) %>%
               rename(cat = X1, count = X2) %>%
               mutate(forest_prop = count /max(count))) %>%
  inner_join(read_csv("tmp/gridstats025degree",col_names = FALSE) %>%
               rename(cat = X1,cat025d=X2)) %>%
  filter(forest_prop > 0.75) %>%
  dplyr::select(-c(forest_prop,cat,count)) %>%
  pivot_longer(cols=-c(cat025d),values_to = "values",names_to = "variables") %>%
  group_by(cat025d,variables) %>%
  summarise(values = sum(values)) %>%
  pivot_wider(names_from = "variables",values_from = "values",id_cols = c("cat025d")) %>%
  write_csv(.,file = "/home/marco/Desktop/testumap.csv")


  
tmp %>%
  dplyr::select(-c(cat)) %>%
  as.data.frame()->tmp1

tmp.umap = umap(tmp1)


