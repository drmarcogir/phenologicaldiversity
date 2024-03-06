phenodat %>%
  filter(forest_prop > 0.75) %>%
  filter(value < 180) %>%




dd[1,]$data[[1]]->x



dd %>%
  filter(count > 1) %>%
  dplyr::select(-c(count)) %>%
  inner_join(tmplc) %>%
  group_by(cat025d) %>%
  nest()  %>%
  mutate(res = map(data,disf)) %>%
  unnest(res) %>%
  rename(dis = res) %>%
  dplyr::select(cat025d,dis) %>%
  inner_join(phenodat4)->phenodat5
  
mod<-ranger(value~bio_01+bio_12+bio_04+bio_15+humi+Htop+dis,
            data=phenodat5,importance = 'permutation')

dd2 %>%
  unnest(res) %>%
  rename(dis = res) %>%
  dplyr::select(cat025d,dis) %>%
  inner_join(phenodat4) %>%
  mutate(value = log(value +1),dis = log(dis+1)) %>%
  filter(value > 2.5) %>%
  filter(dis > 0) %>% {.->> testd }%>%
  ggplot(aes(x=dis,y=value))+
  #geom_point(size=4,alpha=0.1,color="red")+geom_smooth(method="lm",se=FALSE,color="blue")
  geom_bin2d(bins=30)+
  scale_fill_viridis(option = "B",trans ="log")+
  #geom_smooth(method="gam",se=FALSE,color="red")
  geom_smooth(method="lm",se=FALSE,color="blue")+
  scale_fill_viridis(option = "B")

mod<-gam(value~s(lon,lat),data=testd)
testd$res<-residuals(mod)
mod1<-gam(value~s(dis)+s(bio_01)+s(bio_12)+s(lon,lat)+
            s(bio_15)+s(Htop)+s(bio_04)+s(humi),data=testd)
