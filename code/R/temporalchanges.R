phenodat %>% 
  filter(forest_prop > 0.85) %>%
  filter(bio_12 < 4500) %>% {.->> tmp} %>%
  group_by(cat1d) %>%
  mutate(nobs = length(y_2015)) %>%
  filter(nobs > 29) %>%
  #dplyr::select(bio_01,bio_12,y_2003,y_2015) %>
  summarise(change = t.test(y_2015,y_2003)$p.value) %>%
  
  #mutate(dif = y_2015-y_2003) %>%
  
  
  
  ggplot(aes(x=bio_01,y=bio_12,z=dif))+
  stat_summary_2d(fun = function(x) {if(length(x[!is.infinite(x)])>=5)(mean(x))else(NA)},bins = 35,na.rm = FALSE)+
  theme_minimal()+
  scale_fill_distiller(palette = "RdYlBu",limits=c(-10,10),oob=squish)+
  xlab("Temperature")+ylab("Precipitation")+theme(axis.text=element_text(size=15),axis.title=element_text(size=20),
                                                  plot.title=element_text(size=30,hjust=0.5))


midpoint<-function(x){
  as.numeric(str_remove(str_split_fixed(x,",",n=2)[,1],"\\("))->lower 
  as.numeric(str_remove(str_split_fixed(x,",",n=2)[,2],"]"))->upper
  return(lower+(upper-lower)/2)
}

phenodat %>% 
  filter(forest_prop > 0.5) %>%
  filter(bio_12 < 4500) %>%
  dplyr::select(bio_01,bio_12,y_2003,y_2015) %>%
  mutate(
    bin_x = cut(bio_12, breaks = 30),
    bin_y = cut(bio_01, breaks = 30)
  ) %>%
  group_by(bin_x, bin_y) %>%
  filter(n() > 10)%>%
  summarise(sig=t.test(y_2003,y_2015,paired = T)$p.value) %>%
  ungroup()->sig


phenodat %>% 
  filter(forest_prop > 0.5) %>%
  filter(bio_12 < 4500) %>%
  dplyr::select(bio_01,bio_12,y_2003,y_2015) %>%
  #mutate(y_2015 = scales::rescale(y_2015,to=c(0,1)),y_2003 = scales::rescale(y_2003,to=c(0,1))) %>%
  #mutate(dif = (y_2015/y_2003)-1) %>%
  mutate(dif = y_2015-y_2003) %>%
  mutate(
    bin_x = cut(bio_12, breaks = 30),
    bin_y = cut(bio_01, breaks = 30)
  )  %>%
  left_join(sig) %>%
  filter(sig < 0.05) %>%
  group_by(bin_x, bin_y) %>%
  summarise(meandif = sd(dif) ,bio_01=max(bio_01),bio_12=max(bio_12)) %>%
  tidyr::complete(bin_x,bin_y)->df1


df1 %>%
  mutate(bin_xn=midpoint(bin_x),bin_yn=midpoint(bin_y)) %>%
  ggplot(aes(x=bin_yn, y=bin_xn, fill = meandif)) + geom_tile()+theme_minimal()+
  scale_fill_gradient2(low="blue", mid="white", high="red",na.value=rgb(0, 0, 0, alpha=0),
                       limits=c(30,40),oob=scales::squish)+ylab("Precipitation")+
  xlab("Temperature")+theme(axis.text=element_text(size=18),axis.title = element_text(size=16))



