read_csv("/home/marco/Desktop/difference/pheno025.csv") %>%
  inner_join(read_csv("/home/marco/Desktop/difference/temperature.csv")) %>%
  inner_join(read_csv("tmp/humi",col_names = FALSE) %>%
               rename(cat = X1,humi=X2) %>%
               inner_join(read_csv("tmp/gridstats025degree",col_names = FALSE) %>%
                            rename(cat = X1,cat025d=X2)) %>%
               group_by(cat025d) %>%
               summarise(humi = median(humi))) %>%
  inner_join(read_csv("/home/marco/Desktop/diss.csv"))->dd

dd %>%  
  filter(dif < 180 & dif > -180) %>%
  mutate(humif = cut(humi,breaks=10)) %>%
  filter(humif=="(-0.128,12.8]") %>%
  mutate(humif = cut(humi,breaks=10)) %>%
  #group_by(humif) %>%
  #summarise(n=n())
  ggplot()+geom_boxplot(aes(x=humif,y=dif))
  

dd %>%  
  filter(dif < 90 & dif > -90) %>%
  mutate(humif = cut(dis,breaks=10)) %>%
  #group_by(humif) %>%
  #summarise(n=n())
  ggplot()+geom_boxplot(aes(x=humif,y=dif))



dd  %>%
  ggplot(aes(x=dif_bio1,y=dif))+theme_minimal()+geom_bin2d(bins=70)+
  geom_smooth(method = "lm",color="red",size=1,linetype="dashed",se=FALSE)+
  scale_fill_viridis(option = "magma")+
  theme(legend.position = "none",axis.text = element_text(size=15,colour="black"),
        axis.title = element_text(size=20),
        plot.title = element_text(hjust = 0.5,size=20))


