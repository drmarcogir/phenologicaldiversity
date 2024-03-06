library(tidyverse)
library(scales)
for (i in 1:10){
  sample.range <- 1:365
  iq.mean <- 183
  iq.sd <- 55
  iq.dist <- dnorm(sample.range, mean = iq.mean, sd = iq.sd)
  iq.df <- data.frame("IQ" = sample.range, "Density" = iq.dist)
  
}


as_tibble(iq.df) %>%
  mutate(sample = "s1") %>%
  bind_rows(as_tibble(iq.df) %>%
              mutate(sample = "s2")) %>%
  bind_rows(as_tibble(iq.df2) %>%
              mutate(sample = "s3"))->df

df %>%
  mutate(Density = Density*1000) %>%
  group_by(sample) %>%
  mutate(cumsum = cumsum(Density)) %>%
  ggplot(aes(x = IQ, y = cumsum,color = sample)) + geom_line()



sample.range <- 1:365
iq.mean <- 183
iq.sd <- 60
hist(dnorm(sample.range, mean = iq.mean, sd = iq.sd))


rescale(rnorm(365),to=c(0,1))->ndvi

tibble(ndvi) %>%
  filter(ndvi < median(ndvi)) %>%
  arrange(ndvi) %>%
  mutate(DOY = 1:length(ndvi))->p1


tibble(ndvi) %>%
  filter(ndvi < median(ndvi)) %>%
  arrange(ndvi) %>%
  mutate(DOY = 1:length(ndvi)) %>%
  bind_rows(tibble(ndvi) %>%
              filter(ndvi > median(ndvi)) %>%
              arrange(desc(ndvi)) %>%
              mutate(DOY = 184:365))->dat


res<-NULL

for(i in 1:30){
  sample(seq(from=0.6,to=0.9,by=0.01),1)->ceil
  tibble(ndvi = scales::rescale(rnorm(182),to=c(0,ceil))) %>%
  arrange(ndvi) %>%
  mutate(DOY = 1:length(ndvi)) %>%
  #bind_rows(tibble(ndvi = 0.5,DOY=183)) %>%
  bind_rows(tibble(ndvi = scales::rescale(rnorm(182),to=c(ceil,0))) %>%
  arrange(desc(ndvi)) %>%
  mutate(DOY = 184:365))->dd1
  dd1 %>% mutate(id = i)->dd2
  bind_rows(dd2,res)->res
}
  



res %>%
  #ggplot(aes(x=DOY,y=ndvi))+geom_point()
  group_by(id) %>%
  mutate(ndvi_cumsum = cumsum(ndvi)) %>%
  mutate(id = as.factor(id)) %>%
  ggplot(data=.)+geom_line(aes(x=DOY,y=ndvi_cumsum,group=id),colour ='grey20',size=0.1)+
  ylab("Cumulated NDVI")+theme_minimal()+theme(axis.text = element_text(size=14,colour = "black"),
                                               axis.title =element_text (size=17))->p1


res %>%
  #ggplot(aes(x=DOY,y=ndvi))+geom_point()
  group_by(id) %>%
  filter(id %in% c(1:18)) %>%
  mutate(ndvi_cumsum = cumsum(ndvi)) %>%
  arrange(id) %>%
  mutate(Pixel = paste("Pixel", id)) %>%
  mutate(Pixel = factor(Pixel,levels = unique(Pixel))) %>%
  ggplot(data=.)+geom_line(aes(x=DOY,y=ndvi_cumsum),colour ='red',size=0.4)+
  facet_wrap(~Pixel, ncol = 6)+theme_bw()




  mutate(id = as.factor(id)) %>%
  ggplot(data=.)+geom_line(aes(x=DOY,y=ndvi_cumsum,group=id),colour ='grey20',size=0.1)+
  ylab("Cumulated NDVI")+theme_minimal()+theme(axis.text = element_text(size=14,colour = "black"),
                                               axis.title =element_text (size=17))->p1


res %>%
  #ggplot(aes(x=DOY,y=ndvi))+geom_point()
  group_by(id) %>%
  mutate(ndvi_cumsum = cumsum(ndvi)) %>%
  ungroup() %>%
  group_by(DOY) %>%
  summarise(ndvi_cumsum = mean(ndvi_cumsum))->dfmean


res %>%
  #ggplot(aes(x=DOY,y=ndvi))+geom_point()
  group_by(id) %>%
  mutate(ndvi_cumsum = cumsum(ndvi),ndvi_cumsum_norm = rescale(ndvi_cumsum,to=c(0,1))) %>%
  mutate(id = as.factor(id))->dfcum

dfcum %>%
  ungroup() %>%
  group_by(DOY) %>%
  summarise(norm_mean = mean(ndvi_cumsum_norm),
            norm_sd = sd(ndvi_cumsum_norm),
            )->dfmean
  
inner_join(dfmean,tibble (DOY = seq(from=1,to=365,by=8)))->dfpoints
  
dfcum %>%
  ggplot(data=.)+geom_line(aes(x=DOY,y=ndvi_cumsum_norm,group=id),colour ='grey20',size=0.1)+
  ylab("Normalised Cumulated NDVI")+
  geom_line(data=dfmean,aes(x=DOY,y=norm_mean),color="red",size=1)+
  geom_point(data = dfpoints,aes(x=DOY,y=norm_mean),colour="red",size=1)+
  geom_errorbar(data=dfpoints,aes(x=DOY,y=norm_mean,ymin=norm_mean-(norm_sd), 
                                  ymax=norm_mean+(norm_sd)), 
                width=1,color="red",size=0.5)+theme_minimal()+
  theme(axis.text = element_text(size=14,colour = "black"),
        axis.title =element_text (size=17))->p2



grid.arrange(p1,p2,ncol=1)->combined
ggsave(combined,filename = "./figures/supplementary/pheno_algorithm.png",
       width = 6, height = 8,dpi = 400)

