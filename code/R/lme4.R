library(tidyverse)
library(lme4)

read_csv("/home/marco/Desktop/datglmm.csv") %>%
  group_by(cat1d) %>%
  summarise(count = n()) %>%
  filter(count > 68) %>%
  dplyr::select(-c(count)) %>%
  inner_join(read_csv("/home/marco/Desktop/datglmm.csv")) %>%
  group_by(cat1d) %>%
  mutate(bio1 = as.numeric(scale(bio1))) %>%
  inner_join(read_csv("./tmp/ecoregions.csv") %>%
  filter(!WWF_MHTNAM %in% c("Inland Water","Mangroves")) %>%
  inner_join(read_csv("/home/marco/Dropbox/phenology/ecoregions/biomes.csv")))->dd

write_csv(dd,file = "/home/marco/Desktop/datglmm.csv")

mod<-lmer(value~bio1+broadclass+(1|cat2d),data=dd)

dd %>%
  ggplot(aes(x=bio1,y=value))+theme_minimal()+geom_bin2d(bins=60)+
  #geom_smooth(method = "lm",color="red",size=1,linetype="dashed",se=FALSE)+
  scale_fill_viridis(option = 'magma',trans="log10")