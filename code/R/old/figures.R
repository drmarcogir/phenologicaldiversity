library(tidyverse);library(raster)
library(marcoUtils)

read_csv("/home/marco/Desktop/phenology/forplotting.csv") %>%
  filter(metric!="DO2_PE2",metric!="PE2",metric!="GUP2_PE2")->dat

raster("/home/marco/Desktop/phenology/grid5km.tif")->gridr

read_delim("/home/marco/Desktop/phenology/cat",delim = " ",col_names = F) %>%
  rename(cat = X1) %>%
  dplyr::select(cat)->catlookup

# Heatmap of trends (mean of several values )
dat %>%
  bind_rows(dat %>%
              mutate(PFT="global")) %>%
  inner_join(tibble(metric=c("DO1_PE1","GUP1_PE1","P1"),
         metric_f=factor(c("Peak_Dormancy","Greenup_Peak","Peak"),
                         levels = c("Peak_Dormancy","Greenup_Peak","Peak")))) %>%
  inner_join(tibble(PFT=c("BrDc","BrEv","global","NeDe","NeEv"),
                    PFT_f=factor(c("BrDc","BrEv","All","NeDe","NeEv"),
                                    levels = c("All","BrDc","BrEv","NeDe","NeEv")))) %>%
  ggplot(.)+aes(x=MAT_5km,y=PREC_5km,z=beta)+scale_fill_gradientn(name="Slope",colours = RColorBrewer::brewer.pal(9, 'RdBu'), limits = c(-1,1), oob = scales::squish)+
  facet_grid(PFT_f~metric_f,scales = "free")+stat_summary_2d(fun=mean,bins=30)+theme_minimal()+
  theme(strip.text = element_text(size=15,face = "bold"),axis.text = element_text(size = 10,face="bold",colour="black"),
        axis.title = element_text(colour = "black",size=15,face="bold"))+
  xlab("Mean Annual Temperature")+ylab("Total annual precipitation")->fig1

dat %>%
  bind_rows(dat %>%
              mutate(PFT="global")) %>%
  inner_join(tibble(metric=c("DO1_PE1","GUP1_PE1","P1"),
                    metric_f=factor(c("Peak_Dormancy","Greenup_Peak","Peak"),
                                    levels = c("Peak_Dormancy","Greenup_Peak","Peak")))) %>%
  inner_join(tibble(PFT=c("BrDc","BrEv","global","NeDe","NeEv"),
                    PFT_f=factor(c("BrDc","BrEv","All","NeDe","NeEv"),
                                 levels = c("All","BrDc","BrEv","NeDe","NeEv")))) %>%
  ggplot(.)+aes(x=MAT_5km,y=PREC_5km,z=beta)+scale_fill_gradientn(name="SE",colours = RColorBrewer::brewer.pal(9, 'RdBu'),limits = c(0,0.5), oob = scales::squish)+
  facet_grid(PFT_f~metric_f,scales = "free")+stat_summary_2d(fun=std.error,bins=30)+theme_minimal()+
  theme(strip.text = element_text(size=15,face = "bold"),axis.text = element_text(size = 10,face="bold",colour="black"),
        axis.title = element_text(colour = "black",size=15,face="bold"))+
  xlab("Mean Annual Temperature")+ylab("Total annual precipitation")->fig2


      
ggsave(fig1,filename = "/home/marco/Desktop/phenology/results/fig1.png",width=9.5,height=10.5,dpi = 800)
ggsave(fig2,filename = "/home/marco/Desktop/phenology/results/fig2.png",width=9.5,height=10.5,dpi = 800)

# maps of trends
