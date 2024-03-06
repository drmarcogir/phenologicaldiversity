library(tidyverse)
# check out script for figures...

read_csv("./jeodpp_results/glmnet_allvars.csv") %>%
  filter(variable!="(Intercept)") %>%
  inner_join(readRDS("./rastergrids/g2d.RDS")) %>%
  rename(value = estimate,x=x_2,y=y_2)->dat
  
map_create<-function(x){
  x %>%
    pull(value)->val
  lims =c (quantile(val,probs=c(0.1))[1],quantile(val,probs=c(0.90))[1])
  x %>%
    ggplot() +
    #theme_bw()+
    theme(legend.position="none",
          panel.background = element_rect(fill = 'white',color="white"),
          strip.text = element_text(size = rel(2.5), face = "bold"),
          axis.text = element_blank(),axis.ticks = element_blank(),
          panel.grid.major = element_line(colour = 'transparent'))+
    geom_sf(data=world2,color='white',fill='grey25',size=0.09)+
    geom_tile(aes(x=x,y=y,fill=value))+
    xlab("") + ylab("")+
    theme(legend.position = 'bottom', 
          legend.key = element_blank(),
          legend.text = element_text(size = rel(0.8),colour = "black"),
          plot.title = element_text(hjust = 0.5,vjust = 0,size = 11)) + 
    scale_fill_gradient2(low ="blue", mid = "white",high ="red",oob=squish,limits=lims)+
    guides(fill = guide_colourbar(title="",barwidth=12,barheight =0.5),
           direction = 'horizontal')->p2
  return(p2)
}

dat %>%
  group_by(variable) %>%
  nest() %>%
  mutate(plot = map(data,map_create))->res1

res1[1,]$plot[[1]] + theme(plot.margin = unit(c(-2,0,0,0), "cm"))+
  ggtitle("bio_01")->p1
res1[2,]$plot[[1]] + theme(plot.margin = unit(c(-2,0,0,0), "cm"))+
  ggtitle("bio_12")->p2
res1[3,]$plot[[1]] + theme(plot.margin = unit(c(-2,0,0,-0.5), "cm"))+
  ggtitle("bio_04")->p3
res1[4,]$plot[[1]] + theme(plot.margin = unit(c(-5.5,0,0,-0.5), "cm"))+
  ggtitle("bio_15")->p4
res1[5,]$plot[[1]] + theme(plot.margin = unit(c(-5.5,0,0,-0.5), "cm"))+
  ggtitle("humi")->p5
res1[6,]$plot[[1]] + theme(plot.margin = unit(c(-5.5,0,0,-0.5), "cm"))+
  ggtitle("Htop")->p6
res1[7,]$plot[[1]] + theme(plot.margin = unit(c(-15,0,-5,-0.5), "cm"))+
  ggtitle("bio_01_mean")->p7
res1[8,]$plot[[1]] + theme(plot.margin = unit(c(-15,0,-5,-0.5), "cm"))+
  ggtitle("aridity")->p8
res1[9,]$plot[[1]] + theme(plot.margin = unit(c(-15,0,-5,-0.5), "cm"))+
  ggtitle("bio_15_mean")->p9

grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,ncol=3,nrow=3)->combined

ggsave(combined,filename = "./figures/supplementary/local_response_5km_glmnet.png",
       width=9,height=10,dpi=400)

