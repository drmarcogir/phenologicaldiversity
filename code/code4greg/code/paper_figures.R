# Simplied script for Greg
library(tidyverse)


# Read in files -----------------------------------------------------------
# phenodataset
read_csv("./scripts/gregory/data/phenodat.csv")->phenodat

# Fig 1  ------------------------------------------------------------------
# Map ---------------------------------------------------------------------


phenodat %>%
dplyr::select(x,y,value) %>%
  mutate(value2 = value) %>%
  mutate(value2 = cut(value2, breaks = c(0,37.3,56.7,83.2,122.6,179.9), 
                      right = TRUE,  labels = c('0', '37.3', '56.8',
                                                '83.3', '> 122.6'))) %>% {.->> tmp} %>%
  ggplot()+
  #theme_bw()+
  theme(legend.position="none",
        #plot.background=element_rect(fill = 'black'),
        panel.background = element_rect(fill = 'white',color="white"),
        #panel.border = element_rect(colour = "black", fill=NA),
        strip.text = element_text(size = rel(2.5), face = "bold"),
        axis.text = element_blank(),axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = 'transparent'))+
  #geom_sf(data=bbox_rb,fill=NA,size=0.2, color="grey50")+
  #geom_sf(data=wmap_rb, color="grey50",size=0.2)+
  geom_sf(data=world2,color='white',fill='grey5',size=0.09)+
  geom_tile(aes(x, y, fill = value2))+
  xlab("") + ylab("")+
  theme(legend.position = 'bottom', 
        legend.key = element_blank(),
        legend.text = element_text(size = rel(0.8),colour = "black"),
        #legend.background = element_rect(fill="black"),
        plot.title = element_text(hjust = 0.5,vjust = -8,size = 20)) + 
  scale_fill_manual(values = rev(brewer.pal(5, 'Spectral')), na.translate = F,
                    guide = guide_legend(nrow = 1, direction = 'horizontal', 
                                         label.hjust = 0, label.position = 'bottom', 
                                         keywidth = 3.5, keyheight = 0.5, title = ""))->p2

# Histogram of observation counts ---------------------------------------------------------------------
cols <- rev(brewer.pal(5, 'Spectral'))
tmp %>%
  na.exclude() %>%
  ggplot(aes(value2))+
  geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(..x..)))+
  ylab("relative frequencies")+
  xlab("class")+ # remember to modify!!!!!!
  scale_fill_manual(values= cols)+
  # facet_wrap(~variable) +        
  scale_y_continuous(labels=scales::percent)+
  # ylab("relative frequencies")+ 
  scale_x_discrete( drop=FALSE) +
  theme_bw()+theme(axis.text.y = element_text(size=15),legend.position="none",
                   axis.text.x = element_blank(),axis.title.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.title.y = element_text(size = 20))+
  guides(scale="none")->histf


p2+annotation_custom(grob=ggplotGrob(histf), 
                     xmin=-180, xmax=-110,ymin = -105, ymax=-50)->p3


# distribution densities ---------------------------------------------------------------------
phenodat %>%
  #filter(forest_prop > 0.75) %>%
  #filter(value < 180) %>%
  dplyr::select(x,y,value,WWF_MHTNAM) %>%
  inner_join(tibble(WWF_MHTNAM = c("Tundra","Tropical and Subtropical Dry Broadleaf Forests",
                                   "Tropical and Subtropical Grasslands, Savannas and Shrublands",
                                   "Temperate Broadleaf and Mixed Forests",
                                   "Boreal Forests/Taiga",
                                   "Tropical and Subtropical Moist Broadleaf Forests",
                                   "Temperate Conifer Forests") ,
                    WWFMHTNAM1= c("Boreal","Arid","Arid",
                                  "Temperate","Boreal","Tropical","Temperate")) %>%
               mutate(WWFMHTNAM1= factor(WWFMHTNAM1,levels=c("Tropical","Arid","Temperate","Boreal"))))->res


res %>%
  group_by(WWFMHTNAM1) %>%
  summarise(median = median(value)) %>%
  mutate(id=1:length(median))->medres

res %>%
  ggplot(aes(x=value, y=WWFMHTNAM1,fill=WWFMHTNAM1))+
  geom_density_ridges(quantile_lines=TRUE,quantiles = c(0.25,0.5,0.75),jittered_points = FALSE, 
                      point_alpha=1,point_shape=21,
                      scale=1,rel_min_height = 0.01)+
  theme_minimal()+theme(legend.position = "none")+
  scale_fill_manual(values=rev(c("#009E73","#E69F00","#0072B2","#CC79A7")))+
  geom_segment(data = medres1, aes(x = median, xend = median, y = as.numeric(WWFMHTNAM1),
                                   yend = as.numeric(WWFMHTNAM1)+yval_end),
               color = "red")+theme(axis.title = element_blank())->pdensity