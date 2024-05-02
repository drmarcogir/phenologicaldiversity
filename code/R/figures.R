# load required libraries
library(tidyverse)
library(sf)
library(scales)
library(patchwork)
library(RColorBrewer)
library(viridis)
library(ggridges)
library(ggh4x)
library(gridExtra)

# Read in files -----------------------------------------------------------

#read in support GIS files
world <- st_read("./input/background.gpkg")
# phenodataset
phenodat <- read_csv("./input/dat.csv")
# ecoregions file
ecoreg <- st_read("./input/ecoregions.gpkg")
# variable importance output
varimp <- read_csv("./input/variableimportance.csv")
# partial dependence plots results
pdp <- read_csv("./input/pd2d.csv")
# trend analysis results
trends<-read_csv("./input/beta_trends.csv")
# sensitivity to climatic fluctuations
betaclim<-read_csv("./input/beta_clim.csv")




# Fig 1  ------------------------------------------------------------------

# background raster displaying phenological diversity data

basemap <- phenodat %>%
  dplyr::select(x,y,value) %>%
  mutate(value2 = value) %>%
  mutate(value2 = cut(value2, breaks = c(0,37.3,56.7,83.2,122.6,179.9), 
                      right = TRUE,  labels = c('0', '37.3', '56.8',
                                            '83.3', '> 122.6'))) %>% {.->> tmp} %>%
  ggplot()+
  theme(legend.position="none",
        panel.background = element_rect(fill = 'white',color="white"),
        strip.text = element_text(size = rel(2.5), face = "bold"),
        axis.text = element_blank(),axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = 'transparent'))+
  geom_sf(data=world,color='white',fill='grey5',size=0.09)+
  geom_tile(aes(x, y, fill = value2))+
  xlab("") + ylab("")+
  theme(legend.position = 'bottom', 
        legend.key = element_blank(),
        legend.text = element_text(size = rel(0.8),colour = "black"),
        plot.title = element_text(hjust = 0.5,vjust = -8,size = 20)) + 
  scale_fill_manual(values = rev(brewer.pal(5, 'Spectral')), na.translate = F,
                    guide = guide_legend(nrow = 1, direction = 'horizontal', 
                                         label.hjust = 0, label.position = 'bottom', 
                                         keywidth = 3.5, keyheight = 0.5, title = ""))

# Histogram of observation counts 

histf <- tmp %>%
  na.exclude() %>%
  ggplot(aes(value2))+
  geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(..x..)))+
  ylab("relative frequencies")+
  xlab("class")+
  scale_fill_manual(values= c("#2B83BA","#ABDDA4","#FFFFBF","#FDAE61","#D7191C"))+
  scale_y_continuous(labels=scales::percent)+
  scale_x_discrete( drop=FALSE) +
  theme_bw()+theme(axis.text.y = element_text(size=15),legend.position="none",
                   axis.text.x = element_blank(),axis.title.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.title.y = element_text(size = 20))+
  guides(scale="none")


# get percentiles for each biome
pdat <- phenodat %>%
  group_by(biome) %>%
  summarise(p25 = quantile(value,probs=c(.25)),
            p50 = quantile(value, probs = c(.50)),
            p75 = quantile(value, probs = c(.75)))


# get density estimates for each biome (from ggplot objects)
# this step is needed to create vertical bars within each density plot
densdat <- phenodat %>%
  group_by(biome) %>%
  nest() %>%
  mutate(dens = map(data,~ggplot_build(ggplot(data=.x,aes(value))+geom_density())$data[[1]])) %>%
  # get rid of data column
  dplyr::select(-c(data)) %>%
  unnest(cols = dens) %>%
  inner_join(pdat) %>%
  group_by(biome) %>%
  mutate(dens.25p = approx(x, y, xout = p25)[[2]],
         dens.50p = approx(x, y, xout = p50)[[2]],
         dens.75p = approx(x, y, xout = p75)[[2]])


# create density plot
ggplot() +
  geom_density(data = phenodat, aes(x = value, fill = biome)) +
  geom_segment(data = densdat, aes(x = p25, xend = p25, y = 0, yend = dens.25p)) +
  geom_segment(data = densdat, aes(x = p50, xend = p50, y = 0, yend = dens.50p), colour = "red") +
  geom_segment(data = densdat, aes(x = p75, xend = p75, y = 0, yend = dens.75p)) +
  facet_wrap(~biome, nrow = 1,scales = "free")+
  facetted_pos_scales(x = list(
    biome == "Boreal" ~ scale_x_continuous(breaks = c(32.5-5, 45.1, 66.3),labels = c("32.5","45.1","66.3")),
    biome == "Temperate" ~ scale_x_continuous(breaks = c(40.1, 57.2, 84)),
    biome == "Arid" ~ scale_x_continuous(breaks = c(44.5-5, 56.3, 73.3),labels = c("44.5","56.3","73.3")),
    biome == "Tropical" ~ scale_x_continuous(breaks = c(37.2, 55, 80.7))
  ))+
  scale_fill_manual(values = c("#009E73", "#E69F00", "#0072B2", "#CC79A7"))+
  theme(legend.position = "none", strip.text = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(hjust = 0.5, colour = "black",size = 11, angle = 0),
        axis.ticks.x = element_blank()) -> pdens


# biome inset maps
biomemaps <- ecoreg %>%
  mutate(title = WWF_MHT) %>%
  mutate(id = c(2,1,4,3)) %>%
  arrange(id) %>%
  mutate(fillcol = c("#009E73","#E69F00","#0072B2","#CC79A7")) %>%
  group_by(WWF_MHT) %>%
  nest() %>%
  mutate(bmap = map(data,~ggplot(data =.x,aes())+
                      geom_sf(data=world,fill = "grey60", size = 0)+
                      geom_sf(fill=.x$fillcol, size = 0) + theme_void() +
                      theme(legend.position = "none",
                            plot.title = element_text(hjust = 0.5),
                            strip.text = element_blank()) +
                      ggtitle(.x$title)))->biomemaps



# combine everything together
fig1 <- pdens +
  inset_element(biomemaps[1,]$bmap[[1]], 0.05, 0.3,  0.20, 1.1)+
  inset_element(biomemaps[2,]$bmap[[1]], 0.25, 0.3,  0.56, 1.1)+
  inset_element(biomemaps[3,]$bmap[[1]], 0.48, 0.3, 0.80, 1.1)+
  inset_element(biomemaps[4,]$bmap[[1]], 0.67, 0.3, 1.14, 1.1) + basemap +
  inset_element(histf, -0.01, 0.20, 0.14, 0.45)+
  plot_layout(ncol=1,heights = c(0.8,5),widths = c(6.5,7))

# save figure
ggsave(fig1,filename = "./output/Fig1.png",width = 9*2.1,height=9,dpi=400,
       bg="white")


# Fig 2 ----------------------------------------------------------------
# Barchart: variable importance from Random Forests
imopbplot <- varimp %>%
  arrange(value) %>%
  mutate(label = as_factor(label)) %>%
  ggplot(aes(y=label,x=value))+geom_bar(stat = "identity",color="black",
                                        size=0.5,fill="#0072B2")+
  theme_bw()+
  theme(legend.position = "none",axis.text=element_text(size=11,colour = "black"),
        axis.title = element_text(size = 15))+
  xlab("Variable importance [score]")+ylab("")+xlim(0, 1100)+
  annotate("text",x = Inf, y = Inf,label = "A ", 
           vjust = "top", hjust = "right",
           size=6)

# 2D partial dependence plots
pdpheat <- pdp %>%
  group_by(xvar_name,yvar_name) %>%
  nest() %>%
  mutate(plot = map(data,~ggplot(data = .x, aes(x = x, y = y, fill = yhat)) + 
                      geom_raster()+
                      scale_fill_distiller(palette = "RdYlBu",
                                           limits = c(59,78),
                                           oob = squish,
                                           guide = guide_colourbar(title = "Phenological diversity",
                                                                   title.position = 'bottom',title.hjust = 0.5))+
                    theme_bw() +
                      theme(axis.text = element_text(size = 12, colour = "black"),
                            axis.title = element_text(size = 14),
                            legend.title = element_text(size = 16),
                            legend.position = 'bottom',
                            legend.key.width = unit(2, 'cm'))+
                      xlab(unique(.x$xlab))+ylab(unique(.x$ylab))))


# combine everything together
fig2 <- plot_spacer() + 
  inset_element(imopbplot, 0, 0, 1, 1, align_to = 'panel') +
  pdpheat[1,]$plot[[1]] +
  annotate("text", x = Inf, y = Inf, label = "B ", vjust = "top", hjust = "right", size = 6) +
  pdpheat[2,]$plot[[1]] +
  annotate("text", x = Inf, y = Inf, label = "C ", vjust = "top", hjust = "right", size = 6) +
  pdpheat[3,]$plot[[1]] +
  annotate("text", x = Inf, y = Inf, label = "D ", vjust = "top", hjust = "right", size = 6) +
  pdpheat[4,]$plot[[1]] +
  annotate("text", x = Inf, y = Inf, label = "E ", vjust = "top", hjust = "right", size = 6) +
  pdpheat[5,]$plot[[1]] +
  annotate("text", x = Inf, y = Inf, label = "F ", vjust = "top", hjust = "right", size = 6) +
  plot_layout(guides = 'collect', ncol = 2) &
  theme(legend.position = 'bottom')

ggsave(fig2,filename = "./output/Fig2.png",width = 12, height = 13, dpi = 400)

# Fig 3 -------------------------------------------------------------------

# wrapper function for creating change plot
fig3b<-function(x)
{
  # biome-level summary stats
  x %>%
    mutate(biome= factor(biome,levels=c("Tropical","Arid","Temperate","Boreal"))) %>%
    group_by(biome) %>%
    summarise(betamed = median(beta),
              beta05 = as.numeric(quantile(beta,probs=c(0.25))),
              beta95 = quantile(beta,probs=c(0.75)))->betamed
  # global summary stats
  x %>%
    mutate(biome = "Global") %>%
    summarise(betamed = median(beta),
              beta05 = as.numeric(quantile(beta,probs=c(0.25))),
              beta95 = quantile(beta,probs=c(0.75))) %>%
    pull(betamed)->globalavg
  # create plot
  ggplot(data= betamed,aes(x=biome,y=beta,color = biome)) +coord_flip()+
    geom_segment(aes(x = biome, xend =biome,
                     y = 0, yend = betamed),
                 size = 1)+geom_hline(aes(yintercept = 0), color = "gray70", size = 0.6,
                                      linetype="dashed")+
    geom_point(aes(biome, betamed), size = 7)+theme_minimal()+
    theme(legend.position ='none',axis.text = element_text(color="black",size=12),
          axis.title = element_text(size=15))+
    geom_hline(aes(yintercept = globalavg), color = "red", size = 0.6)+
    geom_point(aes(biome, beta05),shape=17, size = 3)+
    geom_point(aes(biome, beta95),shape=17, size = 3)+xlab("Biome")+
    scale_colour_manual(values=rev(c("#009E73","#E69F00","#0072B2","#CC79A7")))->p1
  
  return(p1)
}


# change plot in phenological diversity
trends %>%
  fig3b()+ylab("Change in forest phenological diversity\n (2020-2011 vs. 2003-2010)")+
  theme(panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 1))+
  annotate("text",x = Inf, y = Inf,label = "\n A  ", 
           vjust = "top", hjust = "right",
           size=6)->p1

# temperature coefficient panel
betaclim %>%
  filter(term=="bio1") %>%
  fig3b()+ylab("Annual Mean Temperature\n")+theme(axis.text.y = element_blank(),
                                                  axis.title.y = element_blank())+
  theme(panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 1))+
  annotate("text",x = Inf, y = Inf,label = "\n B  ", 
           vjust = "top", hjust = "right",
           size=6)->p2

# precipitation coefficient panel
betaclim %>%
  filter(term=="bio12") %>%
  fig3b()+ylab("Annual Precipitation\n")+theme(axis.text.y = element_blank(),
                                               axis.title.y = element_blank())+
  theme(panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 1))+
  annotate("text",x = Inf, y = Inf,label = "\n C  ", 
           vjust = "top", hjust = "right",
           size=6)->p3



# create map insets
ecoreg %>%
  mutate(title = WWF_MHT) %>%
  mutate(id = c(2,1,4,3)) %>%
  arrange(id) %>%
  mutate(fillcol = c("#009E73","#E69F00","#0072B2","#CC79A7")) %>%
  group_by(WWF_MHT) %>%
  nest() %>%
  mutate(bmap = map(data,~ggplot(data =.x,aes())+
                      geom_sf(data=world,fill = "grey60", size = 0)+
                      geom_sf(fill=.x$fillcol, size = 0) + theme_void() +
                      theme(legend.position = "none",
                            plot.title = element_text(hjust = 0.5),
                            strip.text = element_blank()) +
                      ggtitle(.x$title)))->biomemaps

# put everything together
trendfig <- grid.arrange(p1,p2,p3,
             biomemaps[1,]$bmap[[1]],
             biomemaps[2,]$bmap[[1]]+ theme(plot.margin = unit(c(-1,0,0,0), "cm")),
             biomemaps[3,]$bmap[[1]]+ theme(plot.margin = unit(c(-2,0,0,0), "cm")),
             biomemaps[4,]$bmap[[1]]+ theme(plot.margin = unit(c(-3.5,0,0,0), "cm")),
             layout_matrix = rbind(c(1,2,3,4),
                                   c(1,2,3,5),
                                   c(1,2,3,6),
                                   c(1,2,3,7)),
             widths = c(1.6,1.3,1.3,0.8))

# save figure
ggsave(trendfig,filename="./output/Fig3.png",dpi =400,height = 8,width = 15)
