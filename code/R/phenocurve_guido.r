library(tidyverse)
library(scales)


# Panel 1: NDVI stack -----------------------------------------------------


library(stars)



res<-NULL



# 
# 
# make_toy_rasterTH <- function(m) {
#   m <- matrix(rnorm(5*6, mean = 20, sd = 2), nrow = 5, ncol = 6)
#   dim(m) <- c(x = 5, y = 6) # named dim
#   s <- st_as_stars(m)
#   names(s) <- 'Canopy Height'
#   s
# }
# 
# 
# for(i in 1:30){
#   sample(seq(from=0.6,to=0.9,by=0.01),1)->ceil
# asdTH <- make_toy_rasterTH()
# 
# }



for(i in 1:400){
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

resX_Y <- res %>% 
  mutate(x = as.numeric(id)%%20) |>
  mutate(x = case_when(x == 0 ~ 20, 
                       # id == 2~ 1, 
                        TRUE ~ x))|>
  mutate(y = case_when(id <= 20~ 1, 
                       id <= 40 ~ 2,
                       id <= 60 ~ 3,
                       id <= 80 ~ 4,
                       id <= 100 ~ 5,
                       id <= 120~ 6, 
                       id <= 140 ~ 7,
                       id <= 160 ~ 8,
                       id <= 180 ~ 9,
                       id <= 200 ~ 10,
                       id <= 220~ 11, 
                       id <= 240 ~ 12,
                       id <= 260 ~ 13,
                       id <= 280 ~ 14,
                       id <= 300 ~ 15,
                       id <= 320~ 16, 
                       id <= 340 ~ 17,
                       id <= 360 ~ 18,
                       id <= 380 ~ 19,
                       id <= 400 ~ 20,
                       TRUE ~ 0)) |>
  dplyr::select(-id)
  


AAA <- st_as_stars(resX_Y, dims = c("x", "y", "DOY"))

# remotes::install_github("marcosci/layer")
# library(layer)
# 
# 
# tilt_map(AAA)

# AAA2 <- st_xy2sfc(AAA, as_points = TRUE)


AAA2 <- AAA %>%
  # split() %>%
  st_as_sf()


m <- matrix(AAA2$ndvi.V50,  nrow = 20, ncol = 20)
dim(m) <- c(x = 20, y = 20) # named dim
s <- st_as_stars(m)
names(s) <- 'DOY 50'

m <- matrix(AAA2$ndvi.V100,  nrow = 20, ncol = 20)
dim(m) <- c(x = 20, y = 20) # named dim
s100 <- st_as_stars(m)
names(s100) <- 'DOY 100'

m <- matrix(AAA2$ndvi.V200,  nrow = 20, ncol = 20)
dim(m) <- c(x = 20, y = 20) # named dim
s200 <- st_as_stars(m)
names(s200) <- 'DOY 200'

m <- matrix(AAA2$ndvi.V300,  nrow = 20, ncol = 20)
dim(m) <- c(x = 20, y = 20) # named dim
s300 <- st_as_stars(m)
names(s300) <- 'DOY 300'

m <- matrix(AAA2$ndvi.V350,  nrow = 20, ncol = 20)
dim(m) <- c(x = 20, y = 20) # named dim
s350 <- st_as_stars(m)
names(s350) <- 'DOY 350'

MergedFiles <- merge(c(s100,s,s200,s300,s350 ))


A1 <-as(s100, "Raster")

ra <- raster::aggregate(A1, fact=5, FUN = max)
srtm_stars1 = st_as_stars(ra) %>%
  st_as_sf()



allspmgsf <- MergedFiles %>%
  split() %>%
  st_as_sf()

sppolys <- allspmgsf %>% gather("sp", "val", -geometry)



sppolys2 <- srtm_stars1 %>% gather("sp", "val", -geometry)



# shear matric
sm <- matrix(c(2, 1.2, 0, 1), 2, 2)

# warp the polygon geometries

sppolysTH_tilt3 <- sppolys %>% mutate(geometry = geometry * sm)
sppolysTH_tilt2 <- sppolys2 %>% mutate(geometry = geometry * sm)


sppolysTH_tilt3 <- sppolysTH_tilt3 %>% 
  mutate(sp = as.factor(sp))%>%
  mutate(sp=fct_relevel(sp,c( 'DOY 50',"DOY 100","DOY 200","DOY 300","DOY 350" )))


sppolysTH_tilt2 %>%
  mutate(sp = "5 km x 5km windows")->sppolysTH_tilt2

sppolysTH_tilt3 %>%
  ggplot() +
  geom_sf(aes(fill = val)) +
  facet_wrap(~sp, ncol = 1)+
  
    viridis::scale_fill_viridis(
    direction = -1, name = "NDVI [-]",
    guide = guide_colorbar(title.position = "top", label.position = "bottom"), na.value = "white"
  ) +
  theme_void() +
  geom_sf(data =sppolysTH_tilt2) +
  theme(legend.position = "bottom",strip.text = element_text(size = 24),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 12))->p1 
# ggsave("Scheme.png", width = 21, height = 27, units = "cm")
ggsave(p1,filename = "./figures/paper/Fig4/components/Scheme.png", width = 5, height = 10, dpi = 800)


# Panel 5: final phenodiversity map ---------------------------------------
library(tidyverse)
library(scales)


library(stars)

A <- st_read('./figures/paper/Fig4/components/FileMarcoClip.gpkg')

A <- st_as_stars(A) 

allspmgsf <- A %>%
  # split() %>%
  st_as_sf()



sppolys <- allspmgsf %>% gather("sp", "val", -geom)




# shear matric
sm <- matrix(c(2, 1.2, 0, 1), 2, 2)

# warp the polygon geometries

sppolysTH_tilt3 <- sppolys %>% mutate(geom = geom * sm)



sppolysTH_tilt3 %>%
  ggplot() +
  geom_sf(aes(fill = val)) +
  geom_sf_text(aes(label = round(val,0)), colour = "grey80",
               fontface = "bold")+
  viridis::scale_fill_viridis(
    direction = -1, name = "PHENODIV [-]",
    guide = guide_colorbar(title.position = "top", label.position = "bottom"), na.value = "white",option= 'A'
  ) +
  theme_void() +
  theme(legend.position = "bottom") 
# ggsave("Scheme.png", width = 21, height = 27, units = "cm")

