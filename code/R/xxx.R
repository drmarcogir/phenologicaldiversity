library(tidyverse)
library(scales)


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

resX_Y <- res %>% 
  mutate(x = as.numeric(id)%%6) |>
  mutate(x = case_when(x == 0 ~ 6, 
                       # id == 2~ 1, 
                        TRUE ~ x))|>
  mutate(y = case_when(id <= 6 ~ 1, 
                       id <= 12 ~ 2,
                       id <= 18 ~ 3,
                       id <= 24 ~ 4,
                       id <= 30 ~ 5,
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


m <- matrix(AAA2$ndvi.V50,  nrow = 6, ncol = 5)
dim(m) <- c(x = 6, y = 5) # named dim
s <- st_as_stars(m)
names(s) <- 'DOY 50'

m <- matrix(AAA2$ndvi.V100,  nrow = 6, ncol = 5)
dim(m) <- c(x = 6, y = 5) # named dim
s100 <- st_as_stars(m)
names(s100) <- 'DOY 100'

m <- matrix(AAA2$ndvi.V200,  nrow = 6, ncol = 5)
dim(m) <- c(x = 6, y = 5) # named dim
s200 <- st_as_stars(m)
names(s200) <- 'DOY 200'

m <- matrix(AAA2$ndvi.V300,  nrow = 6, ncol = 5)
dim(m) <- c(x = 6, y = 5) # named dim
s300 <- st_as_stars(m)
names(s300) <- 'DOY 300'

m <- matrix(AAA2$ndvi.V350,  nrow = 6, ncol = 5)
dim(m) <- c(x = 6, y = 5) # named dim
s350 <- st_as_stars(m)
names(s350) <- 'DOY 350'

MergedFiles <- merge(c(s100,s,s200,s300,s350))


allspmgsf <- MergedFiles %>%
  split() %>%
  st_as_sf()

sppolys <- allspmgsf %>% gather("sp", "val", -geometry)


# shear matric
sm <- matrix(c(2, 1.2, 0, 1), 2, 2)

# warp the polygon geometries

sppolysTH_tilt3 <- sppolys %>% mutate(geometry = geometry * sm)


sppolysTH_tilt3 <- sppolysTH_tilt3 %>% 
  mutate(sp = as.factor(sp))%>%
  mutate(sp=fct_relevel(sp,c( 'DOY 50',"DOY 100","DOY 200","DOY 300","DOY 350" )))




sppolysTH_tilt3 %>%
  group_by(sp) %>%
  mutate(val = mean(val)) %>%
  ggplot() +
  geom_sf(aes(fill = val)) +
  facet_wrap(~sp, ncol = 1)+
  viridis::scale_fill_viridis(
    direction = -1, name = "NDVI [-]",
    guide = guide_colorbar(title.position = "top", label.position = "bottom"), na.value = "white"
  ) +
  theme_void() +
  theme(legend.position = "bottom") 
ggsave("/home/marco/Desktop/Scheme.png", width = 21, height = 27, dpi = 400)
