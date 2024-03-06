fig <- plot_ly(z = ~volcano)
fig <- fig %>% add_surface()

fig


plot_ly(z = m.dune1, x= x, y=y, type = "surface")


readRDS("./jeodpp_results/pd2dres_Mar22.RDS")->dat
dat[1,]$pd2d[[1]] |>
  as_tibble() |>
  mutate(bio_04_mean = bio_04_mean/1000)->tmp
  
  
  plot_ly(z = tmp$yhat , x=tmp$bio_04_mean , y=tmp$bio_15_mean, type = 'mesh3d',
          intensity=tmp$yhat, colorscale='Viridis')->p1
  
  p2 <- layout(p1, scene = list(xaxis = list(title = "Temp seasonality"), 
                              yaxis = list(title = "Precipitation seasonality"), 
                              zaxis = list(title = "Phenodiversity")
  )
  )
  
  
  
  saveWidget(p2, "./3dplotsforMirco/seasonality.html", selfcontained = F, libdir = "lib")
 
  
dat[2,]$pd2d[[1]] |>
    as_tibble() |>
    mutate(bio_01_mean = bio_01_mean/10)->tmp
  
  
  plot_ly(z = tmp$yhat , x=tmp$bio_01_mean , y=tmp$aridity, type = 'mesh3d',
          intensity=tmp$yhat, colorscale='Viridis')->p1
  
  p2 <- layout(p1, scene = list(xaxis = list(title = "Annual mean temperature"), 
                                yaxis = list(title = "Aridity"), 
                                zaxis = list(title = "Phenodiversity")
  )
  )
  
  saveWidget(p2, "./3dplotsforMirco/ariditytemperature.html", selfcontained = F, libdir = "lib")
  
  
  
   
phenodat |>
  dplyr::select(bio_04_mean,bio_15_mean,x,y) |>
  mutate(bio_04_mean = bio_04_mean/1000) |>
  filter((bio_04_mean > 1.07 & bio_04_mean < 6.53) & (bio_15_mean > 8.5 & bio_15_mean < 20.01)) |>
  dplyr::select(x,y,bio_04_mean,bio_15_mean) |>
  rasterFromXYZ() |>
  writeRaster(filename ='/home/marco/Desktop/test.tif')

phenodat |>
  dplyr::select(bio_04_mean,bio_15_mean,x,y) |>
  mutate(bio_04_mean = bio_04_mean/1000) |>
  filter((bio_04_mean > 0 & bio_04_mean < 6) & (bio_15_mean > 8.5 & bio_15_mean < 70)) |>
  dplyr::select(x,y,bio_04_mean,bio_15_mean) |>
  rasterFromXYZ() |>
  writeRaster(filename ='/home/marco/Desktop/test1.tif')