library(tidyverse)

# create virtual raster file
# tile tifs up into single file
# read files into grass
# extract statistics
# convert into DOY


setwd("/media/marco/marcodata19/Phenology_original/GEEfiles")
yearl<-as.character(2001:2016)

list.files()->filel
  

metrics<-c("Dormancy")

for (y in 1:length(metrics)){
  
  filel %>%
    .[str_detect(.,metrics[y])] ->tmpmetrics
  
  for (i in 1:length(yearl)){
    
    print(i)
    tmpmetrics %>%
      .[str_detect(.,yearl[i])]->tmpfiles
    
    # new file name
    str_split_fixed(tmpfiles[1],"-",n=2)[,1]->outfile
    paste0("/media/marco/marcodata19/Phenology_original/tiled/",outfile,".tif")->outfile1
    
    
    # create vrt file
    system("rm out.vrt")
    system(command=paste0("gdalbuildvrt out.vrt ",paste0(tmpfiles,collapse =" ")))
    
    # create new raster file file
    system(command=paste0("gdal_translate -co 'COMPRESS=LZW' out.vrt ",outfile1))
    
    # read into GRASS GIS
    #system("g.remove -f type=raster name=SL_2003")
    system(command = paste0("r.in.gdal in=",outfile1," out=",outfile," -l --o"))
    #system(command = paste0("r.in.gdal in=",outfile1," out=tmp -l --o"))
    
    # set to NULL cells
    system(command = paste0("r.null map=",outfile," setnull=0"))
    #system(command = paste0("r.null map=tmp setnull=0 --o"))
    # fix negatives
    #system(command = paste0("r.mapcalc '",outfile," = if(tmp > 0,tmp,null())' --o"))
    
  }

}


# reclassify map
system("g.region raster=GUP_2001")
yearl<-2001:2016

# Dormancy, GUP, Senescence Peak, MidGreenup MidGreendown

for (i in 13:length(yearl)){
  print(i)
  
  # remove any old files
  system("rm -f /media/marco/marcodata19/Phenology_original/tmp1/stats")
  
  # outfile
  paste0("MidGreenup_",yearl[i])->outfile
  
  # get statistics out
  system(command = paste0("r.stats -ln ",outfile," > /media/marco/marcodata19/Phenology_original/tmp1/stats"))
  
  # read in info on days
  read_delim("/media/marco/marcodata19/Phenology_original/tmp1/stats",col_names = F,delim=" ") %>%
    rename(unixdate = X1) %>%
    dplyr::select(-c(X2)) %>%
    mutate(date = map(unixdate,function(x){return(as.Date(as.POSIXct(x*86400, origin="1970-01-01")))})) %>%
    unnest(cols = date) %>%
    mutate(doy = lubridate::yday(date),year=lubridate::year(date))->datedf
  
  # convert data into doy
  datedf %>%
    mutate(doy = ifelse(year==(yearl[i]-1),1-doy,doy)) %>%
    mutate(doy = ifelse(year==(yearl[i]+1),365+doy,doy))->rcldf
  
  # create reclassification table
  rcldf %>%
    mutate(rcl = paste0(unixdate," = ",doy)) %>%
    dplyr::select(rcl) %>%
    write.table(.,"/media/marco/marcodata19/Phenology_original/tmp1/rcl",row.names = F,quote = F,col.names = F)
  
  # reclass map
  system(command =paste0("r.reclass in=",outfile," out=",paste0(outfile,"a")," rules=/media/marco/marcodata19/Phenology_original/tmp1/rcl --o"))
  
  # remove rasterf file
  system("rm -f /media/marco/marcodata19/Phenology_original/tmp1/rcl")
}

  

# 500 m composites

system("g.region raster=GUP_2001")
# GUP 
system("r.series input=GUP_2001a,GUP_2002a,GUP_2003a,GUP_2004a,GUP_2005a output=GUP_slice1 method=median --o")
# Dormancy
system("r.series input=Dormancy_2001a,Dormancy_2002a,Dormancy_2003a,Dormancy_2004a,Dormancy_2005a output=Dormancy_slice1 method=median --o")
# Senescence
system("r.series input=Senescence_2001a,Senescence_2002a,Senescence_2003a,Senescence_2004a,Senescence_2005a output=Senescence_slice1 method=median --o")
# Peak
system("r.series input=Peak_2001a,Peak_2002a,Peak_2003a,Peak_2004a,Peak_2005a,Peak_2006a,Peak_2007a,Peak_2008a output=Peak_slice1 method=median --o")
# MidGreenup
system("r.series input=MidGreenup_2001a,MidGreenup_2002a,MidGreenup_2003a,MidGreenup_2004a,MidGreenup_2005a output=MidGreenup_slice1 method=median --o")
# MidGreendown
system("r.series input=MidGreendown_2001a,MidGreendown_2002a,MidGreendown_2003a,MidGreendown_2004a,MidGreendown_2005a output=MidGreendown_slice1 method=median --o")

system("g.region raster=GUP_2001")
# GUP 
system("r.series input=GUP_2016a,GUP_2015a,GUP_2014a,GUP_2013a,GUP_2012a output=GUP_slice2 method=median --o")
# Dormancy
system("r.series input=Dormancy_2016a,Dormancy_2015a,Dormancy_2014a,Dormancy_2013a,Dormancy_2012a output=Dormancy_slice2 method=median --o")
# Senescence
system("r.series input=Senescence_2016a,Senescence_2015a,Senescence_2014a,Senescence_2013a,Senescence_2012a output=Senescence_slice2 method=median --o")
# Peak
system("r.series input=Peak_2016a,Peak_2015a,Peak_2014a,Peak_2013a,Peak_2012a,Peak_2011a,Peak_2010a,Peak_2009a, output=Peak_slice2 method=median --o")
# MidGreenup
system("r.series input=MidGreenup_2016a,MidGreenup_2015a,MidGreenup_2014a,MidGreenup_2013a,MidGreenup_2012a output=MidGreenup_slice2 method=median --o")
# MidGreendown
system("r.series input=MidGreendown_2016a,MidGreendown_2015a,MidGreendown_2014a,MidGreendown_2013a,MidGreendown_2012a output=MidGreendown_slice2 method=median --o")

c("GUP_slice","Dormancy_slice","Senescence_slice","Peak_slice","MidGreenup_slice","MidGreendown_slice")->maps

c("Peak_slice")->maps


system("g.region raster=GUP_2001")
for (i in 1:length(maps)){
  print(i)
  system(command = paste0("r.mapcalc '",maps[i],"2a = int(",maps[i],"2)' --o"))
}



for (i in 1:length(maps)){
  # set region
  system("g.region raster=GUP_2001")
  # create mask
  #system(command = paste0("r.mapcalc 'tmp = ",maps[i],"1a/",maps[i],"1a' --o"))
  #system("r.mask raster=tmp maskcats=1")
  # calculate metrics
  system("g.region raster=grid5km")
  # slice1 
  system(command = paste0("r.resamp.stats in=",maps[i],"1a"," out=",maps[i],"1_sd method=stddev --o"))
  # slice2
  system(command = paste0("r.resamp.stats in=",maps[i],"2a"," out=",maps[i],"2_sd method=stddev --o"))
  # difference
  system(command = paste0("r.mapcalc 'tmp = ",maps[i],"2_sd-",maps[i],"1_sd","' --o"))
  #system("r.mask -r")
  #ystem("r.mask raster=mask5km maskcats=1")
  # save maps
  system(command =paste0("r.out.gdal in=", maps[i],"1_sd out=/media/marco/marcodata19/Phenology_original/sd_slices_maps/",maps[i],"1_sd.tif --o createopt='COMPRESS=LWZ'"))
  system(command =paste0("r.out.gdal in=", maps[i],"2_sd out=/media/marco/marcodata19/Phenology_original/sd_slices_maps/",maps[i],"2_sd.tif --o  createopt='COMPRESS=LWZ'"))
  system(command =paste0("r.out.gdal in=tmp out=/media/marco/marcodata19/Phenology_original/sd_slices_maps/",maps[i],"_dif.tif --o createopt='COMPRESS=LWZ'"))
  system("r.mask -r")
}







  
system("r.stats -ln in=grid5km,GUP_slice1a")


system("r.mask raster=forest_mask_5km maskcat=1")
system("r.resamp.stats in=GUP_slice1 out=GUP_slice_sd method=stddev --o")
# slice 2
system("r.series input=GUP_2016a,GUP_2015a,GUP_2014a,GUP_2013a,GUP_2012a output=GUP_slice2 method=median --o")
system("r.resamp.stats in=GUP_slice2 out=GUP_slice_sd2 method=stddev --o")

system("r.mapcalc 'dif = GUP_slice_sd2-GUP_slice_sd' --o")
system("r.out.gdal in=dif out=/home/marco/Desktop/dif.tif createopt='COMPRESS=DEFLATE' --o")


system("")








# slice1 
system("g.region raster=GUP_2001")
system("r.series input=GUP_2001a,GUP_2002a,GUP_2003a,GUP_2004a,GUP_2005a,GUP_2006a,GUP_2007a,GUP_2008a output=GUP_slice_8 method=median --o")
system("g.region raster=grid5km")
system("r.mask raster=forest_mask_5km maskcat=1")
system("r.resamp.stats in=GUP_slice_8 out=GUP_slice_8_sd method=stddev --o")
# slice 2
system("g.region raster=GUP_2001")
system("r.series input=GUP_2016a,GUP_2015a,GUP_2014a,GUP_2013a,GUP_2012a,GUP_2011a,GUP_2010a,GUP_2009a output=GUP_slice2_8 method=median --o")
system("g.region raster=grid5km")
#system("r.mask raster=forest_mask_5km maskcat=1")
system("r.resamp.stats in=GUP_slice2_8 out=GUP_slice2_8_sd2 method=stddev --o")

system("r.mapcalc 'dif8 = GUP_slice2_8_sd2-GUP_slice_8_sd' --o")
system("r.out.gdal in=dif8 out=/home/marco/Desktop/dif8.tif createopt='COMPRESS=DEFLATE' --o")
system("r.mask -r")


system("g.remove ")



system("r.mapcalc 'slice_mask = GUP_slice1+GUP_slice2'")
system("r.mapcalc 'slice_mask1 = int(slice_mask/slice_mask)' --o")

system("d.erase")
system("d.rast slice_mask1")
system("r.mask raster=slice_mask1 maskcats=1")


system("r.mask raster=forest_mask_5km maskcats=1")
system("r.mapcalc 'dif = GUP_slice_sd2-GUP_slice_sd' --o")


system("r.out.gdal in=dif out=/home/marco/Desktop/dif2.tif createopt='COMPRESS=LWZ'")
system("r.out.gdal in=GUP_slice1 out=/home/marco/Desktop/GUP_slice1.tif createopt='COMPRESS=LWZ'")

system("r.resamp.stats in=GUP_slice1 out=GUP_slice_sd method=stddev --o")
raster("/home/marco/Desktop/dif2.tif")->dif
st_read("/home/marco/Desktop/terr-ecoregions-TNC/tnc_terr_ecoregions.shp")->ecoreg
ecoreg %>%
  dplyr::select(WWF_MHTNAM)->ecoreg1

beginCluster(n=7)
raster::extract(dif,ecoreg1)->values
endCluster()


ecoreg1 %>%
  mutate(values = values)->ecoreg2

# csv files (to be transferred to master script)

tibble(filename = list.files("/media/marco/marcodata19/phenology_csv",full.names = T,pattern = ".csv")) %>%
  mutate(file_contents = map(filename,read_csv)) %>%
  mutate(metric = str_remove(basename(filename),".csv")) %>%
  mutate(year = as.numeric(str_split_fixed(metric,"_",n=4)[,3]),
         metric = str_split_fixed(metric,"_",n=4)[,1]) %>%
  unnest(cols = file_contents) %>%
  dplyr::select(-c(filename,`system:index`,`.geo`,count)) %>%
  rename(value=zone)->metrics

metrics %>%
  group_by(metric,value) %>%
  summarise(stdDev = median(stdDev)) ->metrics1

metrics1 %>%
  spread(key="metric",value="stdDev") %>%
  na.exclude() %>% {.->>tmp}%>%
  dplyr::select(-c(value)) %>%
  prcomp(.,scale. = T,center = T)->prcompres

prcompres$x %>%
  as_tibble() %>%
  bind_cols(tmp)->dd %>%
  inner_join(as_tibble(rastertodf(raster("/mnt/data1tb/phenology/gridGEE/grid_pheno_grass.tif")))) %>%
  dplyr::select(x,y,PC1,PC2,PC3)->dd

# read in official grid
system("r.in.gdal in=/mnt/data1tb/phenology/gridGEE/grid_pheno_grass.tif out=grid5km --o")
system("g.region raster=grid5km")
system("r.stats -ln grid5km")

system("g.region raster=GUP_2001")
yearl<-as.character(2001:2016)
for (i in 1:length(yearl)){
  print(i)
  system(command=paste0("r.stats -n in=","GUP_",yearl[i],",","Peak_",yearl[i],",","SL_",yearl[i],",","Dormancy_",yearl[i],",","Maturity_",yearl[i],",",
                        "Senescence_",yearl[i],",","MidGreenup_",yearl[i],",","MidGreendown_",yearl[i],",","grid5km > /media/marco/marcodata19/Phenology_original/tmp/stats_",yearl[i]))
  
}


# climate data import
#system("r.in.gdal in=/media/marco/marcodata19/Phenology_original/worldclim/wc2.1_2.5m_bio_1.tif out=MAT")
#system("r.in.gdal in=/media/marco/marcodata19/Phenology_original/worldclim/wc2.1_2.5m_bio_12.tif out=PREC")
#system("g.region raster=MAT")
#system("r.stats -An in=grid5km75,MAT > /media/marco/marcodata19/Phenology_original/tmp_stats/MAT")
#system("r.stats -An in=grid5km75,PREC > /media/marco/marcodata19/Phenology_original/tmp_stats/PREC")
read_delim("/media/marco/marcodata19/Phenology_original/tmp_stats/PREC",delim=" ",col_names = F) %>%
  rename(cat =X1, prec=X2) %>%
  group_by(cat) %>%
  summarise(prec = sum(prec)) %>%
  inner_join(read_delim("/media/marco/marcodata19/Phenology_original/tmp_stats/MAT",delim=" ",col_names = F) %>%
               rename(cat =X1, temp=X2) %>%
               group_by(cat) %>%
               summarise(temp = mean(temp)))->climatedata

#
as_tibble(rastertodf(raster("/mnt/data1tb/Dropbox/phenology/gridGEE/grid5km_more75.tif")))->df


# get biome info!
library(sf)
st_read("/media/marco/marcodata19/Phenology_original/biomes/tnc_terr_ecoregions.shp")->biomes

biomes %>%
  pull(WWF_MHTNAM) %>%
  .[str_detect(.,"For")] %>%
  unique()->forl

res<-NULL

for (i in 1:length(forl)){
  biomes %>%
    filter(WWF_MHTNAM==forl[i]) %>%
    st_write("test.shp",delete_layer=T)
  system("v.in.ogr in=test.shp out=test --o")
  system("v.to.rast in=test out=test use=cat --o")
  system("r.mapcalc 'test1 = test/test' --o")
  system("r.mask raster=test1 maskcats=1")
  tibble(value = as.numeric(system("r.stats -nA in=grid5km75",intern = T)),forest=forl[i])->tmpres
  bind_rows(tmpres,res)->res
  system("r.mask -r")
}

# Phenology data
#system("g.region raster=Peak_slice2_sd")
#system("r.mapcalc 'Peak_dif = Peak_slice2_sd -Peak_slice1_sd'")

#system("r.stats -An in=grid5km75,Peak_slice2_sd > /media/marco/marcodata19/Phenology_original/tmp_stats/Peakstatschange2")

read_delim("/media/marco/marcodata19/Phenology_original/tmp_stats/Peakstatschange1",delim=" ",col_names = F) %>%
  rename(cat=X1,period1=X2) %>%
  inner_join(read_delim("/media/marco/marcodata19/Phenology_original/tmp_stats/Peakstatschange2",delim=" ",col_names = F) %>%
               rename(cat=X1,period2=X2)) %>%
  mutate(reldif = (period2*100)/period1) %>%
  #filter(reldif < 1000) %>%
  mutate(reldif = reldif-100) %>%
  mutate(absdif = period2-period1) %>%
  inner_join(climatedata) %>%
  left_join(read_delim("/media/marco/marcodata19/Phenology_original/tmp_stats/pristinecats",delim=" ",col_names = F) %>%
  dplyr::select(X2) %>%
  rename(cat = X2) %>%
  mutate(type="pristine")) %>%
  mutate(type = ifelse(is.na(type),"non-pristine",type)) %>%
  inner_join(res %>%
               rename(cat = value))->dat1

write_csv(dat1,"/media/marco/marcodata19/Phenology_original/tmp/dat1.csv")
read_csv("/media/marco/marcodata19/Phenology_original/tmp/dat1.csv")->dat1

  
# BINNING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  


dat1 %>%
  filter(temp > -10) %>%
  filter(prec < 6000) %>%
  dplyr::select(prec,temp,period1,period2) %>%
  mutate(
    bin_x = cut(prec, breaks = 30),
    bin_y = cut(temp, breaks = 30)
  ) %>%
  group_by(bin_x, bin_y) %>%
  filter(n() > 10)%>%
  summarise(sig=t.test(period1,period2,paired = T)$p.value) %>%
  ungroup()->sig

midpoint<-function(x){
  as.numeric(str_remove(str_split_fixed(x,",",n=2)[,1],"\\("))->lower 
  as.numeric(str_remove(str_split_fixed(x,",",n=2)[,2],"]"))->upper
  return(lower+(upper-lower)/2)
}

# Heatmap of trends: only those that are significant when t-test is performed
# on bins of the vectors. 

dat1 %>%
  filter(temp > -10) %>%
  filter(prec < 6000) %>%
      #dplyr::select(prec,temp,period1,period2) %>%
      mutate(
        bin_xf = cut(prec, breaks = 30),
        bin_yf = cut(temp, breaks = 30)) %>%
  mutate(bin_x=bin_xf ,bin_y = bin_yf) %>%
  left_join(sig) %>%
  filter(sig < 0.05) %>%
  #mutate(absdif = (period2/period1)-1) %>%
  mutate(absdif = case_when(is.na(sig)~NA_real_,TRUE~absdif)) %>%
  mutate(
    bin_x = cut(prec, breaks = 30),
    bin_y = cut(temp, breaks = 30)
  ) %>%
      group_by(bin_xf, bin_yf) %>%
        summarise(meandif = mean(absdif),prec=max(prec),temp=max(temp)) %>%
      tidyr::complete(bin_xf,bin_yf)->df1

df1 %>%
  mutate(bin_xn=midpoint(bin_xf),bin_yn=midpoint(bin_yf)) %>%
  mutate(bin_xn = bin_xn) %>%
  ggplot(aes(x=bin_yn, y=bin_xn, fill = meandif)) + geom_tile()+theme_minimal()+
  scale_fill_gradient2(low = "blue", high = "red",na.value=rgb(0, 0, 0, alpha=0),
                       limits=c(-20,20),oob=scales::squish)+ylab("Precipitation")+
  xlab("Temperature")+theme(axis.text=element_text(size=18),axis.title = element_text(size=16))->p1


ggsave(p1,filename = "/mnt/data1tb/Dropbox/phenology/figures/trends_t_tests.png",width = 9,
       height=8,dpi=400)



  
# Heatmap :  global  
dat1 %>%
  inner_join(df %>%
               rename(cat =value)) %>%
ggplot(aes(x=prec,y=temp,z=absdif))+
  stat_summary_hex(fun =function(x) {if(length(x[!is.infinite(x)])>=5)(mean(x))else(NA)} ,bins =30,na.rm = TRUE)+
  theme_minimal()+
                    theme(legend.position = "right",legend.title = element_blank(),strip.text.x = element_text(size=12,face="bold"),
                          strip.text.y = element_text(size=14,face="bold"),axis.text=element_text(size=21,colour="black"),axis.title = 
                            element_text(size = 25),strip.background = element_rect(colour = "white", fill = "white"),
                          plot.title = element_text(hjust = 0.5,size=30,face="bold"),legend.text=element_text(size=12))+
  xlab("Precipitation")+ylab("Temperature")+
  scale_fill_gradient2(low = "blue", high = "red",na.value=rgb(0, 0, 0, alpha=0),
                       limits=c(-10,10),oob=scales::squish)->global

ggsave(global,filename = "/mnt/data1tb/Dropbox/phenology/figures/trends/globaltrends.png",width = 10, height=10)


as_tibble(rastertodf(raster("/mnt/data1tb/Dropbox/phenology/gridGEE/grid5km_more75_rob.tif")))->df1


dat1 %>%
  inner_join(df1 %>%
               rename(cat =value)) %>%
  dplyr::select(x,y,absdif) %>%
  rename(value = absdif) %>%
  filter(value > -40) %>%
  filter(value < 40) %>%
  mutate(title="first 8 yrs vs. last 8 yrs")->dat2
  

plot_create(dat2)->p
ggsave(p,filename = "/mnt/data1tb/Dropbox/phenology/figures/trends/globaltrends_map.png",width = 10, height=10)
  
  
dat1 %>%
  inner_join(df1 %>%
               rename(cat =value)) %>%
  dplyr::select(x,y,period1) %>%
  mutate(value = period1) %>%
  filter(value < 160) %>%
  mutate(title ="Composite years 2001-2008")->dat2
  #rename(value = absdif) %>%
  #filter(value > -40) %>%

  mutate(title="first 8 yrs vs. last 8 yrs")


plot_create(dat2)->p
ggsave(p,filename = "/mnt/data1tb/Dropbox/phenology/figures/trends/globaldiv_map.png",width = 13, height=13)
  

dat %>%
  mutate(plot_name = title) %>%
  group_by(plot_name) %>%
  nest() %>%
  mutate(res=map(data,plot_create)) %>%
  ungroup() %>% 
  mutate(fileout = str_replace_all(plot_name,"%","P"),
         fileout = str_replace_all(fileout,"\\)",""),
         fileout = str_replace_all(fileout,"\\(",""),
         fileout = str_replace_all(fileout," ","_"),
         fileout = paste0("/mnt/data1tb/phenology/figures/",fileout,".png")) %>% {.->> maps } %>%
  walk2(.x =.$fileout,.y=.$res,.f=~ggsave(filename=.x,plot=.y,width = 12,height=8))



dat1 %>%
  mutate(plot_name = forest) %>%
  group_by(plot_name) %>%
  nest() %>%
  mutate(res=map(data,~ggplot()+
                   stat_summary_hex(aes(x=prec,y=temp,z=absdif),data=.,fun =function(x) {if(length(x[!is.infinite(x)])>=5)(mean(x))else(NA)} ,
                    bins =30,na.rm = TRUE)+
                   theme_minimal()+theme(legend.position = "right",legend.title = element_blank(),strip.text.x = element_text(size=12,face="bold"),
                         strip.text.y = element_text(size=14,face="bold"),axis.text=element_text(size=21,colour="black"),axis.title = 
                           element_text(size = 25),strip.background = element_rect(colour = "white", fill = "white"),
                         plot.title = element_text(hjust = 0.5,size=30,face="bold"),legend.text=element_text(size=12))+
                   xlab("Precipitation")+ylab("Temperature")+
                   scale_fill_gradient2(low = "blue", high = "red",na.value=rgb(0, 0, 0, alpha=0),
                                        limits=c(-10,10),oob=scales::squish)+ggtitle(unique(.$forest)))) %>%
  mutate(fileout = str_replace_all(plot_name,'(?<![0-9])/(?![0-9])', ""),
         fileout = str_replace_all(fileout," ",""),
         fileout = paste0("/mnt/data1tb/Dropbox/phenology/figures/trends/",fileout,".png")) %>%
  walk2(.x =.$fileout,.y=.$res,.f=~ggsave(filename=.x,plot=.y,width = 10,height=10))
  



dat1 %>%
  group_by(forest) %>%
  #mutate(absdif = scales::rescale(absdif,to=c(-1,1))) %>%
  summarise(mean_dif = mean(absdif)) %>%
  arrange((mean_dif)) %>%
  mutate(change=ifelse(mean_dif<0,"decrease","increase")) %>%
  mutate(forest =factor(forest,levels = c(forest))) %>%
  ggplot()+geom_bar(aes(x=mean_dif,y=forest,fill=change),stat = "identity")+theme_minimal()+
  theme(axis.text = element_text(size=20,colour="black"),legend.position = "none")+ylab("")+xlab("")+
  scale_fill_manual(values=c("blue4","brown3"))->barp

ggsave(barp,filename = "/mnt/data1tb/Dropbox/phenology/figures/trends/barp_map.png",width = 13, height=10)








table(cut(dd$absdif,seq(from=-100,to=100,by=2)))





# Pristine forest stuff
system("v.in.ogr in=/media/marco/marcodata19/Phenology_original/pristineforests/ifl_2016.shp out=pristine")
system("g.region raster=grid5km75")
system("v.to.rast in=pristine out=tmp use=cat --o")
system("r.mapcalc 'pristine = tmp/tmp' --o")
system("r.stats -n in=pristine,grid5km75 > /media/marco/marcodata19/Phenology_original/tmp_stats/pristinecats")


# filter rasters (Peak) for plotting
read_delim("/media/marco/marcodata19/Phenology_original/tmp_stats/Peakstatschange1",delim=" ",col_names = F) %>%
  rename(cat=X1,period1=X2) %>%
  inner_join(read_delim("/media/marco/marcodata19/Phenology_original/tmp_stats/Peakstatschange2",delim=" ",col_names = F) %>%
               rename(cat=X1,period2=X2)) %>%
inner_join(as_tibble(rastertodf(raster("/mnt/data1tb/Dropbox/phenology/gridGEE/grid5km_more75.tif"))) %>%
  rename(cat = value)) %>%
  dplyr::select(x,y,period1) %>%
  rasterFromXYZ()->myr1

writeRaster(myr1,filename="/media/marco/marcodata19/Phenology_original/sd_slices_maps/Peak_slice1_sd_clipped.tif")
