  percentile_df<-function(infiles){
    system("r.mask -r")
    # read in data into GRASS
    res<-NULL
    for(i in 1:length(infiles)){
      # import file
      system(command = paste0("r.in.gdal in=",infiles[i]," out=",str_replace(basename(infiles[i]),".tif","")," --o"))
      tibble(mapname=str_replace(basename(infiles[i]),".tif",""))->tb
      bind_rows(tb,res)->res
    } 
    # calculate median
    system("g.region raster=forest_mask_5km")
    system("r.mask raster=forest_mask_5km maskcats=1")
    system(command=paste0("r.series in=",paste0(res$mapname,collapse = ",")," out=median method=median --o"))
    system("r.mask -r")
    # save file and reproject
    system("rm tmpr1.tif")
    system("r.out.gdal in=median out=tmp.tif createopt='compress=LZW' --o")
    system('gdalwarp -s_srs "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" -t_srs "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" -r near -of GTiff tmp.tif tmpr1.tif')
    # read in file
    as_tibble(rastertodf(raster("tmpr1.tif")))->tmpdf
    return(tmpdf)
  } # end of function
  
  plot_create<-function(indf){
    # dataframe with temporary breaks
    indf %>% 
      mutate(value1=cut(value,breaks=round(unique(quantile(value,probs = seq(0, 1, length.out = 7 + 1))),digits = 2)))->tmpdf
    # create labels
    str_replace(unique(tmpdf$value1),"]","") %>%
      str_replace("\\(","") %>%
      str_split_fixed(",",n=2) %>%
      as.data.frame() %>%
      as_tibble() %>%
      filter(V1!="") %>%
      mutate(V1=as.numeric(as.character(V1)),V2=as.numeric(as.character(V2))) %>%
      arrange(V1)->tmplabs
    mylabs<-c(paste0(tmplabs[1:5,]$V1," - ",tmplabs[1:5,]$V2),paste0(">", tmplabs[1:6,]$V2[6]))
    #mylabs<-c(tmplabs$V1[1:4],paste0(">", tmplabs[1:5,]$V2[5]))
    rev(brewer.pal(7,"Spectral"))->mypal
    # smallest unit
    tmpdf %>%
      mutate(value1=cut(value,breaks=round(unique(quantile(value,probs = seq(0, 1, length.out = 7 + 1))),digits = 2))) %>%
      #mutate(value2=as.factor(as.numeric(cut(value,breaks=breaksinfo)))) %>% 
      filter(!is.na(value1)) %>% {.->>tmp} %>%
      ggplot()+
      geom_sf(data=bbox_df_robin, colour='black',fill='black')+theme_classic()+
      geom_sf(data=wmap_df, fill='grey70',size=0)+
      geom_sf(data=ocean,fill='grey20',size=0)+
      geom_sf(data=wmap_df, linetype="dotted", color="grey70",size=0.2)+
      geom_tile(data=,aes(x=x,y=y,fill=value1))+
      theme(legend.position=c(0.5, -0.1),legend.title = element_blank(),
            legend.justification=c(0.5),legend.key.width=unit(2, "mm"),
            legend.text = element_text(size = rel(1.5)),
            strip.background = element_rect(colour = "white", fill = "white"),
            strip.text = element_text(size = rel(2.5), face = "bold"))+
      scale_fill_manual(labels =mylabs,values = mypal,
                        guide = guide_legend(direction = "horizontal",
                                             keyheight = unit(3, units = "mm"),keywidth = unit(180 / length(unique(tmp$value1)), 
                                  units = "mm"),title.position = 'top',title.hjust = 0.5,label.hjust = 1,
                       nrow = 1,byrow = T,reverse = F,label.position = "bottom"))+xlab("")+ylab("")+facet_wrap(~title)->p
    return(p)
  } # end of function
  
  
  trends_extract<-function(infiles){
    system("r.mask -r")
    infiles %>%
    tibble::enframe(name = NULL) %>%
    mutate(year = as.numeric(str_split_fixed(value,"_",n=3)[,3])) %>%
    arrange(year) ->tmp
    # mask
    system("g.region raster=forest_mask_5km")
    system("r.mask raster=forest_mask_5km maskcats=1")
    # first three years median
    system(command=paste0("r.series in=",paste0(tmp$value[1:3],collapse = ",")," out=median method=median --o"))
    # last three years median
    system(command=paste0("r.series in=",paste0(tmp$value[(length(tmp$value)-2):length(tmp$value)]
  ,collapse = ",")," out=median1 method=median --o"))
    # difference 
    system("r.mapcalc 'dif = median1-median' --o")
    # save file and reproject
    system("rm tmpr1.tif")
    system("r.out.gdal in=dif out=tmp.tif createopt='compress=LZW' --o")
    system('gdalwarp -s_srs "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" -t_srs "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" -r near -of GTiff tmp.tif tmpr1.tif')
    # read in file
    as_tibble(rastertodf(raster("tmpr1.tif")))->tmpdf
    return(tmpdf)
  }
  
  
  read_files<-function(infiles,outmap){
    # read in data into GRASS
    res<-NULL
    for (i in 1:length(infiles)){
      system(command = paste0("r.in.gdal in=",infiles[i]," out=",str_replace(basename(infiles[i]),".tif","")," --o"))
      tibble(mapname=str_replace(basename(infiles[i]),".tif",""))->tb
      bind_rows(tb,res)->res
      }
    # calculate median
    system("g.region raster=forest_mask_5km")
    system("r.mask raster=forest_mask_5km maskcats=1")
    system(command=paste0("r.series in=",paste0(res$mapname,collapse = ",")," out=",outmap," method=median --o"))
    system("r.mask -r")
    # return final map name
    return(outmap)
  }
  
  combine_sn<-function(south,north,outmap){
    # remove any mask that maybe present
    system("r.mask -r --quiet")
    # create inverse (!) mask
    system("g.region raster=forest_mask_5km")
    system(command = paste0("r.mapcalc 'tmp = int(",south,"/",south,")' --o"))
    system("r.mask raster=tmp maskcats=1 -i")
    # calculate temporary map for the north
    system(command = paste0("r.mapcalc 'tmp1 = ",north,"' --o"))
    # remove inverse mask
    system("r.mask -r")
    # patch two maps south and north
    system(command = paste0("r.patch in=",south,",tmp1 out=tmp --o"))
    # save file and reproject
    system("rm -f tmpr1.tif")
    system("r.out.gdal in=tmp out=tmp.tif createopt='compress=LZW' --o")
    system('gdalwarp -s_srs "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" -t_srs "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" -r near -of GTiff tmp.tif tmpr1.tif')
    # read in file
    as_tibble(rastertodf(raster("tmpr1.tif"))) %>%
      mutate(title = outmap)->tmpdf
    return(tmpdf)
  }
  
  
  combine_sn_trends<-function(insouth,innorth){
    # remove any mask that maybe present
    system("r.mask -r --quiet")
    # create mask for southern areas
    system(command=paste0("r.series in=",paste0(insouth,collapse = ",")," out=south_mask method=median --o"))
    system("r.mapcalc 'south_mask1 = int(south_mask/south_mask)' --o")
    # loop through years
    years<-2001:2019
    lookup<-c("SD25","SD50","SD75")
    for (i in 1:length(years)){
      for (y in 1:length(lookup)){
        system("r.mask -r --quiet")
        # south for a given year
        paste0("PC_",lookup[y],"10PSOUTH_",years[i])->tmpsouth
        # north for a given year
        paste0("PC_",lookup[y],"10P_",years[i])->tmpnorth  
        # create inverse (!) mask
        system("g.region raster=forest_mask_5km")
        #system(command = paste0("r.mapcalc 'tmp = int(",tmpsouth,"/",tmpsouth,")' --o"))
        system("r.mask raster=south_mask1 maskcats=1 -i")
        # calculate temporary map for the north
        system(command = paste0("r.mapcalc 'tmp1 = ",tmpnorth ,"' --o"))
        # remove inverse mask
        system("r.mask -r")
        # new name for map
        paste0("PC_",lookup[y],"SHIFTED_",years[i])->mapname
        # patch maps
        system(command = paste0("r.patch in=",tmpsouth,",tmp1 out=",mapname," --o")) 
      }
    }    
  } 

get_allyears<-function(infiles){
  system("g.region raster=forest_mask_5km")
  system("r.mask -r")
  system("r.mask raster=forest_mask_5km maskcats=1")
    res<-NULL
    for (i in 1:length(infiles)){
      # write out file
      system(command = paste0("r.out.gdal in=",infiles[i]," out=tmp.tif createopt='compress=LZW' --o"))
      # reproject file
      system("rm -f tmpr1.tif")
      system('gdalwarp -s_srs "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" -t_srs "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" -r near -of GTiff tmp.tif tmpr1.tif')
      # import file
      as_tibble(rastertodf(raster("tmpr1.tif"))) %>%
        mutate(year = as.numeric(str_split_fixed(infiles[i],"_",n=3)[,3]))->tmpdf
      bind_rows(tmpdf,res)->res
    }
    res %>% 
      group_by(x,y) %>%
      summarise(count = n()) %>%
      filter(count > 9) %>%
      inner_join(res) %>%
      dplyr::select(-c(count))->res1
    system("r.mask -r")
    return(res1)
  }
