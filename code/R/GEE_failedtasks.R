######################################################
# Check failed tasks with GEE and recreate export grid
######################################################

library(tidyverse);library(sf)
library(marcoUtils);library(phenoutils)

# read in export grid
st_read("./gridGEE/fine_gridv1.shp")->phenogrid

# docker list
c("marco_g","marco_g12","papilio","dawn","dusk","roller")->doc.l

# get tasks (all including completed!)
ee_tasklist(indocker=doc.l,year=2003)->taskinfo

# import polygons for failed tasks
ee_uploadfailed(intask=taskinfo,year=2008,ingrid=phenogrid)->gridfail
# re-run failed tasks
ee_runfailed(runtask = gridfail$polyID,year=2008)

# Export everything!

year.l<-(2009:2015)

for (i in 1:length(year.l)){
  
  # 1. exports for a given year
  print(i)
  tibble(V1=paste0("docker exec --env year=",year.l[i]," --env minpoly=1 --env maxpoly=506 marco_g bash -c \"python3 /home/scripts/phenodiv_zeroversion.py\" &")) %>%
    bind_rows(tibble(V1=paste0("docker exec --env year=",year.l[i]," --env minpoly=507 --env maxpoly=1013 marco_g12 bash -c \"python3 /home/scripts/phenodiv_zeroversion.py\" &"))) %>%
    bind_rows(tibble(V1=paste0("docker exec --env year=",year.l[i]," --env minpoly=1014 --env maxpoly=1520 papilio bash -c \"python3 /home/scripts/phenodiv_zeroversion.py\" &"))) %>%
    bind_rows(tibble(V1=paste0("docker exec --env year=",year.l[i]," --env minpoly=1521 --env maxpoly=1689 dawn bash -c \"python3 /home/scripts/phenodiv_zeroversion.py\" &"))) %>%
    bind_rows(tibble(V1=paste0("docker exec --env year=",year.l[i]," --env minpoly=1690 --env maxpoly=1858 dusk bash -c \"python3 /home/scripts/phenodiv_zeroversion.py\" &"))) %>%
    bind_rows(tibble(V1=paste0("docker exec --env year=",year.l[i]," --env minpoly=1859 --env maxpoly=2026 roller bash -c \"python3 /home/scripts/phenodiv_zeroversion.py\" &"))) %>%
    bind_rows(tibble(V1="wait"))->tmp
  write.table(tmp,"./scripts/bash/tmpdocker.sh",quote = F,col.names = F,row.names = F)
  system("chmod u+x ./scripts/bash/tmpdocker.sh")
  system("sh ./scripts/bash/tmpdocker.sh")
  # 2. wait for 8 hours 
  date_time<-Sys.time()
  while((as.numeric(Sys.time()) - as.numeric(date_time))<28800){} 
  
  # 3. get info about status change year
  ee_tasklist(indocker=doc.l,year=year.l[i])->taskinfo
  taskinfo %>%
    filter(str_detect(X3,"READY|RUNNING"))->tasksleft
  
  # 4. if no jobs running get failed tasks
  if(dim(tasksleft)[1]==0){
    taskinfo %>%
      filter(!X3=="COMPLETED")->failed
    # import polygons for failed tasks change year
    ee_uploadfailed(intask=taskinfo,year=year.l[i],ingrid=phenogrid)->gridfail
    # re-run failed tasks
    ee_runfailed(runtask = gridfail$polyID,year=year.l[i]) 
    # wait 3 hours
    date_time<-Sys.time()
    while((as.numeric(Sys.time()) - as.numeric(date_time))<10800){}
    
    # 5. check status every 10 minutes
  } else {
    y=1  
    while(TRUE){
      ee_tasklist(indocker=doc.l,year=year.l[i]) %>% filter(str_detect(X3,"READY|RUNNING"))->tasksleft
      if (dim(tasksleft)[1]==0){
        break          
      } else {
        # wait 10 minutes 
        Sys.sleep(time = 600)
      }
      #print(i)            
      #Time in seconds
      y = y + 1
    } # end of while loop
  } # end of else statement
  
  # 6. Once completed re-run failed exports
  ee_tasklist(indocker=doc.l,year=year.l[i]) %>% filter(!X3=="COMPLETED")->failed
  # import polygons for failed tasks
  ee_uploadfailed(intask=taskinfo,year=year.l[i],ingrid=phenogrid)->gridfail
  # re-run failed tasks
  ee_runfailed(runtask = gridfail$polyID,year=year.l[i])
  # wait 3 hours
  date_time<-Sys.time()
  while((as.numeric(Sys.time()) - as.numeric(date_time))<10800){} 
} # end of for loop


# check missing data
st_read("./gridGEE/fine_gridv1.shp") %>%
  as_tibble() %>%
  dplyr::select(-c(geometry)) %>%
  anti_join(list.files("/mnt/data1tb/alessandro_metric/tempfilt/") %>%
  as_tibble() %>%
  mutate(polyID = as.numeric(str_remove_all(value,"Y_2003_|_naamerica.tif")))) %>%
  inner_join(taskinfo %>%
               filter(str_detect(X2,"naamerica")) %>%
               filter(X3!="COMPLETED") %>% 
               filter(str_detect(X4,"outofmemory")) %>%
               dplyr::select(X2) %>%
               mutate(polyID = as.numeric(str_remove_all(X2,"Y_2003_poly_|naamerica|_"))))->missing

phenogrid %>%
  inner_join(missing %>%
               dplyr::select(polyID))->gridfail
  

# new way of coding missing polygons!
res<-NULL

# upload assets to legacy
for (i in 1:dim(gridfail)[1]){
  print(i)
  fileout=paste0("./tmp/","poly_",gridfail[i,]$polyID,".shp")
  st_make_grid(st_bbox(gridfail[i,]), cellsize = 0.9, square = TRUE,crs=latlon,
               what = "polygons") %>%
    st_as_sf() %>%
    rename(geometry = x) %>%
    mutate(polyID = 1:length(geometry)) %>%
    #mutate(polyID = paste0(gridfail[i,]$polyID,"_",polyID)) %>%
    mutate(subtile =paste0(i,"_",polyID)) %>%
    mutate(polyID =i) ->tmp
  bind_rows(tmp,res)->res
}


res  %>%
  st_write("./tmp/missing_2003.shp",delete_layer=TRUE)


taskinfo %>%
  filter(str_detect(X2,"1963"))
