i = 1

c("ciao","ciao1","ciao3","ciao4","ciao5","ciao6","ciao7")->myl

# 1. loop through years
for (y in 1:length(myl)){
  print(myl[y])
  # 1 --- launch export tasks for 1 year. wait 7 hours.
  #date_time<-Sys.time()
  #while((as.numeric(Sys.time()) - as.numeric(date_time))<10){} #dummy while loop
  # 2 --- get info about status
  ee_tasklist(indocker=doc.l,year=2006)->taskinfo
  taskinfo %>%
    filter(X3=="RUNNING|READY")->tasksleft
  # 3 --- if no jobs running get failed tasks
  if(dim(tasksleft)[1]==0){
    taskinfo %>%
      filter(!X3=="COMPLETED")->failed
    # import polygons for failed tasks
    ee_uploadfailed(intask=taskinfo,year=2007,ingrid=phenogrid)->gridfail
    # re-run failed tasks
    ee_runfailed(runtask = gridfail$polyID,year=2007)
    # wait 2 hours
    #date_time<-Sys.time()
    #while((as.numeric(Sys.time()) - as.numeric(date_time))<10){} #dummy while loop
  } else {
    # 3 --- keep checking task status every 10 minutes (while loop)!!!!!!!
    #ee_tasklist(indocker=doc.l,year=2006) %>% filter(X3=="RUNNING|READY")->tasksleft
    # if dim of dataframe finished
    i=1  
    while(TRUE){
      ee_tasklist(indocker=doc.l,year=2006) %>% filter(X3=="RUNNING|READY")->tasksleft
      if (dim(tasksleft)[1]==0){
        break           #A condition to break out of the loop
      } else {
        Sys.sleep(time = 10)
        #ee_tasklist(indocker=doc.l,year=2006)->taskinfo
      }
      print(i)            #Run your code
      #Time in seconds
      i = i + 1
    } # end of while loop
    # WHEN COMPLETED ...
    ee_tasklist(indocker=doc.l,year=2006) %>% filter(!X3=="COMPLETED")->failed
    # import polygons for failed tasks
    ee_uploadfailed(intask=taskinfo,year=2007,ingrid=phenogrid)->gridfail
    # re-run failed tasks
    ee_runfailed(runtask = gridfail$polyID,year=2007)
    # WAIT FOR 2 hrs
    #date_time<-Sys.time()
    #while((as.numeric(Sys.time()) - as.numeric(date_time))<10){} #dummy while loop
  }   
}    
    
