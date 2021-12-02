##--- Libraries and data ####
list.of.packages <- c("readxl" , "viridis", "randomForest" , "sf", "testit", "dbscan", "pracma", "parallel", "caret", "plyr","pbapply", "lubridate", "data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(install.missing.packages==T){
  if(length(new.packages)) install.packages(new.packages)
}
library(readxl)
library(sf)
library(parallel)
library(randomForest)
library(caret)
library(testit)
library(dbscan)
library(pracma)
library(plyr)
library(pbapply)
library(lubridate)
library(data.table)
library(viridis)
"%ni%"=Negate('%in%')
classification_RF=readRDS("R/RF_gear_release_v2.rds")
###--- Assign ping to trip ####
# This function paste the information from the fishing trip (id of the trip) to the initial dataset. Points that does not fall within a trip are removed
assign_trip = function(data, trip_table){
  
  if("datetime" %in% colnames(data)){
    data$datetime = as.POSIXct(data$datetime, 
                               format='%Y-%m-%d %H:%M:%S', 
                               tz="UTC")
    datatest = data[order(data$datetime), ]
  }else{
    data$datetime = as.POSIXct(paste(data$date, data$time), 
                               format='%Y-%m-%d %H:%M:%S', 
                               tz="UTC")
    datatest = data[order(data$datetime), ]
  }
  
  datatest$trip = NA
  for(kk in 1:nrow(trip_table)){
    xx =  trip_table[kk,]
    ref = which(datatest$datetime >= xx$start_timestamp & datatest$datetime <= xx$end_timestamp)
    datatest$trip[ref] = xx$trip
    }
  datatest=datatest[!is.na(datatest$trip),]
  return(datatest) 
}

###--- Build trip ####
# This function assemble and format the output of the fishing trip table result.
build_trip=function(data, index, xstart, xpos, index2, ports_buffer, ports){
  startid = data[data$rowid == xstart, "rowid"]
  endid   = data[ index-1, "rowid"]
  starttime = data[data$rowid == xstart , "time"]
  endtime =data[index-1, "time"]
  startdata = data[data$rowid == xstart , "date"]
  enddata = data[index-1, "date"]
  port_info = get_port(latitude=data[data$rowid == xstart - xpos, "latitude"], longitude=data[data$rowid == xstart -xpos , "longitude"], ports_buffer = ports_buffer, ports = ports)
  departure = port_info$harbour
  country_departure = port_info$Country
  gsa_departure = port_info$GSA
  port_info = get_port(latitude=data[index-1 , "latitude"], longitude=data[index-1, "longitude"], ports_buffer = ports_buffer, ports = ports)
  arrival = port_info$harbour
  country_arrival = port_info$Country
  gsa_arrival = port_info$GSA
  mmsi = data[data$rowid == xstart , "MMSI"]
  joined = 0
  trip = index2
  xtrip = data.frame(mmsi, trip, startid, endid , startdata, starttime, 
                     enddata, endtime, departure, country_departure, gsa_departure, 
                     arrival, country_arrival , gsa_arrival , joined)
  return(xtrip)
}


###--- Classification workflow ----
# Description: classification workflow function applies the entire the data flow. Data are divided by fishing trips, to apply on each the "Core function" (described below). 
# Results of the core functions are passed to the function "decision gear" (described below) to classify the vessel gear. 
# Depending on this, data are processed to create fishing tracks or, for Purse seines, fishing operation centroids 
classification_workflow=function(data, ports, ports_buffer, coastal_ban_zone, pars, coord_sys, output.type, write.output=F, output.name=F){
  # Divide data into fishing trips
  dat_trip=create_fishing_trip(data=data,
                               ports=ports, 
                               ports_buffer = ports_buffer,
                               coastal_ban_zone = coastal_ban_zone)
  if(isempty(dat_trip)){
    if(output.type=="tracks"){
      # do not return nothing
    }else{
      if("datetime" %in% colnames(data)){
        data$datetime = as.POSIXct(data$datetime, 
                                   format='%Y-%m-%d %H:%M:%S', 
                                   tz="UTC")
        data = data[order(data$datetime), ]
      }else{
        data$datetime = as.POSIXct(paste(data$date, data$time), 
                                   format='%Y-%m-%d %H:%M:%S', 
                                   tz="UTC")
        data = data[order(data$datetime), ]
      }
      results_table=data.frame(MMSI=unique(data$MMSI),datetime=data$datetme ,longitude=data$longitude,latitude=data$latitude,trip=NA, fishing=0, gear=NA)
      return(results_table)
    }
    
  }else{
    # Assign fishing trip information to initial dataset
    data=assign_trip(data=data, 
                     trip_table = dat_trip)
    
    # Split data and apply classifications algorithms
    dat_classified=classification_wrapper(vessel_data = data, 
                                          pars=pars,
                                          write.output = F)
    
    # pass results to decision gear function to identify deployed gear
    gear=decision_gear(dat_classified[["classification_result"]])
    if(write.output ==T){
      
      if(output.name!=FALSE){
        
        outfile=output.name
        
      }else{
        
        xmmsi=unique(dat_classified[["classification_result"]]$MMSI)
        outfile=paste("classification_result",xmmsi,format(Sys.Date(),format="%B%d%Y"),sep="_")
      }
      
      write.csv(dat_classified[["classification_result"]], paste0("results/", outfile ,".csv"), row.names = F)
    }
    
    #identification of fishing points
    results_table=identify_fishing_points(data=dat_classified[["data_labelled"]], gear=gear,coord_sys=coord_sys)
    if(output.type=="tracks"){
      ### transform points into LINESTRINGS
      results_table=make_fishing_tracks(results_table, wgs, pars)
      return(results_table)
    }else{
      
      return(results_table)
      
    }
  }
  
}



###--- Classification wrapper ####
classification_wrapper=function(vessel_data, pars, write.output = F, output.name=F){
  start_month=aggregate(datetime~trip, vessel_data, function(x) month(min(x)))
  colnames(start_month)[ncol(start_month)] = "start_month"
  vessel_data=merge(vessel_data, start_month, by="trip")
  vessel_data=split(vessel_data, vessel_data$start_month)
  myres=lapply(vessel_data, function(xvessel_data){
    
    trip_list = split(xvessel_data, xvessel_data$trip)
    trip_analysis=pblapply(trip_list, function(i){
      process_trip=core_function(i, pars)
      return(process_trip)
    })
    
    ## extract results ##
    data_for_gear=do.call(rbind, lapply(trip_analysis, function(i){
      i=i[[1]]
      return(i)
    }))
    data_for_points=do.call(rbind, lapply(trip_analysis, function(i){
      i=i[[2]]
      return(i)
    }))
    
    if(write.output ==T){
      
      if(output.name!=FALSE){
        
        outfile=output.name
        
      }else{
        
        xmmsi=unique(data_for_gear$MMSI)
        outfile=paste("classification_result",xmmsi,format(Sys.Date(),format="%B%d%Y"),sep="_")
      }
      
      write.csv(data_for_gear, paste0("results/", outfile ,".csv"), row.names = F)
    }
    
    xresult=list(classification_result=data_for_gear, data_labelled=data_for_points)
    return(xresult)
  })
  
  data_for_gear=do.call(rbind, lapply(myres, function(i){
    i=i[[1]]
    return(i)
  }))
  data_for_points=do.call(rbind, lapply(myres, function(i){
    i=i[[2]]
    return(i)
  }))
  result=list(classification_result=data_for_gear, data_labelled=data_for_points)
  return(result)
}






###--- Check cluster----
# This function analyze the speed profile of the points contained within a spatial cluster, and indicates if proportion of the point indicated by the target speed is above a specified threshold. 
check_cluster=function(data, gear, threshold, low_speed){
  xvar=paste0("cluster_", gear)
  data=data[!is.na(data[,xvar]),]
  if(nrow(data) > 0){
    points_by_speed=aggregate(list(n=data$id_ping), 
                              by=list(xcluster=data[,xvar] ,cluster=data$cluster), 
                              FUN=length)
    points_by_cluster=aggregate(list(tot=points_by_speed$n), 
                                by=list(xcluster=points_by_speed$xcluster), 
                                FUN=sum)
    summary_points=merge(points_by_speed, points_by_cluster, by="xcluster")
    summary_points=summary_points[summary_points$cluster %in% low_speed,]
    if(nrow(summary_points)>0){
      summary_points$perc=summary_points$n/summary_points$tot
      summary_points=aggregate(list(perc=summary_points$perc), 
                               by=list(summary_points$xcluster), 
                               FUN=sum)
      summary_points=summary_points[summary_points$perc > threshold,]
    }else{
      summary_points=data.frame(perc=as.numeric())
    }
  }else{
    summary_points=data.frame(perc=as.numeric())
  }
  return(summary_points)
}

###--- Classify ####
# this function identify the size of each track
classify=function(data){
  if(nrow(data)>0){
    data=aggregate(list(ping_number=data$ping_number),  
                   by=list(cluster=data$cluster), 
                   FUN=sum)
  }else{
    data=data.frame(cluster=as.numeric(), ping_number=as.numeric())
  }
  return(data)
}

###--- Classify speed ---####
# Description: this function applies the kmeans algorithm on fishing speed data, then it use time information to identify transmission gaps (or data with time lag > parameter specified by user). 
# Classification results are homogenized with a lookahed and finally points are clustered into tracks basing on kmeans result and time information
classify_speed=function(data, gear, xcentroids, pars){
  cent=xcentroids[, gear]
  
  ### Evaluate if possible to execute kmeans
  if(has_error(kmeans(data$speed_smooth, cent, algorithm=c("Forgy")))==TRUE){
    print(paste0("kmeans failed in ",unique(data$trip)))
    result=data.frame(MMSI=unique(data$MMSI), 
                      trip=unique(data$trip), 
                      otb=-2, 
                      ptm=-2, 
                      tbb=-2)
    return(result)
  } else{
    speed_classification=kmeans(data$speed_smooth, cent, algorithm=c("Forgy"))
    data$cluster=speed_classification$cluster+2
    data$interval=0
    for(ii in 2:nrow(data)){
      data[ii, "interval"]=difftime(data[ii,"datetime"], 
                                    data[ii-1,"datetime"], units="mins")
    }
    data$data_block=1
    
    ## identify transmission gaps and noise in classification
    for(k in 2:nrow(data)){
      if(data[k,"interval"] > pars$timepar){
        data[k-1,"cluster"]=1
      }else{
        if(data[k,"cluster"]!=data[k-1,"cluster"]){
          data[k-1,"cluster"]=2
        }
      }
    }
    
    # calculate transmission gaps duration
    gaps_duration=sum(data[data$interval>pars$timepar,]$interval)
    
    ## remove false negatives with a lookhead of three
    for(k in 1:(nrow(data)-3)){
      if(data[k,"cluster"] !=data[k+1,"cluster"]){
        if(data[k,"cluster"]==data[k+2,"cluster"]&data[k,"cluster"]==data[k+3,"cluster"]){
          data[k+1,"cluster"]=data[k,"cluster"]
        }
      }
    }
    
    ## define tracks
    for(k in 2:nrow(data)){
      data[k,"data_block"]=data[k-1,"data_block"]
      if(data[k,"interval"] > pars$timepar | data[k,"cluster"] !=data[k-1,"cluster"]){
        data[k,"data_block"]=data[k-1,"data_block"]+1
      }
    }
    return(list(data, gaps_duration))
  }
}



###--- Core function #### 
# Description: the core function process one fishing trip at time. It search for spatial clusters of points by appliying a dbscan algorithm and then it classify the fishing data basing on the speed by applying a kmeans algorithm. Information on the spatial cluster and on the speed classification are evaluated within a set of rules designed to identify a range of fishing gears. 
core_function=function(trip_data, pars){
  print (paste("MMSI:", unique(trip_data$MMSI),"; start month:", unique(trip_data$start_month), "; trip:", unique(trip_data$trip), sep=" "))
  ## format timestamp
  if("datetime" %in% colnames(trip_data)){
    trip_data$datetime = as.POSIXct(trip_data$datetime, 
                                    format='%Y-%m-%d %H:%M:%S', 
                                    tz="UTC")
  }else{
    trip_data$datetime = format(as.POSIXct(paste(trip_data$date, trip_data$time), 
                                           tz="UTC"), format='%Y-%m-%d %H:%M:%S')
    
  }
  
  trip_data=trip_data[ , c("MMSI", "datetime","latitude", "longitude", "speed","trip", "start_month") ]
  trip_data=trip_data[order(trip_data$datetime), ]
  
  trip_data$year=as.numeric(as.character(lubridate::year(trip_data$datetime)))
  trip_data$id_ping=paste0(trip_data$MMSI, "_", trip_data$trip, "_", trip_data$datetime)
  
  # verify if there are enough data with speed > 0 (less than 90%) 
  speed_check=trip_data
  speed_check$speed=floor(speed_check$speed)
  speed_check=data.frame(table(speed_check$speed))
  names(speed_check)=paste(c("speed", "n"))
  speed_check$zero_ratio=speed_check$n/sum(speed_check$n)
  speed_check$discard=ifelse(speed_check$speed==0 & speed_check$zero_ratio > 0.9 , 1,0)
  speed_check=speed_check[speed_check$discard==1,]
  
  ## get fishing trip information
  trip_duration=difftime(trip_data[nrow(trip_data),]$datetime ,trip_data[1,]$datetime, units="mins")
  ping_number=nrow(trip_data)
  start_month=trip_data[1, "start_month"]
  
  ## if there are more than 90% of data with speed=0 skip the fishing trip 
  if(nrow(speed_check)!=0){
    print(paste0("only zero in ", unique(trip_data$trip)))
    classification_result=data.frame(MMSI=unique(trip_data$MMSI), 
                                     trip=unique(trip_data$trip), 
                                     otb1=-1, 
                                     otb2=-1, 
                                     ptm=-1, 
                                     tbb=-1, 
                                     ps=-1, 
                                     gaps=0.99, 
                                     n_ping=-1, 
                                     start_month=start_month)
    fish_paths=data.frame(MMSI=unique(trip_data$MMSI),
                          trip=unique(trip_data$trip),
                          datetime=trip_data$datetime,
                          latitude=trip_data$latitude, 
                          longitude=trip_data$longitude, 
                          otb1=-1, 
                          ptm=-1, 
                          tbb=-1, 
                          ps=-1, 
                          otb2=-1, 
                          start_month=start_month)
    
    return(list(classification_result, fish_paths))
  }else{
    
    ## if there are less than 12 points skip the fishing trip
    if(ping_number<12){
      print(paste0("few data in ",unique(trip_data$trip)))
      classification_result=data.frame(MMSI=unique(trip_data$MMSI), 
                                       trip=unique(trip_data$trip), 
                                       otb1=-1, 
                                       otb2=-1, 
                                       ptm=-1, 
                                       tbb=-1, 
                                       ps=-1,  
                                       gaps=0.99, 
                                       n_ping=ping_number, 
                                       start_month=start_month)
      fish_paths=data.frame(MMSI=unique(trip_data$MMSI),
                            trip=unique(trip_data$trip),
                            datetime=trip_data$datetime,
                            latitude=trip_data$latitude, 
                            longitude=trip_data$longitude, 
                            otb1=-1, 
                            ptm=-1, 
                            tbb=-1, 
                            ps=-1, 
                            otb2=-1, 
                            start_month=start_month)
      return(list(classification_result, fish_paths))
    }else{
      
      # look for spatial clusters, different parameters are provided for purse seines and pelagic trawlers
      spatial_cluster_ptm=search_cluster(data=trip_data, pars = pars, gear = "ptm")
      trip_data$cluster_ptm=spatial_cluster_ptm$id_cluster
      spatial_cluster_ps=search_cluster(data=trip_data, pars = pars, gear = "ps")
      trip_data$cluster_ps=spatial_cluster_ps$id_cluster
      
      # smooth speed data and apply kmeans with all the gears initial centroids 
      trip_data$speed_smooth=savgol(trip_data$speed, fl=5, forder=2)
      data_otb1=classify_speed(data=trip_data, gear="otb1", xcentroids=centroids, pars = pars)[[1]]
      data_otb2=classify_speed(data=trip_data, gear="otb2", xcentroids=centroids, pars = pars)[[1]]
      data_ptm=classify_speed(data=trip_data, gear="ptm", xcentroids=centroids, pars = pars)[[1]]
      data_tbb=classify_speed(data=trip_data, gear="tbb", xcentroids=centroids, pars = pars)[[1]]
      data_ps=classify_speed(data=trip_data, gear="ps", xcentroids=centroids, pars = pars)[[1]]
      gaps_duration=classify_speed(data=trip_data, "otb1", xcentroids=centroids, pars = pars)[[2]]
      
      ## classification OTB
      tracks_otb1=make_tracks_lite(data=data_otb1, gear="otb1")
      validation_tracks_otb1=tracks_otb1[,c("data_block", "cluster")]
      names(validation_tracks_otb1)[2]="valid_cluster"
      data_otb1=merge(data_otb1, validation_tracks_otb1, by="data_block", all.x=T)
      data_otb1=data_otb1[order(data_otb1$datetime) , ]
      data_otb1$cluster=ifelse(data_otb1$cluster==4 & is.na(data_otb1$valid_cluster)==T, 
                               2,data_otb1$cluster)
      res_otb1=classify(data=tracks_otb1)
      if(nrow(res_otb1)==0){
        result_otb1=0
      }else{
        result_otb1=ifelse(res_otb1[res_otb1$cluster==4,]$ping_number/sum(res_otb1$ping_number) >=pars$ratio_otb,
                           1,0)
      }
      
      ## classification BOT (BOT is just an alternative set of centroids for OTB)
      tracks_otb2=make_tracks_lite(data=data_otb2, gear="otb2")
      validation_tracks_otb2=tracks_otb2[,c("data_block", "cluster")]
      names(validation_tracks_otb2)[2]="valid_cluster"
      data_otb2=merge(data_otb2, validation_tracks_otb2, by="data_block", all.x=T)
      data_otb2=data_otb2[order(data_otb2$datetime) , ]
      data_otb2$cluster=ifelse(data_otb2$cluster==4 & is.na(data_otb2$valid_cluster)==T, 
                               2, 
                               data_otb2$cluster)
      res_otb2=classify(data=tracks_otb2)
      if(nrow(res_otb2)==0){
        result_otb2=0
      }else{
        result_otb2=ifelse(res_otb2[res_otb2$cluster==4,]$ping_number/sum(res_otb2$ping_number) >=pars$ratio_otb, 1, 0)
      }
      
      ## classification TBB
      tracks_tbb=make_tracks_lite(data=data_tbb, gear="tbb")
      validation_tracks_tbb=tracks_tbb[,c("data_block", "cluster")]
      names(validation_tracks_tbb)[2]="valid_cluster"
      data_tbb=merge(data_tbb, validation_tracks_tbb, by="data_block", all.x=T)
      data_tbb=data_tbb[order(data_tbb$datetime) , ]
      data_tbb$cluster=ifelse(data_tbb$cluster==5 & is.na(data_tbb$valid_cluster)==T , 2, data_tbb$cluster)
      res_tbb=classify(data=tracks_tbb)
      if(nrow(res_tbb)==0){
        result_tbb=0}else{
          if(nrow(res_tbb[res_tbb$cluster==5,])==0){
            result_tbb=0
          }else{
            result_tbb=ifelse(res_tbb[res_tbb$cluster==5,]$ping_number/sum(res_tbb$ping_number) >=pars$ratio_tbb,1,0)
          }
        }
      ## classification PTM
      # check mean speed in PTM clusters: if there are more than 80% of low speed values this is not a PTM
      tracks_ptm=make_tracks_lite(data=data_ptm, gear="ptm")
      validation_tracks_ptm=tracks_ptm[,c("data_block", "cluster")]
      names(validation_tracks_ptm)[2]="valid_cluster"
      data_ptm=merge(data_ptm, validation_tracks_ptm, by="data_block", all.x=T)
      data_ptm=data_ptm[order(data_ptm$datetime) , ]
      data_ptm$cluster=ifelse(data_ptm$cluster==4 & is.na(data_ptm$valid_cluster)==T , 2, data_ptm$cluster)
      if(nrow(check_cluster(data=data_ptm, gear="ptm", threshold=0.8, low_speed=3))==0){
        result_ptm=NA;result_ptm
      }else{
        result_ptm=0 
      }
      if(is.na(result_ptm)){
        res_ptm=classify(data=tracks_ptm)
        if(nrow(res_ptm)==0){
          result_ptm=0
        }else{
          
          ## If hauls are too long this is not a PTM
          check_haul_duration=tracks_ptm[tracks_ptm$cluster==4 & tracks_ptm$ping_number > 25,]
          result_ptm=ifelse(nrow(check_haul_duration)==0 & 
                              res_ptm[res_ptm$cluster==4,]$ping_number/sum(res_ptm$ping_number) >=pars$ratio_ptm, 1,0)
        }
      }
      
      ### Classification PS
      #check mean speed in PS clusters: if there are less than 70% of low speed values this is not a PS
      if(nrow(check_cluster(data=data_ps, gear="ps", threshold=0.7, low_speed=c(2,3)))==0){
        result_ps=0
      }else{
        tracks_ps=make_tracks_lite(data=data_ps, gear="ps")
        res_ps=classify(data=tracks_ps)
        if(nrow(res_ps)==0){
          result_ps=0
        }else{
          if(res_ps[res_ps$cluster==3,]$ping_number > pars$size_ps){
            result_ps=0
          }else{
            result_ps=1
          }
        }
      }
      
      # store classified data for later
      fish_paths=data.frame(MMSI=unique(trip_data$MMSI),
                            trip=unique(trip_data$trip),
                            datetime=trip_data$datetime,
                            latitude=data_otb1$latitude, 
                            longitude=data_otb1$longitude, 
                            otb1=data_otb1$cluster, 
                            ptm=data_ptm$cluster, 
                            tbb=data_tbb$cluster, 
                            ps=data_ps$cluster_ps, 
                            otb2=data_otb2$cluster, 
                            start_month=start_month)
      classification_result=data.frame(MMSI=unique(trip_data$MMSI), 
                                       trip=unique(trip_data$trip), 
                                       otb1=result_otb1, 
                                       otb2=result_otb2, 
                                       ptm=result_ptm, 
                                       tbb=result_tbb, 
                                       ps=result_ps, 
                                       gaps=gaps_duration/as.numeric(trip_duration), 
                                       n_ping=ping_number, 
                                       start_month=start_month)
      return(list(classification_result, fish_paths))
    }
  }
}


###--- Create fishing trip ####
#The create_fishing_trip function aims to identify the fishing trips of each vessel. A fishing trip is composed by the sequence of points broadcasted by a vessel, from when it leaves the port until it returns. To run the function, four datasets are required: the sequence of AIS positions of a vessel, the coastal_ban_zone layer and the 2 layers related to the ports
create_fishing_trip <- function(data, ports,  ports_buffer, coastal_ban_zone){
  if(nrow(data )< 10){
    trip_table=NULL
    return(trip_table)
  }else{
    if("datetime" %in% colnames(data)){
      data$datetime = as.POSIXct(data$datetime, 
                                 format='%Y-%m-%d %H:%M:%S', 
                                 tz="UTC")
      data$date=format(data$datetime, format='%Y-%m-%d', 
                       tz="UTC")
      data$time=format(data$datetime, format='%H:%M:%S', 
                       tz="UTC")
      datatest = data[order(data$datetime), ]
    }else{
      data$datetime = format(as.POSIXct(paste(data$date, data$time), 
                                        tz="UTC"), 
                             format='%Y-%m-%d %H:%M:%S', 
                             tz="UTC")
      datatest = data[order(data$datetime), ]
    }
    
    datatest$rowid=seq(1:nrow(datatest))
    inport = find_inport(datatest, ports)
    datatest$inport = ifelse(datatest$rowid %in% inport, 1, 0)
    
    # Initialization
    first=datatest[1,]
    start=0
    k=1
    finish=0
    inport=0
    pos=4
    deltat=4
    buffer=0
    inizio=0
    delta=0
    check=1
    queue.s=datatest[1:4,"inport"]
    queue.v=datatest[1:4,"speed"]
    if(sum(queue.s)==4){
      if(mean(queue.v) < 6 ){
        start=datatest[4,"rowid"]
        inport=1
        pos=3
        buffer=1
        inizio=1
      }
    }else{
      t1=datatest[1,"datetime"]
      t2=datatest[4,"datetime"]
      if(as.numeric(difftime(t2,t1, units="hours"))>deltat){
        start=datatest[4,"rowid"]
        inport=1
        pos=3
        inizio=1
      }
    }
    
    # trip creation
    pb = txtProgressBar(min = 0, max = nrow(datatest) - 5, initial = 0)
    for(j in 5:nrow(datatest)){
      queue.s=c(queue.s[2:4] , datatest[j,"inport"])
      queue.v=c(queue.v[2:4], datatest[j,"speed"])
      if(start == 0 & inport==0 ){
        if(sum(queue.s) == 4 & mean(queue.v) < 6){
          start=datatest[j,"rowid"]
          inport=1
          pos=3
          buffer=1
          inizio=1
        }else{
          t1 = datatest[j-1,"datetime"]
          t2 = datatest[j,"datetime"]
          if(as.numeric(difftime(t2,t1, units="hours"))>deltat){
            start=datatest[j,"rowid"]
            inport=1
            pos=3
            inizio=1
          }
        }
      }else{
        # 140
        if(inizio == 1){
          if(buffer == 1 & inport == 1){
            if(sum(queue.s)==4){
              start=datatest[j,"rowid"]
            }else if(sum(queue.s) == 0){
              inport = 0
              buffer = 0
            }
          }else if(sum(queue.s)==0){ 
            inport=0
          }
        }
        # 154
        if(inizio == 0){
          if(sum(queue.s) ==4 & inport == 0){
            if(mean(queue.v) < 6){
              start = datatest[j,"rowid"]
              inport = 1
              pos = 3
              buffer = 1
              inizio = 1
              delta = 0
            }
          }else{
            if(delta == 1){
              start = datatest[j,"rowid"]
              inport = 1
              pos = 3
              buffer = 1
              inizio = 1
              delta = 0
            }else{
              if(buffer==0 & inport==0){
                t1 = datatest[j-1,"datetime"]
                t2 = datatest[j,"datetime"]
                if(as.numeric(difftime(t2,t1, units="hours"))>deltat){
                  start=datatest[j,"rowid"]
                  inport=1
                  pos=3
                  inizio=1
                }
              }
            }
          }
        }else{
          # 190
          if(inizio==1 & buffer == 0){
            if(sum(queue.s)==4 & inport == 0){
              if(mean(queue.v )< 6){
                ti=datatest[datatest$rowid == start +1 , "datetime"]
                tj=datatest[datatest$rowid == start , "datetime"]
                if(as.numeric(difftime(ti , tj, units="hours")) > 1 ){
                  start= start +1
                }
                if(start > datatest[j-1,"rowid"]){
                  start = start -1
                }
                
                trip_table_j=build_trip(data = datatest, index = j, xstart = start , xpos = pos, index2 = k, ports_buffer = ports_buffer, ports=ports)
                if(exists("trip_table")){
                  trip_table=rbind(trip_table, trip_table_j)
                }else{
                  trip_table=trip_table_j
                }
                k=k+1
                start = datatest[ j , "rowid"]
                finish = 0
                inport = 0
                inizio = 0
                delta = 0
                inizio = 1
                buffer = 1
                inport = 1
              }
            }
            # 259
            if(inport == 0){
              if(exists("trip_table")){
                if(trip_table[trip_table$trip == k-1 , "endid"] != datatest[datatest$rowid == j, "rowid"]){
                  check=1
                }else{
                  check=0
                }
              }
              if(check == 1 ){
                t1=datatest[j-1 , "datetime"]
                t2=datatest[j , "datetime"]
                if(as.numeric(difftime(t2,t1, units="hours"))>deltat){ 
                  ti=datatest[datatest$rowid == start +1 , "datetime"]
                  tj=datatest[datatest$rowid == start , "datetime"]
                  if(as.numeric(difftime(ti , tj, units="hours")) > 1){
                    start = start +1
                  }
                  if( start > datatest[j-1 , "rowid"]){ 
                    start = start -1 
                  }
                  
                  trip_table_j=build_trip(data = datatest, index = j , xstart = start, xpos = pos, index2 = k, ports_buffer = ports_buffer, ports=ports)
                  if(exists("trip_table")){
                    trip_table=rbind(trip_table, trip_table_j)
                  }else{
                    trip_table=trip_table_j
                  }
                  k = k +1
                  start = datatest[j, "rowid"]
                  finish = 0
                  delta = 1  
                  inizio = 1
                  inport = 1
                }
              }
            }
          }
        }
      }
      setTxtProgressBar(pb,j)
    }
    cat("\n", "trip create complete!!!")
    if(exists("trip_table")){
      sz=nrow(trip_table)
      # trip recovery
      if(sz > 1){
        cat("\n", "Recovery trip")
        i=1
        while(i < sz ){
          xdep=is.na(trip_table[i+1, "departure"])
          xarr=is.na(trip_table[i, "arrival"])
          if(xdep == TRUE & xarr == TRUE ){
            finish=trip_table[i, "endid"]
            start=trip_table[i+1, "startid"]
            lat_start=datatest[datatest$rowid == start , "latitude"]
            lon_start=datatest[datatest$rowid == start , "longitude"]
            lat_end=datatest[datatest$rowid == finish , "latitude"]
            lon_end=datatest[datatest$rowid == finish , "longitude"]
            if(trip_table[i+1, "startid"] - trip_table[i, "endid"] > 1 ){
              #break
              arrival=closest_port(lon_end , lat_end, ports)
              departure=closest_port(lon_start , lat_start, ports)
              trip_table[i+1, "departure"]=departure$harbour
              trip_table[i+1, "country_departure"]=departure$Country
              trip_table[i+1, "gsa_departure"]=departure$GSA
              trip_table[i, "arrival"]=arrival$harbour
              trip_table[i, "country_arrival"]=arrival$Country
              trip_table[i, "gsa_arrival"]=arrival$GSA
              trip_table[i, "joined"]=-2
              trip_table[i+1, "joined"]=-2
              i=i+1
            }else{
              # 402
              if(incoastal_ban_zone(lon_start, lat_start , lon_end , lat_end, coastal_ban_zone) != 2){
                ### UNISCO SESIONI
                trip_table[i, "endid"]= trip_table[i+1, "endid"]
                trip_table[i, "enddata"]= trip_table[i+1, "enddata"]
                trip_table[i, "endtime"]= trip_table[i+1, "endtime"]
                trip_table[i, "arrival"]=trip_table[i+1, "arrival"]
                trip_table[i, "country_arrival"]=trip_table[i+1, "country_arrival"]
                trip_table[i, "gsa_arrival"]=trip_table[i+1, "gsa_arrival"]
                trip_table[i, "joined"]=trip_table[i, "joined"]+1
                trip_table = trip_table[trip_table$trip != trip_table[i+1,"trip" ], ]
              }else{
                # 437
                ti=as.POSIXct(paste(trip_table[i,"startdata"] ,trip_table[i,"starttime"] ), tz="UTC")
                tj=as.POSIXct(paste(trip_table[i+1,"enddata"] ,trip_table[i+1,"endtime"] ), tz="UTC")
                if(as.numeric(difftime(tj , ti , units="hours")) < 24){
                  ### UNISCO SESIONI
                  trip_table[i, "endid"]= trip_table[i+1, "endid"]
                  trip_table[i, "enddata"]= trip_table[i+1, "enddata"]
                  trip_table[i, "endtime"]= trip_table[i+1, "endtime"]
                  trip_table[i, "arrival"]=trip_table[i+1, "arrival"]
                  trip_table[i, "country_arrival"]=trip_table[i+1, "country_arrival"]
                  trip_table[i, "gsa_arrival"]=trip_table[i+1, "gsa_arrival"]
                  trip_table[i, "joined"]=trip_table[i, "joined"]+1
                  trip_table = trip_table[trip_table$trip != trip_table[i+1,"trip" ], ]
                }else{
                  #break
                  arrival=closest_port(lon_end , lat_end, ports)
                  departure=closest_port(lon_start , lat_start, ports)
                  trip_table[i, "joined"]=-1
                  trip_table[i+1, "joined"]=-1
                  trip_table[i+1, "departure"]=departure$harbour
                  trip_table[i+1, "country_departure"]=departure$Country
                  trip_table[i+1, "gsa_departure"]=departure$GSA
                  trip_table[i, "arrival"]=arrival$harbour
                  trip_table[i, "country_arrival"]=arrival$Country
                  trip_table[i, "gsa_arrival"]=arrival$GSA
                  i=i+1
                }
              }
            }
          }else{
            i=i+1
          }
          sz=nrow(trip_table)
        }
      }
      if(nrow(trip_table[is.na(trip_table$departure),]) > 0| nrow(trip_table[is.na(trip_table$arrival),])>0){
        cat("\n", "Recovery harbours")
        pb = txtProgressBar(min = 0, max = nrow(trip_table) , initial = 0)
        for(i in 1:nrow(trip_table)){
          xdeparture=trip_table[i,"departure"]
          xarrival=trip_table[i,"arrival"]
          if(is.na(xdeparture)){
            lat=datatest[datatest$rowid ==trip_table[i, "startid"] , "latitude"]
            lon=datatest[datatest$rowid ==trip_table[i, "startid"] , "longitude"]
            departure=closest_port_recovery(longitude=lon , latitude=lat, ports=ports, reference_port=xarrival )
            trip_table[i, "departure"]=departure$harbour
            trip_table[i, "country_departure"]=departure$Country
            trip_table[i, "gsa_departure"]=departure$GSA
          }
          if(is.na(xarrival)){
            lat=datatest[datatest$rowid ==trip_table[i, "endid"] , "latitude"]
            lon=datatest[datatest$rowid ==trip_table[i, "endid"] , "longitude"]
            arrival=closest_port_recovery(longitude=lon , latitude=lat, ports=ports, reference_port=xdeparture )
            trip_table[i, "arrival"]=arrival$harbour
            trip_table[i, "country_arrival"]=arrival$Country
            trip_table[i, "gsa_arrival"]=arrival$GSA
          }
          setTxtProgressBar(pb,i)
        }
      }
      trip_table$start_timestamp = as.POSIXct(paste(trip_table$startdata, trip_table$starttime), 
                                              format='%Y-%m-%d %H:%M:%S', 
                                              tz="UTC")
      trip_table$end_timestamp = as.POSIXct(paste(trip_table$enddata, trip_table$endtime), 
                                            format='%Y-%m-%d %H:%M:%S', 
                                            tz="UTC")
      trip_table$MMSI = trip_table$mmsi
      trip_table = trip_table[,c("MMSI", "trip", "start_timestamp", "end_timestamp", "departure", "arrival")]
      cat("....complete!!!")
      return(trip_table)
    }
    }
  
  
  }




###--- Data partitioning for model ####
data_partition <- function(data){
  ref_gear = c("OTB", "LLS" , "PS", "PTM" , "TBB" , "DRB" , "LLD" )
  ref_gear2 = c("OTB", "PS", "PTM" , "TBB")
  create = data[,c("MMSI", "gear_obs")]
  create = unique(create[,c("MMSI", "gear_obs")])
  create = aggregate(MMSI~gear_obs, data = create, function(x) length(unique(x)))
  colnames(create)[ncol(create)] = "n"
  create$sam = round(10*(create$n/100))
  create_ref = create[,c("gear_obs", "sam")]
  
  #validation
  ref_validation = aggregate(gear_obs ~ MMSI, data = data, function(x) length(unique(x)))
  ref_validation = ref_validation[which(ref_validation$gear_obs == 1),]
  myvalidation =  unique(merge(data, data.frame(MMSI = ref_validation[,1]), by = "MMSI")[,c("MMSI", "gear_obs")])
  myvalidation = merge(myvalidation, create_ref, by = "gear_obs", all.x = T)
  myvalidation = plyr:::ldply(lapply(split(myvalidation, myvalidation$gear_obs), function(x){ 
    xsam = unique(x$sam)
    merge(x[sample(1:nrow(x), unique(x$sam), replace = F),], data, by = c("MMSI", "gear_obs"))
  }))
  
  # data for training/test
  data_for_model = dplyr:::anti_join(data, myvalidation, by = "MMSI")
  data_for_model$ids = 1:nrow(data_for_model)
  data_for_model$id = paste(data_for_model$MMSI, data_for_model$ids)
  
  # data partitionining
  train_ref = as.numeric(unlist(caret:::createDataPartition(paste(data_for_model$gear_obs, data_for_model$year), p = 0.7)))
  mytraining = data_for_model[train_ref,]
  mytest = data_for_model[-train_ref,]
  
  # export
  data_splitted = list(training = mytraining, test = mytest, validation = myvalidation)
  return(data_splitted)
}




###--- Decision gear ---####
# This function function uses the trained Random Forest model to predict the fishing gear for each month., The features used to predict the monthly gear consist in the ratio between  the trip labelled as positive for each gear and the total number of the fishing trips (ratio_otb1; ratio_otb2; ratio_ptm; ratio_tbb; ratio_ps).
decision_gear<-function(data){
  data=split(data, data$start_month)
  monthly_gears=lapply(data, function(xdata){
    data=xdata
    # discard fishing trips with less than 30 points
    data$valid=ifelse(data$n_ping <=30, 0, ifelse(data$gaps >=0.9, 0, 1))
    data$valid[is.na(data$valid)] <- 0
    # summarize classification information
    data[is.na(data)] <- 0
    data_valid=data[data$valid==1, -which(names(data) %in% c("gaps", "n_ping","trip", "valid"))]
    if(nrow(data_valid)==0){
      data_valid=data.frame(MMSI=unique(data$MMSI), 
                            start_month=unique(data$start_month), 
                            total_trips=nrow(data), 
                            valid_trips=0, 
                            otb1=0,
                            otb2=0,
                            ptm=0,
                            tbb=0,
                            ps=0,
                            ratio_otb1=0,
                            ratio_otb2=0,
                            ratio_ptm=0,
                            ratio_tbb=0,
                            ratio_ps=0, 
                            gear="OTHER")
      return(data_valid)
    }else{
      data_valid$valid_trips=nrow(data_valid)
      data_valid$otb2=sum(data_valid$otb2)
      data_valid$otb1=sum(data_valid$otb1)
      data_valid$ptm=sum(data_valid$ptm)
      data_valid$tbb=sum(data_valid$tbb)
      data_valid$ps=sum(data_valid$ps)
      data_valid=data_valid[1,]
      data_valid$ratio_otb1=data_valid$otb1/data_valid$valid_trips
      data_valid$ratio_otb2=data_valid$otb2/data_valid$valid_trips
      data_valid$ratio_ptm=data_valid$ptm/data_valid$valid_trips
      data_valid$ratio_tbb=data_valid$tbb/data_valid$valid_trips
      data_valid$ratio_ps=data_valid$ps/data_valid$valid_trips
      data_backup=data
      data=aggregate(list(total_trips=data$trip), 
                     by=list(MMSI=data$MMSI, start_month=data$start_month), 
                     FUN=length)
      data_valid=merge(data, data_valid, by=c("MMSI", "start_month"))
      data_valid[is.na(data_valid)]<-0
      if(nrow(data_valid)==0){
        data_valid=data.frame(MMSI=unique(data_backup$MMSI), 
                              start_month=unique(data_backup$start_month), 
                              total_trips=nrow(data_backup), 
                              valid_trips=0, 
                              otb1=0,
                              otb2=0,
                              ptm=0,
                              tbb=0,
                              ps=0,
                              ratio_otb1=0,
                              ratio_otb2=0,
                              ratio_ptm=0,
                              ratio_tbb=0,
                              ratio_ps=0, 
                              gear="OTHER")
        return(data_valid)
      }else if( data_valid$valid_trips < 2){
        data_valid=data.frame(MMSI=unique(data_backup$MMSI),
                              start_month=unique(data_backup$start_month), 
                              total_trips=nrow(data_backup), 
                              valid_trips=0, 
                              otb1=0,
                              otb2=0,
                              ptm=0,
                              tbb=0,
                              ps=0,
                              ratio_otb1=0,
                              ratio_otb2=0,
                              ratio_ptm=0,
                              ratio_tbb=0,
                              ratio_ps=0,
                              gear="OTHER")
        return(data_valid)
      }else{
        set.seed(461)
        gear=as.character(predict(classification_RF, 
                                  newdata=data_valid[,c("valid_trips",
                                                        "ratio_otb1",
                                                        "ratio_otb2",
                                                        "ratio_ptm",
                                                        "ratio_tbb",
                                                        "ratio_ps")]))
        data_valid$gear=gear
        if(gear=="OTB"){
          if(data_valid$ratio_otb1 > data_valid$ratio_otb2){
            data_valid$gear="OTB2"
          }else{data_valid$gear="OTB1"}
        }
        data_valid=data_valid[,c("MMSI",
                                 "start_month",
                                 "total_trips", 
                                 "valid_trips",
                                 "otb1", "otb2","ptm","tbb","ps",
                                 "ratio_otb1","ratio_otb2","ratio_ptm","ratio_tbb","ratio_ps",
                                 "gear")]
        return(data_valid)
      }
    }
    
  })
  monthly_gears=do.call(rbind, monthly_gears)
  return(monthly_gears)
  }



###--- Estimate fishing effort ####
estimate_fishing_effort <- function(fishing_tracks, grid){
  lapply(fishing_tracks, function(x){
    if(is.data.frame(x)){
      x = st_sf(x)
    }
    xgear = unique(x$gear)
    st_crs(x)=wgs # set crs 
    if(xgear == "PS"){
      x$duration=difftime(x$f_time, x$s_time, units="secs")
      x=st_as_sf(x)
      xint=st_intersection(grid, x)
      xint=data.frame(xint)
      xint=xint[,c("grid_id", "gear", "duration")]
      xint$fishing_hours=xint$duration/3600
      f_hours = aggregate(fishing_hours~grid_id+gear, data = xint, sum)
      colnames(f_hours)[ncol(f_hours)] = "f_hours"
      grid_edit=merge(grid, f_hours, by="grid_id") # combine effort to grid
      return(grid_edit)
    }else{
      x$distance=st_length(x$geometry, units="m") # estimated fishing track 
      x$duration=difftime(x$f_time, x$s_time, units="secs")
      x$speed_ms=as.numeric(x$distance)/as.numeric(x$duration)
      x=st_as_sf(x)
      xint=st_intersection(grid, x)
      if(nrow(xint) > 0){
        xint$observed_swept=st_length(xint)
        xint=data.frame(xint)
        xint=xint[,c("grid_id", "gear","observed_swept", "speed_ms")]
        xint$fishing_hours=(as.numeric(xint$observed_swept)/xint$speed_ms)/3600
        f_hours = aggregate(fishing_hours~grid_id+gear, data = xint, sum)  # estimate fishing effort in cells grid
        colnames(f_hours)[ncol(f_hours)] = "f_hours"
        grid_edit=merge(grid, f_hours, by="grid_id") # combine effort to grid
        return(grid_edit)
      }
    }
  })
  
}
###--- Find in harbours ####
# This function is used to individuate if there are points (x,y) that fall within the polygon of harbours, and, eventually, it indicates which are these points.
find_inport = function(data, ports){
  data = st_as_sf(data, coords = c("longitude", "latitude"))
  st_crs(data) = wgs
  xintersection = data.frame(suppressMessages(st_intersects(data, ports)))
  inports = xintersection$row.id
  return(inports)
}
###--- Find the closest harbour ####
#This function is used to assign the beginning and ending ports of fishing trips. The departure or the arrival harbor was assigned considering the closest harbor with respect to the first or last position of the trip 
closest_port=function(longitude , latitude, ports){
  pos = data.frame(x = longitude , y =latitude)
  pos = st_as_sf(pos, coords=c("x", "y"))
  st_crs(pos) = wgs
  xdistance = data.frame(distan=st_distance(ports, pos))
  xdistance$id = seq(1:nrow(xdistance))
  xport = ports[xdistance[xdistance$distan == min(xdistance$distan),]$id,]
  return(xport)
}


###--- Find the closest harbour recovery ####
#This function is used to assign the beginning and ending ports of fishing trips during the recovery step of the create fishing trip function. This function individuate if there are harbours closest then 50 km with respect to the first or last position of the trip. If there are harbours, the function select the closest five, then it checks if the reference_port is included in the closest five. If yes, it assign this harbour, if no, it assign the closest harbour.
closest_port_recovery=function(longitude , latitude, ports, reference_port){
  pos = data.frame(x = longitude , y =latitude)
  pos = st_as_sf(pos, coords=c("x", "y"))
  st_crs(pos) = wgs
  xdistance = data.frame(distan=st_distance(ports, pos))
  xdistance$id = seq(1:nrow(xdistance))
  xdistance=xdistance[order(xdistance$distan),]
  xdistance=xdistance[as.numeric(xdistance$distan) <=50000,]
  if(nrow(xdistance)==0){
    xport=data.frame(harbour="At_Sea", Country= "At_Sea", GSA="At_Sea" )
  }else{
    xport = ports[xdistance[1:5,]$id,]
    xport =xport[!is.na(xport$harbour),]
    if(reference_port %in% xport$harbour){
      xport=xport[xport$harbour==reference_port,]
    }else{
      xport = xport[1,]
    }
  }
  return(xport)
}


###--- Find the overlapping harbour ####
# This function is used to assign the beginning and ending ports of fishing trips, by the means of a spatial intersection between coordinates and the ports buffer layer. If the intersection is successful, it return the name of the identified harbour
get_port=function(longitude, latitude, ports_buffer, ports){
  position = data.frame(longitude = longitude, latitude = latitude)
  position = st_as_sf(position, coords=c("longitude", "latitude"))
  st_crs(position) = wgs
  xintersction = data.frame(suppressMessages(st_intersection(ports_buffer, position)))
  if(nrow(xintersction) > 0){
    port_selection=data.frame(ports[ports$harbour == xintersction$harbour,])
    port_selection[,-which(colnames(port_selection) == "geometry")]
  }else{
    port_selection=data.frame(harbour = NA , Country = NA , GSA = NA)
  }
  return(port_selection)
}

###--- Identify fishing points ####
# This function recycles the clusters (obtained from k-means analysis for towed gears and from dbscan for purse seiners) and retrieving points corresponding to fishing clusters. 
identify_fishing_points=function(data, gear,coord_sys){
  xdat=split(data, data$start_month)
  xgear=split(gear, gear$start_month)
  f_point=function(xxdat, xxgear){
    if(nrow(xxgear)==0){
      results_table=xxdat[, c("MMSI", "trip", "datetime", "latitude", "longitude")]
      results_table$cl=NA
      results_table$xxgear="Not enough xxdat"
    }else{
      xxgear=unique(xxgear$gear)
      if(xxgear=="OTHER"){
        results_table=xxdat[, c("MMSI", "trip", "datetime", "latitude", "longitude")]
        results_table$cl=NA
        results_table$gear=xxgear
        results_table$fishing=0
      }else{
        results_table=xxdat[, c("MMSI", "trip", "datetime", "latitude", "longitude")]
        results_table$cl=xxdat[,tolower(xxgear)]
        results_table$gear=xxgear
        if(xxgear=="PS"){
          results_table$fishing=ifelse(is.na(results_table$cl), 0, 1)
        }else if (xxgear=="OTB1"|xxgear=="OTB2"){
          results_table$fishing=ifelse(results_table$cl==4 ,1 , 0)
        }else if (xxgear=="TBB"){
          results_table$fishing=ifelse(results_table$cl==5 ,1 , 0)
        }else if (xxgear=="PTM"){
          results_table$fishing=ifelse(results_table$cl==4 ,1 , 0)
        }
      }
    }
    results_table=results_table[, -which(names(results_table) %in% c("cl"))]
    results_table=results_table[,c("MMSI","datetime","longitude","latitude","trip", "fishing", "gear")]
    return(results_table)
    
  }
  res_table=do.call(rbind,mapply(f_point,xdat,xgear, SIMPLIFY = F))
  res_table=st_as_sf(res_table, coords=c("longitude", "latitude"))
  st_crs(res_table)=coord_sys
  return(res_table)
}


###--- Identify transmission gaps ####
# This function recycles the data gaps from the function "core_function" and retrieving tracks corresponding to gaps in the AIS signal.
identify_trasmission_gaps=function(data, coord_sys){
  data=data[,c("MMSI","datetime","longitude","latitude","trip", "otb1", "start_month")]
  data$gap=0
  for(k in 1:(nrow(data)-1)){
    if(data[k,"otb1"] ==1 ){
      data[k,"gap"]=1
      data[k+1,"gap"]=1
    }
  }
  data$interval=0
  for(ii in 2:nrow(data)){
    data[ii, "interval"]=difftime(data[ii,"datetime"], 
                                  data[ii-1,"datetime"], units="mins")
  }
  
  data$id_gap=1
  for(k in 2:nrow(data)){
    data[k,"id_gap"]=data[k-1,"id_gap"]
    if(data[k,"gap"] != data[k-1,"gap"]| data[k,"interval"] < pars$timepar){
      data[k,"id_gap"]=data[k-1,"id_gap"]+1
    }
  }
  
  data=data[data$gap==1,]
  xdata=data
  if(nrow(xdata)> 0){
    mmsi=unique(xdata$MMSI)  
    s_time=aggregate(list(s_time=xdata$datetime), by=list(id_gap=xdata$id_gap), FUN=min )
    f_time=aggregate(list(f_time=xdata$datetime, trip=xdata$trip), by=list(id_gap=xdata$id_gap), FUN=max )
    result_table_1=merge(s_time, f_time, by="id_gap")
    result_table_1$MMSI=mmsi
    result_table_1$duration=difftime(result_table_1$f_time, result_table_1$s_time, units="mins")
    result_table_2=xdata
    result_table_2=st_as_sf(result_table_2, coords=c("longitude", "latitude"))
    st_crs(result_table_2)=coord_sys
    result_table_2 = split(result_table_2, result_table_2$id_gap)
    result_table_2 =  plyr:::ldply(lapply(result_table_2, function(x) st_combine(x)), data.frame)
    result_table_2$geometry = st_cast(result_table_2$geometry, "LINESTRING")
    xdata=merge(result_table_1, result_table_2, by.x="id_gap", by.y = ".id")
    xdata$year = year(xdata$s_time)
    xdata$month = month(xdata$s_time)
    xdata$gear="GAP"
    xdata=xdata[,c("MMSI",  "year","gear", "month","trip", "id_gap", "s_time", "f_time", "duration", "geometry")]
    xdata = st_sf(xdata)
    return(xdata)
  }
}

###--- Import parameters ----
# This function allows to load the required parameters of several internal functions
inport_parameters <- function(parameters, centroids){
  parameters_tab=read.csv(parameters)
  param=sapply(1:nrow(parameters_tab), function(y) parameters_tab[y,"value"], simplify=F)
  names(param)=as.character(parameters_tab$variable)
  centroids=read.csv(centroids)
  return(list(param, centroids))
}

###--- Inspect coastal ban zone ####
#This function is used to individuate if the last points of a fishing trip (n) and the first point of the subsequent fishing trip (n+1), lies within the coastal ban zone. It returns the number of points individuated.
incoastal_ban_zone=function(longitude_start , latitude_start , longitude_end , latitude_end, coastal_ban_zone){
  final_position = data.frame(x = longitude_end , y = latitude_end)
  final_position = st_as_sf(final_position, coords = c("x", "y"))
  st_crs(final_position) = wgs
  start_position = data.frame(x = longitude_start , y = latitude_start)
  start_position = st_as_sf(start_position, coords = c("x", "y"))
  st_crs(start_position) = wgs
  check_start = data.frame(suppressMessages(st_intersects(coastal_ban_zone, start_position)))
  check_end = data.frame(suppressMessages(st_intersects( coastal_ban_zone, final_position)))
  incost = nrow(check_start) + nrow(check_end)
  return(incost)
}


###--- Make fishing tracks ####
# This function extracts fishing tracks from fishing points, using a temporal threshold (thr_minutes) to connect successively ordered fishing points <= thr_minutes and avoid false fishing tracks connecting two subsequent fishing events. The result of make_fishing_tracks function is a spatial object where the fishing geometries are stored.
make_fishing_tracks=function(data, coord_sys, pars){
  
  data=data[data$gear %in% c("OTB1", "OTB2", "PTM", "TBB", "PS"),]
  data=split(data, data$gear)
  
  f_tracks=lapply(data, function(xdata){
    xdata=data.frame(xdata)
    if(nrow(xdata)>2){
      gear=unique(xdata$gear)
      mmsi=unique(xdata$MMSI)
      xdata$interval=0
      for(ii in 2:nrow(xdata)){
        xdata[ii, "interval"]=difftime(xdata[ii,"datetime"], 
                                       xdata[ii-1,"datetime"], units="mins")
      }
      xdata$id_track=1
      for(k in 2:nrow(xdata)){
        xdata[k,"id_track"]=xdata[k-1,"id_track"]
        if(xdata[k,"fishing"] != xdata[k-1,"fishing"] | xdata[k,"interval"] >pars$timepar){
          xdata[k,"id_track"]=xdata[k-1,"id_track"]+1
        }
      }
      xdata=xdata[xdata$fishing==1,]
      if(nrow(xdata)> 0){
        
        if(gear=="PS"){
          xcoords=st_coordinates(xdata$geometry)
          xdata$longitude=xcoords[,"X"]
          xdata$latitude=xcoords[,"Y"]
          ## For Purse seines fishing xdata are given in the form of fishing operation centroid ##
          s_time=aggregate(list(s_time=xdata$datetime), by=list(id_track=xdata$id_track), FUN=min )
          f_time=aggregate(list(f_time=xdata$datetime,trip=xdata$trip), by=list(id_track=xdata$id_track), FUN=max )
          result_table_1=merge(s_time, f_time, by="id_track")
          result_table_1$MMSI=mmsi
          result_table_2=aggregate(list(mean_x=xdata$longitude, mean_y=xdata$latitude), 
                                   by=list(id_track=xdata$id_track), 
                                   FUN=mean)
          range=merge(aggregate(list(xMin=xdata$longitude, yMin=xdata$latitude),
                                by=list(id_track=xdata$id_track), 
                                FUN=min),
                      aggregate(list(xMax=xdata$longitude, yMax=xdata$latitude),
                                by=list(id_track=xdata$id_track), 
                                FUN=max),
                      by="id_track")
          range$range=((range$xMax-range$xMin)+(range$yMax-range$yMin))/2
          result_table_2$range=range$range
          result_table_2=st_as_sf(result_table_2, coords=c("mean_x", "mean_y"))
          st_crs(result_table_2)=coord_sys
          result_table_2$geometry=st_cast(result_table_2$geometry, "POINT")
          xdata=merge(result_table_1, result_table_2, by="id_track")
          xdata$gear=gear
          xdata$year = year(xdata$s_time)
          xdata$month = month(xdata$s_time)
          xdata=xdata[,c("MMSI",  "year", "month", "gear","trip", "id_track", "s_time", "f_time", "range", "geometry")]
        }else{
          # For towed gears fishing xdata are given in the form of tracks ##
          s_time=aggregate(list(s_time=xdata$datetime), by=list(id_track=xdata$id_track), FUN=min )
          f_time=aggregate(list(f_time=xdata$datetime, trip=xdata$trip), by=list(id_track=xdata$id_track), FUN=max )
          result_table_1=merge(s_time, f_time, by="id_track")
          result_table_1$MMSI=mmsi
          result_table_2=xdata
          result_table_2=st_as_sf(result_table_2)
          st_crs(result_table_2)=coord_sys
          result_table_2 = split(result_table_2, result_table_2$id_track)
          result_table_2 =  plyr:::ldply(lapply(result_table_2, function(x) st_combine(x)), data.frame)
          result_table_2$geometry = st_cast(result_table_2$geometry, "LINESTRING")
          xdata=merge(result_table_1, result_table_2, by.x="id_track", by.y = ".id")
          xdata$gear=gear
          xdata$range=NA
          xdata$year = year(xdata$s_time)
          xdata$month = month(xdata$s_time)
          xdata=xdata[,c("MMSI",  "year", "month","gear","trip", "id_track", "s_time", "f_time", "range", "geometry")]
        }
        xdata = st_sf(xdata)
        return(xdata)
        
      }else{
        return(xdata)
      }
    }
  })
  
  return(f_tracks)
}

###--- Make tracks lite ---####
# this function group points into groups basing on previous function information, than it removes the first and the last steaming tracks
make_tracks_lite=function(data, gear){
  # remove blocks composed by one point
  block_count=data.frame(table(data$data_block, dnn=c("data_block")))
  names(block_count)[2]="ping_number"
  data=merge(data, block_count,by="data_block")
  data=data[data$ping_number > 2,]
  cluster_fishing=ifelse(gear=="tbb",  5, ifelse(gear=="ps", 3, 4))
  data=data[,c("cluster", "data_block",   "ping_number")]
  data=data[duplicated(data)==F,]
  if(nrow(data)==0){
    return(data)
  }else{
    
    # remove steaming at beginning and end
    data$drop="N"
    for(k in 1:nrow(data)){
      if(data[k,"cluster"]!=cluster_fishing){
        data[k,"drop"]="Y"
      }else{
        break
      }
    }
    
    for(k in nrow(data):1){
      if(data[k,"cluster"]!=cluster_fishing){
        data[k,"drop"]="Y"
      }else{
        break
      }
    }
    data=data[data$drop=="N",]
    return(data)
  }
}

###--- Search clusters ---####
# This function applies the dbscan algorithm to identify spatial clusters in the input data
search_cluster=function(data, pars, gear){
  neighborhood_rf=paste0("range_", gear)
  neighborhood=pars[[neighborhood_rf]]
  pts_ref = paste0("n_boats_", gear)
  pts=pars[[pts_ref]]
  data=matrix(c(data$latitude, data$longitude),ncol=2)
  cluster_evaluation=dbscan(data, neighborhood,pts)
  xcluster=data.frame(id_cluster=cluster_evaluation$cluster)
  xcluster$id_cluster=ifelse(xcluster$id_cluster==0,xcluster$id_cluster,1)
  xcluster$index=seq(1:nrow(xcluster))
  xcluster_filter=xcluster[xcluster$id_cluster!=0,]
  if(nrow(xcluster_filter)!=0){
    
    #adjust and paste to dataset
    for(k in 2:nrow(xcluster_filter)){
      xcluster_filter[k,"id_cluster"]=xcluster_filter[k-1,"id_cluster"]
      if((xcluster_filter[k,"index"] - xcluster_filter[k-1,"index"])>1 ){
        xcluster_filter[k,"id_cluster"]=xcluster_filter[k-1,"id_cluster"]+1
      }
    }
    count_points=aggregate(list(n_pts=xcluster_filter$index), 
                           by=list(id_cluster=xcluster_filter$id_cluster), 
                           FUN=length)
    count_points=count_points[count_points$n_pts > pts ,]
    xcluster_filter=xcluster_filter[xcluster_filter$id_cluster %in% count_points$id_cluster,]
    xcluster=merge(data.frame(index=xcluster$index), xcluster_filter, by="index", all.x=T)
  }else{
    xcluster=data.frame(indexd=xcluster$index, id_cluster=NA)
  }
  names(xcluster)[1]="id"
  return(xcluster)
}




















