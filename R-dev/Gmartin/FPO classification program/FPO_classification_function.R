####### THIS FUNCTION SHOULD ONLY BE MODIFIED WITH CARE #######
####### DO NOT USE THIS FILE - USE ONLY THE GIVEN COMMAND (which refers to this file) #######




# Programmer: Sophie de Grissac - DioMed&a Science
# COMPANY N° (SIRET): 882 532 245 00017
# last update: 29/03/2020
# email: s.degrissac@gmail.com
# Contact me for any update, question, problem related to this function.




###### CLASSIFICATION OF FISHING VESSEL ACTIVITY #######

###### This programme uses as data input

#-------------------------------------------- INSTRUCTIONS --------------------------------------------------------------------

######       - A dataset with fishing vessel GPS location + date/time and vessel identity (code) 

#                   * File must be a CSV (coma separation) with the first 4 columns as follows:
#                   * vessel identity / Date+time / longitude / latitude (see instructions). 
#                   * The names of the column doesn't matter
#                   * The format of the Date+Time MUST BE dd/dd/yyy hh:mm:ss
#                   * Ping rates can typically be 5, 3 or 1 min. Different ping rates are allowed in the same dataset
#                   * Classifcation problem will arise for vessels that have changed their ping rate during a trip (they can change in between trips) or for which pings are very irregular
#                   * Therefore, the regularity of the ping rate is tested for each trip and trips with to much irregularity (standard error compared to median of ming rate) are removed
#                   * Trips that are removed from the analyse are kept and saved in a separate file for the record.
##### 




# ______________ START OF PROGRAM _______________________________________________________________________________________
#__________________________________________________________________________________________________________________________

## pour test
#path_harbour <- "D:/MI Fisheries/R analyses/data csv/harbourLoc.csv"
#path_vessel <- "D:/MI Fisheries/R analyses/data csv/vessel_tracks.csv"
#path_folder_result <- "D:/MI Fisheries/R analyses/test final script/"
##


FPO_classification <- function (path_vessel, path_harbour, path_folder_result) {
  
  
  #####     LIBRARIES
  
  t1 <- Sys.time()
  
  requiredPackages = c('moveHMM','geosphere','adehabitatHR','sp','CircStats')
  for(p in requiredPackages){
    if(!require(p,character.only = TRUE)) install.packages(p, dependencies = TRUE)
    library(p,character.only = TRUE)
  }
  
  
  #__________________________________________________________________________________________________________________________
  
  
  ####    SOURCES
  
  # burst function
  
  ## dtime is the time between one location and the NEXT
  ## this function return "burtsid", a numeric vector corresponding to burst numbers, i.e. blocks of successive locations forming a unique trip.
  ## threshold is the minimum time period between 2 locs after which you consider that these 2 locs are 2 different trips.
  ## In the case of fishing vessel, fishing trips are tipically separated by at least 12h. The treshold is arbitrary set to 2 hours based on the assumption that vms do not stop transmitting while at sea.
  
  create_burst <- function(dtime, treshold)
  {
    burstid=0
    j=0
    for (i in 1:(length(dtime)-1))
    {
      if (dtime[i] < treshold)
      {
        burstid <- c(burstid,j)
      }
      else
      {
        burstid <- c(burstid,j+1)
        j <- j+1
      }
    }
    return(burstid)
  }

  
  #__________________________________________________________________________________________________________________________
  
  
  ####    DATA INPUT
  
  ## open vessel data
  vessel <- read.csv(path_vessel) # open GPS locations file already filter with harbour locations and with ping rate information
  
  ## harbour data ---------- NOT IMPLEMENTED SO FAR ------------ WILL BE IF NEEDED WITH NEW HARBOURS ADDED
  harbour <- read.csv(path_harbour) 
  
  #_________________________________________________________________________________________________________________________________________
  
  
  #####     FORMATING
  
  # names and date formating
  
  vessel <- vessel[,1:4]
  names(vessel) <- c('ID', 'Date', 'longitude', 'latitude')
  vessel$Date <- as.POSIXct(vessel[,2], format = "%d/%m/%Y %H:%M:%S", tz='UTC') # format the date
  #reorder by ID and Date
  vessel <- vessel[order(vessel$ID, vessel$Date),]
  vessel$ID <- as.factor(vessel$ID)
  
  
  ## remove harbour locations
  v <- vessel
  for (i in 1:nrow(harbour)) {
    dist_harbour <- distHaversine(c(harbour[i,"Longitude_Center"],harbour[i,"Latitude_Center"]),v[,c("longitude","latitude")],r=6378.160) # distance to harbour in km
    v <- v[which(dist_harbour > 1),]
  }
  vessel <- v
  rm(v, dist_harbour)
  
  
  #### calculate dtime (time between successive locations) and split the different daily trips of each vessel
  #### & create burst, i.e. separate trips when time between pings is 2h or more (arbitrary)
  #### This uses the function "create burst" sourced in the first part of this script
  
  v <- NULL
  #vessel$ID_trip <- NA
  for (j in 1:length(unique(vessel$ID))) {
    d <- vessel[vessel$ID == unique(vessel$ID)[j],]
    d$dtime <- NA
    for (i in 1:nrow(d)-1){
      d$dtime[i] <- difftime(d$Date[i+1],d$Date[i],units="sec")
      # add a column with trip ID ("vesselID_TripNumber")
    }
    b <- create_burst(d$dtime, 7200)
    d$ID_trip <- paste(d$ID,b,sep="_")  
    v <- rbind(v, d)
  }
  
  vessel <- v
  rm(d,b, v)
  
  vessel$ID_trip <- as.factor(vessel$ID_trip)
  
  #set NA dtime for the last point of each trip
  for (i in 1:length(unique(vessel$ID_trip))) {
    vessel$dtime[vessel$ID_trip == unique(vessel$ID_trip)[i]][length(vessel$dtime[vessel$ID_trip == unique(vessel$ID_trip)[i]])] <- NA
  }
  
  #reorder lines by ID and Date
  vessel <- vessel[order(vessel$ID, vessel$Date),]
  
  ntot_trip <- length(unique(vessel$ID_trip))
  
  #______________________________________________________________________________________________________________________________________
  
  #### DIFFERENTIATE TRIPS WITH DIFFERENT PING RATES (1, 3, 5 or 10 min)
  
  ## vessel data = separate tracks by ping rates
  vessel_ping <- NULL
  for (i in 1:length(unique(vessel$ID_trip))) {
    d <- vessel[vessel$ID_trip == unique(vessel$ID_trip)[i],]
    d$ping_rate_min <- ifelse(median(d$dtime, na.rm=TRUE) > 500, 10,
                              ifelse(median(d$dtime, na.rm=TRUE) > 200, 5,
                                     ifelse(median(d$dtime, na.rm=TRUE) > 90, 3, 1)))
    vessel_ping <- rbind(vessel_ping, d)
  }
  
  #reorder
  vessel <- vessel_ping[order(vessel_ping$ID, vessel_ping$Date),]
  
  ### Exclude trips with PING-RATE 10 MIN - Model does not accomodate this properly for now
  del_trips_ping10 <- unique(droplevels(vessel$ID_trip[vessel$ping_rate_min == 10]))
  del_vessel_ping10 <- unique(droplevels(vessel$ID[vessel$ping_rate_min == 10]))
  vessel <- droplevels(vessel[vessel$ping_rate_min < 10,])
  
  #### Exclude short trips that have to few pings for proper classification and may lead to errors in the model
  del_trips_short <- droplevels(unique(vessel$ID_trip[vessel$ID_trip %in% names(which(tapply(vessel$ID, vessel$ID_trip, length) <= 50))]))
  vessel <- droplevels(vessel[vessel$ID_trip %in% names(which(tapply(vessel$ID, vessel$ID_trip, length) > 50)),]) #keeps only trips with > 10 pings
  
  
  ### Exclude trips with too irregular ping rate
  vessel_filter <- NULL
  del_trips_irreg <- NULL
  
  for (i in 1:length(unique(vessel$ping_rate_min))) {
    #print(i)
    n <- unique(vessel$ping_rate_min)[i]*60
    v <- droplevels(vessel[vessel$ping_rate_min == unique(vessel$ping_rate_min)[i],])
    keep <- row.names(as.data.frame(tapply(v$dtime, v$ID_trip, sd, na.rm=T)/n))[which(tapply(v$dtime, v$ID_trip, sd, na.rm=T)/n <= 1)]
    v2 <- droplevels(v[v$ID_trip %in% keep,])
    del <- row.names(as.data.frame(tapply(v$dtime, v$ID_trip, sd, na.rm=T)/n))[which(tapply(v$dtime, v$ID_trip, sd, na.rm=T)/n > 1)]
    vessel_filter <- rbind(vessel_filter, v2)
    del_trips_irreg <- c(del_trips_irreg, del)
  }
  #_______________________________________________________________________________________________________________________________________________
  
  
  ### Tell user how many trips have been removed from analyse
  
  print(paste0('There was ', length(del_vessel_ping10),' vessels (', ntot_trip,' trips) with ping rate 10min NOT analysed'))
  print(paste0('In addition, there was ', length(del_trips_irreg),'/', ntot_trip,' trips with ping rates too irregular to be analysed'))
  print(paste0('In addition, there was ', length(del_trips_short),'/', ntot_trip,' trips with too few locations to be analysed'))
  print(paste0('A total of ', length(unique(vessel$ID)),' vessels (', length(unique(vessel$ID_trip)),' daily trips) will be analysed'))
  
  #reorder
  vessel <- vessel_filter[order(vessel_filter$ID, vessel_filter$Date),]
  
  
  #_____________________________________________________________________________________________________________________________________________
  
  ### RUN HMM MODELS
  
  vessel_result_all <- NULL
  
  for (j in 1:length(unique(vessel$ping_rate_min))) {
    list_mod <- list()
    vessel_result_ping <- NULL
    vessel_pp <- droplevels(vessel[vessel$ping_rate_min == unique(vessel$ping_rate_min)[j],])
    
    n <- unique(vessel_pp$ping_rate_min)
    
    #delete some duplicated data
    if (length(which(duplicated(vessel_pp[,c(2,6)])==TRUE)) > 0) {
      vessel_pp <- droplevels(vessel_pp[-which(duplicated(vessel_pp[,c(2,6)])==TRUE),])
    }
    
    #create a folder to store and check the results
    
    if (dir.exists(paste0(path_folder_result,"Ping_",n,"min_results"))==FALSE) {
      dir.create(paste0(path_folder_result,"Ping_",n,"min_results"))
    }
    
    setwd(paste0(path_folder_result,"Ping_",n,"min_results"))
    cwd <- getwd()
    
    print(paste0('Processing vessels with ping rate ', j, ' min.'))
    
    for (i in 1:length(unique(vessel_pp$ID))) {
      print(paste0('Processing vessel number ', unique(vessel_pp$ID)[i]))
      
      # create a specific folder to store results for each vessel
      if (dir.exists(paste0(cwd,"/Vessel_",unique(vessel_pp$ID)[i]))==FALSE) {
        dir.create(paste0(cwd,"/Vessel_",unique(vessel_pp$ID)[i]))
      }
      path_res <- paste0(cwd,"/Vessel_",unique(vessel_pp$ID)[i])
      
      if (dir.exists(paste0(cwd,"/Vessel_",unique(vessel_pp$ID)[i],'/plot_tracks'))==FALSE) {
        dir.create(paste0(cwd,"/Vessel_",unique(vessel_pp$ID)[i],'/plot_tracks'))
      }
      path_plot <- paste0(cwd,"/Vessel_",unique(vessel_pp$ID)[i],'/plot_tracks')
      
      
      ## select vessel i
      vessel_p <- droplevels(vessel_pp[vessel_pp$ID == unique(vessel_pp$ID)[i],])
      
      
      
      #-----------------------  Rediscretized tracks according to ping rate ------------------------#
      
      Lon=vessel_p$longitude;Lat=vessel_p$latitude;Date=vessel_p$Date; ID=vessel_p$ID; burst=vessel_p$ID_trip
      t = median(vessel_p$dtime, na.rm=TRUE)
      data_traj = as.ltraj(as.data.frame(cbind(Lon,Lat)), Date, 
                           id = ID, burst=burst, slsp="remove", typeII = T)
      
      infolocs = cbind(vessel_p$ID,vessel_p$ping_rate_min)
      
      data_redis <- redisltraj(data_traj, u = t, type="time", burst=burst) # resample at 2min fix rate
      
      data_redis_df <- ld(data_redis)[,c(1:3,11,12)]
      names(data_redis_df) <- c('longitude','latitude','Date','ID','ID_trip')
      
      vessel_p <- data_redis_df
      #---------------------------------------------------------------------------------------------#
      
      
      #data formating and calculate angle/step length
      vessel_prep <- prepData(trackData = vessel_p[,c(1:2,4)], type = "LL",coordNames = c("longitude", "latitude"))
      
      # starting parameters
      if(length(which(vessel_prep$step==0))>0) { #check for zero inflation
        zeroInflation <- TRUE } else zeroInflation <- FALSE
      if (zeroInflation == TRUE) {
        stepPar0 <- c(0.01*n, 0.1*n, 0.2*n,  0.02*n, 0.1*n, 0.1*n,  0.5, 0, 0)
      } else { 
        stepPar0 <- c(0.01*n, 0.1*n, 0.2*n,  0.03*n, 0.1*n, 0.1*n) 
      }
      anglePar0 <- c(0,0,0,10,1,10)
      
      mod3 <- fitHMM(data = vessel_prep, nbStates = 3, stepPar0 = stepPar0, anglePar0 = anglePar0)
      list_mod[[i]] <- mod3
      names(list_mod)[i] <- paste0('HMM_pr',j,'_',i)  ### save an object with all models if visual checking necessary
      
      vessel_p$HMM3<- viterbi(mod3) #include model results in dataset
      
      ###________ Speed Filter
      #to reduce mode 2. All locations classified as 2 for which speed is > than mode 3 mean speed are reclassified as mode 3.
      #dtime
      vessel_d <- NULL
      #vessel$ID_trip <- NA
      for (k in 1:length(unique(vessel_p$ID_trip))) {
        d <- vessel_p[vessel_p$ID_trip == unique(vessel_p$ID_trip)[k],]
        d$dtime <- NA
        for (kk in 1:nrow(d)-1){
          d$dtime[kk] <- difftime(d$Date[kk+1],d$Date[kk],units="sec")
        }
        vessel_d <- rbind(vessel_d, d)
      }
      
      #speed
      vessel_d$step <- vessel_prep$step
      vessel_d$angle <- vessel_prep$angle
      vessel_d$speed <- (vessel_d$step*1000)/vessel_d$dtime
      
      
      #filter
      q3 = as.numeric(quantile(vessel_d$speed[vessel_d$HMM3 ==3], na.rm=TRUE)[2])
      #q11 = as.numeric(quantile(vessel_d$speed[vessel_d$HMM3 ==1], na.rm=TRUE)[2])
      #q12 = as.numeric(quantile(vessel_d$speed[vessel_d$HMM3 ==1], na.rm=TRUE)[4])
      vessel_d$HMM3_speedFilter <- ifelse(vessel_d$HMM3 == 2 & vessel_d$speed >= q3, 3, vessel_d$HMM3)
      #vessel_d$HMM3_speed_turn_Filter <- ifelse(vessel_d$HMM3_speedFilter == 2 & vessel_d$speed >= q11 & vessel_d$speed <= q12 & vessel_d$angle < pi/8, 1, vessel_d$HMM3_speedFilter)

      
      vessel_p <- vessel_d
      rm(vessel_d)
      
      ### PLOT ALL TRACKS FOR VESSEL i
      png(filename=paste0(path_plot,"/AllTrips_Vessel_",unique(vessel_pp$ID)[i],"_ping",n,"min.jpg"),width = 1920, height = 1200)
      colours <- ifelse(vessel_p$HMM3_speedFilter == 1, "red", ifelse(vessel_p$HMM3_speedFilter == 2, "orange", "grey40"))
      plot(vessel_p$longitude, vessel_p$latitude, asp=1, pch=20, cex = .8, type="p", xlab="x", ylab="y", 
           col=colours, main = paste0('Vessel ', unique(vessel_pp$ID)[i], 'all trips - ping rate ', n, 'min'),
           xlim=c(min(vessel_p$longitude),max(vessel_p$longitude)))
      legend("bottomleft", 
             legend = c("1 - Fishing", "2 - Mix Transit/Fishing", "3 - Transit"), 
             col = c("red","orange", "grey40"), 
             pch = c(20,20), 
             bty = "n", 
             pt.cex = 1, 
             cex = 0.9, 
             text.col = "black", 
             horiz = F , 
             inset = c(0.01, 0.01))
      dev.off()
      
      ### PLOT INDIVIDUAL TRACKS
      
      for (k in 1:length(unique(vessel_p$ID_trip))){
        png(filename=paste0(path_plot,"/Trips_Vessel_",unique(vessel_pp$ID_trip)[k],"_ping",n,"min.jpg"),width = 1280, height = 800)
        idt = unique(vessel_p$ID_trip)[k]
        colours <- ifelse(vessel_p$HMM3_speedFilter[vessel_p$ID_trip == idt] == 1, "red", ifelse(vessel_p$HMM3_speedFilter[vessel_p$ID_trip == idt] == 2, "orange", "grey40"))
        plot(vessel_p$longitude[vessel_p$ID_trip == idt], vessel_p$latitude[vessel_p$ID_trip == idt], asp=1, pch=20, cex = .8, type="p", xlab="x", ylab="y", col=colours, main = paste('Vessel ', unique(vessel_pp$ID)[i], 'trip #',idt,' - ping rate ', n, 'min'),
             xlim=c(min(vessel_p$longitude[vessel_p$ID_trip == idt]),max(vessel_p$longitude[vessel_p$ID_trip == idt])))
        segments(vessel_p$longitude[vessel_p$ID_trip == idt][-length(vessel_p$longitude[vessel_p$ID_trip == idt])],vessel_p$latitude[vessel_p$ID_trip == idt][-length(vessel_p$latitude[vessel_p$ID_trip == idt])],vessel_p$longitude[vessel_p$ID_trip == idt][-1L],vessel_p$latitude[vessel_p$ID_trip == idt][-1L],col=colours)
        points(vessel_p$longitude[vessel_p$ID_trip == idt], vessel_p$latitude[vessel_p$ID_trip == idt], asp=1, pch=20, cex = .8, type="p", xlab="x", ylab="y", col=colours)
        legend("bottomleft", 
               legend = c("1 - Fishing", "2 - Mix Transit/Fishing", "3 - Transit"), 
               col = c("red","orange", "grey40"), 
               pch = c(20,20), 
               bty = "n", 
               pt.cex = 1, 
               cex = 1, 
               text.col = "black", 
               horiz = F , 
               inset = c(0.01, 0.01))
        dev.off()
      }

      vessel_p$Activity_classification <- ifelse(vessel_p$HMM3_speedFilter==1, 'FISHING', ifelse(vessel_p$HMM3_speedFilter==2, 'MIX_TRANSIT', 'TRANSIT'))
      write.csv(vessel_p, paste0(path_res, "/VMS+HMMresults_Vessel_",unique(vessel_pp$ID)[i],"_ping",n,"min.csv"), row.names=FALSE)
      vessel_result_ping <- rbind(vessel_result_ping, vessel_p)
    }
    
    write.csv(vessel_result_ping, paste0(cwd,'/FPO_vms_ping',n,'min_resultClassification.csv'), row.names = FALSE)
    #rm(i, j, vessel, vessel_p)
    
    save(list_mod, file = paste0(cwd,'/models_RawOutput_ping',n,'min.Rdata'))
    vessel_result_all <- rbind(vessel_result_all, vessel_result_ping)
  }
  
  write.csv(vessel_result_all, paste0(path_folder_result,'FPO_vms_resultClassification.csv'), row.names = FALSE)
  
  t2 <- Sys.time()
  
  timing <- round(difftime(t2,t1,'mins'),1)
  
  txt <- paste0('Task done! It took ',as.numeric(timing), ' minutes to process. A new file with the result of the classification model has been written in ', path_folder_result, ' You can ignore the following warnings.')
  
  return(txt)
}
  