# R4AIS v1.0.1
# Authors: Galdelli A., Armelloni E.N., Ferra C., Pulcinella J., Tassetti A.N.
# Release date: 14/05/2021
# For any issue write to maps.irbim@gmail.com
#
#This R code aims to analyze Automatic Identification System (AIS) data. 
#The processing workflow was developed on historical annotated data of the Adriatic Sea and serve to 
#(i) identify individual fishing trips;
#(ii) classify them on a monthly basis according to 5 predefined gear classes: bottom otter trawl (OTB), pelagic pair trawl (PTM), beam trawl (TBB), purse seine (PS), and "other" fishing (OTHER, including dredges and passive gears); 
#(iii) extract fishing activity and store it in spatial objects. 
#In the workflow are also provided some example for plotting the results.
#


# General settings ----

rm(list=ls(all=FALSE)) # clear previous variables etc
#options(digits=3) # displays all numbers with three significant digits as default
options(dplyr.summarise.inform=FALSE)
options("pbapply.pb"="txt")
setwd("..") # Working directory where the folder is stored. Default is one level backward to "R" folder
mydir=getwd()

# Required File names ----

dirmaps="maps" # path of the maps directory
file_parameters="data/parameters.csv" # import parameter table
file_centroids="data/centroids.csv" # import centroids 
outdir="results"

# User settings for the analysis ----

wgs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" # Insert your coordinates system
install.missing.packages=T # set to TRUE if want to allow automatic installation of missing packages.
write.output=T # set to TRUE to require table with results in output file 

# Load functions and parameters ----

source("R/global_functions.R") # load all internal function required

## the following lines load and arrange spatial layers required in the next analyses
ports<-read_sf(file.path(dirmaps, "med_harb_gsa")) # import list of ports
port_buf<-st_buffer(ports, 0.001) # create a buffer
st_crs(port_buf)=wgs # set crs
coastal_ban_zone=read_sf(file.path(dirmaps, "coastal_ban_zone")) # import managment depth layer
st_crs(coastal_ban_zone)=wgs # set crs
grid<-read_sf(file.path(dirmaps, "grid01degrees")) # import a grid
grid$grid_id=seq(1:nrow(grid)) # create cell grid id
st_crs(grid)=wgs # set crs

## the following lines load the parameters required in the next analyses
centroids=inport_parameters(file.path(file_parameters), file.path(file_centroids))[[2]]
pars=inport_parameters(file.path(file_parameters), file.path(file_centroids))[[1]]

## download baselayer from natural earth
worldmap <- rnaturalearth::ne_countries(scale='medium', type='map_units',   returnclass='sf') # import the map of land (need rnaturalearth library)
worldmap <- worldmap[,c("name_long", "geometry")]

## Application on single vessel data ----
all_dat<-read.csv("data/datatest.csv") # load all data
all_dat<-all_dat[,c("MMSI", "datetime", "longitude", "latitude", "speed")] # select fields of interest
vessels <-1 # Select a vessel. In the released sample there are OTB1, TBB1, PTM1, PS1 and OTHER1. 
dat=all_dat[which(all_dat$MMSI == vessels),] # select a vessel. In the released sample there are OTB1, TBB1, PTM1, PS1 and OTHER1.

# Fishing trip 
# The create_fishing_trip function allows to identify the starting and the ending point of all fishing trips performed by a vessel, 
# as well as information regarding the port of departure and of arrival (harbor name, country and statistical area).

dat_trip=create_fishing_trip(data=dat,
                             ports=ports, 
                             ports_buffer=port_buf,
                             coastal_ban_zone=coastal_ban_zone)

# The assign_trip function paste the information from the fishing trip (id of the trip) to the initial dataset. 
# Points that does not fall within a trip are removed.
dat_with_trip=assign_trip(data=dat, 
                          trip_table=dat_trip)


# Classification 
# The classification_wrapper function applies a cascade of classification algorithms on each fishing trip. 
# The output is a list of two objects: "classification_result" and "data_labelled". 
# "classification_result" contains the results of the classification algorithm for each fishing trip; 
# "data_labelled" returns the input data with one additional column for each classification algorithm, used to indicate the type of activity of the point. 
# The write.output argument permit to store the data needed to train the Random Forest model.
# The output.name argument is optional and permit to set the name of oputput file.
dat_classified=classification_wrapper(vessel_data=dat_with_trip, 
                                      pars=pars,
                                      write.output=T,
                                      output.name = "test1")

# The decision gear function applies the Random Forest prediction on the result of the "classification_wrapper" function.
gear=decision_gear(data=dat_classified[["classification_result"]])


# Export of fishing data 
# The identify_fishing_points function uses the prediction of the Random Forest model to identify the gear deployed, 
# and label each point of the original as fishing (1) or no fishing (0).
fishing_points=identify_fishing_points(data=dat_classified[["data_labelled"]], gear=gear, coord_sys = wgs)

# The make_fishing_tracks function trasform the fishing points into spatial segments.
fishing_tracks=make_fishing_tracks(data=fishing_points, coord_sys=wgs, pars=pars)


# Mapping the results 
# aggregation on the full time series
vessel_grid=estimate_fishing_effort(fishing_tracks, grid=grid)

for(i in 1:length(vessel_grid)){
  xfishing_tracks=fishing_tracks[[i]]
  xgear_grid=vessel_grid[[i]]
  xgear_grid = xgear_grid[!is.na(xgear_grid$gear),]
  xrange = extendrange(xgear_grid$long, f = 1) 
  yrange = extendrange(xgear_grid$lat, f = 1) 
  xmmsi=unique(xfishing_tracks$MMSI)
  ref_gear=unique(xfishing_tracks$gear)
  ref_year=unique(lubridate::year(xfishing_tracks$s_time))
  
  p = ggplot()+
    geom_sf(data=worldmap)+
    geom_sf(data=xgear_grid, aes(fill=f_hours), color=NA)+
    geom_sf(data=xfishing_tracks, aes(colour = as.factor(trip))) +
    coord_sf(xlim = xrange, ylim = yrange) +
    scale_fill_viridis() +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = NA, colour = NA),
          legend.justification = 'center',
          legend.position = "bottom",
          legend.direction = "horizontal") +
    guides(colour = guide_legend(title = "Trip", nrow = 1)) +
    ggtitle("Fishing effort")
 
  if(dir.exists(file.path(outdir, "plots"))==F){
    dir.create(file.path(outdir, "plots"))
  }
  
  ggsave(file.path(outdir, "plots", paste0(xmmsi, "-",ref_gear, "-", ref_year, ".png")), p)
}


# Full workflow application, single vessel ----

# The classification_workflow applies all the functions presented in the previous section in one single command. 
fishing_tracks=classification_workflow(data=dat,
                                       ports=ports, 
                                       ports_buffer=port_buf,
                                       coastal_ban_zone=coastal_ban_zone,
                                       pars=pars,
                                       coord_sys=wgs,
                                       output.type="tracks",
                                       write.output = T)


# Full workflow application,multiple vessels by gear and month ----
all_dat<-read.csv("data/datatest.csv")
all_dat=all_dat[,c("MMSI", "datetime", "longitude", "latitude", "speed")]
all_dat$MMSI=as.character(all_dat$MMSI)
vessels=unique(all_dat$MMSI)
all_fishing_tracks=list()
for(i in 1:length(vessels)){
  cat("\n", "vessel", i, "of", length(vessels))
  cat("\n")
  xvessel=all_dat[which(all_dat$MMSI == vessels[i]),]
  fishing_tracks=classification_workflow(data=xvessel,
                                         ports=ports, 
                                         ports_buffer=port_buf,
                                         coastal_ban_zone=coastal_ban_zone,
                                         pars=pars,
                                         coord_sys=wgs,
                                         output.type="tracks",
                                         write.output=T)
  all_fishing_tracks[[i]]=fishing_tracks
  names(all_fishing_tracks)[i]=vessels[i]
}

ref_gear=c("OTB1", "PTM", "PS", "TBB")

for(j in 1:length(ref_gear)){
  xx=lapply(all_fishing_tracks, function(x){
    rbindlist(lapply(x, function(y){
      if(nrow(y) != 0){
        if(unique(y$gear) == ref_gear[j]){
          return(y)
        }
      }
    }))
  })
  xgear=ldply(xx, data.frame)
  ref_time=unique(data.frame(xgear)[,c("year", "month")])
  for(i in 1:nrow(ref_time)){
    xvessel=xgear[which(xgear$year == ref_time$year[i] & xgear$month == ref_time$month[i]),]
    xvessel_ls=list(xvessel)
    xmap=estimate_fishing_effort(xvessel_ls, grid=grid)
    xmap=ldply(xmap, data.frame)
    xmap$year=ref_time$year[i]
    xmap$month=ref_time$month[i]
    xmap$gear=ref_gear[j]
    xmap=st_sf(xmap)
    if(dir.exists(file.path(outdir, "tables"))==F){
      dir.create(file.path(outdir, "tables"))
    }
    
    saveRDS(xmap, file.path(outdir, "tables", paste0(ref_gear[j], "_", ref_time$year[i], "-", ref_time$month[i], ".rData")))
    worldmap=st_crop(worldmap, st_buffer(xmap, 2.5)) # create map
    xrange = extendrange(xmap$long, f = 1) 
    yrange = extendrange(xmap$lat, f = 0.5) 
    xvessel_sf=st_sf(xvessel)
    p = ggplot()+
      geom_sf(data=worldmap)+
      geom_sf(data=xmap, aes(fill=f_hours), color=NA) +
      geom_sf(data=xvessel_sf, aes(linetype=MMSI, colour=as.factor(trip))) + 
      coord_sf(xlim=xrange, ylim=yrange) + 
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = NA, colour = NA),legend.justification = 'left', 
            legend.position = ifelse(ref_gear[j] == "PTM" | ref_gear[j] == "PS", "right", "bottom"),
            legend.direction = ifelse(ref_gear[j] == "PTM" | ref_gear[j] == "PS", "vertical", "horizontal")) +
      guides(colour = guide_legend(title = "trip", nrow = 4)) +
      ggtitle(paste0(ref_gear[j], ": ", ref_time$year[i], "-", ref_time$month[i]))
    ggsave(file.path(outdir, "plots", paste0(ref_gear[j], "-", ref_time$year[i], "-", ref_time$month[i], ".png")), p)
    
  }
}



