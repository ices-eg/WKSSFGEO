#HEADER ------------------------------------------------
#
#Author: Jacopo Pulcinella, Galdelli Alessandro
#Email: jacopo.pulcinella@irbim.cnr.it
#
#Date: 2021-12-02
#
#Script Name: 
# trip_ita_cnr_ais.R
#Script Description:
# Apply R4AIS v1.0.1 (10.5281/zenodo.4757505) to external data to identify individual fishing trips.
# The workflow require functions that are loaded via source file: global_functions.R
# Spatial layer are directly load in the workflow.
#Notes:
# setwd to 
# https://github.com/ices-eg/WKSSFGEO/tree/dev_branch/R-dev/galdelli_pulcinella_tassetti
# Currently, the workflow was tested on the data available in  "https://raw.githubusercontent.com/ices-eg/WKSSFGEO/main/data-examples/example_data_AIS.csv". 
# To run the workflow the input dataset should have the following mandatory fields:
# lon/lat,
# MMSI (vessel id)
# datetime (timestamp yyyy-mm-dd hh:mm:ss)
# speed
# The external layer required are:
# coastal_ban_zone - polygon of the 3nm buffer of the coast
# ports - polygons of the harbours with the following attributes:
# harbour (the name of the harbour)
# GSA: the name of the area

# General settings ----
rm(list=ls(all=FALSE))

#options(digits=3) 
options(dplyr.summarise.inform=FALSE)
options("pbapply.pb"="txt")
sf::sf_use_s2(FALSE)

setwd("..")
mydir=getwd()

# Required File names ----
# path of the maps directory
dirmaps="maps" 

# import parameter table
file_parameters="data/parameters.csv" 

# import centroids 
file_centroids="data/centroids.csv" 
outdir="results"

# User settings for the analysis ----
# Insert your coordinates system
wgs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

# set to TRUE if want to allow automatic installation of missing packages.
install.missing.packages=T 

# set to TRUE to require table with results in output file 
write.output=T 

# Load functions and parameters ----
# load all internal function required ----
source("R/global_functions.R") 

# load and arrange spatial layers required in the next analyses ----
## import list of ports 
# this is the list of harbours available https://github.com/MAPSirbim/AIS_data_processing
# we add some columns that are required for the workflow
ports<-readRDS("maps/harbours/ices_med_harbours.rData") 
# rename the area column in GSA
ports <- ports %>%
  dplyr:::rename("GSA" = "area")
st_crs(ports) <- 4326
port_buf<-st_buffer(ports, 0.001) # create a buffer
st_crs(port_buf) <- 4326   # set crs

## import coastal ban zone
# the coastal ban zone was manually estimated as a buffer of 3nm for some northern EU countries
coastal_ban_zone=read_sf("/maps/coastal_ban_zone/nord_eu_ban.shp") # import managment depth layer
st_crs(coastal_ban_zone) <- 4326  # set crs

## load the parameters required in the classficiation ----
# centroids: cetroids od kmeans and dbscann
# pars: generic for different functions
centroids=inport_parameters(file.path(file_parameters), file.path(file_centroids))[[2]]
pars=inport_parameters(file.path(file_parameters), file.path(file_centroids))[[1]]

## Application on single vessel data ----
# Download example file
# AIS
githubURL1 <- "https://raw.githubusercontent.com/ices-eg/WKSSFGEO/main/data-examples/example_data_AIS.csv"
dat <- read.csv(githubURL1)
dat
all_dat<-as.data.frame(dat) %>%
  mutate(datetime = time_stamp, MMSI = vessel_id, longitude = lon, latitude = lat) 
all_dat<-all_dat[,c("MMSI", "datetime", "longitude", "latitude", "speed")] # select fields of interest
vessels <-"EX_1" 

# GPS
githubURL2 <- "https://raw.githubusercontent.com/ices-eg/WKSSFGEO/dev_branch/data-examples/example_data_GPS.csv"
dat <- read.csv(githubURL2)
dat
all_dat<-as.data.frame(dat) %>%
  mutate(datetime = time, MMSI = vessel_id, longitude = lon, latitude = lat) 
all_dat<-all_dat[,c("MMSI", "datetime", "longitude", "latitude", "speed")]
vessels <-"id_Ben" 

dat=all_dat[which(all_dat$MMSI == vessels),] 

# Fishing trip 
# The create_fishing_trip function allows to identify the starting and the ending point of all fishing trips performed by a vessel, 
# as well as information regarding the port of departure and of arrival (harbor name, country and statistical area).
dat_trip=create_fishing_trip(data=dat,
                             ports=ports, 
                             ports_buffer=port_buf,
                             coastal_ban_zone=coastal_ban_zone)
dat_trip

# The assign_trip function paste the information from the fishing trip (id of the trip) to the initial dataset. 
# Points that does not fall within a trip are removed.
dat_with_trip=assign_trip(data=dat, 
                          trip_table=dat_trip)
