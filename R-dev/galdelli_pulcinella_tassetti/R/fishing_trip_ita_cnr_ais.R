# File: fishing_trip_ita_cnr_ais.R
# Author: Pulcinella Jacopo 
# Template Created: Wed Dec 02 2021
# ---------------------------------------------------------------------------
# Description:
# Apply R4AIS v1.0.1 external data to identify individual fishing trips.
# The workflow require external layer that are loaded via source file: global_functions.R
# Currently, to run the workflow in easy way it is convenient to load the R project in the folder.
# ---------------------------------------------------------------------------


# General settings ----
rm(list=ls(all=FALSE))
#options(digits=3) 
options(dplyr.summarise.inform=FALSE)
options("pbapply.pb"="txt")
# Working directory should point to the repository folder https://github.com/ices-eg/WKSSFGEO/tree/dev_branch/R-dev/galdelli_pulcinella_tassetti
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
ports<-readRDS("/home/tekno/Scrivania/repositories/wgssfgeo/harbours.rds") 
st_crs(ports) <- 4326  
ports = ports %>%
  mutate(Country = "DNK", harbour = as.character(1:nrow(ports)), GSA =  as.character(1:nrow(ports)))
port_buf<-st_buffer(ports, 0.001) # create a buffer
st_crs(port_buf) <- 4326   # set crs
## import coastal ban zone
# the coastal ban zone was manually estimated as a buffer of 3nm for some northern EU countries
coastal_ban_zone=read_sf("/home/tekno/Scrivania/AIS/AIS/maps/world/nord_eu_ban.shp") # import managment depth layer
st_crs(coastal_ban_zone) <- 4326  # set crs
## import grid for fishing effort
# Currently, it was load at the start of the workflow and it. 
# grid<-read_sf(file.path(dirmaps, "grid01degrees")) # import a grid
# grid$grid_id=seq(1:nrow(grid)) # create cell grid id
# st_crs(grid)=wgs # set crs
## download baselayer from natural earth
worldmap <- rnaturalearth::ne_countries(scale='medium', type='map_units',   returnclass='sf')
# import the map of land (need rnaturalearth library)
worldmap <- worldmap[,c("name_long", "geometry")]

## load the parameters required in the classficiation ----
# centroids: cetroids od kmeans and dbscann
# pars: generic for different functions
centroids=inport_parameters(file.path(file_parameters), file.path(file_centroids))[[2]]
pars=inport_parameters(file.path(file_parameters), file.path(file_centroids))[[1]]

## Application on single vessel data ----
# Download example file
githubURL1 <- "https://raw.githubusercontent.com/ices-eg/WKSSFGEO/main/data-examples/example_data_AIS.csv"
dat <- readr::read_csv(githubURL1)
dat
all_dat<-as.data.frame(dat) %>%
  mutate(datetime = time_stamp, MMSI = vessel_id, longitude = lon, latitude = lat) 
all_dat<-all_dat[,c("MMSI", "datetime", "longitude", "latitude", "speed")] # select fields of interest
vessels <-"EX_1" # Select a vessel. In the released sample there are OTB1, TBB1, PTM1, PS1 and OTHER1. 
dat=all_dat[which(all_dat$MMSI == vessels),] # select a vessel. In the released sample there are OTB1, TBB1, PTM1, PS1 and OTHER1.

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
