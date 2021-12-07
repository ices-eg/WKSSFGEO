#HEADER ------------------------------------------------
#
#Author: Jacopo Pulcinella
#Email: jacopo.pulcinella@irbim.cnr.it
#
#Date: 2021-12-07
#
#Script Name: Workflow_jepol_edit_jp
#
#Script Description: 
# I changed the path of the harbours file. 
# The new file is stored in the https://github.com/ices-eg/WKSSFGEO/tree/dev_branch/R-dev/galdelli_pulcinella_tassetti
# It contains the ices and meditteranean harbour
#Notes:
# to reproduce the code change the path of the harbour file to the dire
#

library(roxygen2)

jepol workflow ####
https://github.com/ices-eg/WKSSFGEO/blob/main/R-dev/jepol/Workflow_jepol.R
rm(list=ls())
if (!require(pacman)){
  install.packages("pacman", repos="http://ftp.ussg.iu.edu/CRAN/")
  library(pacman)
}

library(pacman)
p_load(data.table, sf, mapview, devtools, readr, dplyr)


# Download example file 
githubURL1 <- "https://raw.githubusercontent.com/ices-eg/WKSSFGEO/main/data-examples/example_data_AIS.csv"
dat <- readr::read_csv(githubURL1)
setDT(dat)
dat

#Remove duplicates
dat <- unique(dat, by = c("vessel_id", "time_stamp"))

#Download the harbours file to you desk and load it to the r environment:
#https://github.com/ices-eg/WKSSFGEO/blob/main/data/harbours.rds
# hbs <- readRDS("//aqua-cp-jepol18/C$/Users/jepol/Downloads/harbours.rds")
hbs <- readRDS(url("https://github.com/ices-eg/WKSSFGEO/blob/dev_branch/R-dev/galdelli_pulcinella_tassetti/maps/harbours/ices_med_harbours.rData?raw=true"))

#Load the function define_trips.R
devtools::source_url("https://raw.githubusercontent.com/ices-eg/WKSSFGEO/main/R-dev/jepol/define_trips_pol.R")
devtools::source_url("https://raw.githubusercontent.com/ices-eg/WKSSFGEO/main/R-dev/jepol/add_harbours.R")
devtools::source_url("https://raw.githubusercontent.com/ices-eg/WKSSFGEO/main/R-dev/jepol/interpolate_ais.R")

# dat2 <- 
# dat %>%
#   dplyr::group_by(vessel_id) %>%
#   dplyr::mutate(whacky = argosfilter::vmask(lat, lon, time_stamp, vmax = 12))

# add harbour 
out1 <- add_harbours(dat, hbs)
out11 <- interpolate_ais(out1)
table(out11$source)

#Use function to extract the trips from the dataset
out2 <- define_trips_pol(out11, min_dur = 0.8, max_dur = 48, 
                         split_trips = T, preserve_all = F)
saveRDS(out2, "data/jepol_pp_trip.rData")
schedule <- out2[,.(depart = min(time_stamp), return = max(time_stamp))
                 , by = .(vessel_id, trip_id)]
schedule <- schedule[!is.na(trip_id)]
schedule[, duration_hrs := as.numeric(difftime(return, depart, units = "hours"))]
schedule

#Look at a single trip
# tst <- out2[trip_id == unique(out2$trip_id)[30]]
# pts <- tst %>%
#   sf::st_as_sf(coords = c("lon","lat")) %>%
#   sf::st_set_crs(4326)
# mapview(pts, zcol = "source")
