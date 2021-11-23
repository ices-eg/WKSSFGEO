rm(list=ls())
#if (!require(pacman)){
#  install.packages("pacman", repos="http://ftp.ussg.iu.edu/CRAN/")
#  library(pacman)
#}
#pacman::p_load(data.table, sf, mapview, devtools, readr)
library(magrittr)

# Download example file

githubURL1 <- "https://raw.githubusercontent.com/ices-eg/WKSSFGEO/main/example_data_AIS.csv"
dat <- 
  readr::read_csv(githubURL1) %>% 
  dplyr::mutate(vid = stringr::str_sub(vessel_id, 4) %>% as.integer()) %>% 
  dplyr::arrange(vessel_id, time_stamp) %>% 
  dplyr::mutate(.rid = 1:dplyr::n())
data.table::setDT(dat)
dat

#Download the harbours file to you desk and load it to the r environment:
#https://github.com/ices-eg/WKSSFGEO/blob/main/jepol/harbours.rds

hbs <- readRDS("jepol/harbours.rds")

#Load the function define_trips.R
#devtools::source_url("https://raw.githubusercontent.com/ices-eg/WKSSFGEO/main/jepol/define_trips.R")
source("jepol/define_trips.R")
#Use function to extract the trips from the dataset
out <- define_trips_pol(dat)


#Look at a single trip
tst <- out[trip_id == unique(out$trip_id)[20]]

pts <- tst %>%
  sf::st_as_sf(coords = c("lon","lat"),
               crs = 4326)
mapview::mapview(pts)


