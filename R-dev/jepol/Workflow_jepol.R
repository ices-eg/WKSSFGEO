rm(list=ls())
if (!require(pacman)){
  install.packages("pacman", repos="http://ftp.ussg.iu.edu/CRAN/")
  library(pacman)
}
p_load(data.table, sf, mapview, devtools, readr, dplyr)


# Download example file

githubURL1 <- "https://raw.githubusercontent.com/ices-eg/WKSSFGEO/main/data-examples/example_data_AIS.csv"
dat <- readr::read_csv(githubURL1)
setDT(dat)
dat

#Download the harbours file to you desk and load it to the r environment:
#https://github.com/ices-eg/WKSSFGEO/blob/main/data/harbours.rds
hbs <- readRDS("//aqua-cp-jepol18/C$/Users/jepol/Downloads/harbours.rds")

#Load the function define_trips.R
devtools::source_url("https://raw.githubusercontent.com/ices-eg/WKSSFGEO/main/R-dev/jepol/define_trips_pol.R")
devtools::source_url("https://raw.githubusercontent.com/ices-eg/WKSSFGEO/main/R-dev/jepol/add_harbours.R")
devtools::source_url("https://raw.githubusercontent.com/ices-eg/WKSSFGEO/main/R-dev/jepol/interpolate_ais.R")

# dat2 <- 
# dat %>%
#   dplyr::group_by(vessel_id) %>%
#   dplyr::mutate(whacky = argosfilter::vmask(lat, lon, time_stamp, vmax = 12))



out1 <- add_harbours(dat, hbs)
out11 <- interpolate_ais(out1)
table(out11$source)

#Use function to extract the trips from the dataset
out2 <- define_trips_pol(out11, min_dur = 0.8, max_dur = 48, 
                        split_trips = T, preserve_all = F)

schedule <- out2[,.(depart = min(time_stamp), return = max(time_stamp))
                , by = .(vessel_id, trip_id)]
schedule <- schedule[!is.na(trip_id)]
schedule[, duration_hrs := as.numeric(difftime(return, depart, units = "hours"))]
schedule

#Look at a single trip
tst <- out2[trip_id == unique(out2$trip_id)[30]]
tst <- out2[trip_id == "EX_10_1"]

pts <- tst %>%
  sf::st_as_sf(coords = c("lon","lat")) %>%
  sf::st_set_crs(4326)

mapview(pts, zcol = "source")

