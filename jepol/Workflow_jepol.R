rm(list=ls())
if (!require(pacman)){
  install.packages("pacman", repos="http://ftp.ussg.iu.edu/CRAN/")
  library(pacman)
}
p_load(data.table, sf, mapview)


# Download example file

githubURL1 <- "https://raw.githubusercontent.com/ices-eg/WKSSFGEO/main/example_data_AIS.csv"
dat <- read_csv(githubURL1)
setDT(dat)
dat

#Download the harbours file to you desk and load it to the r environment:
#https://github.com/ices-eg/WKSSFGEO/blob/main/jepol/harbours.rds

hbs <- readRDS("harbours.rds")

#Load the function define_trips.R
library(devtools)
devtools::source_url("https://raw.githubusercontent.com/ices-eg/WKSSFGEO/main/jepol/define_trips.R")

#Use function to extract the trips from the dataset
out <- define_trips(dat)

#Look at a single trip
tst <- out[trip_id == unique(out$trip_id)[20]]

pts <- tst %>%
  sf::st_as_sf(coords = c("lon","lat")) %>%
  sf::st_set_crs(4326)

mapview(pts)


