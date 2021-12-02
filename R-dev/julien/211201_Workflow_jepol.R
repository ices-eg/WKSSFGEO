rm(list=ls())
if (!require(pacman)){
  install.packages("pacman", repos="http://ftp.ussg.iu.edu/CRAN/")
  library(pacman)
}
p_load(data.table, sf, mapview, devtools, readr, dplyr)


setwd("D:/RodriguezJ/ICES/WKSSFGEO/211201_Day3")
fld <- getwd()
fld.fun <- "D:/RodriguezJ/GeoLoc/Fonctions"
fld.fun %>% list.files()

lapply( paste(fld.fun, c("Clustering_KMeans.R", "DetectDirectionChanges.R", "dfTOsf.R", "CalcStraigthness.R",
                         "tune_RF.R", "set_0nbr.R", "CalcHeading.R", "CustomizedProjectedCRS.R") , sep = "/"), source)

# Download example file

githubURL1 <- "https://raw.githubusercontent.com/ices-eg/WKSSFGEO/main/data-examples/example_data_AIS.csv"
dat <- readr::read_csv(githubURL1)
setDT(dat)
dat

#Remove duplicates
dat <- unique(dat, by = c("vessel_id", "time_stamp"))


#Download the harbours file to you desk and load it to the r environment:
#https://github.com/ices-eg/WKSSFGEO/blob/main/data/harbours.rds
hbs <- readRDS(sprintf("%s/harbours.rds", fld))

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
schedule %>% colnames
out2 %>% colnames
out2 %>% class

out12 = out2[ !is.na(out2$gear),  ]

require (ranger)

out2 %>% summary
out2$speed %>% hist
out2$course %>% hist
out2$course %>% summary

out2$course[ out2$course > 360] <- NA

nnai <- apply( out2[, c("speed" ,     "course",     "behaviour" )], 1, function(x) !anyNA(x))
nnai %>% summary
out2$course[nnai] %>% hist

out2

out2$behaviour <- out2$behaviour %>% as.character %>% factor
optim.rf <- tune_RF( formula = behaviour ~ speed + course, out2[ nnai, ] %>% as.data.frame )
optim.rf

mod.rf <- ranger( behaviour ~ speed + course,  out2[ nnai, ], 
                 importance = "impurity", mtry =  optim.rf$mtry, min.node.size =  optim.rf$min.node.size, num.trees = 500, write.forest = TRUE)

mod.rf

#### 
with(out2, table(vessel_id,  behaviour))
with(out2, table(vessel_id,  gear))
with(out2, table(vessel_id,  trip_id))
nlevels(out2$trip_id %>% as.character %>% factor)

### Calculate new variables
trips <- unique(out2$trip_id)
k = 1

out.trip <- out2[nnai, ] %>% as.data.frame
out.trip <- out.trip[ out.trip$trip_id %in% trips[k], ]


out.NewCovar <- CalcHeading( st_as_sf( out.trip, 
                                       coords = c("lon", "lat"), crs = 4326))
plot(HEADING.deg ~ course, out.NewCovar, pch = "+")
abline( a = 0, b =1)

out.NewCovar <- CalcStraigthness(out.NewCovar,   col.Dir = "course")
out.NewCovar


out.NewCovar %>% colnames
library(sp)
spplot(out.NewCovar %>% as_Spatial, "course")
spplot(out.NewCovar %>% as_Spatial, "speed")
spplot(out.NewCovar %>% as_Spatial, "HEADING.deg")
spplot(out.NewCovar %>% as_Spatial, "abs.HeadingChange")

### Calculate for every trips

trips

out.ComNewCovar <- do.call( rbind, lapply( trips, function(tp){
    
  out.trip <- out2[nnai, ] %>% as.data.frame
  out.trip <- out.trip[ out.trip$trip_id %in% tp, ]
  out.NewCovar <- CalcStraigthness(
                          CalcHeading( st_as_sf( out.trip, 
                                         coords = c("lon", "lat"), crs = 4326))
                          ,   col.Dir = "course")
  return(out.NewCovar)
  
}))


spplot(out.NewCovar %>% as_Spatial, "course")

### Build RandomForest

optim.rf <- tune_RF( formula = behaviour ~ speed + course, out2[ nnai, ] %>% as.data.frame )
optim.rf

mod.rf <- ranger( behaviour ~ speed + course,  out2[ nnai, ], 
                  importance = "impurity", mtry =  optim.rf$mtry, min.node.size =  optim.rf$min.node.size, num.trees = 500, write.forest = TRUE)

mod.rf






Clustering.trip <- DetectDirectionChanges( st_as_sf( out.trip, 
                                                 coords = c("lon", "lat"), crs = 4326), 
                        col.Dir = "course", col.Speed = "speed")

with(Clustering.trip $trip.path, table(Clust.Pass, behaviour))
with(Clustering.trip $trip.path, table(behaviour, Pass.number))

mapview(Clustering.trip$trip.path, zcol = "Clust.Pass")
mapview(Clustering.trip$trip.path, zcol = "Pass.number")
mapview(Clustering.trip$trip.path, zcol = "behaviour")

#Look at a single trip
tst <- out2[trip_id == unique(out2$trip_id)[30]]
tst <- out2[trip_id == "EX_10_5_2"]

pts <- tst %>%
  sf::st_as_sf(coords = c("lon","lat")) %>%
  sf::st_set_crs(4326)

mapview(pts, zcol = "source")


