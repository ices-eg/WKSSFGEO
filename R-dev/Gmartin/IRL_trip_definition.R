# File: IRL_trip_definition.R
# Author: Guillermo Martin 
# Template Created: Wed Dec 01 11:55:15 2021
# ---------------------------------------------------------------------------
# Description:
# Attempt to compare trip definition using the "define_trip_pol.R" function 
# against the methods from the Irish iVMS 
# ---------------------------------------------------------------------------

rm(list=ls())

githubURL1 <- "https://raw.githubusercontent.com/ices-eg/WKSSFGEO/main/data-examples/example_data_AIS.csv"
dat <- readr::read_csv(githubURL1)
setDT(dat)
dat

#Remove duplicates
dat <- unique(dat, by = c("vessel_id", "time_stamp"))


#Download the harbours file to you desk and load it to the r environment:
#https://github.com/ices-eg/WKSSFGEO/blob/main/data/harbours.rds
hbs <- readRDS(file.path("C:/Users/ggonzales/Desktop/gmartin_work_folder/",
                         "WKSSFGEO/github_Repo/WKSSFGEO/data","harbours.rds"))

#Load the function define_trips.R
devtools::source_url("https://raw.githubusercontent.com/ices-eg/WKSSFGEO/main/R-dev/jepol/define_trips_pol.R")
devtools::source_url("https://raw.githubusercontent.com/ices-eg/WKSSFGEO/main/R-dev/jepol/add_harbours.R")
devtools::source_url("https://raw.githubusercontent.com/ices-eg/WKSSFGEO/main/R-dev/jepol/interpolate_ais.R")

# Jepol method to add harbours and interpolate AIS
out1 <- add_harbours(dat, hbs)
out11 <- interpolate_ais(out1)
table(out11$source)


#---------

#Use function to extract the trips from the dataset
out2 <- define_trips_pol(out11, min_dur = 0.8, max_dur = 48, 
                         split_trips = T, preserve_all = F)
length(unique(out2$trip_id))


#--------------
# 1st Irish method, starting from out11 data (after interpolation)
test<-out11
#reorder by ID and Date
test <- test[order(test$vessel_id, test$time_stamp),]

inHarb <- ifelse(test$SI_HARB==0, FALSE, TRUE)
table(inHarb)
  

## assign a trip identifier
test$SI_FT <- 1 # init
idx <- which( ! inHarb)
test[idx, "SI_FT"] <- cumsum(inHarb)[idx] # add a SI_FT index
  
summary(test[idx, "SI_FT"])
length(unique(test[idx, "SI_FT"]))
  
head(test)

length(unique(test$SI_FT[test$SI_HARB==0])) #7 more trips that in jeppol function 

