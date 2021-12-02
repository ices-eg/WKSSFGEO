# File: create_eu_harbours.R
# Author: Pulcinella Jacopo 
# Template Created: Wed Dec 02 2021
# ---------------------------------------------------------------------------
# Description:
# Create a single shp of European harbours, as available in the WKSSFGEO repo: https://github.com/ices-eg/WKSSFGEO/tree/dev_branch/data
# and in the Italian CNR repo: https://github.com/MAPSirbim/AIS_data_processing/tree/main/maps/med_harb_gsa
# ---------------------------------------------------------------------------

library(sf)
library(dplyr)

# Mediterranean harbours
med_harb = read_sf("maps/med_harb_gsa/med_harb_gsa.shp") %>%
  st_set_crs(4326) %>%
  mutate(SI_HARB = 1, area = GSA) %>%
  dplyr:::select(-lon, -lat, -GSA)
 
# Northern Harbours
# harbour layer available https://github.com/ices-eg/WKSSFGEO/tree/dev_branch/data
nord_harb = readRDS("/home/tekno/Scrivania/repositories/wgssfgeo/harbours.rds") %>%
  st_set_crs(4326)
nord_harb_edit = nord_harb %>%
  mutate(Country = 1:nrow(nord_harb), harbour = 1:nrow(nord_harb), area = 1:nrow(nord_harb))

# combine harbours
all_harb = rbind(med_harb, nord_harb_edit)
