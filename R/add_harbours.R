#' Add_harbours
#'
#' A function that will add a column "SI_HARB" to the gps datset
#' SI_HARB = 1 is where the vessel is considered in harbour
#'
#' @param gps dataset
#' @param harbour polygons
#'
#' @return A gps datset with an extra column "SI_HARB"


add_harbours <- function(x, y){
  #add required packages
  require(data.table, sf)
  x[, lon2 := lon]
  x[, lat2 := lat]

  #Make gps dataset spatial
dss <- x %>%
  sf::st_as_sf(coords = c("lon2","lat2")) %>%
  sf::st_set_crs(4326)

x[, lon2 := NULL]
x[, lat2 := NULL]

# Add a column to the dataset, 1 = in Harbour
dss <- sf::st_join(dss, y, join = sf::st_intersects)

setDT(dss)

#If its no inside a harbour set the SI_HARB to 0
dss[is.na(SI_HARB), SI_HARB := 0]

dss[, geometry := NULL]

return(dss)
}
