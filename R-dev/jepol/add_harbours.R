add_harbours <- function(x, y){
  #add required packages
  require(data.table, sf)
  x[, lon2 := lon]
  x[, lat2 := lat]

dss <- x %>%
  sf::st_as_sf(coords = c("lon2","lat2")) %>%
  sf::st_set_crs(4326)

x[, lon2 := NULL]
x[, lat2 := NULL]

dss <- sf::st_join(dss, y, join = sf::st_intersects)

setDT(dss)
dss[is.na(SI_HARB), SI_HARB := 0]

dss[, geometry := NULL]

return(dss)
}
