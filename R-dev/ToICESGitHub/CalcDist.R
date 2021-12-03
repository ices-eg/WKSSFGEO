
### Calculate distance between positions using great circle distance calculations see ?sf::st_distance
### Return the original sf object with a column "DISTANCE.nm" in nautic miles

CalcDist <- function(trip.path, col.time =  "time_stamp"){
  
  if(!inherits(trip.path, "sf")) {stop("trip.path must be a valid sf object")}
  if(!col.time %in% colnames(trip.path)) {stop("Time column not found")}
  if(is.na(sf::st_crs(trip.path))) {stop("CRS must be defined for proper distance calculation")}
  
  trip.path <- trip.path[, !colnames(trip.path) %in% "DISTANCE.nm"]
  timestamp <-   strptime(unlist(sf::st_set_geometry(trip.path[, col.time], NULL)), format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
  trip.path <- trip.path[ order(timestamp), ]

  dist.unit <- units(sf::st_distance(trip.path[1,], trip.path[2,]))[[1]]
  mat.dist <-  matrix(as.numeric( sf::st_distance(trip.path)), nrow = nrow(trip.path), ncol = nrow(trip.path))[ -nrow(trip.path), - 1]
  if( "m" %in% dist.unit) {
    dist.betweenPoints <- c(0, diag(mat.dist))* 0.539957/1000
    }else{ stop("distance unit not recognized")}
  
  trip.path$DISTANCE.nm <-  dist.betweenPoints 
  columns.TripPath <- colnames(trip.path)
  trip.path <- trip.path[, c(which( !columns.TripPath %in% "geometry"), which( columns.TripPath %in% "geometry"))]
  
  return(trip.path)

}

