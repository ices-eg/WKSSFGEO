
### Calculate Acceleration in meters.s-2

#' Calculate Acceleration in meters.s-2
#'
#' @param trip.path xxx
#' @param col.time Varible name containing time (default "time_stamp")
#' @param col.speed Variable name containing speed (default "speed")
#' @param speed.units Speed units (default "knots"), any other interpeted as ms
#'
#' @return A track with additional variable named "acceleration"
#' @export
#'
CalcAcceleration <- function(trip.path, col.time =  "time_stamp", col.speed = "speed", speed.units = "knots"){
  
  colnames(trip.path)[ colnames(trip.path) %in% col.time] <- "DATE_TIME"
  
  timestamp <-   trip.path$DATE_TIME
  speed <- trip.path[, col.speed]
  
  if( inherits(trip.path, "sf") ) {
    speed <- unlist( sf::st_set_geometry(trip.path[, col.speed], NULL))
  }
  
  vector.length <- length(timestamp)
  timestamp <-   strptime(timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
  diff.Time <- c(NA, as.numeric(difftime(timestamp[2:vector.length], timestamp[1:(vector.length -1)], units = "sec")))
  
  if( speed.units %in% "knots"){
    
    speed <- speed * 1852/3600
    
  }
  
  acceleration <- speed / diff.Time
  acceleration[is.na(diff.Time)] <- 0
  diff.Time[is.na(diff.Time)] <- 0
  
  trip.path$acceleration <- acceleration
  
  colnames(trip.path) [ colnames(trip.path) %in% "DATE_TIME"] <- col.time

  if( inherits(trip.path, "sf") ) {
    trip.path <- trip.path[, c(which( !colnames(trip.path) %in% "geometry"), which( colnames(trip.path) %in% "geometry"))]
  }
  
  return(trip.path)
  
}
