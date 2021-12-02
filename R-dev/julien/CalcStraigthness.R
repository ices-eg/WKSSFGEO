
### Calculate Straightness
### absolute difference between heading at t+1 and heading at t 

CalcStraigthness <- function(trip.path,  col.Dir){
  
  heading <- trip.path[, col.Dir]
  
  if( inherits(trip.path, "sf") ) {
    heading <- unlist( st_set_geometry(heading, NULL)) }
  lg.vec <- length(heading)
  
  trip.path$abs.HeadingChange <- abs(c(1, heading[2: lg.vec ]) - c(0, heading[1:( lg.vec -1)]))

  return(trip.path)
  
}
