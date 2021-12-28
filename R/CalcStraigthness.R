
### Calculate Straightness
### absolute difference between heading at t+1 and heading at t 
### Should be problematic for values around 0-360 => over-estimation of the heading change!

#' Calculate Straightness
#'
#' @description Absolute difference between heading at t+1 and heading at t.
#' Should be problematic for values around 0-360 => over-estimation of the 
#' heading change!
#' 
#' @param trip.path xxx
#' @param col.Dir geometry column??
#'
#' @return The original sf object with a column "abs.HeadingChange"
#' @export
#'
CalcStraigthness <- function(trip.path,  col.Dir){
  
  heading <- trip.path[, col.Dir]
  
  if( inherits(trip.path, "sf") ) {
    heading <- unlist( sf::st_set_geometry(heading, NULL)) }
  lg.vec <- length(heading)
  
  trip.path$abs.HeadingChange <- abs(c(1, heading[2: lg.vec ]) - c(0, heading[1:( lg.vec -1)]))

  return(trip.path)
  
}
