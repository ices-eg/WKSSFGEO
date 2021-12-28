
### Calculate speed between two positions
### Return the original sf object with a column "DIFFTIME.secs" in seconds, "SPEED.kn" in knots and eventually "DISTANCE.nm" in nautic miles if CalcDist hasn't been applied before.

#' Calculate speed between two positions
#'
#' @param trip.path xxx
#' @param col.time Varible name containing time (default "time_stamp")
#' @param verbose Details of output (default FALSE)
#'
#' @return The original sf object with a column "DIFFTIME.secs" in seconds, 
#' "SPEED.kn" in knots and eventually "DISTANCE.nm" in nautic miles if
#' CalcDist hasn't been applied before.
#' @export
#'
CalcSpeed <- function(trip.path, col.time =  "time_stamp", verbose = FALSE){
  
  if(!inherits(trip.path, "sf")) {stop("trip.path must be a valid sf object")}
  if(!col.time %in% colnames(trip.path)) {stop("Time column not found")}
  if( !"DISTANCE.nm" %in% colnames(trip.path)) {
    trip.path <- CalcDist(trip.path = trip.path, col.time = col.time)
    if(verbose) {print("Calculate Distance using CalcDist")}
  }
  if(is.na(sf::st_crs(trip.path))) {stop("CRS must be defined for proper distance calculation")}
  
  colnames(trip.path)[ colnames(trip.path) %in% col.time] <- "DATE_TIME"
  nr <- nrow(trip.path)
  
  timestamp <-   strptime(trip.path$DATE_TIME, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
  diff.Time <- c(NA, as.numeric(difftime(timestamp[2:nr], timestamp[1:(nr-1)], units = "sec")))
  
  speed <- trip.path$DISTANCE.nm / ( diff.Time/3600)
  speed[is.na(diff.Time)] <- 0
  diff.Time[is.na(diff.Time)] <- 0
  
  trip.path$DIFFTIME.secs <- diff.Time
  trip.path$SPEED.kn <- speed
  columns.TripPath <- colnames(trip.path)
  trip.path <- trip.path[, c(which( !columns.TripPath %in% "geometry"), which( columns.TripPath %in% "geometry"))]
  colnames(trip.path) [ colnames(trip.path) %in% "DATE_TIME"] <- col.time
  
  return(trip.path)
  
}
