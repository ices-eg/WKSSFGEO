# plagarized from the geo-package that is not on cran
#' acdist
#' 
#' Computes distances between lat/lon data points. 
#'
#' @param lat Latitude of first coordinate or list with lat, lon of first coordinate.
#' @param lon Longitude of first coordinate or list with lat, lon of second coordinate.
#' @param lat1 If lat and lon are vectors of lat,lon positions, then lat1 and lon1 must be given as the second set of positions.
#' @param lon1 If lat and lon are vectors of lat,lon positions, then lat1 and lon1 must be given as the second set of positions.
#' @param scale "nmi" (default) returns value in nautical miles, any other value in kilometers
#'
#' @return A single vector of distances between pairs of points
#' @export
#'

rb_arcdist <- function (lat, lon, lat1 = NULL, lon1 = NULL, scale = "nmi") {
  if (is.null(lat1)) {
    lat1 <- lon$lat
    lon1 <- lon$lon
    lon <- lat$lon
    lat <- lat$lat
  }
  if (scale == "nmi") 
    miles <- 1.852
  else miles <- 1
  rad <- 6367
  mult1 <- (rad/miles)
  mult2 <- pi/180
  return(mult1 * acos(sin(mult2 * lat) * sin(mult2 * lat1) + 
                        cos(mult2 * lat) * cos(mult2 * lat1) * cos(mult2 * lon - 
                                                                     mult2 * lon1)))
}
