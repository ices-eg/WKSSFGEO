# 2021-11-24 Einar Hjorleifsson (einar.hjorleifsson@gmail.com)
#  These functions were taken from a personal package of mine
#  [https://github.com/einarhjorleifsson/ramb] that is sort of under
#  development, future of it unknown. It is kind of me rambling
#  without clear direction or purpose :-)

#' Define trip
#' 
#' Calculates a unique identifier for a change in event. Normally applied via
#' pipeflow within dplyr::mutate.
#'
#' @param vid vector, e.g. containing vessel id
#' @param hid vector, e.g. containing harbour id, value assumed NA or zero if out of harbour
#'
#' @return A vector containing unique integer, negative if in harbour, positive if 
#' out of harbour
#' @export
#'
#' @examples
#' tibble::tibble(vid = c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
#'                hid = c(NA, NA, 202, NA, NA, 202, 202, 202, NA, NA, NA, NA, NA, NA, NA, NA, 202)) %>% 
#'   dplyr::mutate(tid = rb_define_trips(vid, hid))
rb_define_trips <- function(vid = vid, hid = hid) {
  tibble::tibble(vid = {{ vid }},
                 hid = {{ hid }}) %>% 
    dplyr::mutate(inharbour = ifelse(!is.na( hid | hid == 0), TRUE, FALSE)) %>% 
    dplyr::group_by( vid ) %>% 
    dplyr::mutate(.gr0 = data.table::rleid( inharbour )) %>% 
    dplyr::group_by( vid, inharbour) %>% 
    dplyr::mutate(tid = data.table::rleid(.gr0)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(tid = ifelse(inharbour, -tid, tid)) %>% 
    dplyr::pull(tid)
}


# plagarized from the geo-package that is not on cran
#' rb_acdist
#' 
#' Computes APPROXIMATE distances between lat/lon data points.
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
#' @examples
#' 
#' rb_arcdist(65.0, -24.0, 66.0, -24.0, "nmi") # Input in vector format.
#' # works with group_by
#' tibble::tibble(vid = c(1, 1, 1, 2, 2), lon = c(64.1, 64.2, 64.3, 64.5, 64.6), lat = c(-24, -24.5, -25, -25.5, -26)) %>% 
#'   dplyr::group_by(vid) %>% 
#'   dplyr::mutate(sd = rb_arcdist(lat, lon, dplyr::lead(lat), dplyr::lead(lon)))

rb_arcdist <- function (lat, lon, lat1 = NULL, lon1 = NULL, scale = "nmi") {
  if (is.null(lat1)) {
    lat1 <- lon$lat
    lon1 <- lon$lon
    lon <- lat$lon
    lat <- lat$lat
  }
  if (scale == "nmi") 
    miles <- 1.8512
  else miles <- 1
  rad <- 6367
  mult1 <- (rad/miles)
  mult2 <- pi/180
  return(mult1 * acos(sin(mult2 * lat) * sin(mult2 * lat1) + 
                        cos(mult2 * lat) * cos(mult2 * lat1) * cos(mult2 * lon - 
                                                                     mult2 * lon1)))
}

#' ms2kn
#'
#' meters per second to knots for those of us that forget the convertion number
#' 
#' @param x A numerical vector of speed in meters per second
#'
#' @return A vector, speed in knots (nautical miles per hour)
#' @export
#'
ms2kn <- function(x) {
  x * 1.94384449
}

#' kn2ms
#'
#' knots to meters per second for those of us that forget the convertion number
#' 
#' @param x A numerical vector of speed in knots
#'
#' @return A vector, speed in meters per second
#' @export
#'
kn2ms <- function(x) {
  x / 1.94384449
}

