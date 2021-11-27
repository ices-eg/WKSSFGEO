# 2021-11-24 Einar Hjorleifsson (einar.hjorleifsson@gmail.com)
#  These functions were taken from a personal package of mine
#  [https://github.com/einarhjorleifsson/ramb] that is sort of under
#  development, future of it unknown. It is kind of me rambling
#  without clear direction or purpose :-)

#' rb_define_trip
#' 
#' Calculates a unique identifier for a change in event. Normally applied via
#' pipeflow within dplyr::mutate.
#'
#' @param vid a vector, e.g. containing vessel id. But it could be anything (think of it just as x)
#' @param hid a vector, e.g. containing harbour id, value assumed NA or zero if out of harbour. But it could be anything (think of it just as y)
#'
#' @return A vector containing unique integer, negative if in harbour, positive if 
#' out of harbour
#' 
#' @source Einar Hjörleifsson
#' 
#' @export
#'
#' @examples
#' tibble::tibble(vid = c(1, 1, 1, 1, 1, 1, 1, 
#'                        2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
#'                hid = c(NA, NA, 202, NA, NA, 202, 202, 
#'                        202, NA, NA, NA, NA, NA, NA, NA, NA, 202)) %>% 
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
    dplyr::mutate(tid = as.integer(ifelse(inharbour, -tid, tid))) %>% 
    dplyr::pull(tid)
}

# plagarized from the geo-package that is not on cran
#' rb_acdist
#' 
#' Computes APPROXIMATE distances between lat/lon data points
#'
#' @param lon Longitude of first coordinate or list with lat, lon of second coordinate.
#' @param lat Latitude of first coordinate or list with lat, lon of first coordinate.
#' @param lon1 If lat and lon are vectors of lat,lon positions, then lat1 and lon1 must be given as the second set of positions.
#' @param lat1 If lat and lon are vectors of lat,lon positions, then lat1 and lon1 must be given as the second set of positions.
#' @param scale "nmi" (default) returns value in nautical miles, any other value in kilometers
#'
#' @return A single vector of distances between pairs of points
#' @export
#' 
#' @source Einar Hjörleifsson
#'
#' @examples
#' 
#' rb_arcdist(-24.0, 65.0, -24.0, 66.0, "nmi") # Input in vector format.
#' # works with group_by
#' tibble::tibble(vid = c(1, 1, 1, 2, 2), 
#'                lon = c(-24, -24.5, -25, -25.5, -26), 
#'                lat = c(64.1, 64.2, 64.3, 64.5, 64.6)) %>% 
#'   dplyr::group_by(vid) %>% 
#'   dplyr::mutate(sd = rb_arcdist(lon, lat, dplyr::lead(lon), dplyr::lead(lat)))
rb_arcdist <- function (lon, lat, lon1 = NULL, lat1 = NULL, scale = "nmi") {
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

#' @title rb_sd
#' 
#' @description Calculate distance to next point. Insired by traipse::track_distance but 
#' allows to pass more arguement to geodist::geodist. Also the default 
#' is to calculate the distance to next point, not distance from previous
#' point.
#' 
#' @note Why not use geodist::geodist directly? Because it does return a vector 
#' which is length one less than the orginal vector length (because NA's are
#' dropped). And that produces an error if one uses the tidyverse flow.
#'
#' @param x longitude
#' @param y latitude
#' @param to Boolean (default TRUE) then calculation is distance to next point, 
#' hence last point will be NA.
#' @param measure One of "haversine" (default), "vincenty", "geodesic", or "cheap" 
#' specifying desired method of geodesic distance calculation, see help for 
#' geodist::geodist.
#'
#' @return A vector distance in meters.
#' @export
#'
#' @examples
#' rb_sd(c(-24, -24), c(65.0001, 65.0002))
#' 
rb_sd <- function (x, y, to = TRUE, measure = "geodesic") 
{
  x <- 
    geodist::geodist(cbind(x, y), sequential = TRUE, 
                     measure = measure)
  if(to) {
    x <- c(x, NA_real_)
  } else {
    x <- c(NA_real_, x)
  }
  return(x)
}


#' rb_ms2kn
#'
#' meters per second to knots for those of us that forget the convertion number
#' 
#' @param x A numerical vector of speed in meters per second
#'
#' @return A vector, speed in knots (nautical miles per hour)
#' @export
#' 
#' @source Einar Hjörleifsson
#'
#' @examples
#' rb_ms2kn(0:8)
rb_ms2kn <- function(x) {
  x * 1.94384449
}

#' rb_kn2ms
#'
#' knots to meters per second for those of us that forget the convertion number
#' 
#' @param x A numerical vector of speed in knots
#'
#' @return A vector, speed in meters per second
#' @export
#' 
#' @source Einar Hjörleifsson
#'
#' @examples
#' rb_kn2ms(seq(0, 16, by = 2))
rb_kn2ms <- function(x) {
  x / 1.94384449
}

#' rb_event
#' 
#' Identify change in event
#'
#' @param x A vector, normally character or integer. For track data this could
#' e.g. be change in fishing behaviour
#'
#' @return A numerical vector that labels each "discreet" event
#' @export
#' 
#' @source Einar Hjörleifsson
#'
#' @examples
#' tibble::tibble(vid = c(rep(1, 10), rep(2, 10)),
#' behaviour = c(rep("steaming", 4), rep("fishing", 4), rep("steaming", 2),
#'               rep("harbour", 2), rep("steaming", 2), rep("fishing", 6))) %>% 
#'   dplyr::group_by(vid) %>% 
#'   dplyr::mutate(change = rb_event(behaviour))
rb_event <- function(x) {
  x <- dplyr::if_else(x != dplyr::lag(x), 1L, 0L, 1L)
  x <- cumsum(x)
  return(x)
}


rb_speed <- function(time, lon, lat) {
  (rb_arcdist(lon, lat, dplyr::lead(lon), dplyr::lead(lat), scale = "km") * 1e3) /
    as.numeric(difftime(dplyr::lead(time), time, units = "sec")) %>% 
    rb_ms2kn()
}
