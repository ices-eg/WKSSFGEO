#' Add_harbours
#'
#' A function that will add a column "SI_HARB" to the gps datset
#' SI_HARB = 1 is where the vessel is considered in harbour
#'
#' @param x gps dataset
#' @param y harbour polygons
#'
#' @return A gps datset with an extra column "SI_HARB"
#' 
#' @export


add_harbours <- function(x, y) {
  
  # Needed checks
  # x has to contain variables lon and lat
  # y has to be of class sf and contain variable SI_HARB
  
  x %>%
    sf::st_as_sf(coords = c("lon", "lat"),
                 crs = 4326,
                 remove = FALSE) %>% 
    sf::st_join(y, join = sf::st_intersects) %>% 
    sf::st_drop_geometry() %>% 
    dplyr::mutate(SI_HARB = tidyr::replace_na(SI_HARB, 0))

}
