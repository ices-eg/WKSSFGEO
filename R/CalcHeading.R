
### Calculate Heading
### Return the original sf object with a column "HEADING.deg" in degrees

CalcHeading <- function(trip.path){
  
  if(!inherits(trip.path, "sf")) {stop("trip.path must be a valid sf object")}
  if(is.na(sf::st_crs(trip.path))) {stop("CRS must be defined for proper distance calculation")}
  
  # Projection dans un systme de coordonnes planaire centr
  planar.proj <- CustomizedProjectedCRS(trip.path)
  trip.planar <- sf::st_transform(trip.path, planar.proj)
  
  coord <- sf::st_coordinates(trip.planar)
  
  dir <- sapply(1:(nrow(trip.path)-1), 
                function(i)  as.numeric(atan2((coord[i+1,2] - coord[i,2]),  (coord[i+1,1] - coord[i,1]) ))) 
  dir <- c(dir, dir[length(dir)])
  # Valeurs negatives de l'angle 
  dir[dir<0] <- 2 * pi + dir[dir<0] 
  # en degres 
  dir <- dir*180/pi
  
  trip.path$HEADING.deg <- dir
  columns.TripPath <- colnames(trip.path)
  trip.path <- trip.path[, c(which( !columns.TripPath %in% "geometry"), which( columns.TripPath %in% "geometry"))]
  
  return(trip.path)
  
}
