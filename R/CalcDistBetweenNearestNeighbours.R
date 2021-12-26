
### Calculate Distance between the point and its n nearest neighbours
### Return the original sf object with n columns showing the distances in meters to the n nearest neighbours
### Use of the method implemented in RANN::nn2 which must be the fastest existing in R for this purpose

CalcDistBetweenNearestNeighbours <- function(trip.path, nnn = 9){
  
  if(!inherits(trip.path, "sf")) {stop("trip.path must be a valid sf object")}
  if(is.na(sf::st_crs(trip.path))) {stop("CRS must be defined for proper distance calculation")}
  
  # Projection dans un syst?me de coordonn?es planaire centr? 
  planar.proj <- CustomizedProjectedCRS(trip.path)
  trip.planar <- sf::st_transform(trip.path, planar.proj)
  
  coords <- sf::st_coordinates(trip.planar)
  nghb.mat <- RANN::nn2(coords, k = nnn+1)
  dist.mat <- stats::setNames( as.data.frame( as.matrix( nghb.mat$nn.dists[, -1])),
                       paste0( "DistWithNeighbour_", set_0nbr(1:nnn), 1:nnn))
  trip.path <- cbind(trip.path, dist.mat)
  
  trip.path <- trip.path[, c(which( !colnames(trip.path) %in% "geometry"), which( colnames(trip.path) %in% "geometry"))]
  
  return(trip.path)
  
}
