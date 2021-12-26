# Create a customized projected CRS
# Returns a "crs" object

CustomizedProjectedCRS <- function( sfobj ){

  if( inherits(sfobj, "Spatial")) { sfobj <-  sf::st_as_sf(sfobj)  }
  if( !inherits(sfobj, "sf")) stop("sfobj must be a sf spatial Object with valid CRS set")
  if( is.na(st_crs(sfobj))) stop("sfobj must be a sf spatial Object with valid CRS set")

  bb <- sf::st_bbox( sf::st_transform(sfobj, crs = sf::st_crs(4326)))
  bbox <- sf::st_sf("bb", dfTOsf(as.matrix(expand.grid(bb[c("xmin", "xmax")], bb[c("ymin", "ymax")]))[c(1,3,4,2),], type = "polygon"))
  boxLL <- sf::st_bbox(bbox)

  llc <- c(mean( boxLL[c("xmin", "xmax")]), 
         mean(boxLL[c("ymin", "ymax")]))

  # construct the proj4 string
  angle <- 0.00001
  prj = paste0(
    "+proj=omerc +lat_0=",
    llc[2],
    " +lonc=",
    llc[1],
    " +alpha=",
    angle,
    " +gamma=0.0 +k=1.000000 +x_0=0.000 +y_0=0.000 +ellps=WGS84 +units=m "
    )

  return( sf::st_crs(prj))

}

