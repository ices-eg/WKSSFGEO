#' downsample_ais_timestep
#
#Developed by Jeppe Olsen, DTU aqua
#Adds column 'downsample', is the points closest to the time defined 
#in unit and length is defined (1 and 0)
#
#
#' @param x gps dataset
#' @param unit sec/min/hour - the unit of the timestep
#' @param length number of units between timestep
#'
#'
downsample_ais_timestep <- function(x, unit = "min", length = 1){


setDT(x)[,time_stamp2 := time_stamp]
  x[, downsample := NULL]
x[, recid := 1:.N]

require(data.table)
'%!in%' <- function(x,y)!('%in%'(x,y))
progress <- function (x, max = 100) {
  percent <- x / max * 100
  cat(sprintf('\r[%-50s] %d%%',
              paste(rep('=', percent / 2), collapse = ''),
              floor(percent)))
  if (x == max)
    cat('\n')
}

ref <- c()
for(i in unique(x$vessel_id)){
  progress(match(i, unique(x$vessel_id)),length(unique(x$vessel_id)))
  
  ais <- x[vessel_id == i]
  intervals <- seq(round(min(ais$time_stamp, na.rm = T), paste(unit)), 
                 round(max(ais$time_stamp, na.rm = T), paste(unit)), by=paste(length, unit))
ais <- ais[, .SD[.(time_stamp2 = intervals), on = .(time_stamp2), roll = 'nearest']]
ais <- unique(ais, by = "time_stamp")

ref <- c(ref, ais$recid)

}
x[recid %in% ref, downsample := 1]
x[is.na(downsample), downsample := 0]

setorder(x, vessel_id, time_stamp)
x[, time_stamp2 := NULL]
x[, recid := NULL]

}


