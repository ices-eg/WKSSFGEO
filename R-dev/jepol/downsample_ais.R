#Find intervals closest to one minute between and only save those. 

downsample_ais <- function(x, counter = "min", diff = 1){

setDT(x)[,time_stamp2 := time_stamp]

counter <- "sec"
counter <- "min"
counter <- "hour"
diff <- 3

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



out <- data.table()
for(i in unique(x$vessel_id)){
  progress(match(i, unique(x$vessel_id)),length(unique(x$vessel_id)))
  
  ais <- x[vessel_id == i]
  intervals <- seq(round(min(ais$time_stamp, na.rm = T), paste(counter)), 
                 round(max(ais$time_stamp, na.rm = T), paste(counter)), by=paste(diff, counter))
ais <- ais[, .SD[.(time_stamp2 = intervals), on = .(time_stamp2), roll = 'nearest']]
ais <- unique(ais, by = "time_stamp")
ais[, time_stamp2 := NULL]

out <- 
setorder(out, vessel_id, time_stamp)
out[, recid := NULL]
return(out)
