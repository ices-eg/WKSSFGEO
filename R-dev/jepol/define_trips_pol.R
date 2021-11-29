#' define_trips_pol
#'
#' Use the columns "SI_HARB" to determine when a vessel is on a trip. A trip is defined
#' from when it leaves the harbour till it returns
#'
#' @param x gps dataset
#' @param min_dur the minimum trip length (hours)
#' @param max_dur the maximum trip length (hours)
#' @param split_trips If the trip is longer than the maximum hours, it will try to split
#' the trip into two or more trips, if there is long enough intervals between pings
#' @param preserve_all Should all pings inside harbour be preserved also?
#'
#' @return a gps datset

define_trips_pol <- function(x, min_dur = 0.5, max_dur = 72, 
                             split_trips = T, preserve_all = F){
  #add required packages
  require(data.table, sf)
  '%!in%' <- function(x,y)!('%in%'(x,y))
  progress <- function (x, max = 100) {
    percent <- x / max * 100
    cat(sprintf('\r[%-50s] %d%%',
                paste(rep('=', percent / 2), collapse = ''),
                floor(percent)))
    if (x == max)
      cat('\n')
  }
  
  setDT(x)[, recid := 1:.N]
  if("SI_HARB" %!in% names(x) | any(is.na(x$SI_HARB)))
    stop("No harbour column in dataset, please add it (add_harbour)")
  
  
  out <- data.table()
for(i in unique(x$vessel_id)){
  progress(match(i, unique(x$vessel_id)),length(unique(x$vessel_id)))
 
  gps <- x[vessel_id == i]

     
  dss <- gps
  setorder(dss, time_stamp)
  dss[, INTV:=-as.numeric(difftime(data.table::shift(time_stamp, fill = NA, type = "lag"), time_stamp, units = "mins"))]

table(dss$SI_HARB)

dss[, id := 1:.N]
dss[, geometry := NULL]
### Add harbour id and depart / return to dss

dss[, SI_HARB2 := approx((1:.N)[!is.na(SI_HARB)],na.omit(SI_HARB),1:.N)$y]


dss[is.na(SI_HARB), SI_HARB := pos2[dss[is.na(SI_HARB)], on = .(id = id), SI_HARB]]

dss[, SI_HARB := SI_HARB[1], .(cumsum(!is.na(SI_HARB)))]

dss[, HARB_EVENT := data.table::shift(SI_HARB, fill = NA, type = "lag") - SI_HARB]
dss[HARB_EVENT == -1, HARB_EVENT := 2]
dss[is.na(HARB_EVENT), HARB_EVENT := 0]

#if there is no trips for the vessel, move to the next.
if(all(dss$HARB_EVENT == 0))
  next

#If the vessel is out of harbour at the start of the dataset, set the first point as a departure event
if(dss[1]$SI_HARB == 0)
  dss[1, HARB_EVENT := 1]

#If the vessel is out of harbour at the end of the dataset, set the last point as a return event
if(dss[.N]$SI_HARB == 0)
  dss[.N, HARB_EVENT := 2]

#Make data.table with timings of trip
trip <- data.table(vessel_id = i,
                   depart = dss[HARB_EVENT == 1]$time_stamp,
                   return = dss[HARB_EVENT == 2]$time_stamp
)

trip[, trip_id2 := paste0(i, "_", 1:.N)]
trip[, duration_hours := as.numeric(difftime(return, depart, units = "hours"))]


#Only use trips longer than min_dur
trip <- trip[duration_hours > min_dur]

#Split trips longer than max_dur into smaller trips, based on the longest interval between pings
if(split_trips == T)
  while (any(trip$duration_hours > max_dur)) {
    tls <- trip[duration_hours > max_dur][1,]
    cutp2 <- dss[time_stamp > tls$depart & time_stamp < tls$return][base::which.max(INTV[2:.N])]$id
    
    if(dss[id == cutp2]$INTV < 3){
      trip[trip_id2 == tls$trip_id2, duration_hours := max_dur]
      next
    }

    newtrips <- data.table(vessel_id = i,
                           depart = c(tls$depart, dss[id == cutp2]$time_stamp),
                           return = c(dss[id == cutp2-1]$time_stamp, tls$return),
                           trip_id2 = paste(tls$trip_id2, 1:2, sep = "_"))
    newtrips[, duration_hours := as.numeric(base::difftime(return, depart, units = "hours"))]
    trip <- rbindlist(list(trip[trip_id2 != tls$trip_id2], newtrips))
    setorder(trip, depart)
}

trip <- trip[duration_hours != 0]

if(any(trip$duration_hours < min_dur)){
  print(trip[duration_hours < min_dur])
  warning(paste("At least one trip for", i, "is shorter than min_dur, after splitting up trips based on max_dur:") )
  trip <- trip[duration_hours > min_dur]
  
}


setkey(trip, vessel_id, depart, return)
gps[ ,time_stamp2 := time_stamp]
setkey(gps, vessel_id, time_stamp, time_stamp2)

midi <- foverlaps(gps, trip, type="any", nomatch=NULL) 
gps[, time_stamp2 := NULL]

midi[!is.na(trip_id2), trip_id := trip_id2]
midi[, `:=`(depart = NULL, return = NULL, duration_hours = NULL, time_stamp2 = NULL, trip_id2 = NULL)]
out <- rbindlist(list(out, midi), fill = T)

if(preserve_all)
  out <- rbindlist(list(out, gps[recid %!in% out$recid]), fill = T)
  
}

  setorder(out, vessel_id, time_stamp)
  out[, recid := NULL]
  return(out)

}
