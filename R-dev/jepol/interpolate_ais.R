#' Interpolate AIS
#'
#' Sometimes there is gaps in the gps dataset, this is a way
#' to fill out these gaps. Inspired very much from 
#' vmstools::interpolateTacsat
#'
#' @param x A gps dataset in a standard format
#' @param min_gap a numeric value. Gaps more than this should be interpolated (minutes)   
#' @param max_gap a numeric value. Gaps less than this should be interpolated (minutes)  
#' @param headingAdjustment numeric. If using heading instead of course
#' @param source character. The type of the gps dataset
#' 
#' @return a gps dataset that has added the interpolated points
#' @export
#'
#' @examples
interpolate_ais <- function(x, headingAdjustment = 0, min_gap = 2, max_gap = 120,
                            source = "AIS"){
  #add required packages
  require(data.table)
  '%!in%' <- function(x,y)!('%in%'(x,y))
  
  # Progress bar function
  progress <- function (x, max = 100) {
    percent <- x / max * 100
    cat(sprintf('\r[%-50s] %d%%',
                paste(rep('=', percent / 2), collapse = ''),
                floor(percent)))
    if (x == max)
      cat('\n')
  }
  
  #Function from vmstools
  `lonLatRatio` <-
    function(x1,lat){
      #Based on the Haversine formula
      #At the position, the y-position remains the same, hence, cos(lat)*cos(lat) instead of cos(lat) * cos(y2)
      a <- cos(lat*pi/180)*cos(lat*pi/180)*sin((0.1*pi/180)/2)*sin((0.1*pi/180)/2);
      c <- 2*atan2(sqrt(a),sqrt(1-a));
      R <- 6371;
      dx1 <- R*c

      return(c(dx1/11.12))}
  

  #Make the dataset into a data.table
  setDT(x)
  
  if("SI_HARB" %!in% names(x))
    stop("No harbour column in dataset, please add it (add_harbour)")
   
  #Set datasource
  if("source" %!in% names(x))
    x[, source := source]
  
DT <- data.table()
i <- "EX_1"
for(i in unique(x$vessel_id)){
  progress(match(i, unique(x$vessel_id)),length(unique(x$vessel_id)))
  dss <- x[vessel_id == i]
  
  #Add interval between points (minutes)
  dss[, INTV:=-as.numeric(difftime(data.table::shift(time_stamp, fill = NA, source = "lag"), time_stamp, units = "mins"))]
  
  out <- list()
  
for(startPOS in 1:(nrow(dss)-1)){
  # for(startPOS in 1:(1000)){
  endPOS <- startPOS + 1
  
  #Dont interpolate if more than 2,5 hours or less than 2 minutes between points
  #Or if both points are in harbor
  if(dss$INTV[endPOS] <= max_gap & dss$INTV[endPOS] > min_gap & 
     !(dss$SI_HARB[startPOS] == 1 & dss$SI_HARB[endPOS] == 1 )){

    #Calculate speed if not present in the dataset (from coordinates (knots))
    if(is.na(dss$speed[startPOS])){
      dist <- as.numeric(st_distance(pts[endPOS,], pts[startPOS,]))
      sp <- dist /1000 / dss$INTV[endPOS] *60 / 1.852
      dss$speed[startPOS] <- sp
    }
    
    
    ## Interpolate with headings if available
    if(!is.na(dss$course[endPOS]) & !is.na(dss$course[startPOS])){
      
      params = list(fm = dss$INTV[endPOS]/150, distscale = 20, sigline = 0.2, st = c(15, 20))
      F00 <- numeric()
      F10 <- numeric()
      F01 <- numeric()
      F11 <- numeric()
      # i <- 0
      t <- seq(0, 1, length.out = dss$INTV[endPOS])
      F00 <- 2 * t^3 - 3 * t^2 + 1
      F10 <- t^3 - 2 * t^2 + t
      F01 <- -2 * t^3 + 3 * t^2
      F11 <- t^3 - t^2
      Hx0 <- sin(dss$course[startPOS]/(180/pi))
      Hy0 <- cos(dss$course[startPOS]/(180/pi))
      Hx1 <- sin(dss$course[endPOS - headingAdjustment]/(180/pi))
      Hy1 <- cos(dss$course[endPOS - headingAdjustment]/(180/pi))
      Mx0 <- dss$lon[startPOS]
      Mx1 <- dss$lon[endPOS]
      My0 <- dss$lat[startPOS]
      My1 <- dss$lat[endPOS]
      Hx0 <- Hx0 * params$fm * dss$speed[startPOS]/((params$st[2] - 
                                                      params$st[1])/2 + params$st[1])
      Hx1 <- Hx1 * params$fm * dss$speed[endPOS]/((params$st[2] - 
                                                    params$st[1])/2 + params$st[1])
      Hy0 <- Hy0 * params$fm * lonLatRatio(dss$lon[c(startPOS, 
                                                       endPOS)], dss$lat[c(startPOS, endPOS)])[1] * 
        dss$speed[startPOS]/((params$st[2] - params$st[1])/2 + 
                              params$st[1])
      Hy1 <- Hy1 * params$fm * lonLatRatio(dss$lon[c(startPOS, 
                                                       endPOS)], dss$lat[c(startPOS, endPOS)])[2] * 
        dss$speed[endPOS]/((params$st[2] - params$st[1])/2 + 
                            params$st[1])
      fx <- numeric()
      fy <- numeric()
      fx <- F00 * Mx0 + F10 * Hx0 + F01 * Mx1 + F11 * Hx1
      fy <- F00 * My0 + F10 * Hy0 + F01 * My1 + F11 * Hy1
      sp <- seq(dss$speed[startPOS], dss$speed[endPOS], length.out = dss$INTV[endPOS])
      ti <- seq(dss$time_stamp[startPOS], dss$time_stamp[endPOS], 
                length.out = dss$INTV[endPOS])
      
      if(length(fx)>2)
        out[[startPOS]] <- cbind(fx=fx[2:(length(fx)-1)], fy=fy[2:(length(fx)-1)], sp=sp[2:(length(fx)-1)], ti=ti[2:(length(fx)-1)], ty="INT")
    
      
      ## If heading is missing in one of the points, use linear interpolation:
    }else{
      
      fx <- seq(dss$lon[startPOS], dss$lon[endPOS], 
                length.out = dss$INTV[endPOS])
      fy <- seq(dss$lat[startPOS], dss$lat[endPOS], 
                length.out = dss$INTV[endPOS])
      sp <- seq(dss$speed[startPOS], dss$speed[endPOS], 
                length.out = dss$INTV[endPOS])
      ti <- seq(dss$time_stamp[startPOS], dss$time_stamp[endPOS], 
                length.out = dss$INTV[endPOS])
      
      if(length(fx)>2)
        out[[startPOS]] <- cbind(fx=fx[2:(length(fx)-1)], fy=fy[2:(length(fx)-1)], sp=sp[2:(length(fx)-1)], ti=ti[2:(length(fx)-1)], ty="INT")
      
      }
    
  }
}

if(!identical(out, list())){
  
  dss$id2 <- 1:nrow(dss)
  dss <- rbindlist(list(dss, 
                        rbindlist(lapply(out, as.data.table), idcol='id2')), 
                   use.names=TRUE, fill=TRUE)[order(id2)]
  
  names(dss)[(ncol(dss)-4):ncol(dss)] <- c("lon1", "lat1", "sp", "ti", "ty")
  
  dss[is.na(lon), `:=`(lat = lat1, lon = lon1, speed = sp, time_stamp = ti, source = ty,
                       SI_HARB = 0)]
  
  dss[, INTV:=-as.numeric(difftime(data.table::shift(dss$time_stamp, fill = NA, source = "lag"), dss$time_stamp, units = "secs"))]
  dss[, c("id2","lon1", "lat1","sp", "ti","ty"):=NULL] 
  dss[, vessel_id := i]
  setorder(dss, time_stamp)
}


dss[, id := 1:nrow(dss)]
dss[, year := year(time_stamp)]

DT <- rbindlist(list(DT, dss), fill = T)
}
return(DT)
}

