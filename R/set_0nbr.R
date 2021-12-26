
### Return a numeric ID having constant length

set_0nbr <- function(x, numericID_length = NA) {
  
  if(is.na(numericID_length)){ numericID_length <- max(nchar(x))}
  if( length(x) == 0){
    seq0 <- paste0( paste(rep(0, times = numericID_length-nchar(x) )),collapse = "")
  }else{
    seq0 <- do.call(c, 
                    lapply(x, function (arg) {
                      return(paste0( paste(rep(0, times = numericID_length-nchar(arg) )),collapse = ""))
                    }
                    ))
  }
    return(seq0)
  }
  
