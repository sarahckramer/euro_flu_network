
replaceLeadLag <- function(vals.temp) {
  
  if (any(!is.na(vals.temp))) {
    start.index <- 1
    while (vals.temp[start.index] == 0 | is.na(vals.temp[start.index])) {
      start.index <- start.index + 1
    }
    start.index <- start.index - 1
    
    end.index <- length(vals.temp)
    while (vals.temp[end.index] == 0 | is.na(vals.temp[end.index])) {
      end.index <- end.index - 1
    }
    end.index <- end.index + 1
    
    if (start.index > 0) {
      vals.temp[1:start.index] <- 0
    }
    if (end.index < (length(vals.temp) + 1)) {
      vals.temp[end.index:(length(vals.temp))] <- 0
    }
    
  }
  
  return(vals.temp)
}

