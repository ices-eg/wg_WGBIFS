

deg2min <- function(data, cols = c("Lon_start_deg_dec", "Lat_start_deg_dec"),
                          nams = c("Lon_start", "Lat_start")) {
  
  data <- data.frame(data)
  for(i in 1:length(cols)){
    
    pos <- as.character(data[, cols[i]])
    pos <- do.call(rbind, strsplit(pos, split='\\.'))
    
    deg <- pos[, 1]
    min <- round(as.numeric(paste0(0, ".", pos[, 2])) * 60, 5)
    
    data[, paste0(nams[i], "_deg")] <- deg
    data[, paste0(nams[i], "_min")] <- min
  }
  
  return(data)
}

