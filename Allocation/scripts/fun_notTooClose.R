#function to select one from each too close cluster
notTooClose <- function(data = data, fixed = c()) {
  
  xx <- st_as_sf(data)
  xx <- data.frame(st_distance(xx))
  xx <- data.frame(sapply(xx, function(x) as.numeric(x)))
  
  close <- data.frame(arrayInd(which(xx < 4500), dim(xx), useNames = T))
  
  #get rid of comparison with self and "a with b" "b with a" stuff
  setDT(close)
  close <- close[close$row != close$col, ]
  close <- unique(close[, c("row", "col") := list(pmin(row, col), pmax(row, col))], 
                  by = c("row", "col"))
  
  
  if (nrow(close) > 0){
    #cluster the ones that are too close to the same id
    zz <- data[unique(c(close$row, close$col)),
               c("NrHaul", "Lat_start_deg_dec", "Lon_start_deg_dec")]
    
    kNNdistplot(zz[, c("Lat_start_deg_dec", "Lon_start_deg_dec")], minPts = 2)
    res <- dbscan(zz[, c("Lat_start_deg_dec", "Lon_start_deg_dec")], eps = 0.15, minPts = 2)
    print(res)
    
    #assign cluster
    zz$cluster <- as.factor(res[["cluster"]])
    
    #check not to olarge clusters are made
    freq <- data.frame(table(res[["cluster"]]))
    
    if (TRUE %in% (freq$Freq > 10)){

      zz1 <- zz[! zz$cluster %in% freq[freq$Freq > 10, ]$Var1, ]
      zz2 <- zz[  zz$cluster %in% freq[freq$Freq > 10, ]$Var1, ]

      kNNdistplot(zz2[, c("Lat_start_deg_dec", "Lon_start_deg_dec")], minPts = 2)
      res <- dbscan(zz2[, c("Lat_start_deg_dec", "Lon_start_deg_dec")], eps = 0.05, minPts = 2)
      print(res)

      #assign cluster
      zz2$cluster <- as.factor(paste0(res[["cluster"]], "_v2"))
      zz <- rbind(zz1, zz2)
    }
    
    zz <- merge(data, zz[, c("NrHaul", "cluster")], by = "NrHaul")
    zz <- data.frame(zz)
    
    #randomly select for each cluster or from fixed list (used to dis-select from total data set)
    fun <- function(ii) {
      cluster <- zz[zz$cluster == unique(zz$cluster)[ii], ]
      
      if (TRUE %in% (cluster$NrHaul %in% fixed)){
        cluster <- cluster[! cluster$NrHaul %in% fixed, ]
      } else{
        cluster <- cluster[sample(nrow(cluster), nrow(cluster)-1), ]
      }
      
    }
    unselect <- rbind.fill(lapply(1:length(unique(zz$cluster)), fun))
    data <- data[! data$NrHaul %in% unselect$NrHaul, ]
    
  }
  
  return(data)
}
