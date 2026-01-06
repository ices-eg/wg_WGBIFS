
trackToSf <- function(data, id = "NrHaul") {
  
  setDT(data)
  tds1 <- melt(data, id.vars = id, 
               measure.vars = c(paste0("lat", 1:10)),
               value.name = "lat",
               variable.name = "NrPt")
  
  tds2 <- melt(data, id.vars = id, 
               measure.vars = c(paste0("lon", 1:10)),
               value.name = "lon",
               variable.name = "NrPt")
  
  tds1$NrPt <- as.numeric(gsub("lat", "", tds1$NrPt))
  tds2$NrPt <- as.numeric(gsub("lon", "", tds2$NrPt))
  
  tds <- merge(tds1, tds2, by = c("NrHaul", "NrPt"))
  tds <- tds[order(tds$NrHaul, tds$NrPt), ]
  
  tds <- tds[!is.na(tds$lon), ] %>%
    sf::st_as_sf(coords = c("lon", "lat")) %>% 
    sf::st_set_crs(4326)
  
  
  l <- tds %>% group_by(NrHaul) %>%
    dplyr::summarize(do_union=FALSE, 
                     .groups = 'drop') %>% 
    st_cast("LINESTRING")
  
  return(l)
}
