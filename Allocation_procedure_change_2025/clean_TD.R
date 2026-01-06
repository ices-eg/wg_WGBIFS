library(pacman)
p_load(data.table, sf, mapview, plyr, dplyr, openxlsx2, ggplot2)

sf_use_s2(FALSE)


wd <- "Q:/20-forskning/20-dfad/users/jostou/home/yearly/WGBIFS/Allocation/"

ices <- st_read(paste0(wd, "data/area/ICES_Areas_20160601_cut_dense_3857.shp"))%>%
  st_transform(4326)

dmm2dec <- function(angle) {
  angle <- as.character(angle)
  x <- do.call(rbind, strsplit(angle, split='\\.'))
  x <- apply(x, 1L, function(y) {
    y <- gsub(",", "\\.", y)
    y <- gsub(" ", "", y)
    y <- as.numeric(y)
    y[1] + y[2]/60
  })
  return(x)
}


td <- read.csv2(paste0(wd, "data/TD_v9.csv"), 
                colClasses = "character")
havfis <- read.csv2(paste0(wd, "data/havfisken_stations.csv"), 
                    colClasses = "character")

td <- rbind.fill(td, havfis)

td2 <- td[, c("NrHaul", "Status", "Dist", "Mean_Depth", "Layer", 
              "RectangleAlpha", "EEZ", "NM12" )] #"NM12"

for(i in c(1:10, "_end")){
  print(i)
  lon <- paste0(td[ , paste0("Longitude", i, "_deg")], ".",
                td[ , paste0("Longitude", i, "_dec_min")])
  
  lat <- paste0(td[ , paste0("Latitude", i, "_deg")], ".",
                td[ , paste0("Latitude", i, "_dec_min")])
  
  lon <- dmm2dec(lon)
  lat <- dmm2dec(lat)
  
  td2[, paste0("lat", i)] <- lat
  td2[, paste0("lon", i)] <- lon
  
}

## spatial
setDT(td2)
tds1 <- melt(td2, id.vars = c("NrHaul"), 
            measure.vars = c(paste0("lat", 1:10)),
            value.name = "lat",
            variable.name = "NrPt")

tds2 <- melt(td2, id.vars = c("NrHaul"), 
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

mapview(l)

l$length <- as.numeric(st_length(l))
l$Area <- substr(as.character(l$NrHaul), 1, 2)

#on land
tds$land <- lengths(st_intersects(tds, ices)) == 0
onLand <- td[td$NrHaul %in% tds[tds$land == TRUE, ]$NrHaul, ]
mapview(l[l$NrHaul %in% onLand$NrHaul, ], zcol = "NrHaul")

## Duplicated start positions
dup <- td2
dup$lon_rounded <- round(dup$lon1, 2)
dup$lat_rounded <- round(dup$lat1, 2)
dup$rounded <- paste0(dup$lon_rounded, "_", dup$lat_rounded)

dup <- dup[dup$rounded %in% dup$rounded[duplicated(dup$rounded)],  ]
dup2 <- td[td$NrHaul %in% dup$NrHaul, ]
mapview(l[l$NrHaul %in% dup$NrHaul, ], zcol = "NrHaul")

# chose one to continue with for now
dup_chosen <- dup[! duplicated(dup$rounded),  ]$NrHaul

## wrong lines
ggplot()+
  geom_histogram(data = l[l$NrHaul != 26170, ], aes(x = length, fill=Area))+
  facet_grid("Area")

#obviusly wrong above 2500

ggplot()+
  geom_histogram(data = l[l$length < 25000, ], aes(x = length, fill=Area))+
  facet_grid("Area")

wrong <- td[td$NrHaul %in% l[l$length > 25000, ]$NrHaul, ]
mapview(l[l$NrHaul %in% wrong$NrHaul, ], zcol = "NrHaul")

## maybe wrong
maybe <- td[td$NrHaul %in% l[l$length > 10000 & l$length < 25000, ]$NrHaul, ]
mapview(l[l$NrHaul %in% maybe$NrHaul, ], zcol = "NrHaul")

####
# final dataset
out <- merge(td2, l, by = "NrHaul")
out <- out[! out$NrHaul %in% c(onLand$NrHaul, wrong$NrHaul, dup_chosen), ]
mapview(st_as_sf(out), zcol = "Area")


#write out results
saveRDS(out, paste0(wd, "data/td_clean.rds"))

out <- st_drop_geometry(out)
out$geometry <- NULL

write.csv2(out, paste0(wd, "data/td_clean.csv"),
           row.names = F, quote = F)





