library(pacman)
p_load(sqldf, RODBC, plyr, dplyr, data.table, openxlsx, sf, mapview, ggOceanMaps, ggspatial)

sf_use_s2(F)

wd <- "Q:/20-forskning/20-dfad/users/jostou/home/yearly/WGBIFS/Allocation/"
source(paste0(wd, "scripts/fun_trackToSf.r"))

eez <- st_read('Q:/20-forskning/12-gis/Dynamisk/GEOdata2020/BasicLayers/Boundaries/International/eez_v11.shp')%>%
  st_transform(4326)%>%
  st_zm()

eez <- eez[eez$ISO_SOV1 %in% c("DEU", "DNK", "EST", "LVA", "LTU", "POL", "SWE", "FIN") &
             ! eez$TERRITORY1 %in% c("Faeroe", "Greenland"), ]


td <- read.xlsx(paste0(wd, "data/Trawl_database_v25.xlsx"),
          sheet = "td_all_stations")

track <- read.xlsx(paste0(wd, "data/Trawl_database_v25.xlsx"),
                   sheet = "track_deg_dec")
track <- trackToSf(track)

td <- merge(td, track)
mapview(st_as_sf(td), zcol = "Area")


cruise <- c("BITS-1", "BITS-2")

channel <- odbcConnect("FishLineDW")
dat <- sqlQuery(channel, paste("SELECT
       [year] 
      ,[cruise]
      ,[station]
      ,[stationName]
      ,[dfuArea]
      ,[gearQuality] AS Status
      ,[latPosStartDec] AS lat1
      ,[lonPosStartDec] AS lon1
      ,[latPosEndDec] AS lat2
      ,[lonPosEndDec] AS lon2
      ,[latPosStartText] AS Latitude1_deg
      ,[lonPosStartText] AS Longitude1_deg
      ,[latPosEndText] AS Latitude2_deg
      ,[lonPosEndText] AS Longitude2_deg
      ,[latPosEndText] AS Latitude_end_deg
      ,[lonPosEndText] AS Longitude_end_deg
      ,[statisticalRectangle] AS RectangleAlpha
      ,[depthAvg] AS Mean_Depth
FROM    [FishLineDW].[dbo].[Sample]
WHERE   cruise IN ('",paste0(cruise,collapse="','"), "') AND
        (gearQuality = 'V')
", sep = ""))

## Duplicated start positions
dat$lon_rounded <- round(dat$lon1, 1)
dat$lat_rounded <- round(dat$lat1, 1)
dat$rounded <- paste0(dat$lon_rounded, "_", dat$lat_rounded)

dat$stationName[is.na(dat$stationName)] <- paste0("a_", 1:length(dat$stationName[is.na(dat$stationName)]))
dat <- dat[! duplicated(dat$rounded) & !duplicated(dat$stationName),  ]

td$lon_rounded <- round(td$Lon_start_deg_dec, 1)
td$lat_rounded <- round(td$Lat_start_deg_dec, 1)
td$rounded <- paste0(td$lon_rounded, "_", td$lat_rounded)
dat <- dat[! dat$rounded %in% td$rounded & ! dat$stationName %in% td$NrHaul, ]
dat <- dat[dat$dfuArea %in% 22:28, ]

## saniy check
#max area 24
areas <- unique(dat$dfuArea)
fun <- function(i) {
  a <- dat[dat$dfuArea == areas[i], ]
  nr <- as.numeric(max(td[td$Area == areas[i], ]$NrHaul))
  a$NrHaul <- paste0(areas[i], nr+1:nrow(a))
  a
}
dat <- rbind.fill(lapply(1:length(areas), fun))

zz <- data.table(dat)
zz1 <- melt(zz, id.vars = c("NrHaul"), 
             measure.vars = c(paste0("lat", 1:2)),
             value.name = "lat",
             variable.name = "NrPt")

zz2 <- melt(zz, id.vars = c("NrHaul"), 
             measure.vars = c(paste0("lon", 1:2)),
             value.name = "lon",
             variable.name = "NrPt")

zz1$NrPt <- as.numeric(gsub("lat", "", zz1$NrPt))
zz2$NrPt <- as.numeric(gsub("lon", "", zz2$NrPt))

zz <- merge(zz1, zz2, by = c("NrHaul", "NrPt"))
zz <- zz[order(zz$NrHaul, zz$NrPt), ]

zz <- zz[!is.na(zz$lon) & !is.na(zz$lat), ] %>%
  sf::st_as_sf(coords = c("lon", "lat")) %>% 
  sf::st_set_crs(4326)


l <- zz %>% group_by(NrHaul) %>%
  dplyr::summarize(do_union=FALSE, 
                   .groups = 'drop') %>% 
  st_cast("LINESTRING")


l$Dist <- round(as.numeric(st_length(l))*0.000539956803,3)
l <- l[l$Dist <= 10 & l$Dist > 1, ]

#### l td
td$lat1 <- td$Lat_start_deg_dec
td$lat2 <- td$Lat_end_deg_dec
td$lon1 <- td$Lon_start_deg_dec
td$lon2 <- td$Lon_end_deg_dec

zz <- data.table(td[! is.na(td$Lat_end_deg_dec) &
                      ! is.na(td$Lon_end_deg_dec), ])
zz1 <- melt(zz, id.vars = c("NrHaul"), 
            measure.vars = c(paste0("lat", 1:2)),
            value.name = "lat",
            variable.name = "NrPt")

zz2 <- melt(zz, id.vars = c("NrHaul"), 
            measure.vars = c(paste0("lon", 1:2)),
            value.name = "lon",
            variable.name = "NrPt")

zz1$NrPt <- as.numeric(gsub("lat", "", zz1$NrPt))
zz2$NrPt <- as.numeric(gsub("lon", "", zz2$NrPt))

zz <- merge(zz1, zz2, by = c("NrHaul", "NrPt"))
zz <- zz[order(zz$NrHaul, zz$NrPt), ]

zz <- zz[!is.na(zz$lon) & !is.na(zz$lat), ] %>%
  sf::st_as_sf(coords = c("lon", "lat")) %>% 
  sf::st_set_crs(4326)


tdl <- zz %>% group_by(NrHaul) %>%
  dplyr::summarize(do_union=FALSE, 
                   .groups = 'drop') %>% 
  st_cast("LINESTRING")

####
l <- st_buffer(l, 0.01)
mapview(l) 

xx <- data.frame(st_intersects(l, l))
xx <- xx[! duplicated(xx$row.id) & ! duplicated(xx$col.id), ]
l <- l[xx$row.id, ]
mapview(l)

tdl <- st_buffer(tdl, 0.01)
mapview(l) + mapview(tdl) 

xx <- data.frame(st_intersects(l, tdl))
xx <- xx[! duplicated(xx$row.id),]
l <- l[-xx$row.id, ]
mapview(l) + mapview(tdl) 

##

############## cross with invalid stations
channel <- odbcConnect("FishLineDW")
inv <- sqlQuery(channel, paste("SELECT
       [year] 
      ,[cruise]
      ,[station]
      ,[stationName]
      ,[dfuArea]
      ,[gearQuality] AS Status
      ,[latPosStartDec] AS lat1
      ,[lonPosStartDec] AS lon1
      ,[latPosEndDec] AS lat2
      ,[lonPosEndDec] AS lon2
      ,[latPosStartText] AS Latitude1_deg
      ,[lonPosStartText] AS Longitude1_deg
      ,[latPosEndText] AS Latitude2_deg
      ,[lonPosEndText] AS Longitude2_deg
      ,[latPosEndText] AS Latitude_end_deg
      ,[lonPosEndText] AS Longitude_end_deg
      ,[statisticalRectangle] AS RectangleAlpha
      ,[depthAvg] AS Mean_Depth
FROM    [FishLineDW].[dbo].[Sample]
WHERE   cruise IN ('",paste0(cruise,collapse="','"), "') AND
        (gearQuality = 'I')
", sep = ""))

inv$NrHaul <- 1:nrow(inv)
zz <- data.table(inv)
zz1 <- melt(zz, id.vars = c("NrHaul"), 
            measure.vars = c(paste0("lat", 1:2)),
            value.name = "lat",
            variable.name = "NrPt")

zz2 <- melt(zz, id.vars = c("NrHaul"), 
            measure.vars = c(paste0("lon", 1:2)),
            value.name = "lon",
            variable.name = "NrPt")

zz1$NrPt <- as.numeric(gsub("lat", "", zz1$NrPt))
zz2$NrPt <- as.numeric(gsub("lon", "", zz2$NrPt))

zz <- merge(zz1, zz2, by = c("NrHaul", "NrPt"))
zz <- zz[order(zz$NrHaul, zz$NrPt), ]

zz <- zz[!is.na(zz$lon) & !is.na(zz$lat), ] %>%
  sf::st_as_sf(coords = c("lon", "lat")) %>% 
  sf::st_set_crs(4326)


linv <- zz %>% group_by(NrHaul) %>%
  dplyr::summarize(do_union=FALSE, 
                   .groups = 'drop') %>% 
  st_cast("LINESTRING")
mapview(linv) + mapview(l)

linv <- st_buffer(linv, 0.01)
mapview(l) + mapview(linv) 

xx <- data.frame(st_intersects(l, linv))
xx <- xx[! duplicated(xx$row.id),]
l <- l[-xx$row.id, ]
mapview(l) + mapview(linv) 
mapview(l) + mapview(tdl) 


############################################################

dat <- merge(dat, st_drop_geometry(l))

#find the EEZ
dat <- dat %>% 
  sf::st_as_sf(coords = c("lon1","lat1"), remove = F) %>% 
  sf::st_set_crs(4326)

overlap <- st_intersects(dat, eez)
dat <- st_drop_geometry(dat)
dat <- dat[rep(seq(nrow(dat)), lengths(overlap)), ]

dat$EEZ <- eez[unlist(overlap), ]$ISO_SOV1

#names correction
dat$EEZ[dat$EEZ == "DEU"] <- "GRF"
dat$EEZ[dat$EEZ == "LVA"] <- "LAT"

areas <- unique(dat$dfuArea)
fun <- function(i) {
  a <- dat[dat$dfuArea == areas[i], ]
  nr <- as.numeric(max(td[td$Area == areas[i], ]$NrHaul))
  a$NrHaul <- paste0(nr+1:nrow(a))
  a
}
dat <- rbind.fill(lapply(1:length(areas), fun))

#check
unique(duplicated(dat$NrHaul))
unique(dat$NrHaul %in% td$NrHaul)

# add
dat$Lat_start_deg_dec <- dat$lat1
dat$Lon_start_deg_dec <- dat$lon1
dat$Lat_end_deg_dec <- dat$lat2
dat$Lon_end_deg_dec <- dat$lon2

dat$Remark <- ""
dat$Source <- paste0(dat$year, " DANA trawl computer")
dat$Area <- dat$dfuArea

sf_use_s2(TRUE)
dat <- dist2land(dat, lon = "lon1", lat = "lat1")
dat$NM12 <- ifelse(dat$ldist <= 12, "Yes", "No")

dat$Layer <- cut(dat$Mean_Depth, c(0, 39, seq(60,100, 20)-1, 1000),
                 c(2:6))

## merge
dat <- dat[, c("NrHaul",	"RectangleAlpha",	"Area",
               "Layer",	 "Dist", "Mean_Depth",
               "EEZ", "NM12", "Status", "Source", "Remark",
               "Lat_start_deg_dec", "Lon_start_deg_dec",
               "Lat_end_deg_dec", "Lon_end_deg_dec")]

#save results
OUT <- createWorkbook()
addWorksheet(OUT, "dana")
writeData(OUT, sheet = "dana", x = dat)

addFilter(OUT, sheet = "dana", row = 1, cols = 1:ncol(td))
freezePane(OUT, sheet = "dana" , 
           firstRow = TRUE, firstCol = FALSE) # freeze first row

saveWorkbook(OUT, "Q:/20-forskning/20-dfad/users/jostou/home/yearly/WGBIFS/Allocation_procedure_change_2025/dana_additional.xlsx", 
             overwrite = T)



