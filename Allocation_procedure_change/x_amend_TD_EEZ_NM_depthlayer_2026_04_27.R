
library(openxlsx)
library(pacman)
p_load(sqldf, RODBC, plyr, dplyr, data.table, openxlsx, sf, mapview, ggOceanMaps, ggspatial)

sf_use_s2(FALSE)
wd <- "Allocation/"

td <- read.xlsx(paste0(wd, "data/Trawl_database_v25.xlsx"),
                sheet = "td_all_stations")


td <- td %>%
  st_as_sf(coords = c("Lon_start_deg_dec", "Lat_start_deg_dec"), remove = F) |> 
  st_set_crs(4326)

## EEZ
eez <- st_read('Q:/20-forskning/12-gis/Dynamisk/GEOdata2020/BasicLayers/Boundaries/MaritimeBoundaries_vliz_latest_versions/World_EEZ_v12_20231025/eez_v12.shp')|>
  st_transform(4326)|>
  st_zm()

overlap <- st_intersects(td, eez)
td$EEZ <- eez[unlist(overlap), ]$ISO_SOV1

## nm 12
sf_use_s2(TRUE)
td <- dist2land(td, lon = "Lon_start_deg_dec", lat = "Lat_start_deg_dec")
td$NM12 <- ifelse(td$ldist <= 22.22400, "Yes", "No")
td$ldist <- NULL


## depth stratum
td$Dist <- as.numeric(gsub(",", "\\.", td$Dist))
td$Mean_Depth <- as.numeric(gsub(",", "\\.", td$Mean_Depth))
td$NrHaul <- as.numeric(td$NrHaul)
td$Area <- as.numeric(td$Area)

td$Layer <- as.numeric(as.character(cut(round(td$Mean_Depth),
                           c(seq(0, 120, by = 20), 200)-0.1, c(8:14))))

td <- st_drop_geometry(td)

#save results
OUT <- createWorkbook()
addWorksheet(OUT, "td_all_stations")
writeData(OUT, sheet = "td_all_stations", x = td)


addFilter(OUT, sheet = "td_all_stations", row = 1, cols = 1:ncol(td))
freezePane(OUT, sheet = "td_all_stations" , 
           firstRow = TRUE, firstCol = FALSE) # freeze first row

saveWorkbook(OUT, "Q:/20-forskning/20-dfad/users/jostou/home/yearly/wg_WGBIFS/amend_td2.xlsx", 
             overwrite = T)



