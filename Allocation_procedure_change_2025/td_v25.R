library(pacman)
p_load(data.table, sf, mapview, plyr, dplyr, openxlsx, ggplot2)

wd <- "Q:/20-forskning/20-dfad/users/jostou/home/yearly/WGBIFS/TD_procedure_cleanup_2025/"

td_clean <- readRDS(paste0(wd, "td_clean.rds"))
td_clean[td_clean$EEZ == "RUS", "Status"] <- "I"
td_clean[td_clean$EEZ == "RUS", "Remark"] <- "In Russian EEZ"


td_clean$Lat_start_deg_dec <- td_clean$lat1
td_clean$Lon_start_deg_dec <- td_clean$lon1
td_clean$Lat_end_deg_dec <- td_clean$lat_end
td_clean$Lon_end_deg_dec <- td_clean$lon_end

## merge
td <- td_clean[, c("NrHaul",	"RectangleAlpha",	"Area",
                   "Layer",	 "Dist", "Mean_Depth",
                   "EEZ", "NM12", "Status", "Remark",
                   "Lat_start_deg_dec", "Lon_start_deg_dec",
                   "Lat_end_deg_dec", "Lon_end_deg_dec")]

track <- td_clean[, c(1, 9:28)]

#save results
OUT <- createWorkbook()
addWorksheet(OUT, "td_all_stations")
addWorksheet(OUT, "track_deg_dec")

writeData(OUT, sheet = "td_all_stations", x = td)
writeData(OUT, sheet = "track_deg_dec", x = track)


addFilter(OUT, sheet = "td_all_stations", row = 1, cols = 1:ncol(td))
freezePane(OUT, sheet = "td_all_stations" , 
           firstRow = TRUE, firstCol = FALSE) # freeze first row

addFilter(OUT, sheet = "track_deg_dec", row = 1, cols = 1:ncol(track))
freezePane(OUT, sheet = "track_deg_dec" , 
           firstRow = TRUE, firstCol = FALSE) # freeze first row

saveWorkbook(OUT, "Q:/20-forskning/20-dfad/users/jostou/home/yearly/WGBIFS/Allocation/data/Trawl_database_v25.xlsx", 
             overwrite = T)
