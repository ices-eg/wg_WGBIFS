wd <- "Q:/20-forskning/20-dfad/users/jostou/home/yearly/WGBIFS/Allocation/"

td_clean <- readRDS(paste0(wd, "data/td_clean.rds"))
td_clean <- td_clean[td_clean$Status == "V" & td_clean$EEZ != "RUS", ]

td_clean$codstock <- ifelse(as.numeric(td_clean$Area) < 25, "21-24", "25-32")
td_clean$Latitude1_deg_dec <- td_clean$lat1
td_clean$Longitude1_deg_dec <- td_clean$lon1
td_clean$Latitude_end_deg_dec <- td_clean$lat_end
td_clean$Longitude_end_deg_dec <- td_clean$lon_end

td <- read.csv2(paste0(wd, "data/TD_v9.csv"), 
                 colClasses = "character")
havfis <- read.csv2(paste0(wd, "data/havfisken_stations.csv"), 
                    colClasses = "character")
td <- rbind.fill(td, havfis)

## merge
td_new <- merge(td_clean[, c("NrHaul", "codstock",
                                   "Latitude1_deg_dec", "Longitude1_deg_dec",
                                   "Latitude_end_deg_dec", "Longitude_end_deg_dec")], 
                    td, by = "NrHaul")

td_new<- td_new[ ,c("NrHaul",	"RectangleAlpha",	"ICES.SD",	
                            "Latitude1_deg", "Latitude1_dec_min",	
                            "Longitude1_deg",	"Longitude1_dec_min",
                            "Latitude_end_deg", "Latitude_end_dec_min",	
                            "Longitude_end_deg",	"Longitude_end_dec_min",
                            "Layer",	"EEZ",	
                            "Latitude1_deg_dec",	"Longitude1_deg_dec",
                            "Latitude_end_deg_dec", "Longitude_end_deg_dec",
                            "codstock")]

#save results
OUT <- createWorkbook()
addWorksheet(OUT, "td_all_stations")
writeData(OUT, sheet = "td_all_stations", x = td_new)

saveWorkbook(OUT, paste0(wd, "data/td_all_stations.xlsx"), 
             overwrite = T)

names(td_new)[names(td_new) == "ICES.SD"] <- "ICESSD"
write.csv(td_new, paste0(wd, "data/td_all_stations.csv"), 
          row.names = F, quote = F)


#Q:\00-alle\50-togt-info\Instrukser og skabeloner - Instructions and templates\Torske data stationer
