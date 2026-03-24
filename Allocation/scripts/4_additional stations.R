library(data.table)
library(plyr)
library(dplyr)
library(sf)
library(mapview)
library(RANN)
library(dbscan)
library(ggplot2)
library(openxlsx)

sf_use_s2(FALSE)
wd <- "Allocation/"

ices <- st_read(paste0(wd, "data/area/ICES_Areas_20160601_cut_dense_3857.shp"))%>%
  st_transform(4326)

#get costum functions
source(paste0(wd, "scripts/fun_notTooClose.r"))
source(paste0(wd, "scripts/fun_trackToSf.r"))
source(paste0(wd, "scripts/fun_degDec2DegMin.r"))

#Make plan for
yr <- 2026
qtr <- 1

td <- read.xlsx(paste0(wd, "data/Trawl_database_v25.xlsx"),
                sheet = "td_all_stations")
td <- td[td$Status == "V", ]

track <- read.xlsx(paste0(wd, "data/Trawl_database_v25.xlsx"),
                   sheet = "track_deg_dec")
track <- trackToSf(track)

td <- merge(td, track)
mapview(st_as_sf(td), zcol = "Area")

allocated <- readRDS(paste0(wd, "planned/", yr, "_Q", qtr, "/Allocated_stations.rds"))

#Bonus hauls
status <- read.csv2(paste0(wd, "planned/", yr, "_Q", qtr, "/select_status.csv"))
additional <- status[status$miss == 0, c("SD", "Layer")]

### edit manully from here

additional[additional$SD == 24, "NHauls"] <- 10
additional[additional$SD == 25, "NHauls"] <- 10

####

additional <- additional[! is.na(additional$NHauls), ]

fun <- function(k) {
  print(k)
  area <- additional[k, ]$SD
  layer <- additional[k, ]$Layer
  plan <- additional[k, ]
  
  td4 <- td[td$Area == area & td$Layer %in% layer, ]
  
  if (nrow(td4) != 0)
    td4 <- notTooClose(data = td4, fixed = allocated$NrHaul)
    
  #exclude already chosen ones
  td4 <- td4[! td4$NrHaul %in% allocated$NrHaul, ]
    
  if (nrow(td4) >= plan$NHauls) {
    td4 <- td4[sample(nrow(td4), plan$NHauls), ]
      
  } else{
    print(paste0("Only ", nrow(td4), " Eklstra for ", area, " ", layer))
    td4
  }
 
}
additional <- rbind.fill(lapply(1:nrow(additional), fun))

additional$type = "additional"
res <- rbind(allocated, additional)

mapview(st_as_sf(res), zcol = "type")

## manually select couse stuff
# additional <- additional[additional$NrHaul %in%
#                            c("24183", "24228", "24300", "25171", "24057", "24047",
#                              "25316", "25333", "25229", "25076"), ]
# 
# res <- rbind(allocated, additional)
# mapview(st_as_sf(res), zcol = "type")

## allocate additional to country, (aoptional)
additional$Country <- "DK_extra"

additional <- st_drop_geometry(additional)
additional$geometry <- NULL

#plot with country selection
assigned <- read.csv2(paste0(wd, "planned/", yr, "_Q", qtr, "/Allocated_stations.csv"))

all <- rbind.fill(assigned, additional)

ggplot() +
  geom_sf(data = ices, color = "black", fill = "white")+
  geom_point(data= all, aes(x=Lon_start_deg_dec, y=Lat_start_deg_dec, col=Country),
             size = 3)+
  geom_sf() +
  ggtitle(paste0("planned stations ", yr, " Q", qtr))+
  xlab("Longitude")+
  ylab("Latitude")+
  coord_sf(xlim = c(8, 22), ylim = c(54, 59), expand = FALSE)+
  theme(panel.background = element_rect(fill = "#B2BEB5"),
        panel.border = element_rect(colour = "black", fill=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave(paste0(wd, "planned/", yr, "_Q", qtr, "/station_Overview_w_extra.png"),
       width = 320, height = 300, units = 'mm', dpi = 300)


### into correct format
additional$codstock <- ifelse(as.numeric(additional$Area) < 25, "21-24", "25-32")

additional <- deg2min(additional, 
                    cols = c("Lon_start_deg_dec", "Lat_start_deg_dec",
                             "Lon_end_deg_dec", "Lat_end_deg_dec"),
                    nams = c("Lon_start", "Lat_start",
                             "Lon_end", "Lat_end"))

additional<- additional[ ,c("NrHaul",	"RectangleAlpha",	"Area",	
                            "Lat_start_deg", "Lat_start_min",	
                            "Lon_start_deg", "Lon_start_min",
                            "Lat_end_deg", "Lat_end_min",	
                            "Lon_end_deg", "Lon_end_min",
                            "Layer",	"EEZ",	
                            "Lat_start_deg_dec",	"Lon_start_deg_dec",
                            "Lat_end_deg_dec",	"Lon_end_deg_dec",
                            "codstock",	"Country")]

#save results
OUT <- createWorkbook()
addWorksheet(OUT, "additional hauls DK")
writeData(OUT, sheet = "additional hauls DK", x = additional)

saveWorkbook(OUT, paste0(wd, "planned/", yr, "_Q", qtr, "/additional_hauls_DK.xlsx"), 
             overwrite = T)

write.csv(additional, paste0(wd, "planned/", yr, "_Q", qtr, "/additional_hauls_DK.csv"), 
             row.names = F, quote = F)



res <-  deg2min(res, 
                cols = c("Lon_start_deg_dec", "Lat_start_deg_dec",
                         "Lon_end_deg_dec", "Lat_end_deg_dec"),
                nams = c("Lon_start", "Lat_start",
                         "Lon_end", "Lat_end"))
