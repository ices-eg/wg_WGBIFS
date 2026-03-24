library(data.table)
library(ggplot2)
library(sf)
library(openxlsx)
library(plyr)
library(mapview)

wd <- "Allocation/"

source(paste0(wd, "scripts/fun_degDec2DegMin.r"))

#Make plan for
yr <- 2026
qtr <- 1

sample(100)[1] #should only be done/ changed once pr plan..
set.seed(67)
#-------------------------
ices <- st_read(paste0(wd, "data/area/ICES_Areas_20160601_cut_dense_3857.shp"))|>
  st_transform(4326)

#
allocated <- readRDS(paste0(wd, "planned/", yr, "_Q", qtr, "/Allocated_stations.rds")) 
rects <- read.csv2(paste0(wd, "data/nation_rect_distr.csv"))

PlannedStations<-read.table(paste0(wd, "data/PlannedStations.csv"),header=TRUE,sep=";")
PlannedStations<-PlannedStations[PlannedStations$Quarter==qtr & 
                                   PlannedStations$Country != "Russia",]

PlannedStations$Country[PlannedStations$Country == "Germany"] <- "DE"
PlannedStations$Country[PlannedStations$Country == "Sweden"] <- "SE"
PlannedStations$Country[PlannedStations$Country == "Poland"] <- "PL"
PlannedStations$Country[PlannedStations$Country == "Lithuania"] <- "LT"
PlannedStations$Country[PlannedStations$Country == "Latvia"] <- "LV"
PlannedStations$Country[PlannedStations$Country == "Estonia"] <- "EE"
PlannedStations[PlannedStations$Country == "Denmark" &
                  PlannedStations$Vessel == "Havfisken", "Country"] <- "Havf"
PlannedStations[PlannedStations$Country == "Denmark" &
                  PlannedStations$Vessel == "Dana", "Country"] <- "Dana"

setDT(PlannedStations)
PlannedStations <- PlannedStations[ ,. (plan_country = sum(PlannedNumberOfHauls)),
                                    by = .(Country)] 

#miss 
miss <- unique(allocated$RectangleAlpha[! allocated$RectangleAlpha %in% rects$RectangleAlpha])
#manually assign missing rects to apropriate countries
# rects <- rbind(rects, data.frame(Country = c("Havf", "PL", "PL", "PL"), 
#                                  RectangleAlpha = c("38F9", "40G9", "41H0", "39H0"), 
#                                  Pct = 100))

#special for these rects in this quarter
#rects[rects$RectangleAlpha == "40G5" & rects$Country == "Dana", "Pct"] <- 0
#rects[rects$RectangleAlpha == "40G5" & rects$Country == "SE", "Pct"] <- 100

#first get 100 % squares
td_100 <- merge(allocated, rects[rects$Pct == 100, ], 
                by = "RectangleAlpha")


#update plan  
nr <- data.frame(table(td_100$Country))
names(nr) <- c("Country", "nr")
PlannedStations <- merge(PlannedStations, nr, by = "Country")
PlannedStations$remain <- PlannedStations$plan_country - PlannedStations$nr

#remaining squares to give
not_assigned <- allocated[! allocated$NrHaul %in% td_100$NrHaul, ]
not_assigned <- data.frame(table(not_assigned$RectangleAlpha))
names(not_assigned) <- c("RectangleAlpha", "nr_not_assigned")

#
remaining <- merge(rects, not_assigned, by = "RectangleAlpha")
remaining$pct_assign <- remaining$nr_not_assigned * remaining$Pct/100
remaining$pct_assign[remaining$pct_assign < 1] <- 1
remaining$pct_assign <- round(remaining$pct_assign)

#cannot be greater than stations avalable
setDT(remaining)
remaining[ ,'asign_adjust' := nr_not_assigned - sum(pct_assign), by = .(RectangleAlpha)] 

#add the remaing planned
remaining <- merge(remaining, PlannedStations[, c("Country", "remain")],
                   by = "Country")

#if all the country is already assigend
remaining[remaining$remain == 0, "pct_assign"] <- 0

remaining[ , 'weight' := remain-sum(pct_assign), by = .(Country)]
remaining[ , 'weight_square' := weight/length(RectangleAlpha), by = .(Country)]


# get remaning number pr square
fun <- function(i) {
  print(i)
  rect <- unique(remaining$RectangleAlpha)[i]
  td <- allocated[allocated$RectangleAlpha == rect, ]
  
  all <- remaining[remaining$RectangleAlpha == rect, ]
  adj <- all$asign_adjust[1]
  
  #first square adjustments
  if (adj < 0)
    all <- all[order(all$weight_square), ]
  
  if (adj > 0)
    all <- all[order(-all$weight_square), ]
  
  all[1, "pct_assign"] <- all[1, "pct_assign"] + adj
  all <- all[all$pct_assign > 0, ]
  
  if (TRUE %in% (all$weight_square > 0) &  TRUE %in% (all$weight_square < 0)) {
    all[all$weight_square > 0, "pct_assign"] <- all[all$weight_square > 0, "pct_assign"] +1
    
    all <- all[order(all$weight_square), ]
    all[1, "pct_assign"] <- all[1, "pct_assign"] -1
  }
  
  all <- all[all$pct_assign > 0, ]
  
  #select at random to get some overlap between countries in shared squares
  asign <- rep(all$Country,  all$pct_assign)
  asign <- sample(asign)
  
  td$Country <- asign[1:nrow(td)]
  td
}
td_remain <- rbind.fill(lapply(1:length(unique(remaining$RectangleAlpha)), fun))

assigned <- rbind.fill(td_100, td_remain)


#Number of planed stations pr country adjust from here?
setDT(assigned)
n_assign <- assigned[ ,. (Nrhauls = length(NrHaul)),
                      by = .(Country)]
PlannedStations <- merge(PlannedStations, n_assign, by = "Country", all.x = T)
PlannedStations$remain2 <- PlannedStations$plan_country - PlannedStations$Nrhauls

##########################
PlannedStations
mapview(st_as_sf(assigned), zcol = "Country")

## manual corrections

#change the amounts pr country
assigned[assigned$NrHaul %in% c(26153), "Country"] <- "LT"
assigned[assigned$NrHaul %in% c(25359, 25277, 25450), "Country"] <- "DK"
assigned[assigned$NrHaul %in% c(24059, 25011, 25049, 25051, 25052), "Country"] <- "DE"
assigned[assigned$NrHaul %in% c(26064, 26067, 26073), "Country"] <- "PL"

#check
PlannedStations$Nrhauls <- NULL
PlannedStations$remain2 <- NULL
n_assign <- assigned[ ,. (Nrhauls = length(NrHaul)),
                                       by = .(Country)]
PlannedStations <- merge(PlannedStations, n_assign, by = "Country", all.x = T)
PlannedStations$remain2 <- PlannedStations$plan_country - PlannedStations$Nrhauls
PlannedStations
mapview(st_as_sf(assigned), zcol = "Country")

#switch so they are placed sensible
assigned[assigned$NrHaul %in% c(25167, 25038, 25461, 25081, 25229, 25080), "Country"] <- "PL"
assigned[assigned$NrHaul %in% c(25333, 25407, 25214, 25212, 25062, 25224), "Country"] <- "DK"

assigned[assigned$NrHaul %in% c(24287, 24245, 24251), "Country"] <- "DE"
assigned[assigned$NrHaul %in% c(25510, 25145, 25303), "Country"] <- "SE"
assigned[assigned$NrHaul %in% c(24085, 24300, 24092), "Country"] <- "DK"


## plot 
ggplot() +
  geom_sf(data = ices, color = "black", fill = "white")+
  geom_point(data= assigned, aes(x=Lon_start_deg_dec, y=Lat_start_deg_dec, col=Country),
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

ggsave(paste0(wd, "planned/", yr, "_Q", qtr, "/station_Overview.png"),
       width = 320, height = 300, units = 'mm', dpi = 300)

assigned <- st_drop_geometry(assigned)
assigned$geometry <- NULL

write.csv2(assigned,
           paste0(wd, "planned/", yr, "_Q", qtr, "/Allocated_stations.csv"),
           row.names = F, quote = F)




### into correct format
assigned$codstock <- ifelse(as.numeric(assigned$Area) < 25, "21-24", "25-32")

assigned <- deg2min(assigned, 
                    cols = c("Lon_start_deg_dec", "Lat_start_deg_dec"),
                    nams = c("Lon_start", "Lat_start"))


out<- assigned[ ,c("NrHaul",	"RectangleAlpha",	"Area",	
                  "Lat_start_deg", "Lat_start_min",	
                  "Lon_start_deg", "Lon_start_min",
                  "Layer",	"EEZ",	
                  "Lat_start_deg_dec",	"Lon_start_deg_dec",
                  "codstock",	"Country")]


PlannedStations$nr <- NULL
PlannedStations$remain <- NULL
PlannedStations$remain2 <- NULL
PlannedStations[is.na(PlannedStations)] <- 0

OUT <- createWorkbook()

addWorksheet(OUT, "list of final hauls")
addWorksheet(OUT, "planned vs assigned")

writeData(OUT, sheet = "list of final hauls", x = out)
writeData(OUT, sheet = "planned vs assigned", x = PlannedStations)

saveWorkbook(OUT, paste0(wd, "planned/", yr, "_Q", qtr, "/final_haul_assignment.xlsx"), 
             overwrite = T)


