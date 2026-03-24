
#library(dismo)
#library(raster)
#library(marmap)
library(rgdal)
library("RODBC")
#library(maptools)
#library(rasterVis)
#library(grid)
library(mapplots)
library(shapefiles)

V<-NULL
setwd("H:/Active nonsystem/ICES WG/WGBIFS/Surveys/Allocating of BITS stations")

coast <- read.shapefile('Y:/Dynamisk/GEOdata/BasicLayers/CoastLines/Europe/europe')

latlon <- CRS("+proj=longlat +datum=WGS84")
utm32N <- CRS("+proj=utm +zone=32 +datum=WGS84")


coordinates(TotalHaulsToBeFished) <- c("Latitude1_deg_dec", "Longitude1_deg_dec")
proj4string(TotalHaulsToBeFished) <- latlon
#pos.utm <- spTransform(pos, utm32N)


#Set the limit of the plot to +/- 10% of the extent of the points
xfactor <- (bbox(TotalHaulsToBeFished)[1,2]-bbox(TotalHaulsToBeFished)[1,1])/20
yfactor <- (bbox(TotalHaulsToBeFished)[2,2]-bbox(TotalHaulsToBeFished)[2,1])/20
xlim <- c(bbox(TotalHaulsToBeFished)[1,1]-xfactor, bbox(TotalHaulsToBeFished)[1,2]+xfactor)
ylim <- c(bbox(TotalHaulsToBeFished)[2,1]-yfactor, bbox(TotalHaulsToBeFished)[2,2]+yfactor)
TotalHaulsToBeFished$xlima[TotalHaulsToBeFished$country=="DEN (SD 21-24)"] <- 10
TotalHaulsToBeFished$xlimb[TotalHaulsToBeFished$country=="DEN (SD 21-24)"] <- 16
TotalHaulsToBeFished$ylima[TotalHaulsToBeFished$country=="DEN (SD 21-24)"] <- 54.5
TotalHaulsToBeFished$ylimb[TotalHaulsToBeFished$country=="DEN (SD 21-24)"] <- 57.5

TotalHaulsToBeFished$xlima[TotalHaulsToBeFished$country=="DEN (SD 25-32)"] <- 14
TotalHaulsToBeFished$xlimb[TotalHaulsToBeFished$country=="DEN (SD 25-32)"] <- 21
TotalHaulsToBeFished$ylima[TotalHaulsToBeFished$country=="DEN (SD 25-32)"] <- 54.5
TotalHaulsToBeFished$ylimb[TotalHaulsToBeFished$country=="DEN (SD 25-32)"] <- 57.5

TotalHaulsToBeFished$xlima[TotalHaulsToBeFished$country=="GFR (SD 21-24)"] <- 10
TotalHaulsToBeFished$xlimb[TotalHaulsToBeFished$country=="GFR (SD 21-24)"] <- 16
TotalHaulsToBeFished$ylima[TotalHaulsToBeFished$country=="GFR (SD 21-24)"] <- 54.5
TotalHaulsToBeFished$ylimb[TotalHaulsToBeFished$country=="GFR (SD 21-24)"] <- 56

TotalHaulsToBeFished$xlima[TotalHaulsToBeFished$country=="SWE (SD 25-32)"] <- 14
TotalHaulsToBeFished$xlimb[TotalHaulsToBeFished$country=="SWE (SD 25-32)"] <- 21
TotalHaulsToBeFished$ylima[TotalHaulsToBeFished$country=="SWE (SD 25-32)"] <- 54.5
TotalHaulsToBeFished$ylimb[TotalHaulsToBeFished$country=="SWE (SD 25-32)"] <- 57.5

TotalHaulsToBeFished$xlima[TotalHaulsToBeFished$country=="POL (SD 21-24)"] <- 10
TotalHaulsToBeFished$xlimb[TotalHaulsToBeFished$country=="POL (SD 21-24)"] <- 16
TotalHaulsToBeFished$ylima[TotalHaulsToBeFished$country=="POL (SD 21-24)"] <- 54.5
TotalHaulsToBeFished$ylimb[TotalHaulsToBeFished$country=="POL (SD 21-24)"] <- 56

TotalHaulsToBeFished$xlima[TotalHaulsToBeFished$country=="POL (SD 25-32)"] <- 14
TotalHaulsToBeFished$xlimb[TotalHaulsToBeFished$country=="POL (SD 25-32)"] <- 20
TotalHaulsToBeFished$ylima[TotalHaulsToBeFished$country=="POL (SD 25-32)"] <- 54.5
TotalHaulsToBeFished$ylimb[TotalHaulsToBeFished$country=="POL (SD 25-32)"] <- 57.5

TotalHaulsToBeFished$xlima[TotalHaulsToBeFished$country=="LAT (SD 25-32)"] <- 18
TotalHaulsToBeFished$xlimb[TotalHaulsToBeFished$country=="LAT (SD 25-32)"] <- 22
TotalHaulsToBeFished$ylima[TotalHaulsToBeFished$country=="LAT (SD 25-32)"] <- 56
TotalHaulsToBeFished$ylimb[TotalHaulsToBeFished$country=="LAT (SD 25-32)"] <- 58.5

TotalHaulsToBeFished$xlima[TotalHaulsToBeFished$country=="RUS (SD 25-32)"] <- 18
TotalHaulsToBeFished$xlimb[TotalHaulsToBeFished$country=="RUS (SD 25-32)"] <- 22
TotalHaulsToBeFished$ylima[TotalHaulsToBeFished$country=="RUS (SD 25-32)"] <- 54.5
TotalHaulsToBeFished$ylimb[TotalHaulsToBeFished$country=="RUS (SD 25-32)"] <- 56.5

TotalHaulsToBeFished$xlima[TotalHaulsToBeFished$country=="LTU (SD 25-32)"] <- 18
TotalHaulsToBeFished$xlimb[TotalHaulsToBeFished$country=="LTU (SD 25-32)"] <- 22
TotalHaulsToBeFished$ylima[TotalHaulsToBeFished$country=="LTU (SD 25-32)"] <- 54.5
TotalHaulsToBeFished$ylimb[TotalHaulsToBeFished$country=="LTU (SD 25-32)"] <- 56.5


for (v in TotalHaulsToBeFished$country)
{

xlim <- c(TotalHaulsToBeFished$xlima, TotalHaulsToBeFished$xlimb)
ylim <- c(TotalHaulsToBeFished$ylima,TotalHaulsToBeFished$ylimb)



########
#col <- terrain.colors(12)


#####################################################################################################
##
##  Draw the maps
##
#####################################################################################################

basemap(xlim=xlim, ylim=ylim, main = "Haul possition map") #, bg="white")

draw.shape(coast, col="cornsilk", border="transparent", xlim=xlim, ylim=ylim)

draw.rect()

points(TotalHaulsToBeFished$Longitude1_deg_dec,TotalHaulsToBeFished$Latitude1_deg_dec, pch=20, cex=.5, col="black")

}

#dev.off()






#Udskriv til fil:
#png("H:/Active nonsystem/Noeglefiskerprojekt/Programmer til noeglefiskerrapport/output/PosmapRuse2011.png", width=40, height=40, res=300,units = "cm", pointsize = 20)


