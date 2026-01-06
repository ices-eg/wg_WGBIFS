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

setwd("H:/Active nonsystem/ICES WG/WGBIFS/Surveys/Allocating of BITS stations")

coast <- read.shapefile('Y:/Dynamisk/GEOdata/BasicLayers/CoastLines/Europe/europe')

latlon <- CRS("+proj=longlat +datum=WGS84")
FinalAllocatedList <- read.table("H:/Active nonsystem/ICES WG/WGBIFS/Surveys/Allocating of BITS stations/Allocation/3 Allocation of hauls to country/Output files/Final list of total allocated hauls processed 2019 1q for circulation.csv", header=TRUE, sep=";")
 
test2<-FinalAllocatedList[,c("country")] 

test2<-unique(test2)

for (countr in test2)
{
  CountryTotalHaulsToBeFished<-FinalAllocatedList[FinalAllocatedList$country==countr,]
  
  test <- CountryTotalHaulsToBeFished 
  test1<-test[,c("country")] 
  test1<-unique(test1)
  
  coordinates(test) <- c("Longitude1_deg_dec", "Latitude1_deg_dec")
  proj4string(test) <- latlon
 
  #Set the limit of the plot to +/- 10% of the extent of the points
  xfactor <- (bbox(test)[1,2]-bbox(test)[1,1])/20
  yfactor <- (bbox(test)[2,2]-bbox(test)[2,1])/20
  xlim <- c(bbox(test)[1,1]-xfactor, bbox(test)[1,2]+xfactor)
  ylim <- c(bbox(test)[2,1]-yfactor, bbox(test)[2,2]+yfactor)

  ########
  #col <- terrain.colors(12)
  #####################################################################################################
  ##
  ##  Draw the maps       
  #                    
  ##
  #####################################################################################################

  
 
  basemap(xlim=xlim, ylim=ylim, main = "Haul position map",test1) #, bg="white") 

  draw.shape(coast, col="cornsilk", border="transparent", xlim=xlim, ylim=ylim)

  draw.rect()

  points(CountryTotalHaulsToBeFished$Longitude1_deg_dec,CountryTotalHaulsToBeFished$Latitude1_deg_dec, pch=20, cex=1.0, col="black")
  CountryTotalHaulsToBeFished$NrHaulalpha<- as.character(CountryTotalHaulsToBeFished$NrHaul)
  text(CountryTotalHaulsToBeFished$Longitude1_deg_dec,CountryTotalHaulsToBeFished$Latitude1_deg_dec,CountryTotalHaulsToBeFished$NrHaulalpha,cex=0.5,adj=0,pos=2,col="black")
  
  }

#dev.off()
#Udskriv til fil:
#png("H:/Active nonsystem/Noeglefiskerprojekt/Programmer til noeglefiskerrapport/output/PosmapRuse2011.png", width=40, height=40, res=300,units = "cm", pointsize = 20)

#H:\Active nonsystem\ICES WG\WGBIFS\Surveys\Allocating of BITS stations\Allocation\4 Mapping of allocated hauls by country\Output\Maps

