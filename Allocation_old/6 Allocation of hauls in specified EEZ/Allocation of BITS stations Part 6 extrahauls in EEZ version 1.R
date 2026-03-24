
####################################################################################################################################
#Part 6 of Haul allocation program for BITS (stratified random sampling)

#The program select extra hauls for given EEZ, Sub-division and depth interval. 

#Haul allocation program for BITS Part 1 and Haul allocation program for BITS Part 2 have to be run previous to this.

# 1.  Chose the Sub-division, depth interval and EZZ to allocate more hauls.
# 2.  Chose the number of extra hauls to be selected.
# 3.  Run the rest of the program.
#############################
# Depth interval	Layer     #
#   0-10/20       	1       #
#   10/20-40	      2       #
#   40-60	          3       #
#   60-80	          4       #
#   80-100	        5       #
#   < 100	          6       #
#############################
# Country              Code #
# Poland                POL #
# Denmark (incl. grey)  DEN #
# Germany               GFR #
# Sweden                SWE #
# Latvia                LAT #
# Lithuania             LTU #
# Estonia               EST #
# Finland               FIN #
# Rusia                 RUS #
#############################

#output: CSV-file listing the extra haul numbers in TD to and maps the positions 
####################################################################################################################################
library(plyr)
library(mapplots)
library(shapefiles)
library(rgdal)
#############################################################################
#############################################################################
Sub_div<-25
DepthInterval<-4
NHaulstoDraw<-1
NationalZone<-"DEN"
Survey<-"1q 2022"
#############################################################################
#############################################################################
TD<-read.table("H:/Active nonsystem/ICES WG/WGBIFS/Surveys/Allocating of BITS stations/Trawl database/Database/TD_updated 6.csv",header=TRUE,sep=";")
TD <-TD[TD$Status=="V",]
TotalHaulsToBeFished <- read.table("H:/Active nonsystem/ICES WG/WGBIFS/Surveys/Allocating of BITS stations/Allocation/3 Allocation of hauls to country/Output files/old outputs/2019/Final list of total allocated hauls processed 2019 1q for circulation.csv", header=TRUE, sep=";")
#TotalHaulsToBeFished <- read.table("C:/Arbejdsting der skal tilbage p? H-drev/BITS 2017 spring/2017/2017 spring/Planned hauls for Q1 2017.csv", header=TRUE,sep=";")
TotalHaulsToBeFished <-TotalHaulsToBeFished [,c("NrHaul", "ICES.SD")]
TotalHaulsToBeFished$id<-1
haulRemaining<- merge(TotalHaulsToBeFished,TD, all=TRUE)
haulRemaining$id[is.na(haulRemaining$id)] <- 2
haulRemaining<-haulRemaining[haulRemaining$id==2,]
haulRemaining$EEZ1<-haulRemaining$EEZ
haulRemaining$EEZ1[haulRemaining$EEZ=="grey"]<-"DEN"
haulRemaining<-haulRemaining[haulRemaining$ICES.SD==Sub_div & haulRemaining$Layer==DepthInterval & haulRemaining$EEZ1==NationalZone,]
haulRemaining$ID <- 1:nrow(haulRemaining)
haulRemaining1<-haulRemaining[,c("NrHaul", "ID")] 
haulRemaining1 <- data.frame(haulRemaining)
ID<-sample(1:nrow(haulRemaining1), NHaulstoDraw, replace=FALSE) 
addtionalHauls <- data.frame(ID)
extrahauls<- merge(addtionalHauls, haulRemaining, by="ID", all.selected=FALSE)
extrahauls$Latitude1_deg_dec<-(extrahauls$Latitude1_dec_min/60*100/100)+extrahauls$Latitude1_deg
extrahauls$Longitude1_deg_dec<-(extrahauls$Longitude1_dec_min/60*100/100)+extrahauls$Longitude1_deg
extrahauls[extrahauls$ICES.SD<25,"codstock"]<-"21-24"
extrahauls[extrahauls$ICES.SD>24,"codstock"]<-"25-32"

extrahauls$country<-paste( extrahauls$EEZ, "(",extrahauls$codstock, ")")
extrahauls <-extrahauls [,c("NrHaul", "RectangleAlpha", "ICES.SD", "Latitude1_deg", "Latitude1_dec_min", "Longitude1_deg", "Longitude1_dec_min", "Layer", "EEZ", "Latitude1_deg_dec",	"Longitude1_deg_dec",	"codstock",	"country")]
name<-paste("Adjustmenthaul",NationalZone, Survey ,Sys.Date(),".csv",sep = "")

#"M:/Sektion for kystøkologi/FPLEJE/Generelle programmer og supportfiler til KFish/output",paste(name, sep = ""))
#write.table(extrahauls, "H:/Active nonsystem/ICES WG/WGBIFS/Surveys/Allocating of BITS stations/Allocation/6 Allocation of extra hauls/Output files,paste(name, sep = ""))

write.table(extrahauls, "H:/Active nonsystem/ICES WG/WGBIFS/Surveys/Allocating of BITS stations/Allocation/6 Allocation of hauls in specified EEZ/Output files/Extra hauls DEN 1q 2022_26_1 layer 6.csv", sep=";")


coast <- read.shapefile('H:/Active nonsystem/ICES WG/WGBIFS/Surveys/Allocating of BITS stations/Shapefiles/europe')
#coast <- read.shapefile('Y:/Dynamisk/GEOdata/BasicLayers/CoastLines/Europe/europe')
latlon <- CRS("+proj=longlat +datum=WGS84")

  coordinates(extrahauls) <- c("Longitude1_deg_dec", "Latitude1_deg_dec")
  proj4string(extrahauls) <- latlon
  
  #Set the limit of the plot to +/- 10% of the extent of the points
  xfactor <- (bbox(extrahauls)[1,2]-bbox(extrahauls)[1,1])/20
  yfactor <- (bbox(extrahauls)[2,2]-bbox(extrahauls)[2,1])/20
  xlim <- c(bbox(extrahauls)[1,1]-xfactor, bbox(extrahauls)[1,2]+xfactor)
  ylim <- c(bbox(extrahauls)[2,1]-yfactor, bbox(extrahauls)[2,2]+yfactor)
  ########
  #Alternative fixed mapping area
  #xlim <- c(14.5, 16.5)
  #ylim <- c(54.5, 55.5)
  
  #col <- terrain.colors(12)
  #####################################################################################################
  ##
  ##  Draw the maps       
  #                    
  ##
  #####################################################################################################
  
  basemap(xlim=xlim, ylim=ylim, main = "Haul position map","Extra hauls") #, bg="white") 
  draw.shape(coast, col="cornsilk", border="transparent", xlim=xlim, ylim=ylim)
  draw.rect()
  points(extrahauls$Longitude1_deg_dec,extrahauls$Latitude1_deg_dec, pch=20, cex=1.0, col="black")
  extrahauls$NrHaulalpha<- as.character(extrahauls$NrHaul)
  text(extrahauls$Longitude1_deg_dec,extrahauls$Latitude1_deg_dec,extrahauls$NrHaulalpha,cex=0.5,adj=0,pos=2,col="black")
  

