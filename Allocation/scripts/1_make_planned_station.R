library(plyr)
library(icesDatras)
library(stats)

#Make plan for
yr <- 2026
qtr <- 1

#READ INPUT FILES
wd <- "Allocation/"

#Area per SD and depth layer (Fixed, WGBFAS-report):
AreaSDDist<-read.table(paste0(wd, "data/AreaSDDist.csv"),header=TRUE,sep=";")
AreaDepthLayerDist<-read.table(paste0(wd, "data/AreaDepthLayerDist.csv"),header=TRUE,sep=";")

#Runnin mean of 3 previous yers CPUE(cod) per SD and depth strata (DATRAS):
CpueSDDist<-read.csv2(paste0(wd, "planned/", yr, "_Q", qtr, "/CPUE_weights_area.csv"))
CpueDepthDist<-read.csv2(paste0(wd, "planned/", yr, "_Q", qtr, "/CPUE_weights_depth.csv"))

#Number of planed stations (WGBFAS-report):
PlannedStations<-read.table(paste0(wd, "data/PlannedStations.csv"),header=TRUE,sep=";")

#SPECIAL RULES AGREED BY THE WGBIFS
#   Denmark takes always 5 hauls in SD 23:
FixedNHaulsDK23<-5  
FixedCPUEDK23<-0

#   Sweden takes always 10 hauls in SD 27:
FixedNHaulsSWE27<-10
FixedCPUESWE27<-0

#   Germany always takes 45 hauls in SD 24. All other planned hauls in AreaSection 2124 minus hauls of Havfisken in SD 21 are allocated to SD 22 
FixedNhaulsGER24<-45

#   Number of fixed stations taken in Kattegat (sd21) by Denmark:
FixedNHaulsDK21<-0

###################################################################
#Abowe adjustments are implemented:
PlannedStations<-PlannedStations[PlannedStations$Quarter==qtr,]
CpueSDDist$CpueAdj <- CpueSDDist$Cpue
CpueSDDist[CpueSDDist$SD==23,"CpueAdj"] <- FixedCPUEDK23
CpueSDDist[CpueSDDist$SD==27,"CpueAdj"] <- FixedCPUESWE27

PlannedStations[PlannedStations$Vessel=="Havfisken","PlannedNumberOfHauls"] <-
  PlannedStations[PlannedStations$Vessel=="Havfisken","PlannedNumberOfHauls"]- FixedNHaulsDK21     
    
#The number of hauls by Sub-division (including abowe adjustments) is calculated based on the relative distributions calculated 
#based on a weighted input of area (0.6) and CPUE (0.4)

#Relative area distribution per SD is calculated:
setDT(AreaSDDist)
AreaSDDist[ , 'SumArea' := sum(Area), by=.(AreaSection)]
AreaSDDist$RelArea<-AreaSDDist$Area/AreaSDDist$SumArea*100

#CPUE per AreaSection is calculated:
setDT(CpueSDDist)
CpueSDDist[ , 'SumCpue' := sum(CpueAdj), by = .(AreaSection)]

# #Relative CPUE distribution per SD is calculated:
CpueSDDist$RelCpue<-CpueSDDist$CpueAdj/CpueSDDist$SumCpue*100
SDHaulDist<-merge(AreaSDDist,CpueSDDist)
# 

temp <- ddply(PlannedStations, c("Quarter","AreaSection"),summarize, 
              SumPlannedStations=sum(PlannedNumberOfHauls))

SDStationAlocation<- merge(SDHaulDist, temp, by = "AreaSection", all.x=TRUE)

#station weights are calculated
SDStationAlocation$NHaulsRel<-((0.6*SDStationAlocation$RelArea)+(0.4*SDStationAlocation$RelCpue))/100
SDStationAlocation$SumPlannedStationsAdj<-SDStationAlocation$SumPlannedStations

SDStationAlocation[SDStationAlocation$AreaSection==2224,"SumPlannedStationsAdj"] <- 
  SDStationAlocation[SDStationAlocation$AreaSection==2224,"SumPlannedStations"]- FixedNHaulsDK23 

SDStationAlocation[SDStationAlocation$AreaSection==2528,"SumPlannedStationsAdj"] <- 
  SDStationAlocation[SDStationAlocation$AreaSection==2528,"SumPlannedStations"]- FixedNHaulsSWE27

SDStationAlocation$NHaulsPrim<-SDStationAlocation$NHaulsRel*SDStationAlocation$SumPlannedStationsAdj
SDStationAlocation$NHaulsSD<-SDStationAlocation$NHaulsPrim

#The number per SD is adjusted with the SPECIAL RULES AGREED BY THE WGBIFS:
SDStationAlocation[SDStationAlocation$SD==24,"NHaulsSD"] <- 
  PlannedStations[(PlannedStations$Country=="Poland") & (PlannedStations$AreaSection==2224),"PlannedNumberOfHauls"]+FixedNhaulsGER24

SDStationAlocation[SDStationAlocation$SD==23,"NHaulsSD"] <- FixedNHaulsDK23

SDStationAlocation[SDStationAlocation$SD==22,"NHaulsSD"] <- 
  SDStationAlocation[SDStationAlocation$SD==22,"SumPlannedStations"] - SDStationAlocation[SDStationAlocation$SD==23,"NHaulsSD"] - SDStationAlocation[SDStationAlocation$SD==24,"NHaulsSD"]

SDStationAlocation[SDStationAlocation$SD==27,"NHaulsSD"] <- FixedNHaulsSWE27

SDStationAlocation$NHaulsSD<-round(SDStationAlocation$NHaulsSD,0) #(This file can be used for check of haul distribution by SD)
SDStatAlocation<-SDStationAlocation[,c("SD", "AreaSection", "Quarter", "NHaulsSD")] 

#The number of hauls by Sub-division and depth stratum is calculated based on the number of hauls by SD calculated above: 
setDT(AreaDepthLayerDist)
AreaDepthLayerDist[ , 'SumArea' := sum(Area), by = .(SD)]
AreaDepthLayerDist$RelDepth<-AreaDepthLayerDist$Area/AreaDepthLayerDist$SumArea

setDT(CpueDepthDist)
CpueDepthDist[ , 'SumCpue' := sum(Cpue), by = .(SD)]
CpueDepthDist$RelCpue<-CpueDepthDist$Cpue/CpueDepthDist$SumCpue

DepthStatAlocation<-merge(AreaDepthLayerDist, CpueDepthDist)
DepthStatAlocation$NHaulsDepthRel<-((0.6*DepthStatAlocation$RelDepth)+(0.4*DepthStatAlocation$RelCpue)) #(This file can be used for check of haul distribution by depth strata)
DepthStatAlocation<-DepthStatAlocation [,c("SD", "DepthInterval", "Layer", "NHaulsDepthRel")] 

#make the final total file of planned hauls
Nplanned<-merge(SDStatAlocation, DepthStatAlocation, by = "SD", all.x = T)
Nplanned$NHauls<-round(Nplanned$NHaulsDepthRel*Nplanned$NHaulsSD,0)
Nplanned<-Nplanned [,c("AreaSection","SD", "Quarter","DepthInterval", "Layer", "NHauls")] 

#chaeck total hauls pr area is still strue
kontrol <- Nplanned[ ,. (Nplanned = sum(NHauls)),
                by = .(AreaSection)]

kontrol <- merge(kontrol, temp, by = "AreaSection")
kontrol$adjust <- kontrol$Nplanned != kontrol$SumPlannedStations

####################################################################
#Final adjustment of discrepancy due to rounding errors.
#   Initialy no adjustments should be made (all set to "0")
####################################################################
#Add as many strata as needed
Nplanned[Nplanned$SD==25 & Nplanned$Layer==2,"NHauls"] <- 
  Nplanned[Nplanned$SD==25 & Nplanned$Layer==2,"NHauls"]+2


#chaeck total hauls pr area is still strue
kontrol <- Nplanned[ ,. (Nplanned = sum(NHauls)),
                     by = .(AreaSection)]

kontrol <- merge(kontrol, temp, by = "AreaSection")
kontrol$adjust <- kontrol$Nplanned != kontrol$SumPlannedStations

####################################################################

#write out results
write.table(Nplanned, paste0(wd, "planned/", yr, "_Q", qtr,
                                 "/NHaulPlanned_lengthCPUE.csv"), sep=";")
            


