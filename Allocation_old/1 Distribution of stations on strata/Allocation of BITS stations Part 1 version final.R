####################################################################################################################################
#Part 1 of Haul allocation program for BITS (stratified random sampling)

#Calculates the number of hauls to be allocated to each depth layer in each Sub-division

# 1.  Update the assigned number of hauls by country by quarter (file:PlannedStations) from the WGBIFS report. If Russia does not participate 15 stations is assigned anyway.  
# 2.  Update the running mean of CPUE of 1+ cod (5 most reason years) by      1) Subdivision
#                                                                             2) Subdivision, Depth layer
# 3.  Select the Quarter in line 33
# 4.  Assure that the rounding adjustments (line 128-131) are all "0"
# 4.  Compare total number of planned stations (file:PlannedStations) with the file: NHaulPlanned and adjust the numbers in lines 128-131 if needed
# 4.  Rerun line 128 and out
# 4.  RUN the PROGRAM "Allocation of BITS stations Part 2" and "Allocation of BITS stations Part 3" 

#output: CSV-file stating the number of hauls to be allocated to each depth layer in each Sub-division
####################################################################################################################################
library(plyr)
#READ INPUT FILES
#Area per SD (Fixed, WGBFAS-report):
AreaSDDist<-read.table("Q:/20-forskning/20-dfad/users/jostou/home/yearly/WGBIFS/Allocation_orig/1 Distribution of stations on strata/Input files/Permanent/AreaSDDist.csv",header=TRUE,sep=";")
#Area per SD (Fixed, WGBFAS-report):
AreaDepthLayerDist<-read.table("Q:/20-forskning/20-dfad/users/jostou/home/yearly/WGBIFS/Allocation_orig/1 Distribution of stations on strata/Input files/Permanent/AreaDepthLayerDist.csv",header=TRUE,sep=";")
#Runnin mean of 3 previous yers CPUE(cod) per SD (DATRAS):
CpueSDDist<-read.table("Q:/20-forskning/20-dfad/users/jostou/home/yearly/WGBIFS/Allocation_orig/1 Distribution of stations on strata/Input files/CpueSDDist.csv",header=TRUE,sep=";")
#Runnin mean of 3 previous yers CPUE(cod) per depth strata (DATRAS):
CpueDepthDist<-read.table("Q:/20-forskning/20-dfad/users/jostou/home/yearly/WGBIFS/Allocation_orig/1 Distribution of stations on strata/Input files/CpueDepthDist.csv",header=TRUE,sep=";")
#Number of planed stations (WGBFAS-report):
PlannedStations<-read.table("Q:/20-forskning/20-dfad/users/jostou/home/yearly/WGBIFS/Allocation_orig/1 Distribution of stations on strata/Input files/PlannedStations.csv",header=TRUE,sep=";")

####################################################################
#   QUARTER IS SELECTED:
    Quarter<-4
####################################################################

#          SPECIAL RULES AGREED BY THE WGBIFS
#   Denmark takes always 5 hauls in SD 23:
    FixedNHaulsDK23<-0  #as this SD is not yet included in the allocation scheme;
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
PlannedStations<-PlannedStations[PlannedStations$Quarter==Quarter,]
CpueSDDist$CpueAdj <- CpueSDDist$Cpue
CpueSDDist[CpueSDDist$SD==23,"CpueAdj"] <- FixedCPUEDK23  
CpueSDDist[CpueSDDist$SD==27,"CpueAdj"] <- FixedCPUESWE27    
#PlannedStations[c(PlannedStations$AreaSection==2224 | PlannedStations$Country=="Denmark"),"PlannedNumberOfHauls"] <- FixedNHaulsDK21 #Virker ikke   
PlannedStations[PlannedStations$Vessel=="Havfisken","PlannedNumberOfHauls"] <-PlannedStations[PlannedStations$Vessel=="Havfisken","PlannedNumberOfHauls"]- FixedNHaulsDK21     
    

require(stats)

#The number of hauls by Sub-division (including abowe adjustments) is calculated based on the relative distributions calculated 
#based on a weighted input of area (0.6) and CPUE (0.4)
#Area per AreaSection is calculated:
temp<-(tapply(AreaSDDist$Area,AreaSDDist$AreaSection, FUN=sum))
temp = data.frame( AreaSection = names(temp), SumArea=temp)
distSDArea<- merge(AreaSDDist, temp, all.x=TRUE)
#Relative area distribution per SD is calculated:
distSDArea$RelArea<-distSDArea$Area/distSDArea$SumArea*100

#CPUE per AreaSection is calculated:
temp<-(tapply(CpueSDDist$CpueAdj,CpueSDDist$AreaSection, FUN=sum))    
temp = data.frame( AreaSection = names(temp), SumCpue=temp)
distSDCpue<- merge(CpueSDDist, temp, all.x=TRUE)

#Relative CPUE distribution per SD is calculated:
distSDCpue$RelCpue<-distSDCpue$CpueAdj/distSDCpue$SumCpue*100
SDHaulDist<-merge(distSDArea,distSDCpue)

temp <- ddply(PlannedStations, c("Quarter","AreaSection"),summarize, SumPlannedStations=sum(PlannedNumberOfHauls))

SDStationAlocation<- merge(SDHaulDist, temp, all.x=TRUE)
#Fixed number of stations in Kattegat is deducted:



SDStationAlocation$NHaulsRel<-((0.6*SDStationAlocation$RelArea)+(0.4*SDStationAlocation$RelCpue))/100

SDStationAlocation$SumPlannedStationsAdj<-SDStationAlocation$SumPlannedStations
SDStationAlocation[SDStationAlocation$AreaSection==2224,"SumPlannedStationsAdj"] <- SDStationAlocation[SDStationAlocation$AreaSection==2224,"SumPlannedStations"]- FixedNHaulsDK23  
SDStationAlocation[SDStationAlocation$AreaSection==2528,"SumPlannedStationsAdj"] <- SDStationAlocation[SDStationAlocation$AreaSection==2528,"SumPlannedStations"]- FixedNHaulsSWE27

SDStationAlocation$NHaulsPrim<-SDStationAlocation$NHaulsRel*SDStationAlocation$SumPlannedStationsAdj
SDStationAlocation$NHaulsSD<-SDStationAlocation$NHaulsPrim

#The number per SD is adjusted with the SPECIAL RULES AGREED BY THE WGBIFS:
SDStationAlocation[SDStationAlocation$SD==24,"NHaulsSD"] <- PlannedStations[(PlannedStations$Country=="Poland") & (PlannedStations$AreaSection==2224),"PlannedNumberOfHauls"]+FixedNhaulsGER24
SDStationAlocation[SDStationAlocation$SD==23,"NHaulsSD"] <- FixedNHaulsDK23
SDStationAlocation[SDStationAlocation$SD==22,"NHaulsSD"] <- SDStationAlocation[SDStationAlocation$SD==22,"SumPlannedStations"] - SDStationAlocation[SDStationAlocation$SD==23,"NHaulsSD"] - SDStationAlocation[SDStationAlocation$SD==24,"NHaulsSD"]
SDStationAlocation[SDStationAlocation$SD==27,"NHaulsSD"] <- FixedNHaulsSWE27

SDStationAlocation$NHaulsSD<-round(SDStationAlocation$NHaulsSD,0) #(This file can be used for check of haul distribution by SD)
SDStatAlocation<-SDStationAlocation [,c("SD", "AreaSection", "Quarter", "NHaulsSD")] 

#The number of hauls by Sub-division and depth stratum is calculated based on the number of hauls by SD calculated above: 
temp<-(tapply(AreaDepthLayerDist$Area,AreaDepthLayerDist$SD, FUN=sum))
temp = data.frame( SD = names(temp), SumArea=temp)
distDepth<- merge(AreaDepthLayerDist, temp, all.x=TRUE)
distDepth$RelDepth<-distDepth$Area/distDepth$SumArea

temp<-(tapply(CpueDepthDist$Cpue,CpueDepthDist$SD, FUN=sum))
temp = data.frame( SD = names(temp), SumCpue=temp)
distCpue<- merge(CpueDepthDist, temp, all.x=TRUE)
distCpue$RelCpue<-distCpue$Cpue/distCpue$SumCpue
DepthStationAlocation<-merge(distDepth,distCpue)

DepthStationAlocation$NHaulsDepthRel<-((0.6*DepthStationAlocation$RelDepth)+(0.4*DepthStationAlocation$RelCpue)) #(This file can be used for check of haul distribution by depth strata)
DepthStatAlocation<-DepthStationAlocation [,c("SD", "DepthInterval", "Layer", "NHaulsDepthRel")] 

NHaulPlanned<-merge(SDStatAlocation, DepthStatAlocation)
NHaulPlanned$NHauls<-round(NHaulPlanned$NHaulsDepthRel*NHaulPlanned$NHaulsSD,0)
NHaulPlanned<-NHaulPlanned [,c("AreaSection","SD", "Quarter","DepthInterval", "Layer", "NHauls")] 

kontrol<-(tapply(NHaulPlanned$NHauls,NHaulPlanned$AreaSection, FUN=sum))
# Check in the "Environment/values/Kontrol" that the number of hauls/AreaSection is as planned. Othervice, adjust below 
####################################################################
#Final adjustment of discrepancy due to rounding errors.
#   Initialy no adjustments should be made (all set to "0")
####################################################################
NHaulPlanned[NHaulPlanned$SD==22 & NHaulPlanned$Layer==2,"NHauls"] <- NHaulPlanned[NHaulPlanned$SD==22 & NHaulPlanned$Layer==2,"NHauls"]-0
NHaulPlanned[NHaulPlanned$SD==25 & NHaulPlanned$Layer==2,"NHauls"] <- NHaulPlanned[NHaulPlanned$SD==25 & NHaulPlanned$Layer==2,"NHauls"]+0
NHaulPlanned[NHaulPlanned$SD==25 & NHaulPlanned$Layer==3,"NHauls"] <- NHaulPlanned[NHaulPlanned$SD==25 & NHaulPlanned$Layer==3,"NHauls"]-0
NHaulPlanned[NHaulPlanned$SD==24 & NHaulPlanned$Layer==2,"NHauls"] <- NHaulPlanned[NHaulPlanned$SD==24 & NHaulPlanned$Layer==2,"NHauls"]-1 
NHaulPlanned[NHaulPlanned$SD==25 & NHaulPlanned$Layer==4,"NHauls"] <- NHaulPlanned[NHaulPlanned$SD==25 & NHaulPlanned$Layer==4,"NHauls"]+0 
NHaulPlanned[NHaulPlanned$SD==26 & NHaulPlanned$Layer==4,"NHauls"] <- NHaulPlanned[NHaulPlanned$SD==26 & NHaulPlanned$Layer==4,"NHauls"]+0
#Add as many strata as needed
####################################################################

write.table(NHaulPlanned, "H:/Active nonsystem/ICES WG/WGBIFS/Surveys/Allocating of BITS stations/Allocation/1 Distribution of stations on strata/Output files/NHaulPlanned.csv", sep=";")
#write.table(NHaulPlanned, "C:/Arbejdsting der skal tilbage på H-drev/BITS 2017 spring/Allocating of BITS stations/NHaulPlanned.csv", sep=";")
#Automatisk naming procedure skal inkoopereres (quarter, year)