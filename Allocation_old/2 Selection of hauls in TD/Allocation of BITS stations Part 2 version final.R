####################################################################################################################################
#Part 2 of Haul allocation program for BITS (stratified random sampling)

#The program selects the hauls to be allocated to each depth layer in each Sub-division from the Trawl Database (TD)

# Part 1 of Haul allocation program for BITS has to be run previous to this

# 1.  Update the TD call to most reasent TD version
# 2.  RUN FIRST STEP TO LINE 194
# 3.  ACCEPT OR ADJUST THE STRATA FOR THE ADDITIONAL SELECTION BY USE OF "STRATA ADJUSTMENTS" (step 2) BASED ON COMMENTS IN LOG
#     AND BY INSPECTION OF "AdditionalSelect" and "MSamplePopulation"
# 4.  RUN SECOND STEP FROM LINE 196 AND OUT.
# 5.  RUN the PROGRAM "Allocation of BITS stations Part 3"

#output: CSV-file listing the haul numbers in TD to be distributed among participating countries
####################################################################################################################################

#Automatisk naming procedure skal inkoopereres (quarter, year)
library(plyr)
NHaulPlanned <- read.table("H:/Active nonsystem/ICES WG/WGBIFS/Surveys/Allocating of BITS stations/Allocation/1 Distribution of stations on strata/Output files/NHaulPlanned.csv", header=TRUE,sep=";")
NHaulPlanned <- NHaulPlanned [NHaulPlanned$NHauls>0,]
#TD data  from all Sub-Division is read
TD<-read.table("H:/Active nonsystem/ICES WG/WGBIFS/Surveys/Allocating of BITS stations/Trawl database/Database/TD_updated 4.csv",header=TRUE,sep=";")
TD <-TD[TD$Status=="V",]
TD <-TD [,c("NrHaul", "RectangleAlpha", "ICES.SD", "Latitude1_deg", "Latitude1_dec_min", "Longitude1_deg", "Longitude1_dec_min", "Layer", "EEZ")] 
TD$Latitude1_dec_min<-as.numeric(TD$Latitude1_dec_min)
#Each haul is allocated to a Rec9 (=sub split of each ICES Statistical Rectangle into 9 sub-rectangles):
TD$latGr<- floor(floor(TD$Latitude1_dec_min)/10)+1
TD$latGr1<- ifelse(TD$latGr>3, TD$latGr -3, TD$latGr)
TD$lonGr<- (floor(floor(TD$Longitude1_dec_min)/20)+1)
TD$Rec9<- ifelse(TD$latGr1>1, TD$lonGr +3, TD$lonGr)
TD$Rec9<- ifelse(TD$latGr1>2, TD$Rec9 +3, TD$Rec9)
TD$Rec9<-paste(TD$RectangleAlpha, TD$Rec9)
TD <-TD [,c("NrHaul", "RectangleAlpha", "ICES.SD", "Latitude1_deg", "Latitude1_dec_min", "Longitude1_deg", "Longitude1_dec_min", "Layer", "EEZ", "Rec9")]
#...and the list for random selection of SubRec9 is made:
Rec9List<-unique(TD [,c("RectangleAlpha","Rec9", "ICES.SD", "Layer")])

Allocated1<- NULL
Allocated2<- NULL
Allocated3<- NULL
census<-NULL
census1<-NULL
Type2add<-NULL
Type3add<-NULL
Selectedhauls1<-NULL
ManualSelec1<-NULL
ManualSelec2<-NULL
AdditionalSelect1<-NULL
AdditionalSelect2<-NULL
AddHaulsToBeSelected<-NULL

subNHaulPlannedSD<-unique(NHaulPlanned ["SD"])


for (i in subNHaulPlannedSD$SD)  #SubDiv  
  {
  subNHaulPlanned<-NHaulPlanned[NHaulPlanned$SD==i,]
  Allocated2<- NULL
  for (j in subNHaulPlanned$Layer) #DepthInterval
    
    {
    
    SubRec9TotalList <- Rec9List[Rec9List$ICES.SD==i & Rec9List$Layer==j,]
    
    if (nrow(SubRec9TotalList)==0)
    {
      SubNHaulspl<-NHaulPlanned[NHaulPlanned$SD==i & NHaulPlanned$Layer==j,]
      
      p<-SubNHaulspl$NHauls

      tekst1<-paste("...
Information:No hauls availble in TD for SD ",i," and depth layer",j,"even though",p,"hauls were planned in this stratum
                    .......................................................................")
      cat(tekst1)

      tekst2<-paste("...
GUIDENCE:",p,"hauls have to be selected by use of strata adjustments for SD ",i," and depth layer",j,"(",p,",0 hauls)
                    .......................................................................")
      cat(tekst2)

    ManualSelec1$SD<-i
    ManualSelec1$Layer<-j
    ManualSelec1$N<-p
    remain1 <- data.frame(ManualSelec1)
    AdditionalSelect1<-rbind(AdditionalSelect1, remain1) 

    }else{
      SubRec9TotalList <- SubRec9TotalList[,c("RectangleAlpha","Rec9", "ICES.SD", "Layer")]
      SubRec9TotalList$ID <- 1:nrow(SubRec9TotalList)
      SubAntHauls<-NHaulPlanned[NHaulPlanned$SD==i & NHaulPlanned$Layer==j,]     #Number of hauls to be drawn in SD, Layer
      SubRec9TotalList$Count<-1
      temp1<-(tapply(SubRec9TotalList$Count,SubRec9TotalList$Count, FUN=sum))
      availRec9 = data.frame(TDcheck = names(temp1), SumRec9=temp1)
  
    if (SubAntHauls$NHauls> availRec9$SumRec9) {
      m<-SubAntHauls$NHauls
      n<-availRec9$SumRec9
      
      tekst3<-paste("...
Information: (-> Type II or Type III) The number of different Sub-rectangles selected for sampling in Sub-Div",i,"depth layer",j,"
is larger than the number of Sub-rectangles available in the same stratum in TD. (",m,"hauls,",n,"sub-rec.)
.......................................................................")
      cat(tekst3)
      
      censusHauls<-TD[TD$ICES.SD==i & TD$Layer==j,]
      censusHauls$Count<-1
      censusHauls$ID <- 1:nrow(censusHauls)
      temp2<-(tapply(censusHauls$Count,censusHauls$Count, FUN=sum))
      availHauls = data.frame(TDcheck1 = names(temp2), SumHauls=temp2)
    
        if (SubAntHauls$NHauls> availHauls$SumHauls) {
          #Type III. inkluder alle tilg?ngelige tr?k (i,j) i selected hauls (resten tr?kkes manuelt fra andre strata)
          a<-availHauls$SumHauls
          Nmissing2<-m-a
          tekst4<-paste("...
GUIDENCE: (Type III)" ,Nmissing2,"hauls have to be selected by use of strata adjustments from another stratum because of insuficient number 
of hauls in the TD in Sub-Div",i,"depth layer",j,". (",m,",",a,"hauls.)
.......................................................................")
          cat(tekst4)
          censusHauls$Type<-3
          Type3ad<-censusHauls[,c("NrHaul","Type")] 
          Type3add<-rbind(Type3add, Type3ad) 
          Type3ad<-NULL
          ManualSelec2$SD<-i
          ManualSelec2$Layer<-j
          ManualSelec2$N<-Nmissing2
          remain2 <- data.frame(ManualSelec2)
          AdditionalSelect2<-rbind(AdditionalSelect2, remain2)  
 
        } else{
          #Type II. tr?k det planlagte antal tr?k fra tilg?ngelige tr?k i stratum (i,j,k)
          b<-availHauls$SumHauls
          Nmissing3<-m
          ID<-sample(1:nrow(censusHauls), SubAntHauls$NHauls, replace=FALSE)
          
          SubSelectedIDRec9 <- data.frame(ID)                                               #Selected hauls in SD, Layer
          Type2ad<- merge(censusHauls, SubSelectedIDRec9, by="ID", all.selected=FALSE)
          Type2ad$Type<-2
          Type2ad<-Type2ad[,c("NrHaul", "Type")] 
          Type2add<-rbind(Type2add, Type2ad)
          Type2ad<-NULL
          
          tekst5<-paste("...
Information: (Type II.)" ,Nmissing3,"hauls has been selected for sampling in Sub-Div",i,"depth layer",j," 
where the number of hauls planned is bigger than the number of Rec9 available 
but the number of hauls available in TD is sufficient.(",m,",",b,"hauls.)
.......................................................................") 
          cat(tekst5)
          } 
      
      } else  {
        #Type I. tr?k det planlagte antal Rec9 fra tilg?ngelige Rec9 i stratum (i,j) 
        ID<-sample(1:nrow(SubRec9TotalList), SubAntHauls$NHauls, replace=FALSE)
        SubSelectedIDRec9 <- data.frame(ID)                                               #Selected hauls in SD, Layer
        SubSelecRec9ad<- merge(SubRec9TotalList, SubSelectedIDRec9, by="ID", all.selected=FALSE)
        SubSelecRec9ad$Count<-1
        temp<-tapply(SubSelecRec9ad$Count, SubSelecRec9ad$Rec9, FUN=sum)
        SubNumberRec9 = data.frame(Rec9 = names(temp), SumHauls=temp)
      
      for (k in SubNumberRec9$Rec9)  #Sub-Rectangle
        {  
        #Type I. tr?k det planlagte antal tr?k fra tilg?ngelige tr?k i stratum (i,j,k)
        
        Type1HaulAvail<-TD[TD$ICES.SD==i & TD$Layer==j & TD$Rec9==k,]
        Type1HaulAvail$ID <- 1:nrow(Type1HaulAvail)
        Type1HaulAvail<-Type1HaulAvail[,c("NrHaul", "ID")] 
        Type1HaulAvail <- data.frame(Type1HaulAvail)
        Type1SubNumberHauls<- SubNumberRec9[SubNumberRec9$Rec9==k,]
        ID<-sample(1:nrow(Type1HaulAvail), Type1SubNumberHauls$SumHauls, replace=FALSE) 
        Type1add <- data.frame(ID)
        Type1add<- merge(Type1add, Type1HaulAvail, by="ID", all.selected=FALSE)
        Type1add$Type<-1
        Allocated1<-rbind(Allocated1, Type1add)
        }
    Allocated2<-rbind(Allocated2, Allocated1)
    Allocated1<-NULL 
      }  
    }}
  Allocated3<-rbind(Allocated3, Allocated2)

  }

Allocated3<-Allocated3[,c("NrHaul","Type")]
Selectedhauls1<-rbind(Allocated3,Type3add,Type2add)
Selectedhauls1$sekvens<-order(Selectedhauls1$NrHaul)
Selectedhauls1<-Selectedhauls1[Selectedhauls1$sekvens,]
Selectedhauls<- merge(Selectedhauls1, TD, by="NrHaul", all.selected=FALSE)
NHaulSelected<-nrow(Selectedhauls)
NHaulPlannedTotal<-sum(NHaulPlanned$NHauls)
Missing<-NHaulPlannedTotal-NHaulSelected
tekst6<-paste("...
Information: In all areas a total of",Missing,"hauls have to be selected by use of strata adjustments.
.......................................................................")
cat(tekst6)

write.table(Selectedhauls, "H:/Active nonsystem/ICES WG/WGBIFS/Surveys/Allocating of BITS stations/Allocation/2 Selection of hauls in TD/Output data/list of allocated hauls.csv", sep=";")

# SECOND STEP:  Selection the remaining planned hauls by use of strata adjustments

NrHaul<-(TD [,c("NrHaul")])
TDIndex<-data.frame(NrHaul)
NrHaul<-(Selectedhauls [,c("NrHaul")]) 
IndexSelected <- data.frame(NrHaul)
IndexSelected$Marker<-1
MSamplePop<-merge(TDIndex,IndexSelected, by= "NrHaul", all=TRUE)
MSamplePop$Marker[is.na(MSamplePop$Marker)] <- 0
MSamplePop<-MSamplePop[MSamplePop$Marker==0,] 
MSamplePopulation<-merge(TD,MSamplePop, by= "NrHaul", all.selected=FALSE) 

#options(error = utils::recover)

AdditionalSelect<-rbind(AdditionalSelect1, AdditionalSelect2) 
ModAdditionalSelect<-AdditionalSelect
ModAdditionalSelect$cond<-paste(ModAdditionalSelect$SD, ModAdditionalSelect$Layer, sep="")

#   Implementation of strata adjustment of not succesful haul allocations
#   Based on inspection of "AdditionalSelect" and "MSamplePopulation" the substituting Layer is defined below
#   ModAdditionalSelect$cond defined as concatenating of "SD" and "Layer"
#   FORMAT: ModAdditionalSelect$Layer[ModAdditionalSelect$cond=="SD missing hauls""layer missing hauls"] <- "Substitute layer"

#ModAdditionalSelect$Layer[ModAdditionalSelect$cond==224] <- 2
ModAdditionalSelect$Layer[ModAdditionalSelect$cond==244] <- 3
ModAdditionalSelect$Layer[ModAdditionalSelect$cond==252] <- 3
ModAdditionalSelect$Layer[ModAdditionalSelect$cond==266] <- 5
ModAdditionalSelect$Layer[ModAdditionalSelect$cond==282] <- 3
#Add as many lines as needed

ModAdditionalSelect$cond<-paste(ModAdditionalSelect$SD, ModAdditionalSelect$Layer, sep="")

for (l in ModAdditionalSelect$cond)  #SubDivLayer  
{ 
  AddHaulsToBeSelectedadd<-NULL 
  i<-substr(l,1,2)
  j<-substr(l,3,3)

  SubMSamplePopulation<-MSamplePopulation[MSamplePopulation$ICES.SD==i & MSamplePopulation$Layer==j,]
  Nrhaul1<- SubMSamplePopulation[,c("NrHaul")]
  SubMSamplePopulation <- data.frame(Nrhaul1)
  SubMSamplePopulation$ID <- 1:nrow(SubMSamplePopulation)
  SubAdditionalSelect<-ModAdditionalSelect[ModAdditionalSelect$SD==i & ModAdditionalSelect$Layer==j,]
  ID<-sample(1:nrow(SubMSamplePopulation), SubAdditionalSelect$N, replace=FALSE)
  AddHaulsToBeSelectedadd <- data.frame(ID)
  AddHaulsToBeSelectedadd<-merge(AddHaulsToBeSelectedadd, SubMSamplePopulation, by= "ID", all.selected=FALSE)
  AddHaulsToBeSelectedadd$NrHaul<-AddHaulsToBeSelectedadd$Nrhaul1
  AddHaulsToBeSelectedadd<-merge(AddHaulsToBeSelectedadd, MSamplePopulation, by= "NrHaul", all.selected=FALSE)
  AddHaulsToBeSelected<-rbind(AddHaulsToBeSelected, AddHaulsToBeSelectedadd)
}
check<-AddHaulsToBeSelected
check$count<-1
z<-(tapply(check$count,check$Marker, FUN=sum))

tekst7<-paste("...
Check: In all areas a total of",z,"hauls have been selected by use of strata adjustments out of",Missing,"requested.
              .......................................................................")


HaulsToBeFished1<-Selectedhauls[,c("NrHaul", "RectangleAlpha", "ICES.SD", "Latitude1_deg", "Latitude1_dec_min", "Longitude1_deg", "Longitude1_dec_min", "Layer", "EEZ")]
HaulsToBeFished2<-AddHaulsToBeSelected[,c("NrHaul", "RectangleAlpha", "ICES.SD", "Latitude1_deg", "Latitude1_dec_min", "Longitude1_deg", "Longitude1_dec_min", "Layer", "EEZ")]
TotalHaulsToBeFished<-rbind(HaulsToBeFished1, HaulsToBeFished2)


write.table(TotalHaulsToBeFished, "H:/Active nonsystem/ICES WG/WGBIFS/Surveys/Allocating of BITS stations/Allocation/2 Selection of hauls in TD/Output data/list of totalallocated hauls.csv", sep=";")
cat(tekst7)

TD$Latitude1_deg_dec<-(TD$Latitude1_dec_min/60*100/100)+TD$Latitude1_deg
TD$Longitude1_deg_dec<-(TD$Longitude1_dec_min/60*100/100)+TD$Longitude1_deg

write.table(TD, "H:/Active nonsystem/ICES WG/WGBIFS/Surveys/Allocating of BITS stations/Trawl database/Database/TD for plotting.csv", sep=";")

