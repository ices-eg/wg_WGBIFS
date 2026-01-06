#Processes the output from Part2 
####################################################################################################################################
#Part 3 of Haul allocation program for BITS (stratified random sampling)

#The program allocate a priori the hauls selected by "Haul allocation program for BITS Part 2". 

#Haul allocation program for BITS Part 1 and Haul allocation program for BITS Part 2 have to be run previous to this.

# 1.  RUN FIRST STEP TO LINE 46.
# 2.  MANUALLY SHIFT AROUND AND ADJUST THE DISTRIBUTION BETWEEN COUNTRIES SO IT FITS THE NATIONAL PLANNED NUMBER OF HAULS
#     BY INSPECTION OF "PlannedStations" and "NHaulPlanned".
# 3.  RUN SECOND STEP FROM LINE 48 AND OUT.


#output: CSV-file listing the haul numbers in TD to be distributed among participating countries and maps by country of hauls distributed 
####################################################################################################################################

#STEP 1
TotalHaulsToBeFished <- read.table("H:/Active nonsystem/ICES WG/WGBIFS/Surveys/Allocating of BITS stations/Allocation/2 Selection of hauls in TD/Output data/list of totalallocated hauls.csv", sep=";", header=TRUE)
TotalHaulsToBeFished$Latitude1_deg_dec<-(TotalHaulsToBeFished$Latitude1_dec_min/60*100/100)+TotalHaulsToBeFished$Latitude1_deg
TotalHaulsToBeFished$Longitude1_deg_dec<-(TotalHaulsToBeFished$Longitude1_dec_min/60*100/100)+TotalHaulsToBeFished$Longitude1_deg
TotalHaulsToBeFished[TotalHaulsToBeFished$ICES.SD<25,"codstock"]<-"21-24"
TotalHaulsToBeFished[TotalHaulsToBeFished$ICES.SD>24,"codstock"]<-"25-32"
TotalHaulsToBeFished$country<-"notassigned"
TotalHaulsToBeFished[TotalHaulsToBeFished$codstock=="21-24" & TotalHaulsToBeFished$EEZ=="DEN","country"]<-"DEN (SD 21-24)"
TotalHaulsToBeFished[TotalHaulsToBeFished$ICES.SD==27 & TotalHaulsToBeFished$codstock=="25-32","country"]<-"SWE (SD 25-32)"
TotalHaulsToBeFished[TotalHaulsToBeFished$ICES.SD==24,"country"]<-"GFR (SD 21-24)"
TotalHaulsToBeFished[TotalHaulsToBeFished$EEZ=="grey","country"]<-"DEN (SD 25-32)"
TotalHaulsToBeFished[TotalHaulsToBeFished$codstock=="21-24" & TotalHaulsToBeFished$EEZ=="POL","country"]<-"POL (SD 21-24)"
TotalHaulsToBeFished[TotalHaulsToBeFished$codstock=="25-32" & TotalHaulsToBeFished$EEZ=="POL","country"]<-"POL (SD 25-32)"
TotalHaulsToBeFished[TotalHaulsToBeFished$EEZ=="LTU","country"]<-"LTU (SD 25-32)"
TotalHaulsToBeFished[TotalHaulsToBeFished$EEZ=="RUS","country"]<-"RUS (SD 25-32)"
TotalHaulsToBeFished[TotalHaulsToBeFished$EEZ=="EST","country"]<-"EST (SD 25-32)"
TotalHaulsToBeFished[TotalHaulsToBeFished$EEZ=="LAT" | TotalHaulsToBeFished$EEZ=="LAT","country"]<-"LAT (SD 25-32)"

TotalHaulsToBeFished[TotalHaulsToBeFished$EEZ=="DEN" & TotalHaulsToBeFished$ICES.SD>24,"country"]<-"DEN (SD 25-32)"
TotalHaulsToBeFished[TotalHaulsToBeFished$EEZ=="GFR" & TotalHaulsToBeFished$codstock=="21-24","country"]<-"GFR (SD 21-24)"
TotalHaulsToBeFished[TotalHaulsToBeFished$EEZ=="SWE" & TotalHaulsToBeFished$codstock=="25-32","country"]<-"SWE (SD 25-32)"
TotalHaulsToBeFished[TotalHaulsToBeFished$RectangleAlpha=="37G0"|TotalHaulsToBeFished$RectangleAlpha=="37G1"|TotalHaulsToBeFished$RectangleAlpha=="37G2","country"]<-"GFR (SD 21-24)"
TotalHaulsToBeFished[TotalHaulsToBeFished$RectangleAlpha=="39G5"|TotalHaulsToBeFished$RectangleAlpha=="40G6"|TotalHaulsToBeFished$RectangleAlpha=="40G7"|
  TotalHaulsToBeFished$RectangleAlpha=="41G7","country"]<-"DEN (SD 25-32)"
TotalHaulsToBeFished[TotalHaulsToBeFished$RectangleAlpha=="41G8" & TotalHaulsToBeFished$EEZ=="SWE","country"]<-"POL (SD 25-32)"
TotalHaulsToBeFished[TotalHaulsToBeFished$RectangleAlpha=="43G8"|TotalHaulsToBeFished$RectangleAlpha=="43G9"|TotalHaulsToBeFished$RectangleAlpha=="44G9"|
  TotalHaulsToBeFished$RectangleAlpha=="45G7"|TotalHaulsToBeFished$RectangleAlpha=="42G8"|
  TotalHaulsToBeFished$RectangleAlpha=="45G9"|TotalHaulsToBeFished$RectangleAlpha=="45G8","country"]<-"SWE (SD 25-32)"

TotalHaulsToBeFished[TotalHaulsToBeFished$RectangleAlpha=="40G5" & TotalHaulsToBeFished$EEZ=="DEN","country"]<-"DEN (SD 25-32)"
write.table(TotalHaulsToBeFished, "H:/Active nonsystem/ICES WG/WGBIFS/Surveys/Allocating of BITS stations/Allocation/3 Allocation of hauls to country/Output files/list of totalallocated hauls processed.csv", sep=";")

