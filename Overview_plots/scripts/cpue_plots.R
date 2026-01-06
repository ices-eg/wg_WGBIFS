library(ggplot2)
library(sf)
library(data.table)
library(icesDatras)

sf_use_s2(F) #flat earth for easier and fatser plotting

## Input
wd <- "Overview_plots/"
year <- 2026
years <- c(2025:2026)
quarters <- c(1, 4)
species <- c("Gadus morhua", "Limanda limanda", "Platichthys flesus", 
             "Pleuronectes platessa", "Scophthalmus maximus", "Scophthalmus rhombus")

#areas
ices <- st_read("inputs/area/ICES_Areas_20160601_cut_dense_3857.shp")%>%
  st_transform(4326)

#placement of plots
dir.create(paste0(wd, year, "/cpue"), showWarnings = FALSE)
dir.create(paste0(wd, year, "/cpue/q", quarters[1]), showWarnings = FALSE)
dir.create(paste0(wd, year, "/cpue/q", quarters[2]), showWarnings = FALSE)

##plot function
plotBuble <- function(dat, tit, z) {
  dat$z <- dat[, z]
  dat$z[is.na(dat$z)] <- 0
  
  max_val = max(aa[, names(aa)[names(aa) %like% "CPUE."]], na.rm = T)
  max_rad = 30
  max_areal <- pi * max_rad^2
  
  areal <- max(dat$z, na.rm = T)/max_val * max_areal
  radius <- sqrt(areal/pi)
  
  dat$catch <- ifelse(dat$z > 0, "yes", "no")
  
  ggplot() + 
    geom_sf(data = ices, color = "black", fill = "white")+
    geom_point(data= dat, aes(x=ShootLon, y=ShootLat, size=z), 
               alpha=0.5, col = "blue")+
    scale_size(range = c(.1, radius),  
               name="Catch in numbers per hour of hauling")+
    geom_point(data= dat, aes(x=ShootLon, y=ShootLat, col=catch), 
               size = 1.5)+
    scale_colour_manual("Non-Zero", 
                        breaks = c("yes", "no"),
                        values = c("green", "red"))+
    geom_sf() +
    ggtitle(paste0(tit))+
    xlab("Longitude")+
    ylab("Latitude")+
    coord_sf(xlim = c(8, 22), ylim = c(54, 59), expand = FALSE)+
    theme(text = element_text(size = 15),
          panel.background = element_rect(fill = "#B2BEB5"),
          panel.border = element_rect(colour = "black", fill=NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  ggsave(paste0(wd, year, "/cpue/q", q, "/",tit, ".png"), 
         width = 500, height = 300, units = 'mm', dpi = 200)
  
}

#get cpue for selected years and quarters and plot pr species
for (yr in years) { #subset year
  
  for (q in quarters) { #subset quarter
    
    if (yr == years[1] & q == quarters[1] |
        yr == years[2] & q == quarters[2]) #get only last quarter of first year and first quarter of latest year
      next
    
    cpue1 <- getCPUELength("BITS", yr, q) #datras pakage funtion for direct download
    
    for (sp in species) {
      
      print(paste0(yr, " - ", q, " - ", sp))
      cpue2 <- cpue1[cpue1$Species == sp, ] #subset by species, specefies on top
      ###
      
      cpue2<-subset(cpue2,!( is.na(ShootLon) | is.na(ShootLat)))
      
      cpue2$Lngt_cat<-NA
      
      # add a column for length groups, species dependent
      for (i in 1:nrow(cpue2)){
        
        if (sp == "Gadus morhua") {
          
          if(cpue2$LngtClas[i]<250){
            cpue2$Lngt_cat[i]<-1
          }
          if(cpue2$LngtClas[i]>=250&cpue2$LngtClas[i]<450){
            cpue2$Lngt_cat[i]<-2
          }
          if(cpue2$LngtClas[i]>=450){
            cpue2$Lngt_cat[i]<-3
          }
          
        } else if (sp == "Limanda limanda"){
          
          if(cpue2$LngtClas[i]<150){
            cpue2$Lngt_cat[i]<-1
          }
          if(cpue2$LngtClas[i]>=150){
            cpue2$Lngt_cat[i]<-2
          }
        } else {
          
          if(cpue2$LngtClas[i]<200){
            cpue2$Lngt_cat[i]<-1
          }
          if(cpue2$LngtClas[i]>=200){
            cpue2$Lngt_cat[i]<-2
          }
        }
      }
      
      # Aggregate cpue per length category
      
      cpue_byLngt_tmp<-aggregate(cpue2$CPUE_number_per_hour,by=list(cpue2$Year,cpue2$Quarter,cpue2$Ship,cpue2$Gear,cpue2$HaulNo,cpue2$ShootLat,cpue2$ShootLon,cpue2$Lngt_cat),FUN="sum",na.rm=T)
      names(cpue_byLngt_tmp)<-c("Year","Quarter","Ship","Gear","HaulNo","ShootLat","ShootLon","Lngt_cat","CPUE")
      
      #put the lengthCat as columns
      aa<-reshape(cpue_byLngt_tmp,v.names="CPUE",idvar=c("Year","Quarter","Ship","Gear","HaulNo","ShootLat","ShootLon"),timevar="Lngt_cat",direction="wide")
      
      #---------------------------------------------------------------------------------------------------
      #plotting pr length groups for the individual species
      #-------------------------------------------------------------------------------
      if (sp == "Gadus morhua"){
        
        plotBuble(dat = aa, z = "CPUE.1", 
                  tit = paste0(sp, " ", yr, " Q", q, " less than 25cm"))
        
        plotBuble(dat = aa, z = "CPUE.2", 
                  tit = paste0(sp, " ", yr, " Q", q, " 25-45 cm"))
        
        plotBuble(dat = aa, z = "CPUE.3", 
                  tit = paste0(sp, " ", yr, " Q", q, " greater than 45 cm"))
        
      } else if (sp == "Limanda limanda"){
        plotBuble(dat = aa, z = "CPUE.1", 
                  tit = paste0(sp, " ", yr, " Q", q, " less than 15 cm"))
        
        plotBuble(dat = aa, z = "CPUE.2", 
                  tit = paste0(sp, " ", yr, " Q", q, " greater than 15 cm"))
      } else {
        
        plotBuble(dat = aa, z = "CPUE.1", 
                  tit = paste0(sp, " ", yr, " Q", q, " less than 20 cm"))
        
        plotBuble(dat = aa, z = "CPUE.2", 
                  tit = paste0(sp, " ", yr, " Q", q, " greater than 20 cm"))
        
      }
    } 
  }
}

