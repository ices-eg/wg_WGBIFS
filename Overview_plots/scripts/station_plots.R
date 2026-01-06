library(ggplot2)
library(sf)
library(data.table)
library(icesDatras)
library(icesVocab)

#setwd("Q:/20-forskning/20-dfad/users/jostou/home/yearly/WGBIFS/")
sf_use_s2(F)

## Input
wd <- "Overview_plots/"
year <- 2026
years <- c(2025, 2026)
quarters <- c(1, 4)

dir.create(paste0(wd, year, "/stations"), showWarnings = FALSE)
dir.create(paste0(wd, year, "/lendist"), showWarnings = FALSE)

#
species <- c("Gadus morhua", "Limanda limanda", "Platichthys flesus", 
             "Pleuronectes platessa", "Scophthalmus maximus")

sps <- getCodeList("SpecWoRMS", date = NULL)
sps$Valid_Aphia <- sps$Key

#areas
ices <- st_read("inputs/area/ICES_Areas_20160601_cut_dense_3857.shp")%>%
  st_transform(4326)

#colors
col <- read.csv(paste0(wd, "inputs/color_map.csv"))
col <- setNames(col$color, col$nation)

plotStation <- function(dat, tit) {
  
  ggplot() + 
    geom_sf(data = ices, color = "black", fill = "white")+
    geom_point(data= dat, aes(x=ShootLong, y=ShootLat, col=Country), 
               size = 3)+
    scale_color_manual(values=col)+
    geom_sf() +
    ggtitle(paste0(tit))+
    xlab("Longitude")+
    ylab("Latitude")+
    coord_sf(xlim = c(8, 22), ylim = c(54, 59), expand = FALSE)+
    theme(panel.background = element_rect(fill = "#B2BEB5"),
          panel.border = element_rect(colour = "black", fill=NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  ggsave(paste0(wd, year, "/stations/", tit, ".png"), 
         width = 320, height = 300, units = 'mm', dpi = 300)
  
}

plotGears <- function(dat, x, y, tit) {
  
  dat$x <- dat[, x]
  dat$y <- dat[, y]
  
  dat$y[dat$y == -9] <- NA
  dat <- dat[!is.na(dat$y), ]
  
  ggplot() + 
    geom_point(data= dat, aes(x=x, y=y, col=Country), size = 2)+
    scale_color_manual(values=col)+
    facet_grid(Gear ~ ., scales="free")+
    xlab(x)+
    ylab(y)+
    theme_bw(base_size = 20)
  
  ggsave(paste0(wd, year, "/stations/", tit, ".png"), 
         width = 320, height = 300, units = 'mm', dpi = 300)
  
}

plotLen <- function(dat, sp, tit) {
  
  dat <- dat[dat$Description == sp, ]

  dat1 <- dat[dat$Year == yr, ]
  dat2 <- dat[dat$Year != yr, ]
  setDT(dat2)
  dat2 <- dat2[ ,. (HLNoAtLngt = sum(HLNoAtLngt, na.rm = T)),
                   by = .(Year, LngtClass)]
  dat2 <- dat2[ ,. (HLNoAtLngt = mean(HLNoAtLngt, na.rm = T)),
                by = .(LngtClass)]
  
  ggplot() + 
    geom_bar(data= dat1, 
             aes(fill=Country, x=LngtClass, y=HLNoAtLngt), size = 2,
             position="stack", stat="identity")+
    geom_line(data = dat2, aes(x=LngtClass, y=HLNoAtLngt, col = "mean")) +
    scale_colour_manual("5 Year Mean", 
                        breaks = c("mean"),
                        values = c("black"))+ 
    xlab("Length cm")+
    ylab("Numbers")+
    ggtitle(sp)+
    theme_bw(base_size = 20)
  
  ggsave(paste0(wd, year, "/lendist/lenDist_", tit, ".png"), 
         width = 320, height = 300, units = 'mm', dpi = 300)
  
}

#get data for selected years and quarters and plot pr species
for (yr in years) { #subset year
  
  for (q in quarters) { #subset quarter
    
    if (yr == years[1] & q == quarters[1] |
        yr == years[2] & q == quarters[2]) #get only last quarter of first year and first quarter of latest year
      next
    
    ##get data
    print("hh")
    hh <- getDATRAS(record = "HH", survey = "BITS", c((yr-5):yr), q) #datras pakage funtion for direct download
    print("hl")
    hl <- getDATRAS(record = "HL", survey = "BITS", c((yr-5):yr), q) #datras pakage funtion for direct download
    
    #get data type for hl, to calculate length dist evenly between countries
    hl <- merge(hl, hh[, c("Year", "Country", "Ship", "HaulNo", "DataType", "HaulDur")],
                by = c("Year", "Country", "Ship", "HaulNo"))
    
    #cut hh to present year
    hh <- hh[hh$Year == yr, ]
    
    print("plotting",)
    ## plot for hh
    plotStation(dat = hh, tit = paste0("stations_", yr, "_Q", q))
    
    plotGears(dat = hh, x = "Depth", y = "DoorSpread", 
              tit = paste0("Doospread_", yr, "_Q", q))
    
    plotGears(dat = hh, x = "Depth", y = "Warplngt", 
              tit = paste0("Warplngt_", yr, "_Q", q))
    
    plotGears(dat = hh, x = "Depth", y = "Netopening", 
              tit = paste0("Netopening_", yr, "_Q", q))
    
    ## hl data
    hl[hl$DataType == "R", "HLNoAtLngt"] <- 
      hl[hl$DataType == "R", "HLNoAtLngt"] * (60/hl[hl$DataType == "R", "HaulDur"])
    
    ## fix stupid length code differences....(could additionally be rounded for prettier plots..)
    hl <- merge(hl, sps, by = "Valid_Aphia")
    hl$LngtClass[hl$LngtClass == -9] <- NA
    hl <- hl[!is.na(hl$LngtClass), ]
    hl[hl$LngtCode == ".", "LngtClass"] <- hl[hl$LngtCode == ".", "LngtClass"]/10
    hl[hl$LngtCode == "0", "LngtClass"] <- hl[hl$LngtCode == "0", "LngtClass"]/10

    #correct wrong data...
    hl[hl$Description == "Platichthys flesus" & hl$Year == 2020 & hl$Quarter == 4 & 
         hl$Country  == "DE" & hl$LngtClass == 420, "LngtClass"] <- 42
    
    for (sp in species) {
      
      plotLen(dat = hl, sp = sp, tit = paste0(sp, "_", yr, "_Q", q))
      
    }
    
  }
}


