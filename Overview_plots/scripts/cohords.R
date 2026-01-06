library(ggplot2)
library(sf)
library(data.table)
library(icesDatras)
library(plyr)

sf_use_s2(F) #flat earth for easier and fatser plotting
## Input

wd <- "Overview_plots/"
year <- 2026
years <- c(2024:2025)
quarters <- c(1, 4)
species <- c("Gadus morhua", "Limanda limanda", "Platichthys flesus", 
             "Pleuronectes platessa", "Scophthalmus maximus", "Scophthalmus rhombus")

dir.create(paste0(year, "/cohorts"), showWarnings = FALSE)

#get cpue for selected years and quarters and plot pr species
for (yr in years) { #subset year
  
  for (q in quarters) { #subset quarter
    
    if (yr == years[1] & q == quarters[1] |
        yr == years[2] & q == quarters[2]) #get only last quarter of first year and first quarter of latest year
      next
    
    #datras pakage funtion for direct download
    cpue1 <- rbind(getCPUEAge("BITS", yr, q) 
    ,getCPUEAge("BITS", yr-1, q) 
    ,getCPUEAge("BITS", yr-2, q) 
    ,getCPUEAge("BITS", yr-3, q) 
    ,getCPUEAge("BITS", yr-4, q) 
    )
    
    for (sp in species) {
      
      print(paste0(yr, " - ", q, " - ", sp))
      cpue2 <- data.table(cpue1[cpue1$Species == sp, ]) #subset by species, specefies on top
      cpue2$nrow <- 1
      cpue2 <- cpue2[ ,. (Age_1 = sum(Age_1)/sum(nrow),
                          Age_2 = sum(Age_2)/sum(nrow),
                          Age_3 = sum(Age_3)/sum(nrow),
                          Age_4 = sum(Age_4)/sum(nrow),
                          Age_5 = sum(Age_5)/sum(nrow)),
                      by = .(Year, Area)]
      
      
      
      
      for (area in unique(cpue2$Area)) {
        
        path <- paste0(wd, year, "/cohorts/", area, "/")
        dir.create(path, showWarnings = FALSE)
        
        cpue3 <- data.frame(cpue2[cpue2$Area == area, ]) 
        cpue3 <- cpue3[order(cpue3$Year), ]
        
        #display all cohords
        dat <- data.table(cpue3)
        dat <- melt(dat, id.vars = "Year", measure.vars = paste0("Age_", 1:5),
                             variable.name = "Age", value.name = "CPUE")
        ggplot()+
          geom_point(data = dat, aes(x = Year, y = Age, size = CPUE))+
          ggtitle("Catches pr year")+
          theme_bw()
        
        ggsave(paste0(path, "buble ", sp, " ", yr, " Q", q,".png"), 
               width = 500, height = 300, units = 'mm', dpi = 300)
        
        #specefik cohorts
        fun <- function(k) {
          xx <- data.frame(cohort= diag(data.matrix(cpue3[k:nrow(cpue3), c(3:7)])))
          xx$Year <- as.factor(cpue3$Year[k])
          xx$Age <- 1:nrow(xx)
          
          xx
        }
        cohort <- rbind.fill(lapply(1:nrow(cpue3), fun))
        
        #as lines
        ggplot()+
          geom_line(data = cohort, aes(x = Age, y = cohort, color = Year))+
          geom_point(data = cohort, aes(x = Age, y = cohort, color = Year))+
          ggtitle("Cohorts")+
          ylab("Cohort CPUE")+
          theme_bw()
        
        ggsave(paste0(path, "line ", sp, " ", yr, " Q", q,".png"), 
               width = 500, height = 300, units = 'mm', dpi = 300)
        
        #as retention %
        setDT(cohort)
        cohort <- cohort[, 'pct' := cohort/max(cohort)*100, 
                         by=.(Year)]
        
        ggplot()+
          geom_tile(data = cohort, aes(x = Age, y = Year, fill = pct), col = "green")+
          geom_text(data = cohort, aes(x = Age, y = Year, label = round(pct)), col = "white") +
          ggtitle("Retention")+
          scale_y_discrete(limits=rev)+
          theme_bw()
        
        ggsave(paste0(path, "retention ", sp, " ", yr, " Q", q,".png"), 
               width = 500, height = 300, units = 'mm', dpi = 300)

      }
    } 
  }
}

