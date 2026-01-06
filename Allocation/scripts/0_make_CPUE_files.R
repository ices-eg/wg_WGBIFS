
library(data.table)
library(plyr)
library(icesDatras)

wd <- "Allocation/"

#Make plan for
yr <- 2028
qtr <- 1

#the get CPUE only works by year...
fun <- function(i) {
  cpue <- getCPUELength("BITS", (yr-i), qtr)

  cpue <- cpue[cpue$Species == "Gadus morhua" &
                 cpue$LngtClas >= 200 &
                 cpue$Area != 21, ]
  
  setDT(cpue)
  cpue <- cpue[ ,. (num_h = sum(CPUE_number_per_hour)),
                by = .(Year, Ship, HaulNo, Area, Depth)]
}
cpue <- rbind.fill(lapply(1:3, fun))

cpue$SD <- cpue$Area
cpue$AreaSection <- cut(cpue$SD,
                        c(0, 24, 28), c(2224, 2528))
## by area
#sum by year
setDT(cpue)
cpue_sd <- cpue[ ,. (num_h = sum(num_h)),
                 by = .(Year, SD, AreaSection)]

#mean over the years
cpue_sd <- cpue_sd[ ,. (Cpue = mean(num_h)),
                 by = .(SD, AreaSection)]

## by depth strata
cpue$Layer <- cut(round(cpue$Depth),
                  c(10, 40, seq(60, 120, by = 20))-0.1, c(2:6))

#sum by year
cpue_l <- cpue[ ,. (num_h = sum(num_h)),
                 by = .(Year, SD, Layer)]

#mean over the years
cpue_l <- cpue_l[ ,. (Cpue = mean(num_h)),
                    by = .(SD, Layer)]

#write results
dir.create(paste0(wd, "planned/", yr, "_Q", qtr, "/"), showWarnings = F)

write.csv2(cpue_sd, paste0(wd, "planned/", yr, "_Q", qtr, "/CPUE_weights_area.csv"), 
           row.names = F, quote = F)

write.csv2(cpue_l, paste0(wd, "planned/", yr, "_Q", qtr, "/CPUE_weights_depth.csv"), 
           row.names = F, quote = F)

