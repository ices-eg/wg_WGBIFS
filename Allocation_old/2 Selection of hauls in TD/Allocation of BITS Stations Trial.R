library(data.table)
library(plyr)
library(sf)
library(mapview)
library(RANN)

sf_use_s2(FALSE)

year <- 2025
wd <- "Q:/20-forskning/20-dfad/users/jostou/home/yearly/WGBIFS/Allocation/"

#get costum functions
source(paste0(wd, "fun_notTooClose.r"))

#Nplan<- read.table(paste0(wd, "1 Distribution of stations on strata/Output files/NHaulPlanned_no_CPUE.csv"), header=TRUE,sep=";")
Nplan<- read.table(paste0(wd, "1 Distribution of stations on strata/Output files/NHaulPlanned.csv"), header=TRUE,sep=";")

Nplan <- Nplan[Nplan$NHauls > 0, ]

td <- readRDS(paste0(wd, "2 Selection of hauls in TD/Input data/td_clean.rds"))
td <- td[td$Status == "V", ]
mapview(st_as_sf(td))

## Allocate to strata at random.
#each line in Nplan is a trata
fun <- function(i) {
  print(i)
  area <- Nplan[i, ]$SD
  layer <- Nplan[i, ]$Layer
  Nhauls <- Nplan[i, ]$NHauls
  plan <- Nplan[i, c("SD", "Layer")]
  
  td2 <- td[td$Area == area & td$Layer == layer, ]
  
  #deal with clustered hauls
  if (nrow(td2) != 0)
    td2 <- notTooClose(data = td2)
  
  if (nrow(td2) >= Nhauls) {
    plan$comment <-  "OK"
    plan$allocated <- Nhauls
    
    #sample at random the apropriate number of hauls for the strata
    td2 <- td2[sample(nrow(td2), Nhauls), ]
    
  } else{
    plan$comment <-  "Not enough stations in strata"
    plan$allocated <- nrow(td2)
  }
  
  append <- T
  wNames <- F
  if (i == 1){
    append <- F
    wNames <- T
  }
  write.table(plan, paste0(wd, "2 Selection of hauls in TD/Output data/select_status.csv"),
              sep = ";", dec = ".", append = append, col.names = wNames, row.names = F)
  
  td2
}
res <- rbind.fill(lapply(1:nrow(Nplan), fun))

mapview(st_as_sf(res), zcol = "Area")

##############################################################################################
#status
status <- read.csv2(paste0(wd, "2 Selection of hauls in TD/Output data/select_status.csv"))
status <- merge(Nplan, status, by = c("SD", "Layer"))

status$miss <- status$NHauls - status$allocated
miss <- status[status$miss > 0, ]

#by area so no stations overlap with selcted..
fun <- function(j) {
  area <- unique(miss$SD)[j]
  miss2 <- miss[miss$SD == area, ]
  
  #find closets strata automatically
  lookup <- Nplan[Nplan$SD == area & ! Nplan$Layer %in% miss2$Layer, ]
  index <- nn2(query = miss2[, c("SD", "Layer")], 
               data = lookup[, c("SD", "Layer")], 
               k = 1)
  
  #new_area <- Nplan[index[["nn.idx"]], ]$SD
  new_layer <- lookup[index[["nn.idx"]], ]$Layer
  
  td3 <- td[td$Area == area & td$Layer %in% new_layer, ]
  
  fun <- function(jj) {
    tdx <- td3[td3$Layer == new_layer[jj], ]
    
    if (nrow(tdx) != 0)
      tdx <- notTooClose(data = tdx, fixed = res$NrHaul)
    
    #exclude already chosen ones
    tdx <- tdx[! tdx$NrHaul %in% res$NrHaul, ]
    
    if (nrow(tdx) >= miss2$miss[jj]) {
      tdx <- tdx[sample(nrow(tdx), miss2$miss[jj]), ]
    
    } else{
      print(paste0("Still missing ", miss2$miss[jj] - nrow(tdx), 
                   " for ", area, " ", miss2$Layer))
      tdx
    }
  }
  td3 <- rbind.fill(lapply(1:length(new_layer), fun))
  
}
new_layer <- rbind.fill(lapply(1:length(unique(miss$SD)), fun))

res$type = "Allowcated"
new_layer$type <- "Mooved to new Layer"

res <- rbind(res, new_layer)
mapview(st_as_sf(res), zcol = "type")

pts <- res %>%
  sf::st_as_sf(coords = c("lon1", "lat1")) %>% 
  sf::st_set_crs(4326)
mapview(pts, zcol = "Area")

##############################################################################################
#Bonus hauls..
additional <- status[status$miss == 0, c("SD", "Layer")]
additional$NHauls <- 2

fun <- function(k) {
  print(k)
  area <- additional[k, ]$SD
  layer <- additional[k, ]$Layer
  plan <- additional[k, ]
  
  td4 <- td[td$Area == area & td$Layer %in% layer, ]
  
  if (nrow(td4) != 0)
    td4 <- notTooClose(data = td4, fixed = res$NrHaul)
    
  #exclude already chosen ones
  td4 <- td4[! td4$NrHaul %in% res$NrHaul, ]
    
  if (nrow(td4) >= plan$NHauls) {
    td4 <- td4[sample(nrow(td4), plan$NHauls), ]
      
  } else{
    print(paste0("Only ", nrow(td4), " Eklstra for ", area, " ", layer))
    td4
  }
 
}
additional <- rbind.fill(lapply(1:nrow(additional), fun))

additional$type = "additional"
res <- rbind(res, additional)

mapview(st_as_sf(res), zcol = "type")

pts <- res %>%
  sf::st_as_sf(coords = c("lon1", "lat1")) %>% 
  sf::st_set_crs(4326)
mapview(pts, zcol = "Layer")





