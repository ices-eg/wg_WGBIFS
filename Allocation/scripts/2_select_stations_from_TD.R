library(data.table)
library(plyr)
library(sf)
library(mapview)
library(RANN)
library(dbscan)
library(openxlsx)

sf_use_s2(FALSE)
wd <- "Allocation/"

#get costum functions
source(paste0(wd, "scripts/fun_notTooClose.r"))
source(paste0(wd, "scripts/fun_trackToSf.r"))

#------Make plan for--------
yr <- 2026
qtr <- 1

sample(100)[1] #should only be done/ changed once pr plan..
set.seed(58)

#----------------------------
Nplan<- read.table(paste0(wd, "planned/", yr, "_Q", qtr, "/NHaulPlanned_lengthCPUE.csv"), 
                   header=TRUE,sep=";")

td <- read.xlsx(paste0(wd, "data/Trawl_database_v25.xlsx"),
                sheet = "td_all_stations")
td <- td[td$Status == "V", ]

track <- read.xlsx(paste0(wd, "data/Trawl_database_v25.xlsx"),
                   sheet = "track_deg_dec")
track <- trackToSf(track)

td <- merge(td, track)
mapview(st_as_sf(td), zcol = "Area")

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
  write.table(plan, paste0(wd, "planned/", yr, "_Q", qtr, "/select_status.csv"),
              sep = ";", dec = ".", append = append, col.names = wNames, row.names = F)
  
  td2
}
res <- rbind.fill(lapply(1:nrow(Nplan), fun))

mapview(st_as_sf(res), zcol = "Area")

##############################################################################################
#status
status <- read.csv2(paste0(wd, "planned/", yr, "_Q", qtr, "/select_status.csv"))
status <- merge(Nplan, status, by = c("SD", "Layer"))

status$miss <- status$NHauls - status$allocated
miss <- status[status$miss > 0, ]

##############################################################################################
#try to moove a depth strata to fulfill the requirements
fun <- function(j) {
  area <- unique(miss$SD)[j]
  miss2 <- miss[miss$SD == area, ]
  
  #find closets strata automatically
  lookup <- Nplan[Nplan$SD == area & ! Nplan$Layer %in% miss2$Layer, ]
  index <- nn2(query = miss2[, c("SD", "Layer")], 
               data = lookup[, c("SD", "Layer")], 
               k = 1)
  
  new_layer <- lookup[index[["nn.idx"]], ]$Layer
  
  td3 <- td[td$Area == area & td$Layer %in% new_layer, ]
  
  fun <- function(jj) {
    plan <- miss2[jj, c("SD", "Layer")]
    plan$mooved_to_layer <- paste0(new_layer[jj], " from layer ", plan$Layer) 
    
    tdx <- td3[td3$Layer == new_layer[jj], ]
    if (nrow(tdx) != 0)
      tdx <- notTooClose(data = tdx, fixed = res$NrHaul)
    
    #exclude already chosen ones
    tdx <- tdx[! tdx$NrHaul %in% res$NrHaul, ]
    
    if (nrow(tdx) >= miss2$miss[jj])
      tdx <- tdx[sample(nrow(tdx), miss2$miss[jj]), ]
      
    #update plan  
    plan$n_mooved <- nrow(tdx)
    append <- T
    wNames <- F
    if (j == 1){
      append <- F
      wNames <- T
    }
    write.table(plan, paste0(wd, "planned/", yr, "_Q", qtr, "/select_status.csv"),
                sep = ";", dec = ".", append = append, col.names = wNames, row.names = F)
    
    tdx
  }
  td3 <- rbind.fill(lapply(1:length(new_layer), fun))
  
}
new_layer <- rbind.fill(lapply(1:length(unique(miss$SD)), fun))

res$type = "Allocated"
new_layer$type <- "Mooved to new Layer"
res <- rbind(res, new_layer)
mapview(st_as_sf(res), zcol = "type")


#amend status
status2 <- read.csv2(paste0(wd, "planned/", yr, "_Q", qtr, "/select_status.csv"))
status <- merge(status, status2, by = c("SD", "Layer"), all.x = T)
status$miss_after_move <- status$miss - status$n_mooved 
status$miss_after_move[is.na(status$miss_after_move)] <- 0

write.csv2(status, paste0(wd, "planned/", yr, "_Q", qtr, "/select_status.csv"), 
           row.names = F, quote = F)

##############################################################################################
miss <- status[status$miss_after_move > 0, ]

#try to moove an area to fulfill the requirements
fun <- function(j) {
  area <- unique(miss$SD)[j]
  miss2 <- miss[miss$SD == area, ]
  
  #find closets strata automatically
  lookup <- Nplan[Nplan$SD != area & Nplan$Layer == miss2$Layer &
                    Nplan$AreaSection == miss2$AreaSection, ]
  index <- nn2(query = miss2[, c("SD", "Layer")], 
               data = lookup[, c("SD", "Layer")], 
               k = 1)
  
  new_Area <- lookup[index[["nn.idx"]], ]$SD
  td3 <- td[td$Area == new_Area & td$Layer == miss2$Layer, ]
  
  if (nrow(td3) != 0)
    td3 <- notTooClose(data = td3, fixed = res$NrHaul)
  
  #exclude already chosen ones
  td3 <- td3[! td3$NrHaul %in% res$NrHaul, ]
  
  if (nrow(td3) >= miss2$miss_after_move)
    td3 <- td3[sample(nrow(td3), miss2$miss_after_move), ]
  
  plan <- miss2[, c("SD", "Layer")]
  plan$mooved_to_area <- paste0(new_Area, " from area ", area) 
  
  #update plan  
  plan$n_mooved_area <- nrow(td3)
  append <- T
  wNames <- F
  if (j == 1){
    append <- F
    wNames <- T
  }
  write.table(plan, paste0(wd, "planned/", yr, "_Q", qtr, "/select_status.csv"),
              sep = ";", dec = ".", append = append, col.names = wNames, row.names = F)
  
  td3
  
}
new_area <- rbind.fill(lapply(1:length(unique(miss$SD)), fun))

new_area$type <- "Mooved to new area"
res <- rbind(res, new_area)
mapview(st_as_sf(res), zcol = "type")


#amend status
status2 <- read.csv2(paste0(wd, "planned/", yr, "_Q", qtr, "/select_status.csv"))
status <- merge(status, status2, by = c("SD", "Layer"), all.x = T)
status$miss_after_move_area <- status$miss_after_move - status$n_mooved_area 
status$miss_after_move_area[is.na(status$miss_after_move_area)] <- 0

write.csv2(status, paste0(wd, "planned/", yr, "_Q", qtr, "/select_status.csv"), 
           row.names = F, quote = F)


##############################################################################################
miss <- status[status$miss_after_move_area > 0, ]
miss[, c("SD", "Layer", "miss_after_move_area")]

potential <- status[status$miss == 0, c("SD", "Layer")]
potential$assign_exess <- 0

potential[potential$SD %in% c(25:26), "assign_exess"] <- 5
potential <- potential[potential$assign_exess > 0, ]

#potential <- data.frame(SD = 26, Layer = 3, assign_exess = 1)
#tmanually select where to allocate
fun <- function(k) {
  print(k)
  area <- potential[k, ]$SD
  layer <- potential[k, ]$Layer
  plan <- potential[k, ]
  
  td4 <- td[td$Area == area & td$Layer == layer, ]
  
  if (nrow(td4) != 0)
    td4 <- notTooClose(data = td4, fixed = res$NrHaul)
  
  #exclude already chosen ones
  td4 <- td4[! td4$NrHaul %in% res$NrHaul, ]
  
  if (nrow(td4) >= plan$assign_exess) {
    td4 <- td4[sample(nrow(td4), plan$assign_exess), ]
    
  } else{
    print(paste0("Only ", nrow(td4), " Eklstra for ", area, " ", layer))
    td4
  }
  
}
manual <- rbind.fill(lapply(1:nrow(potential), fun))

manual$type <- "assigned manually"
res <- rbind(res, manual)
mapview(st_as_sf(res), zcol = "type")

#ammedns staus
status <- merge(status, potential, by = c("SD", "Layer"), all.x = T)
write.csv2(status, paste0(wd, "planned/", yr, "_Q", qtr, "/select_status.csv"), 
           row.names = F, quote = F)

#final chack
nrow(res) == sum(Nplan$NHauls)

## final selection
saveRDS(res, paste0(wd, "planned/", yr, "_Q", qtr, "/Allocated_stations.rds"))






