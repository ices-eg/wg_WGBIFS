if (!require("pacman")) install.packages("pacman")
p_load(sf, mapview, data.table, ggplot2, ggthemes, 
       ggrepel, dplyr, gridExtra, ggpubr)

sf_use_s2(F)

wd <- "Overview_plots/"
year <- 2026

dir.create(paste0(wd, year, "/litter"), showWarnings = FALSE)

#areas
ices <- st_read("inputs/area/ICES_Areas_20160601_cut_dense_3857.shp")%>%
  st_transform(4326)

source("scripts/add_rect_buble_pie.R")
litter <- read.csv("Litter Assessment Output_2025-04-11 08_11_43.csv")

#exclude 0 or -9.....
litter <- litter[litter$LT_Weight > 0, ]

## genreal form check
table(litter$Country, litter$LTREF)
unique(litter$UnitWgt)

#adjust weights
litter$LT_Weight <- ifelse(litter$UnitWgt == "g/haul" | 
                             (litter$Country == "LT" & litter$UnitWgt == "-9"),
                           litter$LT_Weight/1000, litter$LT_Weight)


litter$LT_Items[litter$LT_Items == "-9"] <- 1
litter$Litter_type <- substr(litter$PARAM, 1, 1)
litter$uniq <- paste0(litter$Quarter, litter$StNo)

litter$Litter_type[litter$Litter_type == "A"] <- "A - Plastic"
litter$Litter_type[litter$Litter_type == "B"] <- "B - Metals"
litter$Litter_type[litter$Litter_type == "C"] <- "C - Rubber"
litter$Litter_type[litter$Litter_type == "D"] <- "D - Glass"
litter$Litter_type[litter$Litter_type == "E"] <- "E - Nat. Prod."
litter$Litter_type[litter$Litter_type == "F"] <- "F - Miscellaneous"


setDT(litter)
amount <- litter[ ,. (Total_kg = sum(LT_Weight),
                      Total_numbers = sum(LT_Items),
                      Number_of_records = length(unique(uniq))),
               by = .(Year, Country, Litter_type)]


amount <- amount[amount$Litter_type != "L", ]

###
ggplot()+
  geom_bar(data=amount, aes(x=Year, y=Number_of_records, fill=Country),
           stat="identity")+
  scale_x_continuous(breaks = sort(unique(amount$Year)))+
  xlab("")+
  theme_bw(base_size = 60)+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.1))

ggsave(paste0(wd, year, "/litter/records_overview.png"), 
       width = 600, height = 300, units = 'mm', dpi = 200)

##
p1 <- ggplot()+
  geom_bar(data=amount, aes(x=Year, y=Total_numbers, fill=Litter_type),
           stat="identity")+
  scale_x_continuous(breaks = sort(unique(amount$Year)))+
  xlab("")+
  theme_bw(base_size = 40)+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.01))

# ggsave(paste0(year, "/litter/numbers_overview.png"), 
#        width = 400, height = 200, units = 'mm', dpi = 200)


##
p2 <- ggplot()+
  geom_bar(data=amount, aes(x=Year, y=Total_kg, fill=Litter_type),
           stat="identity")+
  scale_x_continuous(breaks = sort(unique(amount$Year)))+
  xlab("")+
  theme_bw(base_size = 40)+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.01))

# ggsave(paste0(year, "/litter/kg_overview.png"), 
#        width = 400, height = 300, units = 'mm', dpi = 200)

ggarrange(p1, p2, ncol = 2, common.legend = TRUE, legend="bottom")
ggsave(file=paste0(wd, year, "/litter/kg_number_overview.png"), 
       width = 600, height = 300, units = 'mm', dpi = 200)

for (yr in years) { #subset year
  
    dat <- litter[litter$Year == yr & litter$Quarter == q, ]
    
    #amount pr station
    setDT(dat)
    amount <- dat[ ,. (Tot_kg = sum(LT_Weight)),
                   by = .(Country, StNo, ShootLat, ShootLong)]
    
    
    #types pr square
    xlim <- c(8, 22)
    ylim <- c(54, 59)
    
    dat$PARAM2 <- substr(dat$PARAM, 1, 1)
    types <- dcast(dat, StatRec ~ PARAM2)
      
    types <- addMidpoint(data = types, rectColmn = "StatRec")
    
    
    ggplot() +
      geom_sf(data = ices, color = "black", fill = "white")+
      drawPie(data = types, 
              valueColmns = c("A", "B", "C", "D", "E", "F"),
              x = "mid_x", y = "mid_y",
              group  = "StatRec",
              radius = 0.5)+
      drawRect(xlim =  xlim, ylim = ylim) +
      theme(panel.background = element_rect(fill = "#B2BEB5"))
    
    
    ggsave(paste0(wd, year, "/litter/type_distribution.png"), 
           width = 400, height = 200, units = 'mm', dpi = 300)
    
  
}