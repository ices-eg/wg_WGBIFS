if (!require("pacman")) install.packages("pacman")
p_load(sf, mapview, data.table, ggplot2, ggthemes, ggrepel, dplyr)

wd <- "Allocation/"

base <-  st_read("Q:/20-forskning/12-gis/Dynamisk/GEOdata2020/BasicLayers/Boundaries/ICES/ICES_Areas_20160601_cut_dense_3857.shp")%>%
  st_transform(4326)

rects <- read.csv2(paste0(wd, "data/nation_rect_distr.csv"))

setDT(rects)
rects <- dcast(rects, RectangleAlpha ~ Country, value.var = "Pct")
rects <- addMidpoint(data = rects, rectColmn = "RectangleAlpha")

## plot 
xlim <- c(8, 22)
ylim <- c(54, 59)

ggplot()+
  geom_sf(data = base, color = "black", fill = "#a6cee3", linewidth = 1)+
  drawPie(data = rects, 
          valueColmns = c("DE", "Dana", "EE", "Havf", "LT", "LV", "PL", "SE"),
          x = "mid_x", y = "mid_y",
          group  = "RectangleAlpha",
          radius = 0.5)+
  drawRect(xlim =  xlim, ylim = ylim) +
  theme(panel.background = element_rect(fill = "cornsilk"))

ggsave(paste0(wd, "data/rect_nation_overview.png"),
       width = 320, height = 300, units = 'mm', dpi = 300)
