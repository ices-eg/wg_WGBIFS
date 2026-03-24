library(data.table)
library(sf)
library(cartography)
library(mapview)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(openxlsx)

sf_use_s2(FALSE)

wd <- "Allocation/"

ices <- st_read(paste0(wd, "data/area/ICES_Areas_20160601_cut_dense_3857.shp"))|>
  st_transform(4326)
ices <- ices[ices$Area_27 %in% c("3.a.21", "3.b.23", "3.c.22", 
                                 paste0("3.d.", 24:29), "3.d.28.1", "3.d.28.2"), ]

ices_borders <- st_read(paste0(wd, "data/area/ices_borders.shp"))|>
  st_transform(4326)

eez <- st_read(paste0(wd, "data/area/eez_borders.shp"))|>
  st_transform(4326)
eez2 <- st_intersection(eez, ices)

nm12 <- st_read(paste0(wd, "data/area/nm12.shp"))

color_map <- read.csv("Overview_plots/inputs/color_map.csv")

## plot helpers
pal <- colorFactor(
  palette = color_map$color,
  domain = color_map$nationCode
)

pal2 <- colorFactor(
  palette = color_map$color[1:6],
  domain = c(1:6)
)

bbox <- st_bbox(ices) %>% 
  as.vector()

#***----------------------------------------------------------------------------****
#all the stations
td <- read.xlsx(paste0(wd, "data/Trawl_database_v25.xlsx"),
                sheet = "td_all_stations")
td <- td[td$Status == "V" & ! is.na(td$Layer), ]


MapBase <- leaflet() |>
  fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) |>
  addPolygons(data = ices,
              fillColor = "#a6cee3",
              color = 'black',
              fillOpacity = 1,
              weight = 1) |>
  addPolylines(data = eez2,
               fillColor = "transparent",
               color = 'red',
               fillOpacity = 1,
               weight = 1)|> 
  addCircleMarkers(data = td,
                   lng = ~Lon_start_deg_dec,
                   lat = ~Lat_start_deg_dec,
                   radius = 3,
                   color = ~pal2(Layer),
                   popup = ~NrHaul,
                   group = 'NrHaul') |>
  addLegend(data = td,
            position = "bottomright",
            pal = pal2, values = ~Layer,
            title = "Depth Layer",
            opacity = 1) |>
  addLabelOnlyMarkers(data = td,
                      lng = ~Lon_start_deg_dec,
                      lat = ~Lat_start_deg_dec,
                      label = ~NrHaul,
                      group = 'label',
                      labelOptions = labelOptions(
                        noHide = T, direction = 'top', 
                        textOnly = T, textsize = "20px")) |>
  addLayersControl(overlayGroups = c('label'),
                   options = layersControlOptions(collapsed = FALSE),
                   position = "bottomright") |>
  hideGroup("label")

saveWidget(MapBase, file=paste0(wd, "data/all_stations.html"))

#***----------------------------------------------------------------------------****


#Make plan for
yr <- 2025
qtr <- 4

##

allocated <- read.xlsx(paste0(wd, "planned/", yr, "_Q", qtr, 
                              "/Final allocated hauls by country for circulation 2025Q4.xlsx"),
                        startRow = 3)

allocated$nationCode <- gsub(" .*", "", allocated$Country)


##

MapBase <- leaflet() |>
  fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) |>
  addPolygons(data = ices,
              fillColor = "#a6cee3",
              color = 'black',
              fillOpacity = 1,
              weight = 1) |>
  addPolylines(data = eez2,
               fillColor = "transparent",
               color = 'red',
               fillOpacity = 1,
               weight = 1)|> 
  addCircleMarkers(data = allocated,
                   lng = ~Longitude1_deg_dec,
                   lat = ~Latitude1_deg_dec,
                   radius = 3,
                   color = ~pal(nationCode),
                   popup = ~NrHaul,
                   group = 'NrHaul') |>
  addLegend(data = allocated,
            position = "bottomright",
            pal = pal, values = ~nationCode,
            title = "Legend",
            opacity = 1) |>
  addLabelOnlyMarkers(data = allocated,
                      lng = ~Longitude1_deg_dec,
                      lat = ~Latitude1_deg_dec,
                      label = ~NrHaul,
                      group = 'label',
                      labelOptions = labelOptions(
                        noHide = T, direction = 'top', 
                        textOnly = T, textsize = "20px")) |>
  addLayersControl(overlayGroups = c('label'),
                   options = layersControlOptions(collapsed = FALSE),
                   position = "bottomright") |>
  hideGroup("label")

saveWidget(MapBase, file=paste0(wd, "planned/", yr, "_Q", qtr,
                                    "/Mapbase.html"))
           
           
Mapnm12 <- leaflet() |>
  fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) |>
  addPolygons(data = ices,
              fillColor = "#a6cee3",
              color = 'black',
              fillOpacity = 1,
              weight = 1) |>
  addPolylines(data = nm12,
               fillColor = "transparent",
               color = 'darkgrey',
               fillOpacity = 1,
               weight = 1)|> 
  addPolylines(data = eez2,
               fillColor = "transparent",
               color = 'red',
               fillOpacity = 1,
               weight = 1)|> 
  addCircleMarkers(data = allocated,
                   lng = ~Lon_start_deg_dec,
                   lat = ~Lat_start_deg_dec,
                   radius = 3,
                   color = ~pal(nationCode),
                   popup = ~NrHaul,
                   group = 'NrHaul') |>
  addLegend(data = allocated,
            position = "bottomright",
            pal = pal, values = ~nationCode,
            title = "Legend",
            opacity = 1) |>
  addLabelOnlyMarkers(data = allocated,
                      lng = ~Lon_start_deg_dec,
                      lat = ~Lat_start_deg_dec,
                      label = ~NrHaul,
                      group = 'label',
                      labelOptions = labelOptions(
                        noHide = T, direction = 'top', 
                        textOnly = T, textsize = "20px")) |>
  addLayersControl(overlayGroups = c('label'),
                   options = layersControlOptions(collapsed = FALSE),
                   position = "bottomright") |>
  hideGroup("label")

saveWidget(Mapnm12, file=paste0(wd, "planned/", yr, "_Q", qtr,
                                "/Mapnm12.html"))

###########################
MapDetailed <- leaflet() |>
  addTiles() |> 
  fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) |>
  addPolygons(data = ices,
              fillColor = "#a6cee3",
              color = 'black',
              fillOpacity = 1,
              weight = 1) |>
  addPolylines(data = nm12,
               fillColor = "transparent",
               color = 'darkgrey',
               fillOpacity = 1,
               weight = 1)|> 
  addPolylines(data = eez,
               fillColor = "transparent",
               color = 'red',
               fillOpacity = 1,
               weight = 1)|> 
  addCircleMarkers(data = allocated,
                   lng = ~Lon_start_deg_dec,
                   lat = ~Lat_start_deg_dec,
                   radius = 3,
                   color = ~pal(nationCode),
                   popup = ~NrHaul,
                   group = 'NrHaul') |>
  addLegend(data = allocated,
            position = "bottomright",
            pal = pal, values = ~nationCode,
            title = "Legend",
            opacity = 1) |>
  addLabelOnlyMarkers(data = allocated,
                      lng = ~Lon_start_deg_dec,
                      lat = ~Lat_start_deg_dec,
                      label = ~NrHaul,
                      group = 'label',
                      labelOptions = labelOptions(
                        noHide = T, direction = 'top', 
                        textOnly = T, textsize = "20px")) |>
  addLayersControl(overlayGroups = c('label'),
                   options = layersControlOptions(collapsed = FALSE),
                   position = "bottomright") |>
  hideGroup("label")

saveWidget(MapDetailed, file=paste0(wd, "planned/", yr, "_Q", qtr,
                                    "/MapDetailed.html"))


###########################
Mapdetailed2 <- leaflet() |>
  addTiles() |> 
  fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) |>
  addPolylines(data = ices_borders,
               fillColor = "transparent",
               color = 'black',
               fillOpacity = 1,
               weight = 1)|> 
  addPolylines(data = eez,
               fillColor = "transparent",
               color = 'red',
               fillOpacity = 1,
               weight = 1)|> 
  addCircleMarkers(data = allocated,
                   lng = ~Lon_start_deg_dec,
                   lat = ~Lat_start_deg_dec,
                   radius = 3,
                   color = ~pal(nationCode),
                   popup = ~NrHaul,
                   group = 'NrHaul') |>
  addLegend(data = allocated,
            position = "bottomright",
            pal = pal, values = ~nationCode,
            title = "Legend",
            opacity = 1) |>
  addLabelOnlyMarkers(data = allocated,
                      lng = ~Lon_start_deg_dec,
                      lat = ~Lat_start_deg_dec,
                      label = ~NrHaul,
                      group = 'label',
                      labelOptions = labelOptions(
                        noHide = T, direction = 'top', 
                        textOnly = T, textsize = "20px")) |>
  addLayersControl(overlayGroups = c('label'),
                   options = layersControlOptions(collapsed = FALSE),
                   position = "bottomright") |>
  hideGroup("label")

saveWidget(Mapdetailed2, file=paste0(wd, "planned/", yr, "_Q", qtr,
                                    "/MapDetailed2.html"))
