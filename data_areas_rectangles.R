library(data.table)
library(sf)
library(ggplot2)
library(dplyr)
library(ncdf4)
library(raster)
library(lwgeom)
#library(httr)

# Read ICES Areas
# ices_areas <- st_read("https://gis.ices.dk/gis/rest/services/ICES_reference_layers/ICES_Areas/MapServer/0/query?where=1%3D1&outFields=%2A&returnGeometry=true&f=geojson")
# ices_subdivisions <- c('21','22','23','24','25','26','27','28','29','30','31','32')
ices_areas <- st_read("Data/ICES_Areas_Raw/ICES_areas_raw_20160301.shp", query = "SELECT * FROM \"ICES_areas_raw_20160301\" WHERE SubDivisio IN ('21','22','23','24','25','26','27','28','29','30','31','32')")
ices_areas <- ices_areas %>% rename(SubDivision = SubDivisio)
#ices_areas <- ices_areas %>% st_transform(crs = 3035)

st_crs(ices_areas)
st_is_valid(ices_areas)
ggplot() + geom_sf(data = ices_areas) + coord_sf()

# Read ICES Statistical Rectangles
ices_rectangles <- st_read("Data/ICES_statrec/ICES_statrec_20091023.shp")
#ices_rectangles <- ices_rectangles %>% st_transform(crs = 3035)

st_crs(ices_rectangles)
st_is_valid(ices_rectangles)
ggplot() + geom_sf(data = ices_rectangles) + coord_sf()

# Combine ICES Areas with Statistical Rectangles
ices_areas_rectangles <- st_intersection(ices_areas, ices_rectangles)

st_crs(ices_areas_rectangles)
st_is_valid(ices_areas_rectangles)
ggplot() + geom_sf(data = ices_areas_rectangles) + coord_sf()

# Read GEBCO
gebco <- raster("D:/GIS/GEBCO/GEBCO_2020.nc")

# Crop to ICES Areas
gebco_baltic <- crop(gebco, extent(st_transform(ices_areas, crs = 4326)))

# Make contour 10 m depth contour line as polygon transformed into CRS 3035 and make geometries valid by the buffer trick
gebco_baltic_contour <- rasterToContour(gebco_baltic, levels = c(-10)) %>% st_as_sf() %>% st_cast("MULTIPOLYGON") %>% st_buffer(0.0)
#gebco_baltic_contour <- rasterToContour(gebco_baltic, levels = c(-10)) %>% st_as_sf() %>% st_cast("MULTIPOLYGON") %>% st_transform(crs = 3035) %>% st_buffer(0.0)

st_is_valid(gebco_baltic_contour)
ggplot() + geom_sf(data = gebco_baltic_contour) + coord_sf()

# Combined
ices_areas_rectangles_contour <- st_intersection(ices_areas_rectangles, gebco_baltic_contour)

ggplot() + geom_sf(data = ices_areas_rectangles_contour) + coord_sf()

ices_areas_rectangles_contour$Area__Rectangle <- paste0(ices_areas_rectangles_contour$Area_Full,".", ices_areas_rectangles_contour$ICESNAME)
ices_areas_rectangles_contour$Area__M2 <- as.numeric(st_area(ices_areas_rectangles_contour))
ices_areas_rectangles_contour$Area__KM2 <- as.numeric(st_area(ices_areas_rectangles_contour) * 1 / (1000 * 1000))
ices_areas_rectangles_contour$Area__NM2 <- as.numeric(st_area(ices_areas_rectangles_contour) * 1 / (1852 * 1852))

# Write
st_write(ices_areas_rectangles_contour[c("Area__Rectangle", "Area__NM2")], "Data/WGBIFS_mapped_areas_R.csv")

# Misc
# st_write(wgbifs_mapped_areas[c("SubDivision", "Rectangle", "Area_NM")], "Data/b.csv")
# st_write(gebco_baltic_contour_areas_rectangles[c("SubDivisio", "ICESNAME", "Area_NM2")], "Data/c.csv")
# 
# x <- st_intersection(ices_areas, ices_rectangles) %>%
#   st_intersection(gebco_baltic_contour)
# 
# y <- st_intersection(gebco_baltic_contour, ices_areas) %>%
#   st_intersection(ices_rectangles)
# 
# z <- st_intersection(gebco_baltic_contour, ices_rectangles) %>%
#   st_intersection(ices_areas)
# 
# sisp_8_areas <- fread("Data/SISP_8_areas.csv")
# 
# wgbifs_mapped_areas <- st_read("Data/WGBIFS_mapped_areas.csv")
# 
# ggplot() + geom_sf(data = ices_areas_rectangles_contour) + coord_sf()
# 
# # 38G0
 rectangle <- st_as_sfc("POLYGON((10 54.5, 11 54.5, 11 55, 10 55, 10 54.5))")
 st_crs(rectangle) = 4326
 st_transform(rectangle, crs = 3035)
 as.numeric(st_area(rectangle) * 1 / (1000 * 1000)) # km2
 as.numeric(st_area(rectangle) * 1 / (1852 * 1852)) # nmi2
# 
# #52G9
 rectangle <- st_as_sfc("POLYGON((19 61.5, 20 61.5, 20 62, 19 62, 19 61.5))")
 st_crs(rectangle) = 4326
 st_transform(rectangle, crs = 3035)
 as.numeric(st_area(rectangle) * 1 / (1000 * 1000)) # km2
 as.numeric(st_area(rectangle) * 1 / (1852 * 1852)) # nmi2
 