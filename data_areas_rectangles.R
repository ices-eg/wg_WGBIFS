library(data.table)
library(sf)
library(ggplot2)
library(dplyr)
library(ncdf4)
library(raster)
library(stars)
library(smoothr)

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

# Read GEBCO or EMODnet
dataset <- "GEBCO_08"

if (dataset=="GEBCO_08") {
        raster <- raster("D:/GIS/GEBCO/GEBCO_08.nc")
}
if (dataset=="GEBCO_2020") {
        raster <- raster("D:/GIS/GEBCO/GEBCO_2020.nc")
}
if (dataset=="EMODnet") {
        raster <- raster("D:/GIS/EMODnet/EMODnet_mosaic.nc")
}
names(raster) <- "depth"

# Crop to ICES Areas
baltic_raster <- crop(raster, extent(st_transform(ices_areas, crs = 4326)))

# Recalculate raster values to facilitate raster to polygon functions - 10 m
m <- c(-9999, -10, 1,  -10, 9999, 2)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
baltic_raster_bin <- reclassify(baltic_raster, rclmat)

#Create polygon
baltic_polygon <- st_as_stars(baltic_raster_bin) %>%
        st_as_sf(merge = TRUE) %>%
        filter(depth < 2) %>%
        st_make_valid()
st_is_valid(baltic_polygon)
ggplot() + geom_sf(data = baltic_polygon) + coord_sf()

# Combined with ices areas rectangles
ices_areas_rectangles_polygon <- st_intersection(ices_areas_rectangles, baltic_polygon)
ices_areas_rectangles_polygon <- st_collection_extract(ices_areas_rectangles_polygon, c("POLYGON"))
ices_areas_rectangles_polygon <- densify(ices_areas_rectangles_polygon, max_distance = 0.05)

ggplot() + geom_sf(data = ices_areas_rectangles_polygon) + coord_sf()

# Group polygons into multipolygons by SubDivision and Rectangle
ices_subdivision_rectangles_multipolygon <- ices_areas_rectangles_polygon %>%
        group_by(SubDivision, Rectangle = ICESNAME) %>%
        summarise()

ices_subdivision_rectangles_multipolygon$SubDivision_Rectangle <- paste0(ices_subdivision_rectangles_multipolygon$SubDivision,"_", ices_subdivision_rectangles_multipolygon$Rectangle)
#ices_subdivision_rectangles_multipolygon$Area_M2 <- as.numeric(st_area(ices_subdivision_rectangles_multipolygon))
#ices_subdivision_rectangles_multipolygon$Area_KM2 <- as.numeric(st_area(ices_subdivision_rectangles_multipolygon) * 1 / (1000 * 1000))
ices_subdivision_rectangles_multipolygon$Area_NM2 <- as.numeric(st_area(ices_subdivision_rectangles_multipolygon) * 1 / (1852 * 1852))

ggplot() + geom_sf(data = ices_subdivision_rectangles_multipolygon) + coord_sf()

# Write
st_write(ices_subdivision_rectangles_multipolygon, paste0("Data/WGBIFS_ices_subdivision_rectangles_multipolygon_", dataset, "_R.csv"), layer_options = "GEOMETRY=AS_WKT")
st_write(ices_subdivision_rectangles_multipolygon, paste0("Data/WGBIFS_ices_subdivision_rectangles_multipolygon_", dataset, "_R.shp"))

# Merge results
a1 <- fread("D:/GitHub/wg_WGBIFS/Data/WGBIFS_SISP_8_areas.csv") %>%
        rename(SISP_Area_NM2 = Area_NM2) %>%
        setkey("SubDivision", "Rectangle")
a2 <- fread("D:/GitHub/wg_WGBIFS/Data/WGBIFS_StoX_areas.csv") %>%
        rename(StoX_Area_NM2 = Area_NM2) %>%
        setkey("SubDivision", "Rectangle")
a3 <- fread("D:/GitHub/wg_WGBIFS/Data/WGBIFS_ices_subdivision_rectangles_multipolygon_GEBCO_08_R.csv")[, .(SubDivision, Rectangle, Area_NM2 = round(Area_NM2, 1))] %>%
        rename(GEBCO_08_Area_NM2 = Area_NM2) %>%
        setkey("SubDivision", "Rectangle")
a4 <- fread("D:/GitHub/wg_WGBIFS/Data/WGBIFS_ices_subdivision_rectangles_multipolygon_GEBCO_2020_R.csv")[, .(SubDivision, Rectangle, Area_NM2 = round(Area_NM2, 1))] %>%
        rename(GEBCO_2020_Area_NM2 = Area_NM2) %>%
        setkey("SubDivision", "Rectangle")
a5 <- fread("D:/GitHub/wg_WGBIFS/Data/WGBIFS_ices_subdivision_rectangles_multipolygon_EMODnet_R.csv")[, .(SubDivision, Rectangle, Area_NM2 = round(Area_NM2, 1))] %>%
        rename(EMODnet_Area_NM2 = Area_NM2) %>%
        setkey("SubDivision", "Rectangle")

a <- Reduce(function(...) merge(..., all = TRUE), list(a1, a2, a3, a4, a5))

st_write(a, "Data/WGBIFS_ices_subdivision_rectangles_multipolygon_Comparison_R.csv")
