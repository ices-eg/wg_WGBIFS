library(data.table)
library(sf)
library(ggplot2)
library(dplyr)
library(ncdf4)
library(raster)
library(lwgeom)
library(terra)
library(rgeos)
library(maptools)
library(smoothr)

ices_areas <- st_read("Data/ICES_areas_raw_20160301.shp", query = "SELECT * FROM \"ICES_areas_raw_20160301\" WHERE SubDivisio IN ('21','22','23','24','25','26','27','28','29','30','31','32')")
ices_areas <- ices_areas %>% rename(SubDivision = SubDivisio)


ices_rectangles <- st_read("Data/ICES_statrec_20091023.shp")
st_crs(ices_rectangles)<-4326

ices_areas_rectangles <- st_intersection(ices_areas, ices_rectangles)

#choose INPUT Raster
raster_input <- raster("emodnet/EMODnet_mosaic.nc")
#raster_input <- raster("data/GEBCO_2020.nc")
raster_input_baltic <- crop(raster_input, extent(ices_areas))

# Recalculate raster values to facilitate raster to polygon functions
m <- c(-9999, -10, 1,  -10, 9999, 2)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
raster_input_baltic_bin <- reclassify(raster_input_baltic, rclmat)


# Convert to SpatRaster for Terra library
raster_input_baltic_bin_rast<-rast(raster_input_baltic_bin)
# Convert to Polygon
raster_input_10m_poly<-as.polygons(raster_input_baltic_bin_rast)

#Choose depth field for GEBCO or EMODnet
raster_input_10m_poly_deep<-raster_input_10m_poly[raster_input_10m_poly$Elevation.relative.to.sea.level<2]
#raster_input_10m_poly_deep<-raster_input_10m_poly[raster_input_10m_poly$depth<2]
v_sf<-as(raster_input_10m_poly_deep,"Spatial")
raster_input_10m_poly_deep_sf <- as(v_sf, "sf")
raster_input_10m_poly_deep_sf_valid<-st_make_valid(raster_input_10m_poly_deep_sf)
ices_areas_rectangles_valid<-st_make_valid(ices_areas_rectangles)



unionlayer<-st_intersection(raster_input_10m_poly_deep_sf_valid,ices_areas_rectangles_valid)
unionlayer_poly <- st_collection_extract(unionlayer, "POLYGON")
unionlayer_poly<-densify(unionlayer_poly, max_distance = 0.05)
unionlayer_poly$Area_ST<-st_area(unionlayer_poly)
unionlayer_poly$Area_ST_nm<-unionlayer_poly$Area_ST/(1852*1852)
unionlayer_poly$Area_ST<-NULL
unionlayer_poly$BIFS_code <- paste0(unionlayer_poly$ICESNAME,".",unionlayer_poly$Area_Full)
