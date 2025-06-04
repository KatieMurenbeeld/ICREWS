library(tidyverse)
library(raster)
library(sf)
library(terra)
library(tigris)
library(stringr)
library(RCurl)
library(stars)
library(spdep)
library(gstat)
library(spatstat)
library(raster)

# Set timeout to 10 minutes
options(timeout=6000)

# Set the projection and load the idaho reference raster
projection <- "epsg:5070"

ref_rast <- rast(here::here("data/processed/wfrc_BP_ID_3km_2024-12-03.tif"))
ref_rast_proj <- project(ref_rast, projection)

# create a for loop to calculate the road density (number of roads as lines that cross a 3km pixel)
# for each county in Idaho and create a large raster

#counties <- c("Ada County", "Canyon County", "Camas County")
id_counties <- tigris::counties(state = "ID", year = 2023)
id_counties_proj <- st_transform(id_counties, projection)

# template raster function
template_rast <- function(tmp_vector) {
  XMIN <- ext(tmp_vector)$xmin
  XMAX <- ext(tmp_vector)$xmax
  YMIN <- ext(tmp_vector)$ymin
  YMAX <- ext(tmp_vector)$ymax
  aspectRatio <- (YMAX-YMIN)/(XMAX-XMIN)
  cellSize <- 3000
  NCOLS <- as.integer((XMAX-XMIN)/cellSize)
  NROWS <- as.integer(NCOLS * aspectRatio)
  templateRas <- rast(ncol=NCOLS, nrow=NROWS, 
                    xmin=XMIN, xmax=XMAX, ymin=YMIN, ymax=YMAX,
                    vals=1, crs=crs(tmp_vector))
}


out_rast <- template_rast(id_counties_proj)
out_rast <- resample(out_rast, ref_rast) 
out_rast

for (i in id_counties_proj$NAMELSAD){
  print(i)
  tmp_roads <- tigris::roads(state = "ID", county = i, year = 2023)
  tmp_roads_proj <- st_transform(tmp_roads, projection)
  #tmp_ref_rast <- template_rast(tmp_roads_proj)
  tmp_county <- id_counties_proj %>%
    filter(NAMELSAD == i)
  tmp_ref_rast <- crop(ref_rast_proj, tmp_county)
  tmp_roads_lines <- as.lines(vect(tmp_roads_proj))
  road_cross_rast <- rasterizeGeom(tmp_roads_lines, tmp_ref_rast, fun = "crosses", unit = "km")
  road_cross_rast_crop <- crop(road_cross_rast, tmp_county, mask = TRUE)
  out_rast <- mosaic(out_rast, road_cross_rast_crop)
}
plot(out_rast)

id_road_cross_crop <- crop(out_rast, ref_rast_proj, mask = TRUE)

plot(id_road_cross_crop)
# check that the number of pixels match the reference raster
nrow(as.data.frame(ref_rast_proj))
nrow(as.data.frame(id_road_cross_crop))


# Save the raster
writeRaster(id_road_cross_crop, here::here(paste0("data/processed/all_road_type_crosses_id_3km_pred_crop_", 
                                              Sys.Date(), ".tif")), overwrite = TRUE)


