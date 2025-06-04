library(tidyverse)
library(raster)
library(sf)
library(terra)
library(tigris)

# Set timeout to 10 minutes
options(timeout=6000)

# Set the projection and load the idaho reference raster
projection <- "epsg:5070"

ref_rast <- rast(here::here("data/processed/wfrc_BP_ID_3km_2024-12-03.tif"))
ref_rast_proj <- project(ref_rast, projection)

# Load the Idaho counties
id_counties <- tigris::counties(state = "ID", year = 2023)
id_counties_proj <- st_transform(id_counties, projection)

# Load the rails data
rails <- tigris::rails(year = 2023)
id_rails <- st_crop(rails, id_counties, mask = TRUE)
id_rails_proj <- st_transform(id_rails, projection)
#plot(id_rails_proj$geometry)

# get the rail lines that cross each pixel

v <- vect(id_rails_proj)
v
plot(v)
rails <- as.lines(v)
rails
plot(rails)
rail_rast <- rasterizeGeom(rails, ref_rast_proj, fun = "crosses", unit = "km")
rail_rast_crop <- crop(rail_rast, ref_rast_proj, mask = TRUE)
plot(rail_rast_crop)

# calculate the distance from the rails
rail_dist_rast <- rasterize(vect(id_rails_proj), ref_rast_proj)
#test_rast <- rasterize(vect(nw_roads_proj), templateRas)
rail_dist_rast <- terra::distance(test_rast)
rail_dist_rast_crop <- crop(rail_dist_rast / 1000, ref_rast_proj, mask = TRUE)
plot(rail_dist_rast_crop)

# Save the rasters
writeRaster(rail_rast_crop, here::here(paste0("data/processed/rail_crosses_id_3km_pred_crop_", 
                                                   Sys.Date(), ".tif")), overwrite = TRUE)

writeRaster(rail_dist_rast_crop, here::here(paste0("data/processed/rail_dist_id_3km_pred_crop_", 
                                              Sys.Date(), ".tif")), overwrite = TRUE)

