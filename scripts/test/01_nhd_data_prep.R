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

# Set projection
projection <- "epsg:5070"
# Load the reference raster
ref_rast <- rast(here::here("data/processed/wfrc_BP_ID_3km_2024-12-03.tif"))

# Load the nhd line feature data
water_line <- st_read(here::here("data/original/water/NHD_H_Idaho_State_Shape (1)/Shape/NHDFlowline_0.shp"))
test <- st_zm(water_line)
test_1 <- test %>%
  filter(visibility == 5000000) %>%
  filter(ftype == 460)
#plot(st_geometry(water_line))
#plot(st_geometry(test_1)) # I think I want this visibility filter
test_1_proj <- st_transform(test_1, projection)

test_2 <- test %>%
  filter(visibility == 1000000)
plot(st_geometry(test_2))

water_line_proj <- st_transform(water_line, projection)
plot(st_geometry(water_line_proj))

# could I get area water for each pixel using tigris?
library(tigris)

boise_water <- area_water("ID", "Ada County")

plot(boise_water$geometry)

# I think this is what I want
# using the rivers and streams that 


# Rasterize the power plant shapefile
test1_rast <- rasterize(vect(test_1_proj), ref_rast)

# Calculate the distance from power plants
test1_dist <- terra::distance(test1_rast)
plot(test1_dist)

test1_dist_crop <- crop(test1_dist, ref_rast, mask = TRUE)
plot(test1_dist_crop)
nrow(as.data.frame(test1_dist_crop))
nrow(as.data.frame(ref_rast))

# Convert to km
test1_dist_crop$river_dist_km <- test1_dist_crop$layer / 1000
plot(test1_dist_crop$river_dist_km)
