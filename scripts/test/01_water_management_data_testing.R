library(sf)
library(terra)
library(raster)
library(tidyverse)
library(tigris)

# Set timeout to 10 minutes
options(timeout=6000)

# Create a new subdirectory for the data in the data/original directory
data_dir <- here::here("data/original/water")
sub_dir <- "management"

# check if sub directory exists 
if (file.exists(sub_dir)){
  
} else {
  
  # create a new sub directory inside
  # the data directory
  dir.create(file.path(data_dir, sub_dir))
}

# project all to projection = "epsg:5070"
projection = "epsg:5070"

# Unzip the data into the new subdirectory 
unzip(here::here("data/original/water/management/municipal_service_area_boundaries.zip"), exdir = here::here("data/original/water/management/"))

# Load the shapefiles
msab <- read_sf(here::here("data/original/water/management/municipal.shp"))

msab_proj <- st_transform(msab, projection)
plot(msab_proj$geometry)



