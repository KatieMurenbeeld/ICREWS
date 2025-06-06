library(stringr)
library(sf)
library(terra)
library(tidyverse)
library(haven)
library(tigris)
library(readxl)
library(spdep)
library(gstat)
library(stars)

# Set timeout to 10 minutes
options(timeout=6000)


# Use API to download data from the Idaho Department of Water Resouces website
# https://data-idwr.hub.arcgis.com/datasets/wild-and-scenic-watersheds-1/about

# Load the shape file
wildscenic <- read_sf(here::here("data/original/water/Wild_And_Scenic_Watersheds/Wild_And_Scenic_Watersheds.shp")) 

# Set the projection
projection <- "epsg:5070"

## check for validity, remove empty geometries, and reproject 
if (!all(st_is_valid(wildscenic)))
  wild <- st_make_valid(wildscenic)

wildscenic <- wildscenic %>%
  filter(!st_is_empty(.))

wildscenic_proj <- wildscenic %>%
  st_transform(projection)
#plot(wildscenic_proj$geometry)

# download the idaho state boundary and create a tempalte raster
## Load county boundaries from tigris
### Set the year to account for changes to FIPS codes
counties <- tigris::counties() 
### Filter for Idaho (FIPS = 16)
counties <- counties %>%
  filter(STATEFP == 16) %>%
  dplyr::select(GEOID, geometry)

counties <- st_transform(counties, projection)

# use the Wildfire data as the reference raster
ref_rast <- rast(here::here("data/processed/wfrc_BP_ID_3km_2024-12-03.tif"))

# Rasterize the critical habitat areas shapefile
wildscenic_rast <- rasterize(vect(wildscenic_proj), ref_rast)

# Calculate the distance from critical habitat areas
wildscenic_dist_rast <- terra::distance(wildscenic_rast)

# Crop to the reference raster and update variable names
wildscenic_dist_crop <- crop(wildscenic_dist_rast, ref_rast, mask = TRUE)
plot(wildscenic_dist_crop)
names(wildscenic_dist_crop) <- "distance_to_wildscenic_m"
nrow(as.data.frame(wildscenic_dist_crop))
nrow(as.data.frame(ref_rast))

# Save the raster
writeRaster(wildscenic_dist_crop, here::here(paste0("data/processed/wildscenic_dist_id_3km_pred_crop_", 
                                              Sys.Date(), ".tif")), overwrite = TRUE)
