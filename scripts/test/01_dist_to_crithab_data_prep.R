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

# Create a new subdirectory for the data in the data/original directory
data_dir <- here::here("data/original/")
sub_dir <- "usfs"

# check if sub directory exists 
if (file.exists(sub_dir)){
  
} else {
  
  # create a new sub directory inside
  # the data directory
  dir.create(file.path(data_dir, sub_dir))
}

# update this to detect .zip in the file name as well
download_data <- function(url, file_name) {
  if (str_detect(url, ".zip")) {
    tmp <- tempfile()
    download.file(url, tmp)
    unzip(zipfile = tmp, exdir = here::here("data/original/"))
  } else if (str_detect(file_name, ".zip")) {
    download.file(url, here::here(paste0("data/original/", file_name)))
    unzip(zipfile = here::here(paste0("data/original/", file_name)))
  }
  else {
    download.file(url, here::here(paste0("data/original/", file_name)))
  }
}

# FWS Critical Habitat
url <- "https://ecos.fws.gov/docs/crithab/crithab_all/crithab_all_layers.zip"
file_name <- ""

# Still need to fix so that it unzips to the new subdirectory
download_data(url, file_name)
#unzip(here::here("data/original/usa.zip"), exdir = here::here("data/original/cejst/"))

# Load the shape file
crithab <- read_sf(here::here("data/original/usfs/crithab_poly.shp")) 
  
# Set the projection
projection <- "epsg:5070"

## check for validity, remove empty geometries, and reproject 
if (!all(st_is_valid(crithab)))
  crithab <- st_make_valid(crithab)

crithab <- crithab %>%
  filter(!st_is_empty(.))

crithab_proj <- crithab %>%
  st_transform(projection)

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
crithab_rast <- rasterize(vect(crithab_proj), ref_rast)

# Calculate the distance from critical habitat areas
crithab_dist_rast <- terra::distance(crithab_rast)

# Crop to the reference raster and update variable names
crithab_dist_crop <- crop(crithab_dist_rast, ref_rast, mask = TRUE)
plot(crithab_dist_crop)
names(crithab_dist_crop) <- "distance_to_crithab_m"
nrow(as.data.frame(crithab_dist_crop))
nrow(as.data.frame(ref_rast))

# Save the raster
writeRaster(crithab_dist_crop, here::here(paste0("data/processed/crithab_dist_id_3km_pred_crop_", 
                                         Sys.Date(), ".tif")), overwrite = TRUE)
