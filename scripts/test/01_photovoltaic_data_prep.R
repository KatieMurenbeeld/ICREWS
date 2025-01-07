library(sf)
library(terra)
library(raster)
library(tidyverse)
library(tigris)

# Set timeout to 10 minutes
options(timeout=6000)

# Create a new subdirectory for the data in the data/original directory
data_dir <- here::here("data/original/")
sub_dir <- "uspvdb"

# check if sub directory exists 
if (file.exists(sub_dir)){
  
} else {
  
  # create a new sub directory inside
  # the data directory
  dir.create(file.path(data_dir, sub_dir))
}

# download and unzip the data into the new subdirectory (for next time)
# update this to detect .zip in the file name as well
download_data <- function(url, sub_dir) {
  if (str_detect(url, ".zip")) {
    tmp <- tempfile()
    download.file(url, tmp)
    unzip(zipfile = tmp, exdir = here::here(paste0("data/original/", sub_dir)))
  } else if (str_detect(file_name, ".zip")) {
    download.file(url, here::here(paste0("data/original/", sub_dir)))
    unzip(zipfile = here::here(paste0("data/original/", sub_dir)))
  }
  else {
    download.file(url, here::here(paste0("data/original/", sub_dir)))
  }
}

# United States Photovoltaic Database (solar)
url <- "https://eerscmap.usgs.gov/uspvdb/assets/data/uspvdbSHP.zip"
file_name <- sub_dir

# Download and unzip the data
download_data(url, file_name)

# Load the shapefiles
uspv <- read_sf(here::here("data/original/uspvdb/uspvdb_v2_0_20240801.shp"))

# project all to projection = "epsg:5070"
projection = "epsg:5070"

uspv <- st_transform(uspv, projection)

# filter for states bordering Idaho
nw_pv <- uspv %>%
  filter(p_state %in% c("ID", "WA", "MT", "OR", "NV", "UT", "WY", "CA"))

# calculate the distance from any power plant for Idaho

## get the state boundaries
states <- tigris::states()

## reproject
states_proj <- st_transform(states, crs = crs(uspv))

## get the idaho boundary
id_bdry <- states_proj %>%
  filter(STUSPS == "ID")

## get the boundary for states neighboring Idaho 
## we will use the NW states to calculate the distance to energy sources
nw_states <- states_proj %>%
  filter(STUSPS %in% c("ID", "WA", "MT", "OR", "NV", "UT", "WY", "CA"))

# Create a template raster for the shapefiles
XMIN <- ext(nw_states)$xmin
XMAX <- ext(nw_states)$xmax
YMIN <- ext(nw_states)$ymin
YMAX <- ext(nw_states)$ymax
aspectRatio <- (YMAX-YMIN)/(XMAX-XMIN)
cellSize <- 3000
NCOLS <- as.integer((XMAX-XMIN)/cellSize)
NROWS <- as.integer(NCOLS * aspectRatio)
templateRas <- rast(ncol=NCOLS, nrow=NROWS, 
                    xmin=XMIN, xmax=XMAX, ymin=YMIN, ymax=YMAX,
                    vals=1, crs=crs(nw_states))

# Rasterize the power plant shapefile
nw_pv_rast <- rasterize(vect(nw_pv), templateRas)

# Calculate the distance from power plants
nwpv_dist_rast <- terra::distance(nw_pv_rast)

# Convert to km
nwpv_dist_rast$dist_to_photovoltaic_km <- nwpv_dist_rast$layer / 1000

# Resample to Idaho using the wildfire raster as a reference raster
ref_rast <- rast(here::here("data/processed/wfrc_BP_ID_3km_2024-12-03.tif"))

id_pv_dist_rast <- resample(nwpv_dist_rast, ref_rast)
plot(id_pv_dist_rast$dist_to_photovoltaic_km)

# Crop to Idaho using the reference raster
id_pv_dist_crop <- crop(id_pv_dist_rast, ref_rast, mask = TRUE)
plot(id_pv_dist_crop$dist_to_photovoltaic_km)
nrow(as.data.frame(id_pv_dist_crop$dist_to_photovoltaic_km))
nrow(as.data.frame(ref_rast))

# Save the raster
writeRaster(id_pv_dist_crop$dist_to_photovoltaic_km, here::here(paste0("data/processed/dist_to_photovoltaic_id_3km_pred_crop_", 
                                         Sys.Date(), ".tif")), overwrite = TRUE)


