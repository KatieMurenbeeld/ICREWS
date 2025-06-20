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

# Set the projection
projection <- "epsg:5070"

# Create a new subdirectory for the data in the data/original directory
data_dir <- here::here("data/original/water/")
sub_dir <- "geotherm_wells"

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

# IDWR Geothermal Wells Dataset
#url <- "https://data-idwr.hub.arcgis.com/datasets/IDWR::geothermal-wells/explore?location=44.137418%2C-114.005077%2C6.19" 
#file_name <- ""

# Still need to fix so that it unzips to the new subdirectory
#download_data(url, file_name)
unzip(here::here("data/original/water/Geothermal_Wells.zip"), exdir = here::here("data/original/water/geotherm_wells"))

# Load the shapefiles
geo_wells <- read_sf(here::here("data/original/water/geotherm_wells/Geothermal_Wells.shp"))
geo_wells_proj <- st_transform(geo_wells, projection)


# Load the reference raster and reproject
ref_rast <- rast(here::here("data/processed/wfrc_BP_ID_3km_2024-12-03.tif"))

geo_wells_rst <- rasterize(geo_wells_proj, ref_rast, fun="count", field = "WellID", background=0)
#plot(geo_wells_rst)
geo_wells_rst_crop <- crop(geo_wells_rst, ref_rast, mask = TRUE)
plot(geo_wells_rst_crop)


# Rasterize the critical habitat areas shapefile
geo_rast <- rasterize(vect(geo_wells_proj), ref_rast)

# Calculate the distance from critical habitat areas
geo_dist_rast <- terra::distance(geo_rast)

# Crop to the reference raster and update variable names
geo_dist_crop <- crop(geo_dist_rast / 1000, ref_rast, mask = TRUE)
plot(geo_dist_crop)


# save the rasters
writeRaster(geo_wells_rst_crop, here::here(paste0("data/processed/idwr_geo_wells_all_count_id_3km_crop_", 
                                                 Sys.Date(), ".tif")), overwrite = TRUE)

writeRaster(geo_dist_crop, here::here(paste0("data/processed/idwr_geo_wells_dist_id_3km_crop_", 
                                                      Sys.Date(), ".tif")), overwrite = TRUE)


