library(tidyverse)
library(sf)
library(terra)
library(tigris)
library(stringr)
library(RCurl)
library(stars)
library(spdep)
library(gstat)

# Set timeout to 10 minutes
options(timeout=6000)

# Create a new subdirectory for the data in the data/original directory
data_dir <- here::here("data/original/")
sub_dir <- "infrastructure"

# check if sub directory exists 
if (file.exists(sub_dir)){
  
} else {
  
  # create a new sub directory inside
  # the data directory
  dir.create(file.path(data_dir, sub_dir))
}

# Download the fractional impervious from the National Land Cover Dataset
# 

# Load the shapefiles
imp_rst <- rast(here::here("data/original/infrastructure/Annual_NLCD_FctImp_2023_CU_C1V0.tif"))

# Project all to projection = "epsg:5070"
projection = "epsg:5070"
imp_rst_proj <- project(imp_rst, projection)

# Resample to the reference raster
ref_rast <- rast(here::here("data/processed/wfrc_BP_ID_3km_2024-12-03.tif"))

imp_rst_resamp <- resample(imp_rst, ref_rast)
imp_rst_proj <- project(imp_rst_resamp, "epsg:5070")

# Crop to Idaho
imp_rst_crop <- crop(imp_rst_proj, ref_rast, mask = TRUE)
plot(imp_rst_crop)
nrow(as.data.frame(imp_rst_crop))

# Save the raster
writeRaster(imp_rst_crop, here::here(paste0("data/processed/fraction_impervious_id_3km_pred_crop_", 
                                                                    Sys.Date(), ".tif")), overwrite = TRUE)


