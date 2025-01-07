library(tidyverse)
library(sf)
library(terra)
library(raster)
library(ncdf4)
library(tigris)
library(stringr)
library(RCurl)
library(stars)
library(spdep)
library(gstat)

# read in the SWE data as a raster (annual maximum swe)

swe_rst <- raster(here::here("data/original/clim_lab/annual_maximum_swe_CTRL.nc"))
swe_rst <- as(swe_rst, "SpatRaster")
plot(swe_rst)

# reproject, aggregate, and crop to Idaho
# Set the projection
projection <- "epsg:5070"
swe_proj <- project(swe_rst, projection)

# Resample to Idaho using the wildfire raster as a reference raster
ref_rast <- rast(here::here("data/processed/wfrc_BP_ID_3km_2024-12-03.tif"))
swe_rst_resamp <- resample(swe_proj, ref_rast)
swe_crop <- crop(swe_rst_resamp, ref_rast, mask = TRUE)
plot(swe_crop)
nrow(as.data.frame(swe_crop))

# save the raster file
writeRaster(swe_crop, here::here(paste0("data/processed/swe_ann_max_id_3km_crop_", 
                                         Sys.Date(), ".tif")), overwrite = TRUE)
