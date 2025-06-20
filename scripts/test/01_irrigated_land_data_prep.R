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
#sub_dir <- "usgs_irrigated_lands"
sub_dir <- "irrigated_lands"

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
#url <- "https://www.sciencebase.gov/catalog/item/5ece996d82ce30fd9808527b" 
#file_name <- ""

# Still need to fix so that it unzips to the new subdirectory
#download_data(url, file_name)
unzip(here::here("data/original/water/usgs_irrigated_lands/Verified_Irr_Ag_2002-17.gdb.zip"), exdir = here::here("data/original/water/usgs_irrigated_lands/"))
unzip(here::here("data/original/water/irrigated_lands/mirad1km_12v4 (All).zip"), exdir = here::here("data/original/water/irrigated_lands/"))

# Load the reference raster and reproject
ref_rast <- rast(here::here("data/processed/wfrc_BP_ID_3km_2024-12-03.tif"))

# read in the geodatabase
irr_st <- st_read(here::here("data/original/water/usgs_irrigated_lands/Verified_Irr_Ag_2002-17.gdb"), promote_to_multi = FALSE)
irr_proj <- st_transform(irr_st, projection)
plot(st_geometry(irr_proj))


irr_test <- st_cast(irr_proj, "MULTIPOLYGON") %>% st_cast("POLYGON")

# need to crop to Idaho
## get the state boundaries
states <- tigris::states()

## reproject
states_proj <- st_transform(states, projection)

## get the idaho boundary
id_bdry <- states_proj %>%
  filter(STUSPS == "ID")
plot(st_geometry(id_bdry))
plot(st_geometry(irr_proj))

irr_id <- st_crop(irr_proj, id_bdry)
plot(st_geometry(irr_id))

## rasterize?
irr_rst <- rasterize(irr_proj, ref_rast, field = "IRG_STATUS", background=0)
plot(irr_rst)



## idwr
# links: Mountain Home 2023: https://research.idwr.idaho.gov/GIS/Spatial/LandCover_Vegetation/MountainHome/RF_IrrigatedLands_2023_MH.zip
# Raft River 2022: https://research.idwr.idaho.gov/GIS/Spatial/LandCover_Vegetation/RaftRiverValley/RF_IrrigatedLands_2022_RRV.zip
# ESRP 2022: https://research.idwr.idaho.gov/GIS/Spatial/LandCover_Vegetation/SnakePlain/RF_IrrigatedLands_2022_ESPA.zip
# Bruneau Grandview 2007: https://research.idwr.idaho.gov/GIS/Spatial/LandCover_Vegetation/Bruneau-Grandview/RF_IrrigatedLands_2007_BG.zip
## nass: Moderate Resolution Imaging Spectroradiometer (MODIS) Irrigated Agriculture Datasets for the Conterminous United States (MIrAD-US)
# https://www.sciencebase.gov/catalog/item/5db08e84e4b0b0c58b56e04f

# Percent area that is irrigated land 
irr_rast <- rast(here::here("data/original/water/irrigated_lands/mirad1km_17v4/mirad1km_17v4.tif"))
plot(irr_rast)

irr_rst_proj <- project(irr_rast, ref_rast)
plot(irr_rst_proj)
irr_rst_crop <- crop(irr_rst_proj, ref_rast, mask = TRUE)
plot(irr_rst_crop)
nrow(as.data.frame(irr_rst_crop))
nrow(as.data.frame(ref_rast))

# save the Raster
writeRaster(irr_rst_crop, here::here(paste0("data/processed/irrigated_lands_pct_id_3km_crop_", 
                                                  Sys.Date(), ".tif")), overwrite = TRUE)
