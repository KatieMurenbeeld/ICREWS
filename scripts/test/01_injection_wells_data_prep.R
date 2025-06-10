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
sub_dir <- "injection_wells"

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

# IDWR Injection Wells Dataset
url <- "https://data-idwr.hub.arcgis.com/maps/9167ab2d41334b1d8d437e126fc10c1f" 
file_name <- ""

# Still need to fix so that it unzips to the new subdirectory
#download_data(url, file_name)
unzip(here::here("data/original/water/InjectionWells.zip"), exdir = here::here("data/original/water/injection_wells"))

# Load the shapefiles
id_inj_wells <- read_sf(here::here("data/original/water/injection_wells/InjectionWells.shp"))
id_inj_wells_proj <- st_transform(id_inj_wells, projection)


# Load the reference raster and reproject
ref_rast <- rast(here::here("data/processed/wfrc_BP_ID_3km_2024-12-03.tif"))
ref_rast_proj <- project(ref_rast, projection)



id_inj_wells_rst <- rasterize(id_inj_wells_proj, ref_rast_proj, fun="count", field = "UICNumber", background=0)
plot(id_inj_wells_rst)
id_inj_wells_rst_crop <- crop(id_inj_wells_rst, ref_rast_proj, mask = TRUE)
plot(id_inj_wells_rst_crop)

# just look at wells with >0 production
id_inj_wells_active <- id_inj_wells_proj %>%
  filter(StatusDesc == "Active")
id_inj_wells_act_rst <- rasterize(id_inj_wells_active, ref_rast_proj, fun="count", field = "WellID", background=0)
id_inj_wells_act_rst_crop <- crop(id_inj_wells_act_rst, ref_rast_proj, mask = TRUE)
plot(id_inj_wells_act_rst_crop)

nrow(as.data.frame(ref_rast_proj))
nrow(as.data.frame(id_inj_wells_rst_crop))
nrow(as.data.frame(id_inj_wells_act_rst_crop))

length(unique(id_inj_wells_proj$Facility))
length(unique(id_inj_wells_active$Facility))

# Save the rasters
writeRaster(id_inj_wells_rst_crop, here::here(paste0("data/processed/idwr_inject_wells_all_count_id_3km_crop_", 
                                              Sys.Date(), ".tif")), overwrite = TRUE)

writeRaster(id_inj_wells_act_rst_crop, here::here(paste0("data/processed/idwr_inject_wells_active_count_id_3km_crop_", 
                                              Sys.Date(), ".tif")), overwrite = TRUE)
