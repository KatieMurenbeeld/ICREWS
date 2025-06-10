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
sub_dir <- "wells"

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

# IDWR Wells Dataset
url <- "https://data-idwr.hub.arcgis.com/maps/9167ab2d41334b1d8d437e126fc10c1f" 
file_name <- ""

# Still need to fix so that it unzips to the new subdirectory
#download_data(url, file_name)
unzip(here::here("data/original/water/Wells.zip"), exdir = here::here("data/original/water/wells"))

# Load the shapefiles
id_wells <- read_sf(here::here("data/original/water/wells/Wells.shp"))
id_wells_proj <- st_transform(id_wells, projection)


# Load the reference raster and reproject
ref_rast <- rast(here::here("data/processed/wfrc_BP_ID_3km_2024-12-03.tif"))
ref_rast_proj <- project(ref_rast, projection)



id_wells_rst <- rasterize(id_wells_proj, ref_rast_proj, fun="count", field = "WellID", background=0)
plot(id_wells_rst)
id_wells_rst_crop <- crop(id_wells_rst, ref_rast_proj, mask = TRUE)
plot(id_wells_rst_crop)

# just look at wells with >0 production
id_wells_productive <- id_wells_proj %>%
  filter(Production > 0)
id_wells_prod_rst <- rasterize(id_wells_productive, ref_rast_proj, fun="count", field = "WellID", background=0)
id_wells_prod_rst_crop <- crop(id_wells_prod_rst, ref_rast_proj, mask = TRUE)
plot(id_wells_prod_rst_crop)

# wells with >0 production sum by production
id_wells_productive <- id_wells_proj %>%
  filter(Production > 0)
id_wells_prod_sum_rst <- rasterize(id_wells_productive, ref_rast_proj, fun="sum", field = "Production", background=0)
id_wells_prod_sum_rst_crop <- crop(id_wells_prod_sum_rst, ref_rast_proj, mask = TRUE)
plot(id_wells_prod_sum_rst_crop)

nrow(as.data.frame(ref_rast_proj))
nrow(as.data.frame(id_wells_rst_crop))
nrow(as.data.frame(id_wells_prod_rst_crop))
nrow(as.data.frame(id_wells_prod_sum_rst_crop))

length(unique(id_wells_proj$Owner))
length(unique(id_wells_productive$Owner))

# Save the rasters
writeRaster(id_wells_rst_crop, here::here(paste0("data/processed/idwr_wells_all_count_id_3km_crop_", 
                                              Sys.Date(), ".tif")), overwrite = TRUE)

writeRaster(id_wells_prod_rst_crop, here::here(paste0("data/processed/idwr_wells_productive_count_id_3km_crop_", 
                                              Sys.Date(), ".tif")), overwrite = TRUE)

writeRaster(id_wells_prod_sum_rst_crop, here::here(paste0("data/processed/idwr_wells_production_sum_id_3km_crop_", 
                                                      Sys.Date(), ".tif")), overwrite = TRUE)





