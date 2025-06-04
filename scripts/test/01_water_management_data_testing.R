library(sf)
library(terra)
library(raster)
library(tidyverse)
library(ggplot2)
library(tmap)
library(tigris)
library(spdep)
library(mapview)
library(here)

# Set timeout to 10 minutes
options(timeout=6000)

# Set the projection
projection <- "epsg:5070"

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

#----Load the HarDWR (harmonized database water rights) for Idaho----
# See Lisk et al., 2024 Harmonized Database of Western U.S. Water Rights


# Unzip files 
unzip(here::here("data/original/water/management/municipal_service_area_boundaries.zip"), exdir = here::here("data/original/water/management/"))
unzip("~/Analysis/ICREWS/test_project/data/original/HarDWR/stateWMAs.zip", exdir = here::here("data/original/HarDWR/"))
unzip("~/Analysis/ICREWS/test_project/data/original/HarDWR/IdahoRights.zip", exdir = here::here("data/original/HarDWR/"))
unzip("~/Analysis/ICREWS/test_project/data/original/HarDWR/h79e1-k3h91.zip", exdir = here::here("data/original/HarDWR/"))
unzip("~/Analysis/ICREWS/test_project/data/original/HarDWR/h79e1-k3h91/stateWaterRightsHarmonized_RData.zip", 
      exdir = here::here("data/original/HarDWR/h79e1-k3h91/"))
unzip("~/Analysis/ICREWS/test_project/data/original/HarDWR/h79e1-k3h91/stateWaterRightsHarmonized.zip", 
      exdir = here::here("data/original/HarDWR/h79e1-k3h91/"))


# explore some of the harmonized Idaho data
id_full <- read_csv(here::here("data/original/HarDWR/h79e1-k3h91/stateWaterRightsHarmonized/idaho/idFullHarmonizedRights.csv"))
head(id_full)
tail(id_full)
# Some of the priority dates don't look like they are in the correct format

# Load the shapefiles
id_pod <- read_sf(here::here("data/original/HarDWR/IdahoRights_PointOfDiversion.shp"))
id_pou <- read_sf(here::here("data/original/HarDWR/h79e1-k3h91/stateWaterRightsHarmonized/idaho/idStatePOU.shp"))
wma <- read_sf(here::here("data/original/HarDWR/stateWMAs.shp"))

# Load the reference raster and reproject
ref_rast <- rast(here::here("data/processed/wfrc_BP_ID_3km_2024-12-03.tif"))
ref_rast_proj <- project(ref_rast, projection)

# reproject 
id_pod_proj <- st_transform(id_pod, projection)
id_pou_proj <- st_transform(id_pou, projection)
wma_proj <- st_transform(wma, projection)

# create a new category of source type, basically combining all the surface water types into one type
id_pod_proj <- id_pod_proj %>%  
  mutate(source_type = case_when(
    grepl("STREAM", Source) ~ "SURFACE",
    grepl("CREEK", Source) ~ "SURFACE",
    grepl("SLOUGH", Source) ~ "SURFACE",
    grepl("RIVER", Source) ~ "SURFACE",
    grepl("FORK", Source) ~ "SURFACE",
    grepl("RUN", Source) ~ "SURFACE",
    grepl("GULCH", Source) ~ "SURFACE",
    grepl("CANYON", Source) ~ "SURFACE",
    grepl("POND", Source) ~ "SURFACE",
    grepl("LAKE", Source) ~ "SURFACE",
    grepl("DITCH", Source) ~ "SURFACE",
    grepl("WASTE", Source) ~ "WASTE",
    grepl("DRAIN", Source) ~ "DRAIN",
    grepl("GROUND", Source) ~ "GROUND",
    grepl("SPRING", Source) ~ "SPRING"))

# just want the count per pixel (3km resolution)
id_pod_sw <- id_pod_proj %>%
  filter(source_type == "SURFACE")
id_pod_sw_rst <- rasterize(id_pod_sw, ref_rast_proj, fun="count", background=0)
id_pod_sw_crop <- crop(id_pod_sw_rst, ref_rast_proj, mask = TRUE)
plot(id_pod_sw_crop)
# check that the rasters have the same number of cells
#nrow(as.data.frame(ref_rast)) 
#nrow(as.data.frame(id_pod_sw_crop))

id_pod_gw <- id_pod_proj %>%
  filter(source_type == "GROUND")
id_pod_gw_rst <- rasterize(id_pod_gw, ref_rast_proj, fun="count", background=0)
id_pod_gw_crop <- crop(id_pod_gw_rst, ref_rast_proj, mask = TRUE)
plot(id_pod_gw_crop)
# check that the rasters have the same number of cells
#nrow(as.data.frame(id_pod_gw_crop))

# Save the rasters
writeRaster(id_pod_sw_crop, here::here(paste0("data/processed/waterright_sw_pod_count_id_3km_pred_crop_", 
                                         Sys.Date(), ".tif")), overwrite = TRUE)

writeRaster(id_pod_gw_crop, here::here(paste0("data/processed/waterright_gw_pod_count_id_3km_pred_crop_", 
                                              Sys.Date(), ".tif")), overwrite = TRUE)


