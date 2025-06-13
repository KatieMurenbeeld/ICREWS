library(sf)
library(terra)
library(raster)
library(tidyverse)
library(tigris)

# Set timeout to 10 minutes
options(timeout=6000)

# Create a new subdirectory for the data in the data/original directory
data_dir <- here::here("data/original/")
sub_dir <- "power_plant"

# check if sub directory exists 
if (file.exists(sub_dir)){
  
} else {
  
  # create a new sub directory inside
  # the data directory
  dir.create(file.path(data_dir, sub_dir))
}

# You have to download the data from the EIA mapviewer 
# https://atlas.eia.gov/datasets/eia::power-plants/explore

# Unzip the data into the new subdirectory 
unzip(here::here("data/original/power_plant/Power_Plants.zip"), exdir = here::here("data/original/power_plant/"))

# Load the shapefiles
pplant <- read_sf(here::here("data/original/power_plant/Power_Plants.shp"))

# project all to projection = "epsg:5070"
projection = "epsg:5070"

pplant <- st_transform(pplant, projection)

# filter for states bordering Idaho
nw_pp <- pplant %>%
  filter(State %in% c("Idaho", "Washington", "Montana", 
                      "Oregon", "Nevada", "Utah", 
                      "Wyoming", "California"))

id_pp <- pplant %>%
  filter(State == "Idaho")
plot(st_geometry(id_pp))
# filter to exclude wind and solar
## Note: could update this list for specific plant types

nw_pp <- nw_pp %>%
  filter(PrimSource %in% c("petroleum", "hydroelectirc", "natural gas", 
                           "nuclear", "coal", "pumped storage", "geothermal", 
                           "biomass", "batteries", "other"))

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
nw_pp_rast <- rasterize(vect(nw_pp), templateRas)

# Calculate the distance from power plants
nwpp_dist_rast <- terra::distance(nw_pp_rast)

# Convert to km
nwpp_dist_rast$dist_to_powerplant_km <- nwpp_dist_rast$last / 1000

# Resample to Idaho using the wildfire raster as a reference raster
ref_rast <- rast(here::here("data/processed/wfrc_BP_ID_3km_2024-12-03.tif"))
id_pp_dist_rast <- resample(nwpp_dist_rast, ref_rast)

# Crop to Idaho
id_pp_dist_crop <- crop(id_pp_dist_rast, ref_rast, mask = TRUE)
plot(id_pp_dist_crop$dist_to_powerplant_km)
nrow(as.data.frame(id_pp_dist_crop$dist_to_powerplant_km))

# Save the raster
writeRaster(id_pp_dist_crop$dist_to_powerplant_km, here::here(paste0("data/processed/dist_to_powerplant_id_3km_pred_crop_", 
                                         Sys.Date(), ".tif")), overwrite = TRUE)


id_pp_count_rst <- rasterize(id_pp, ref_rast, fun="count", field = "OBJECTID", background=0)
id_pp_crop <- crop(id_pp_count_rst, ref_rast, mask = TRUE)
plot(id_pp_crop)
