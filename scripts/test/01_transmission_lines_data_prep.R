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

# Download transmission line data from EIA
# https://atlas.eia.gov/search?q=transmission

# Unzip the data into the new subdirectory 
unzip(here::here("data/original/infrastructure/Electric__Power_Transmission_Lines.zip"), exdir = here::here("data/original/infrastructure/"))

# Load the shapefiles
trans_lines <- read_sf(here::here("data/original/infrastructure/Electric__Power_Transmission_Lines.shp"))

# project all to projection = "epsg:5070"
projection = "epsg:5070"

trans_lines <- st_transform(trans_lines, projection)

# calculate the distance from any transmission line for Idaho

## get the state boundaries
states <- tigris::states()

## reproject
states_proj <- st_transform(states, crs = projection)

## get the idaho boundary
id_bdry <- states_proj %>%
  filter(STUSPS == "ID")

## get the boundary for states neighboring Idaho 
## we will use the NW states to calculate the distance to energy sources
nw_states <- states_proj %>%
  filter(STUSPS %in% c("ID", "WA", "MT", "OR", "NV", "UT", "WY", "CA"))

# I need where the rail and Idaho counties intersect
nw_translines <- st_intersection(trans_lines, nw_states) 

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
nw_transline_rast <- rasterize(vect(nw_translines), templateRas)

# Calculate the distance from power plants
nwtl_dist_rast <- terra::distance(nw_transline_rast)

# Convert to km
nwtl_dist_rast$dist_to_transline_km <- nwtl_dist_rast$layer / 1000

# Resample to Idaho using the wildfire raster as a reference raster
ref_rast <- rast(here::here("data/processed/wfrc_BP_ID_3km_2024-12-03.tif"))
id_tl_dist_rast <- resample(nwtl_dist_rast, ref_rast)


# Crop to Idaho
id_tl_dist_crop <- crop(id_tl_dist_rast, ref_rast, mask = TRUE)
plot(id_tl_dist_crop$dist_to_transline_km)
nrow(as.data.frame(id_tl_dist_crop$dist_to_transline_km))

# Save the raster
writeRaster(id_tl_dist_crop$dist_to_transline_km, here::here(paste0("data/processed/dist_to_transline_id_3km_pred_crop_", 
                                                                     Sys.Date(), ".tif")), overwrite = TRUE)
