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

# Create a new subdirectory for the data in the data/original directory
data_dir <- here::here("data/original/")
sub_dir <- "usfs"

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

# National Wilderness Areas
url <- "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.Wilderness.zip"
file_name = ""

# Still need to fix so that it unzips to the new subdirectory
download_data(url, file_name)
#unzip(here::here("data/original/usa.zip"), exdir = here::here("data/original/cejst/"))

# Load the shape file
wild <- read_sf(here::here("data/original/usfs/S_USA.Wilderness.shp")) 

# Set the projection
projection <- "epsg:5070"

## check for validity, remove empty geometries, and reproject 
if (!all(st_is_valid(wild)))
  wild <- st_make_valid(wild)

wild <- wild %>%
  filter(!st_is_empty(.))

wild_proj <- wild %>%
  st_transform(projection)

# download the idaho state boundary and create a tempalte raster
## Load county boundaries from tigris
### Set the year to account for changes to FIPS codes
counties <- tigris::counties() 
### Filter for Idaho (FIPS = 16)
counties <- counties %>%
  filter(STATEFP == 16) %>%
  dplyr::select(GEOID, geometry)

counties <- st_transform(counties, projection)

## Create a template raster for the shapefiles
XMIN <- ext(counties)$xmin
XMAX <- ext(counties)$xmax
YMIN <- ext(counties)$ymin
YMAX <- ext(counties)$ymax
aspectRatio <- (YMAX-YMIN)/(XMAX-XMIN)
cellSize <- 3000
NCOLS <- as.integer((XMAX-XMIN)/cellSize)
NROWS <- as.integer(NCOLS * aspectRatio)
templateRas <- rast(ncol=NCOLS, nrow=NROWS, 
                    xmin=XMIN, xmax=XMAX, ymin=YMIN, ymax=YMAX,
                    vals=1, crs=crs(counties))

# Rasterize the critical habitat areas shapefile
wild_rast <- rasterize(vect(wild_proj), templateRas)

# Calculate the distance from critical habitat areas
wild_dist_rast <- terra::distance(wild_rast)

# Crop to the reference raster and update variable names
wild_dist_crop <- crop(wild_dist_rast, counties, mask = TRUE)
plot(wild_dist_crop)
names(wild_dist_crop) <- "distance_to_wilderness_m"

# Save the raster
writeRaster(wild_dist_crop, here::here(paste0("data/processed/wildarea_dist_id_3km_pred_crop_", 
                                                 Sys.Date(), ".tif")), overwrite = TRUE)
