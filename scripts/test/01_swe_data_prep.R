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

grd <- st_as_stars(templateRas)

swe_rst_resamp <- resample(swe_proj, templateRas)
swe_crop <- crop(swe_rst_resamp, counties, mask = TRUE)
plot(swe_crop)

# save the raster file
writeRaster(swe_crop, here::here(paste0("data/processed/swe_ann_max_id_3km_crop_", 
                                         Sys.Date(), ".tif")), overwrite = TRUE)
