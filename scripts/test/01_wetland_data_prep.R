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
sub_dir <- "water"

# check if sub directory exists 
if (file.exists(sub_dir)){
  
} else {
  
  # create a new sub directory inside
  # the data directory
  dir.create(file.path(data_dir, sub_dir))
}

# Download wetland geodatabase from FWS
# https://documentst.ecosphere.fws.gov/wetlands/data/State-Downloads/ID_geodatabase_wetlands.zip
# https://documentst.ecosphere.fws.gov/wetlands/data/State-Downloads/ID_geopackage_wetlands.zip

# Unzip the data into the new subdirectory 
unzip(here::here("data/original/water/ID_geopackage_wetlands.zip"), exdir = here::here("data/original/water/"))

# Load the shapefiles
#wetland_test <- st_read(here::here("data/original/water/ID_geodatabase_wetlands.gdb"))
wetlan_test <- st_read(here::here("data/original/water/ID_Wetlands_Geopackage.gpkg"))

# project all to projection = "epsg:5070"
projection = "epsg:5070"

plot(wetlan_test$Shape)
