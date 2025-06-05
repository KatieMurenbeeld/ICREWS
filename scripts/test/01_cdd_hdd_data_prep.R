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

# read in the max and min daily temperatures from GRIDMET
# Calculate the heating degree days (hdd) and the cooling degree days (cdd)
# See equations from https://www.weather.gov/key/climate_heat_cool
# If (max t + min t) / 2 > 65
# then calculate cdd
# mean - 65 = cdd
# If (max t + min t) / 2 < 65
# then calculate hdd
# mean - 29 = hdd

# set the projection and bring in a reference raster
projection <- "epsg:5070"
ref_rast <- rast(here::here("data/processed/wfrc_BP_ID_3km_2024-12-03.tif"))

# load the max and min daily temp
# first review the metadata and the dates from netcdf
# it is easier to figure out the correct slice when the days are converted to dates
nc_tmin <- nc_open(here::here("data/original/clim_lab/gridMET/agg_met_tmmn_1979_CurrentYear_CONUS.nc"))
# Save the print(nc) dump to a text file
{
  sink(here::here("data/original/clim_lab/gridMET/agg_met_tmmn_1979_CurrentYear_CONUS.txt"))
  print(nc_tmin)
  sink()
}
lon <- ncvar_get(nc_tmin, "lon")
lat <- ncvar_get(nc_tmin, "lat", verbose = F)
t <- ncvar_get(nc_tmin, "day")
t <- as.Date(t, origin = "1900-01-01")
tail(t)

# get the index for 12-31-2014 and 01-01-2024
t[16436]
t[12785]
# note that the values are in K

test.array <- ncvar_get(nc_tmin) # store the data in a 3-dimensional array
dim(test.array) 
test.slice <- test.array[, , 12785]
dim(test.slice)
test.slice[4,180]

test_tmin <- rast(here::here("data/original/clim_lab/gridMET/agg_met_tmmn_1979_CurrentYear_CONUS.nc"))[[12785:12795]]
test_tmin[100, 100, 1]

tmin_convert <- (test_tmin - 273.15) * (9/5) + 32
tmin_convert

test_tmax <- rast(here::here("data/original/clim_lab/gridMET/agg_met_tmmx_1979_CurrentYear_CONUS.nc"))[[12785:12795]]
test_tmax

tmax_convert <- (test_tmax - 273.15) * (9/5) + 32
tmax_convert

test_ave <- (tmax_convert - tmin_convert) / 2
test_ave



