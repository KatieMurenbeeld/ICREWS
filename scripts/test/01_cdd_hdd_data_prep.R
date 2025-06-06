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

# bring in the nc files as rasters
test_tmin <- rast(here::here("data/original/clim_lab/gridMET/agg_met_tmmn_1979_CurrentYear_CONUS.nc"))[[12785:16436]]
#test_tmin[100, 100, 1]

test_tmax <- rast(here::here("data/original/clim_lab/gridMET/agg_met_tmmx_1979_CurrentYear_CONUS.nc"))[[12785:16436]]
#test_tmax

## convert from K to F
#tmin_convert <- (test_tmin - 273.15) * (9/5) + 32
#tmin_convert

#tmax_convert <- (test_tmax - 273.15) * (9/5) + 32
#tmax_convert

# create a raster of the daily average (combine the conversion with the averaging)
#test_ave <- (tmax_convert + tmin_convert) / 2
#test_ave
tave <- (((test_tmax - 273.15) * (9/5) + 32) + ((test_tmin - 273.15) * (9/5) + 32) / 2)

# from the daily average calculate the heating degree days and the cooling degree days 
# create a separate raster for each

# create a heating degree day 
hdd_rast <- ifel(tave < 65, (65 - tave), 0)

# create a cooling degree day 
cdd_rast <- ifel(tave > 65, (tave - 65), 0)

hdd_rast
cdd_rast

plot(hdd_rast["daily_maximum_temperature_day=41638"])

# make the hdd and cdd into a brick and sum or average for the selected time period
# here 2014-2024
sum_cdd_rst <- rast(sum(brick(cdd_rast)))
sum_cdd_rst

ave_cdd_rst <- rast(mean(brick(cdd_rast)))
ave_cdd_rst

sum_hdd_rst <- rast(sum(brick(hdd_rast)))
sum_hdd_rst

ave_hdd_rst <- rast(mean(brick(hdd_rast)))
ave_hdd_rst
plot(ave_hdd_rst)
plot(ave_cdd_rst) # doesn't seem correct, average is less than 1. But maybe

# reproject, resample (bilinear) and crop to the reference raster
ave_hdd_rst_proj_ref <- project(ave_hdd_rst, ref_rast, method = "bilinear", mask = TRUE)
#ave_hdd_rst_proj_ref
#ave_hdd_rst_proj <- project(ave_hdd_rst, projection)
#ave_hdd_rst_proj
#ave_hdd_resamp <- resample(ave_hdd_rst_proj, ref_rast,  method = "bilinear",)
#ave_hdd_resamp
#ave_hdd_crop <- crop(ave_hdd_resamp, ref_rast, mask = TRUE)
#ave_hdd_crop
ave_hdd_rst_proj_ref_crop <- crop(ave_hdd_rst_proj_ref, ref_rast, mask = TRUE)
ave_hdd_rst_proj_ref_crop
#plot(ave_hdd_crop)
plot(ave_hdd_rst_proj_ref_crop)

# reproject, resample (bilinear) and crop to the reference raster
ave_cdd_rst_proj_ref <- project(ave_cdd_rst, ref_rast, method = "bilinear", mask = TRUE)
#ave_cdd_rst_proj_ref
#ave_cdd_rst_proj <- project(ave_cdd_rst, projection)
#ave_cdd_rst_proj
#ave_cdd_resamp <- resample(ave_cdd_rst_proj, ref_rast,  method = "bilinear",)
#ave_cdd_resamp
#ave_cdd_crop <- crop(ave_cdd_resamp, ref_rast, mask = TRUE)
#ave_cdd_crop
ave_cdd_rst_proj_ref_crop <- crop(ave_cdd_rst_proj_ref, ref_rast, mask = TRUE)
ave_cdd_rst_proj_ref_crop
#plot(ave_cdd_crop)
plot(ave_cdd_rst_proj_ref_crop)

# I get slightly different results if I project using the ref_raster or if I project using the projection 
# and then resample. Not sure what to do about this. 


# save the rasters
writeRaster(ave_cdd_rst_proj_ref_crop, here::here(paste0("data/processed/cdd_10yr_ave_2014_2024_id_3km_crop_", 
                                               Sys.Date(), ".tif")), overwrite = TRUE)
writeRaster(ave_hdd_rst_proj_ref_crop, here::here(paste0("data/processed/hdd_10yr_ave_2014_2024_id_3km_crop_", 
                                               Sys.Date(), ".tif")), overwrite = TRUE)

# something to try later with all of the time series data is a difference in average between 2014 and 2024
# I know part of the characterize goal is to look at some of the variables and their variance through time
