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

# read in the reference ET, which represents water loss from a hypothetical well watered actively growing grass surface
# potential water loss through ET? mm/day
projection <- "epsg:5070"

pet_rst <- rast(here::here("data/original/clim_lab/agg_met_pet_1979_CurrentYear_CONUS.nc"))
pet_rst
test <- pet_rst["daily_mean_reference_evapotranspiration_grass"]
test

test_slice <- test[[2483:3213]]
test_slice
pet_rst <- as(pet_rst, "SpatRaster")
#plot(pdsi_rst)

pet_rst_proj <- project(test_slice, projection)
pet_rst_proj
names(pet_rst_proj)

tenyr_slice <- pet_rst["daily_mean_reference_evapotranspiration_grass"][[12785:16436]]

pet_rst_proj <- project(tenyr_slice, projection)
pet_rst_proj
names(pet_rst_proj)

r.pet <- brick(pet_rst_proj)
r.pet
plot(mean(r.pet))
test_final <- rast(mean(r.pet))
test_final
plot(test_final)

ref_rast <- rast(here::here("data/processed/wfrc_BP_ID_3km_2024-12-03.tif"))
test_final_resamp <- resample(test_final, ref_rast)
plot(test_final_resamp) 
test_final_crop <- crop(test_final_resamp, ref_rast, mask = TRUE)
plot(test_final_crop)
nrow(as.data.frame(test_final_crop))
nrow(as.data.frame(ref_rast))

# save the raster file
writeRaster(test_final_crop, here::here(paste0("data/processed/ret_mean_2014_2024_id_3km_crop_", 
                                        Sys.Date(), ".tif")), overwrite = TRUE)

### Notes for cleaning this up. 1) do I want 10 years of data? normally I look at around 5.
### 2) would I want to look at areas where drought is increasing over the last 5-10 years?
### 3) need to clean up code in general, but I will need to use some netcdf functions in order to get the correct slices.

pdsi_rst
# following example from https://rpubs.com/boyerag/297592
nc_data <- nc_open(here::here("data/original/clim_lab/agg_met_pet_1979_CurrentYear_CONUS.nc"))
# Save the print(nc) dump to a text file
{
  sink(here::here("data/original/clim_lab/agg_met_pet_1979_CurrentYear_CONUS.txt"))
  print(nc_data)
  sink()
}

lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "day")
t <- as.Date(t, origin = "1900-01-01")
head(lon) # look at the first few entries in the longitude vector
tail(t)

t[12785]
length(t)
t[16436]

pdsi.array <- ncvar_get(nc_data, "category") # store the data in a 3-dimensional array
dim(pdsi.array) 

fillvalue <- ncatt_get(nc_data, "daily_mean_palmer_drought_severity_index", "_FillValue")
fillvalue

nc_close(nc_data)

pdsi.slice <- pdsi.array[, , 3203:3213] 
dim(pdsi.slice)


pdsi.test <- pdsi.array[, , 2500]
plot(pdsi.test)
#r <- raster(t(pdsi.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
#plot(r)

r_brick <- brick(pdsi.slice, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r_brick

m <- c(0, 3, 1,  3, 10, 0)
rclmat <- matrix(m, ncol = 3, byrow = TRUE)

r.drought <- reclassify(r_brick, rclmat)
r.drought


plot(r.drought[[1]])
plot(sum(r.drought))

dfreq_rst <- rast(sum(r.drought))
plot(dfreq_rst)
dfreq_rst

# Set the projection
projection <- "epsg:5070"
dfreq_rst_proj <- project(dfreq_rst, projection)
dfreq_rst_proj
plot(dfreq_rst_proj)

# Resample to Idaho using the wildfire raster as a reference raster
ref_rast <- rast(here::here("data/processed/wfrc_BP_ID_3km_2024-12-03.tif"))
dfreq_rst_resamp <- resample(dfreq_rst, ref_rast)
dfreq_crop <- crop(dfreq_rst_resamp, ref_rast, mask = TRUE)
plot(dfreq_crop)
nrow(as.data.frame(dfreq_crop))


test_dfreq <- stackApply(r_brick, indices = 1, fun = sum)

#test <- tapp(pdsi_rst, "day", min)

r_brick[r_brick <=-2]
r_brick[r_brick<=-2] <- 1
r_brick[r_brick >-2] <- NA
r_brick_sum <- calc(r_brick, function(x) sum(x, na.rm = TRUE))

r_brick_sum
plot(r_brick_sum)
r_brick
