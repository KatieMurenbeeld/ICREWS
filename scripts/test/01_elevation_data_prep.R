## Download elevation data and calculate a topographic complexity index
## focal while loop from stack overflow: https://stackoverflow.com/questions/73271223/how-to-fill-na-gaps-by-idw-using-focal-r-raster-terra

# 0. Load libraries, set projection, and load reference raster
library(sf) # for working with vector data
library(raster) # for working with rasters
library(sp) # for working with spatial (vector) data
library(geodata) # this package removed from CRAN May 27, 2024...
library(terra) # for working with rasters
library(tigris) # needed for state/CONUS boundaries
library(dplyr) # for manipulating dataframes 
library(dismo) # needed to calculate biovars
library(gstat)
library(stars)

projection <- "epsg:5070"
ref_rast <- rast(here::here("data/processed/wfrc_BP_ID_3km_2024-12-03.tif"))

# 1. Download the data using the geodata package
#r_ele <- geodata::elevation_30s(country = "US", path = here::here("data/original/"))

r_ele <- rast(here::here("data/original/elevation/USA_elv_msk.tif"))

# 2. Reproject and crop to reference raster
ele_proj <- project(r_ele, projection)

ele_crop <- crop(ele_proj, ref_rast)
plot(ele_crop)

# 3. Calculate the roughness using the terra:terrain function
roughness <- terrain(ele_crop, v = "roughness")
plot(roughness)

# 4. Resample and crop to reference raster
rough_resamp <- resample(roughness, ref_rast)
rough_crop <- crop(rough_resamp, ref_rast, mask = TRUE)
nrow(as.data.frame(rough_crop))
nrow(as.data.frame(ref_rast))

# 5. Save the raster
writeRaster(rough_crop, here::here(paste0("data/processed/roughness_id_3km_pred_crop_", 
                                              Sys.Date(), ".tif")), overwrite = TRUE)
