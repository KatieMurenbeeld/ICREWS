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
sub_dir <- "census"

# check if sub directory exists 
if (file.exists(sub_dir)){
  
} else {
  
  # create a new sub directory inside
  # the data directory
  dir.create(file.path(data_dir, sub_dir))
}

# Change in population (from Census)
# from https://www.census.gov/data/tables/time-series/demo/popest/2020s-counties-total.html
#url <- "https://www2.census.gov/programs-surveys/popest/datasets/2020-2023/counties/totals/co-est2023-alldata.csv"
#url <- "https://www2.census.gov/programs-surveys/popest/tables/2020-2024/counties/totals/co-est2024-pop.xlsx"
url <- "https://www2.census.gov/programs-surveys/popest/datasets/2020-2024/counties/totals/co-est2024-alldata.csv"
file_name <- "co-est2024-alldata.csv"

download.file(url, here::here(paste0("data/original/", sub_dir, "/", file_name)))

# Set the projection
projection <- "epsg:5070"

# Load the data
del_pop_cen <- read_csv(here::here("data/original/census/co-est2024-alldata.csv"))

# Download the county data for Idaho. Set the year to 2023
counties_2023 <- tigris::counties(year = 2023) 
counties_2023 <- counties_2023 %>%
  filter(STATEFP == 16) %>%
  dplyr::select(GEOID, geometry)
counties_2023 <- st_transform(counties_2023, projection)

## Net and ave change in population and % change in migration (rate?), using census data
delpop_cen <- del_pop_cen %>% 
  mutate(pct_mig_5yr = ((NPOPCHG2024 - NPOPCHG2020)/NPOPCHG2020) * 100,
         ave_del_pop = ((NPOPCHG2020 + NPOPCHG2021 + NPOPCHG2022 + NPOPCHG2023 + NPOPCHG2024)/5)) %>%
  dplyr::select(STATE, COUNTY, NPOPCHG2023, NETMIG2022, ave_del_pop, pct_mig_5yr)

delpop_cen <- delpop_cen %>%
  mutate(STATE = as.character(STATE), 
         COUNTY = as.character(COUNTY)) %>%
  mutate(FIPS = paste(STATE, COUNTY, sep = ""))

## Make sure all FIPS codes are padded with 0s for a total of 5 characters
update_fips <- function(data_set) {
  data_set$FIPS <- as.character(data_set$FIPS)
  data_set$FIPS <- str_pad(data_set$FIPS, 5, side="left", pad="0")
  return(data_set)
}

delpop_fips <- update_fips(delpop_cen)

## join to the 2023 counties
delpop_county <- left_join(counties_2023, delpop_fips,
                           by = c("GEOID" = "FIPS"))

## check for validity, remove empty geometries, and reproject 
if (!all(st_is_valid(delpop_county)))
  delpop_county <- st_make_valid(delpop_county)

delpop_county <- delpop_county %>%
  filter(!st_is_empty(.))

delpop_proj <- delpop_county %>%
  st_transform(projection)

## Create a template raster for the shapefiles
# use the Wildfire data as the reference raster
ref_rast <- rast(here::here("data/processed/wfrc_BP_ID_3km_2024-12-03.tif"))

XMIN <- ext(ref_rast)$xmin
XMAX <- ext(ref_rast)$xmax
YMIN <- ext(ref_rast)$ymin
YMAX <- ext(ref_rast)$ymax
aspectRatio <- (YMAX-YMIN)/(XMAX-XMIN)
cellSize <- 3000
NCOLS <- as.integer((XMAX-XMIN)/cellSize)
NROWS <- as.integer(NCOLS * aspectRatio)
templateRas <- rast(ncol=NCOLS, nrow=NROWS, 
                    xmin=XMIN, xmax=XMAX, ymin=YMIN, ymax=YMAX,
                    vals=1, crs=crs(ref_rast))

grd <- st_as_stars(templateRas)

# Rasterize the Net migration 2022 data using the templateRas as the reference raster
delpop.rst <- rasterize(delpop_proj, ref_rast, field = "NETMIG2022", fun = "mean")
nrow(as.data.frame(delpop.rst))

# Fill NAs with focal
delpop_fill <- focal(delpop.rst, 3, mean, na.policy='only', na.rm = TRUE)
plot(delpop_fill)
nrow(as.data.frame(delpop_fill))

# Crop to reference raster
delpop_fill_crop <- crop(delpop_fill, ref_rast, mask = TRUE)
plot(delpop_fill_crop)
nrow(as.data.frame(delpop_fill_crop))

# Rasterize the ave population change from 2020-2024 data using the templateRas as the reference raster
avedelpop.rst <- rasterize(delpop_proj, ref_rast, field = "ave_del_pop", fun = "mean")
nrow(as.data.frame(avedelpop.rst))

# Fill NAs with focal
avepop_fill <- focal(avedelpop.rst, 3, mean, na.policy='only', na.rm = TRUE)
plot(avepop_fill)
nrow(as.data.frame(avepop_fill))

# Crop to reference raster
avepop_fill_crop <- crop(avepop_fill, ref_rast, mask = TRUE)
plot(avepop_fill_crop)
nrow(as.data.frame(avepop_fill_crop))

# Save rasters 
writeRaster(delpop_fill_crop, here::here(paste0("data/processed/net_mig_2023_id_3km_crop_", 
                                Sys.Date(), ".tif")))

writeRaster(avepop_fill_crop, here::here(paste0("data/processed/ave_pop_change_2020_2024_id_3km_crop_", 
                                                Sys.Date(), ".tif")))

