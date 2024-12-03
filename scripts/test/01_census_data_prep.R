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
url <- "https://www2.census.gov/programs-surveys/popest/datasets/2020-2023/counties/totals/co-est2023-alldata.csv"
file_name <- "co-est2023-alldata.csv"

download.file(url, here::here(paste0("data/original/", sub_dir, "/", file_name)))

# Set the projection
projection <- "epsg:5070"

# Load the data
del_pop_cen <- read_csv(here::here("data/original/census/census_2023_comp_est.csv"))

# Download the county data for Idaho. Set the year to 2023
counties_2023 <- tigris::counties(year = 2023) 
counties_2023 <- counties_2023 %>%
  filter(STATEFP == 16) %>%
  dplyr::select(GEOID, geometry)

## Net and ave change in population and % change in migration (rate?), using census data
delpop_cen <- del_pop_cen %>% 
  mutate(pct_mig = ((NPOPCHG2023 - NPOPCHG2020)/NPOPCHG2020) * 100,
         ave_del_pop = ((NPOPCHG2020 + NPOPCHG2021 + NPOPCHG2022 + NPOPCHG2023)/4)) %>%
  dplyr::select(STATE, COUNTY, NPOPCHG2023, NETMIG2022, ave_del_pop, pct_mig)

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

## join to the 2020 counties
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
XMIN <- ext(delpop_proj)$xmin
XMAX <- ext(delpop_proj)$xmax
YMIN <- ext(delpop_proj)$ymin
YMAX <- ext(delpop_proj)$ymax
aspectRatio <- (YMAX-YMIN)/(XMAX-XMIN)
cellSize <- 3000
NCOLS <- as.integer((XMAX-XMIN)/cellSize)
NROWS <- as.integer(NCOLS * aspectRatio)
templateRas <- rast(ncol=NCOLS, nrow=NROWS, 
                    xmin=XMIN, xmax=XMAX, ymin=YMIN, ymax=YMAX,
                    vals=1, crs=crs(delpop_proj))

grd <- st_as_stars(templateRas)

# Rasterize the Net migration 2022 data using the templateRas as the reference raster
delpop.rst <- rasterize(delpop_proj, templateRas, field = "NETMIG2022", fun = "mean")

# Crop to reference raster
delpop_crop <- crop(delpop.rst, delpop_proj, mask = TRUE)
plot(delpop_crop)

# Save raster 
writeRaster(delpop_crop, here::here(paste0("data/processed/net_mig_2023_id_3km_crop_", 
                                Sys.Date(), ".tif")))

