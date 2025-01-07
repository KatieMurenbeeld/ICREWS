library(tidyverse)
library(sf)
library(terra)
library(tigris)
library(stringr)
library(RCurl)
library(stars)
library(spdep)
library(gstat)
library(readxl)

# Set timeout to 10 minutes
options(timeout=6000)

# Create a new subdirectory for the data in the data/original directory
data_dir <- here::here("data/original/")
sub_dir <- "bric"

# check if sub directory exists 
if (file.exists(sub_dir)){
  
} else {
  
  # create a new sub directory inside
  # the data directory
  dir.create(file.path(data_dir, sub_dir))
}

# Baseline Resilience Indicators for Communities (BRIC) data
url <- "https://sc.edu/study/colleges_schools/artsandsciences/centers_and_institutes/hvri/documents/bric/bric2020_us_forweb.xlsx"

download.file(url, destfile = here::here("data/original/bric/bric2020_us.xlsx"))

# Set the projection
projection <- "epsg:5070"

# use the Wildfire data as the reference raster
ref_rast <- rast(here::here("data/processed/wfrc_BP_ID_3km_2024-12-03.tif"))

# Load the data
bric <- read_excel("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/bric2020_us.xlsx")

## Load county boundaries from tigris
### Set the year to account for changes to FIPS codes
counties_2020 <- tigris::counties(year = 2020) 
### Filter for Idaho (FIPS = 16)
counties_2020 <- counties_2020 %>%
  filter(STATEFP == 16) %>%
  dplyr::select(GEOID, geometry)

counties_2020 <- st_transform(counties_2020, projection)

## BRIC: select GEOID and community capital
bric_2020 <- bric %>% # needs 2020 counties
  dplyr::select("GEOID", "COMM CAPITAL", "SOCIAL") %>%
  rename("FIPS" = "GEOID", 
         "COMM_CAP" = "COMM CAPITAL",
         "SOC_CAP" = "SOCIAL")

## Make sure all FIPS codes are padded with 0s for a total of 5 characters
update_fips <- function(data_set) {
  data_set$FIPS <- as.character(data_set$FIPS)
  data_set$FIPS <- str_pad(data_set$FIPS, 5, side="left", pad="0")
  return(data_set)
}

bric_fips <- update_fips(bric_2020)

## join to the 2020 counties
bric_county <- left_join(counties_2020, bric_fips,
                         by = c("GEOID" = "FIPS"))

## check for validity, remove empty geometries, and reproject 
if (!all(st_is_valid(bric_county)))
  bric_county <- st_make_valid(bric_county)

bric_county <- bric_county %>%
  filter(!st_is_empty(.))

bric_proj <- bric_county %>%
  st_transform(projection)

## Create a template raster for the shapefiles
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

# rasterize the community capitol
bric.comm.rst <- rasterize(bric_proj, ref_rast, field = "COMM_CAP", fun = "mean")
plot(bric.comm.rst)
nrow(as.data.frame(bric.comm.rst))

# crop to the bric county shapefile
bric_comm_crop <- crop(bric.comm.rst$COMM_CAP, ref_rast, mask = TRUE)
plot(bric_comm_crop)
nrow(as.data.frame(bric_comm_crop))
nrow(as.data.frame(ref_rast))

# rasterize the social capitol
bric.soc.rst <- rasterize(bric_proj, ref_rast, field = "SOC_CAP", fun = "mean")
plot(bric.soc.rst)
# crop to the bric county shapefile
bric_soc_crop <- crop(bric.soc.rst$SOC_CAP, ref_rast, mask = TRUE)
plot(bric_soc_crop)
nrow(as.data.frame(bric_soc_crop))

# save the rasters
writeRaster(bric_comm_crop, here::here(paste0("data/processed/bric_commcap_id_3km_pred_crop_", 
                                         Sys.Date(), ".tif")), overwrite = TRUE)

writeRaster(bric_soc_crop, here::here(paste0("data/processed/bric_soccap_id_3km_pred_crop_", 
                                              Sys.Date(), ".tif")), overwrite = TRUE)





