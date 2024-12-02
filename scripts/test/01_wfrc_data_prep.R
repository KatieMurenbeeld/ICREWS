library(tidyverse)
library(sf)
library(terra)
library(tigris)
library(stringr)
library(RCurl)
library(gstat)

# Set the timeout to 100 minutes (6000 seconds)
options(timeout=6000)

# Create a new subdirectory for the data in the data/original directory
data_dir <- here::here("data/original/")
sub_dir <- "wfrc"

# check if sub directory exists 
if (file.exists(sub_dir)){
  
} else {
  
  # create a new sub directory inside
  # the data directory
  dir.create(file.path(data_dir, sub_dir))
}

# Download the WHP, BP, RPS for Idaho

download_fire <- function(data){    
  tmp <- tempfile()
  fs.url <- "https://usfs-public.box.com/shared/static/jgmjss1rgjc7pnguc7480544ktvk5ild.zip"
  download.file(fs.url, tmp)
  tmp2 <- tempfile()
  unzip(zipfile=tmp, exdir = tmp2 )
  dir.name <- list.files(tmp2)
  rast.file <- list.files(paste0(tmp2,"/", dir.name), pattern="*.tif$", full.names = TRUE)
  tmp.rast.file <- rast.file[grepl(data, rast.file)]
  rasters <- rast(tmp.rast.file)
  fnames <- paste0("data/original/", sub_dir, "/", names(rasters), ".tif")
  print(fnames)
  writeRaster(rasters, filename = fnames, overwrite = TRUE)
  return(fnames)
}

# create a list of the data you would like
data_list <- c("WHP", "BP", "RPS")

for (data in data_list) {
  download_fire(data)
}

# Aggregate to 3km (3000m, fact = 100) 
agg_fire <- function(ogrst, fac, res){
  rasters <- rast(ogrst)
  fnames.process <- paste0("data/processed/",names(rasters), "_id_", res, "_", Sys.Date(), ".tif")
  rasters.agg <- aggregate(rasters, fact=fac, cores = 2)
  writeRaster(rasters.agg, fnames.process, overwrite=TRUE)
  return(fnames.process) 
}

for (rst in fnames_list) {
  agg_fire(rst, 100, "3km")
}
