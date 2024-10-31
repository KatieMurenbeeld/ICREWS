library(tidyverse)
library(sf)
library(terra)
library(tigris)
library(stringr)
library(RCurl)

# Some files are large so set the timeout to 100 minutes (6000 seconds)
options(timeout=6000)

# There are three file types we will download: csv, shp, and tif/img.

# update this to detect .zip in the file name as well
download_data <- function(url, file_name) {
  if (str_detect(url, ".zip")) {
    tmp <- tempfile()
    download.file(url, tmp)
    unzip(zipfile = tmp, exdir = here::here("data/original/"))
  } else if (str_detect(file_name, ".zip")) {
    download.file(url, here::here(paste0("data/original/", file_name)))
    unzip(zipfile = here::here(paste0("data/original/", file_name)))
  }
  else {
    download.file(url, here::here(paste0("data/original/", file_name)))
  }
}

# Climate and Economic Justice Screening Tool
url <- "https://static-data-screeningtool.geoplatform.gov/data-versions/1.0/data/score/downloadable/1.0-shapefile-codebook.zip"
file_name <- ""

download_data(url, file_name)


