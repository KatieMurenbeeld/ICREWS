library(tidyverse)
library(terra)
library(raster)
library(sf)
library(ggplot2)
library(tmap)
library(geocmeans)
library(RColorBrewer)
library(viridis)
library(future)
library(spdep)
library(classInt)

#----Load the data----
#gfcm_result_k3_mod <- readRDS(here::here("outputs/SGFCM_all_attr_k6_2024-10-15.rds"))
sgfcm_result_k8_mod <- readRDS(here::here("outputs/models/sgfcm_mod_k8_2025-01-08.rds"))

## set the projection
projection <- "epsg:5070"

# need to rerun the cluster to get model results
rst_sc <- rast(here::here("data/processed/rast_stack_all_attributes_scaled_2025-01-07.tif"))

# Format for use in geocmeans
dataset <- lapply(names(rst_sc), function(n){
  aband <- rst_sc[[n]]
  return(aband)
})
names(dataset) <- names(rst_sc)

# Use Spatial Generalized Fuzzy C-Means clustering 
# FCM seed = 1234, Silhouette index = 0.48, k = 6, m = 1.9, window =  7x7 (w2), alpha = 0.6, beta = 0.4
# FCM seed = 1234, Silhouette index = 0.45, k = 8, m = 1.9, window =  3x3 (w1), alpha = 0.5, beta = 0.4

w <- matrix(1, nrow = 5, ncol = 5)

SGFCM_result_k8 <- SGFCMeans(dataset, k = 8, m = 1.9, standardize = FALSE,
                             lag_method = "mean",
                             window = w, alpha = 1.6, beta = 0.2,
                             seed = 456, tol = 0.001, verbose = TRUE, init = "kpp")


# Calculate Entropy
## k = 8
arch_all_rst <- rast(SGFCM_result_k8$rasters)
arch_all_rst_conus_belong <- subset(arch_all_rst, 1:8)

arch_all_rst_conus_df <- as.data.frame(arch_all_rst, xy = TRUE)

all_ent_conus <- calcUncertaintyIndex(as.matrix(as.data.frame(arch_all_rst_conus_belong)))

all_ent_conus <- cbind(arch_all_rst_conus_df, all_ent_conus)
all_ent_rst_conus <- rast(all_ent_conus, crs = projection)

writeRaster(all_ent_rst_conus, here::here(paste0("outputs/models/SGFCM_k8_entropy_", Sys.Date(), ".tif")))


