library(tidyverse)
library(terra)
library(raster)
library(sf)
library(ggplot2)
library(geocmeans)
library(RColorBrewer)
library(viridis)


#----Load the data----
rst_sc <- rast(here::here("data/processed/rast_stack_all_attributes_scaled_2025-01-07.tif"))

# Format for use in geocmeans
dataset <- lapply(names(rst_sc), function(n){
  aband <- rst_sc[[n]]
  return(aband)
})
names(dataset) <- names(rst_sc)

# Calculate the correlation between the attributes

# For k = 3 GFCM k = 3, m = 1.8, beta = 0.7, SI = 0.45, XB = 0.79, seed = 6891
# For k = 8 SGFCM k = 8, m = 1.9, w = 5x5, alpha = 1.6, beta = 0.2, SI = 0.48, XB = 271.43, seed = 456

w <- matrix(1, nrow = 5, ncol = 5)

#----k = 3----
GFCM_result_k3 <- GCMeans(dataset, k = 3, m = 1.8, standardize = FALSE,
                          beta = 0.7, seed = 6891, tol = 0.001,
                          verbose = TRUE, init = "kpp")

saveRDS(GFCM_result_k3, here::here(paste0("outputs/models/gfcm_mod_k3_",
                                           Sys.Date(), ".rds")))

map_GFCM_result_k3 <- rast(GFCM_result_k3$rasters)
plot(map_GFCM_result_k3[["Groups"]])

writeRaster(map_GFCM_result_k3[["Groups"]], here::here(paste0("outputs/models/gfcm_result_k3_", 
                                                               Sys.Date(), ".tif")))

#----k = 8--------
SGFCM_result_k8 <- SGFCMeans(dataset, k = 8, m = 1.9, standardize = FALSE,
                                 lag_method = "mean",
                                 window = w, alpha = 1.6, beta = 0.2,
                                 seed = 456, tol = 0.001, verbose = TRUE, init = "kpp")

saveRDS(SGFCM_result_k8, here::here(paste0("outputs/models/sgfcm_mod_k8_",
                                               Sys.Date(), ".rds")))

map_SGFCM_result_k8 <- rast(SGFCM_result_k8$rasters)
plot(map_SGFCM_result_k8[["Groups"]])

writeRaster(map_SGFCM_result_k8[["Groups"]], here::here(paste0("outputs/models/sgfcm_result_k8_", 
                                                               Sys.Date(), ".tif")))
