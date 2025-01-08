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
#sgfcm_all_result_k6_mod <- readRDS(here::here("outputs/SGFCM_all_attr_k6_2024-10-15.rds"))
#sgfcm_all_result_k8_mod <- readRDS(here::here("outputs/SGFCM_all_attr_k8_2024-10-15.rds"))
# need to rerun the cluster to get model results
rst_sc <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_scaled_2024-10-08.tif")
rst <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_2024-10-08.tif")

# Format for use in geocmeans
dataset <- lapply(names(rst_sc), function(n){
  aband <- rst_sc[[n]]
  return(aband)
})
names(dataset) <- names(rst_sc)

# Use Spatial Generalized Fuzzy C-Means clustering 
# FCM seed = 1234, Silhouette index = 0.48, k = 6, m = 1.9, window =  7x7 (w2), alpha = 0.6, beta = 0.4
# FCM seed = 1234, Silhouette index = 0.45, k = 8, m = 1.9, window =  3x3 (w1), alpha = 0.5, beta = 0.4

w1 <- matrix(1, nrow = 3, ncol = 3)
w2 <- matrix(1, nrow = 7, ncol = 7)

SGFCM_all_result_k6 <- SGFCMeans(dataset, k = 6, m = 1.9, standardize = FALSE,
                                 lag_method = "mean",
                                 window = w2, alpha = 0.6, beta = 0.4,
                                 seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")


SGFCM_all_result_k8 <- SGFCMeans(dataset, k = 8, m = 1.9, standardize = FALSE,
                                 lag_method = "mean",
                                 window = w1, alpha = 0.5, beta = 0.4,
                                 seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")


# Calculate Entropy
## k = 6
arch_all_rst <- rast(SGFCM_all_result_k6$rasters)
arch_all_rst_conus_belong <- subset(arch_all_rst, 1:6)

arch_all_rst_conus_df <- as.data.frame(arch_all_rst, xy = TRUE)

all_ent_conus <- calcUncertaintyIndex(as.matrix(as.data.frame(arch_all_rst_conus_belong)))

all_ent_conus <- cbind(arch_all_rst_conus_df, all_ent_conus)
all_ent_rst_conus <- rast(all_ent_conus)

writeRaster(all_ent_rst_conus, here::here(paste0("outputs/SGFCM_all_k6_entropy_", Sys.Date(), ".tif")))

## k = 8
arch_all_rst <- rast(SGFCM_all_result_k8$rasters)
arch_all_rst_conus_belong <- subset(arch_all_rst, 1:8)

arch_all_rst_conus_df <- as.data.frame(arch_all_rst, xy = TRUE)

all_ent_conus <- calcUncertaintyIndex(as.matrix(as.data.frame(arch_all_rst_conus_belong)))

all_ent_conus <- cbind(arch_all_rst_conus_df, all_ent_conus)
all_ent_rst_conus <- rast(all_ent_conus)

writeRaster(all_ent_rst_conus, here::here(paste0("outputs/SGFCM_all_k8_entropy_", Sys.Date(), ".tif")))

