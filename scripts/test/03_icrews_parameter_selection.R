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
rst_sc <- rast(here::here("data/processed/rast_stack_all_attributes_scaled_2025-01-07.tif"))

# Format for use in geocmeans
dataset <- lapply(names(rst_sc), function(n){
  aband <- rst_sc[[n]]
  return(aband)
})
names(dataset) <- names(rst_sc)

#----Use a non spatial and non generalized fuzzy c-means to determine number of k and value for m
future::plan(future::multisession(workers = 2))
FCMvalues <- select_parameters.mc(algo = "FCM", data = dataset, 
                                  k = 2:10, m = seq(1.1,2,0.1), spconsist = FALSE, 
                                  indices = c("XieBeni.index", "Explained.inertia",
                                              "Negentropy.index", "Silhouette.index"),
                                  seed = 1234, verbose = TRUE) 

write_csv(FCMvalues, here::here(paste0("outputs/parameter_selection/fcm_param_indices_",
                            Sys.Date(), ".csv")), append = FALSE)

# plotting the silhouette index
ggplot(FCMvalues) + 
  geom_raster(aes(x = k, y = m, fill = Silhouette.index)) + 
  geom_text(aes(x = k, y = m, label = round(Silhouette.index,2)), size = 2.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=2)

# plotting the Xie Beni index
ggplot(FCMvalues) + 
  geom_raster(aes(x = k, y = m, fill = XieBeni.index)) + 
  geom_text(aes(x = k, y = m, label = round(XieBeni.index,2)), size = 2.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=2)

#----Use a generalized fuzzy c-means to determine the value for m and beta
# k = 3
GFCMvalues_k3 <- select_parameters.mc(algo = "GFCM", data = dataset, seed = 6891,
                                      k = 3, m = seq(1.1,2,0.1), beta = seq(0.1,0.9,0.1),
                                      spconsist = FALSE, verbose = TRUE, init = "kpp",
                                      indices = c("XieBeni.index", "Explained.inertia",
                                                  "Negentropy.index", "Silhouette.index"))  

write_csv(GFCMvalues_k3, here::here(paste0("outputs/parameter_selection/gfcm_param_indices_k3_",
                                          Sys.Date(), ".csv")), append = FALSE)


# plotting the silhouette index
ggplot(GFCMvalues_k3) + 
  geom_raster(aes(x = m, y = beta, fill = Silhouette.index)) + 
  geom_text(aes(x = m, y = beta, label = round(Silhouette.index,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)

# plotting the Xie Beni
ggplot(GFCMvalues_k3) + 
  geom_raster(aes(x = m, y = beta, fill = XieBeni.index)) + 
  geom_text(aes(x = m, y = beta, label = round(XieBeni.index, 2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)

# k = 8
GFCMvalues_k8 <- select_parameters.mc(algo = "GFCM", data = dataset, seed = 6891,
                                      k = 8, m = seq(1.1,2,0.1), beta = seq(0.1,0.9,0.1),
                                      spconsist = FALSE, verbose = TRUE, init = "kpp",
                                      indices = c("XieBeni.index", "Explained.inertia",
                                                  "Negentropy.index", "Silhouette.index"))  

write_csv(GFCMvalues_k8, here::here(paste0("outputs/parameter_selection/gfcm_param_indices_k8_",
                                           Sys.Date(), ".csv")), append = FALSE)


# plotting the silhouette index
ggplot(GFCMvalues_k8) + 
  geom_raster(aes(x = m, y = beta, fill = Silhouette.index)) + 
  geom_text(aes(x = m, y = beta, label = round(Silhouette.index,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)

# plotting the Xie Beni
ggplot(GFCMvalues_k8) + 
  geom_raster(aes(x = m, y = beta, fill = XieBeni.index)) + 
  geom_text(aes(x = m, y = beta, label = round(XieBeni.index, 2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)
