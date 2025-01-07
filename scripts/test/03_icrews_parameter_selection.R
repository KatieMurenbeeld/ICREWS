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
