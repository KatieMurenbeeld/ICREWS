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

# k = 3, m = 1.2, SI = 0.41, XB = 0.86
# k = 8, m = 1.6, SI = 0.45, XB = 21.25

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

# k = 3, m = 1.8, beta = 0.7, SI = 0.45, XB = 0.79

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

# k = 8, m = 1.9, beta = 0.2, SI = 0.47, XB = 40.73 

#----Spatial FCM----
w1 <- matrix(1, nrow = 3, ncol = 3)
w2 <- matrix(1, nrow = 5, ncol = 5)
w3 <- matrix(1, nrow = 7, ncol = 7)

# k = 3
SFCMvalues_k3 <- select_parameters.mc(algo = "SFCM", data = dataset, 
                                      k = 3, m = 1.8,
                                      alpha = seq(0.1,2,0.1),
                                      window = list(w1,w2,w3),
                                      spconsist = TRUE, nrep = 5, 
                                      verbose = TRUE, chunk_size = 4,
                                      seed = 6891, init = "kpp",
                                      indices = c("XieBeni.index", "Explained.inertia",
                                                  "Negentropy.index", "Silhouette.index"))


dict <- data.frame(
  w = c(1,2,3),
  window = c("3x3","5x5","7x7")
)

SFCMvalues_k3$window <- dict$window[match(SFCMvalues_k3$window,dict$w)]
write_csv(SFCMvalues_k3, here::here(paste0("outputs/parameter_selection/sfcm_param_indices_k3_",
                                           Sys.Date(), ".csv")), append = FALSE)


# plotting the silhouette index
ggplot(SFCMvalues_k3) + 
  geom_raster(aes(x = alpha, y = window, fill = Silhouette.index)) + 
  geom_text(aes(x = alpha, y = window, label = round(Silhouette.index,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=0.5)

# plotting the Xie Beni
ggplot(SFCMvalues_k3) + 
  geom_raster(aes(x = alpha, y = window, fill = XieBeni.index)) + 
  geom_text(aes(x = alpha, y = window, label = round(XieBeni.index, 2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=0.5)

# k = 3, m = 1.8, window = 5x5, alpha = 1.7, SI = 0.41, XB = 1083.55 

# k = 8
SFCMvalues_k8 <- select_parameters.mc(algo = "SFCM", data = dataset, 
                                      k = 8, m = 1.9,
                                      alpha = seq(0.1,2,0.1),
                                      window = list(w1,w2,w3),
                                      spconsist = TRUE, nrep = 5, 
                                      verbose = TRUE, chunk_size = 4,
                                      seed = 6891, init = "kpp",
                                      indices = c("XieBeni.index", "Explained.inertia",
                                                  "Negentropy.index", "Silhouette.index"))


dict <- data.frame(
  w = c(1,2,3),
  window = c("3x3","5x5","7x7")
)

SFCMvalues_k8$window <- dict$window[match(SFCMvalues_k8$window,dict$w)]
write_csv(SFCMvalues_k8, here::here(paste0("outputs/parameter_selection/sfcm_param_indices_k8_",
                                           Sys.Date(), ".csv")), append = FALSE)


# plotting the silhouette index
ggplot(SFCMvalues_k8) + 
  geom_raster(aes(x = alpha, y = window, fill = Silhouette.index)) + 
  geom_text(aes(x = alpha, y = window, label = round(Silhouette.index,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=0.5)

# plotting the Xie Beni
ggplot(SFCMvalues_k8) + 
  geom_raster(aes(x = alpha, y = window, fill = XieBeni.index)) + 
  geom_text(aes(x = alpha, y = window, label = round(XieBeni.index, 2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=0.5)

# Lot's of options with k = 8, m = 1.9
# w = 5x5, alpha = 1.4, SI = 0.49, XB = 255.2

#-----Spatial GFCM-----
future::plan(future::multisession(workers = 2))

# k = 3
SGFCMvalues_k3 <- select_parameters.mc(algo = "SGFCM", data = dataset,
                                       k = 3, m = 1.8,
                                       beta = seq(0.1,0.9,0.1), alpha = seq(0.5,2,0.1),
                                       window = w2,
                                       spconsist = TRUE, nrep = 5, 
                                       verbose = TRUE, chunk_size = 4,
                                       seed = 456, init = "kpp",
                                       indices = c("XieBeni.index", "Explained.inertia",
                                                   "Negentropy.index", "Silhouette.index"))


write_csv(SGFCMvalues_k3, here::here(paste0("outputs/parameter_selection/sgfcm_param_indices_k3_",
                                           Sys.Date(), ".csv")), append = FALSE)

# showing the silhouette index
ggplot(SGFCMvalues_k3) + 
  geom_raster(aes(x = alpha, y = beta, fill = Silhouette.index)) + 
  geom_text(aes(x = alpha, y = beta, label = round(Silhouette.index,2)), size = 2.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)

# showing the Xie Beni index
ggplot(SGFCMvalues_k3) + 
  geom_raster(aes(x = alpha, y = beta, fill = XieBeni.index)) + 
  geom_text(aes(x = alpha, y = beta, label = round(XieBeni.index,2)), size = 2.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)

# k = 3, m = 1.8, w = 5x5, alpha = 1.7, beta = 0.1, SI = 0.41, XB = 636.37

# k = 8
SGFCMvalues_k8 <- select_parameters.mc(algo = "SGFCM", data = dataset,
                                       k = 8, m = 1.9,
                                       beta = seq(0.1,0.9,0.1), alpha = seq(0.5,2,0.1),
                                       window = w2,
                                       spconsist = TRUE, nrep = 5, 
                                       verbose = TRUE, chunk_size = 4,
                                       seed = 456, init = "kpp",
                                       indices = c("XieBeni.index", "Explained.inertia",
                                                   "Negentropy.index", "Silhouette.index"))


write_csv(SGFCMvalues_k8, here::here(paste0("outputs/parameter_selection/sgfcm_param_indices_k8_",
                                            Sys.Date(), ".csv")), append = FALSE)

# showing the silhouette index
ggplot(SGFCMvalues_k8) + 
  geom_raster(aes(x = alpha, y = beta, fill = Silhouette.index)) + 
  geom_text(aes(x = alpha, y = beta, label = round(Silhouette.index,2)), size = 2.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)

# showing the Xie Beni index
ggplot(SGFCMvalues_k8) + 
  geom_raster(aes(x = alpha, y = beta, fill = XieBeni.index)) + 
  geom_text(aes(x = alpha, y = beta, label = round(XieBeni.index,2)), size = 2.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)

# k = 8, m = 1.8, w = 5x5, alpha = 1.6, beta = 0.2, SI = 0.48, XB = 271.43
