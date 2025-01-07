library(tidyverse)
library(terra)


# Load the rasters
commcap_crop <- rast(here::here("data/processed/bric_commcap_id_3km_pred_crop_2024-12-02.tif"))


#----check for NAs or unequal lengths of data----

nrow(as.data.frame(commcap_crop)) #23857


# Check alignment by stacking the rasters
rast_stack <- c(commcap_crop, )

# Rename variables
names(rast_stack) <- c("comm_cap", )

## Save the raster
writeRaster(x = rast_stack, filename = here::here(paste0("data/processed/rast_stack_all_attributes_", 
                                              Sys.Date(), ".tif")), overwrite = TRUE)
## Scale the data from 0-1 and save the raster
rast_stack_sc <- (rast_stack - global(rast_stack, "min", na.rm=TRUE)[,1])/(global(rast_stack, "max", na.rm=TRUE)[,1] - global(rast_stack, "min", na.rm=TRUE)[,1])
writeRaster(x = rast_stack_sc, filename = here::here(paste0("data/processed/rast_stack_all_attributes_scaled_", 
                                                            Sys.Date(), ".tif")), overwrite = TRUE)