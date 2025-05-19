# Code from https://www.climatologylab.org/uploads/2/2/1/3/22133936/terraclimate_downloadv2.r
# https://www.climatologylab.org/terraclimate.html

install.packages("RNetCDF")              #! 'RNetCDF' package substitution
library(RNetCDF)

#Enter lat and lon ranges
lat.range=c(41.988188,49.000852)        #! Ranges instead of point values. Order does not matter
lon.range=c(-111.043507,-117.24304)

# enter in variable you want to download see: http://thredds.northwestknowledge.net:8080/thredds/terraclimate_aggregated.html

var="ppt"

baseurlagg <- paste0(paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_",var),"_1958_CurrentYear_GLOBE.nc")

nc <- open.nc(baseurlagg)
lon <- var.get.nc(nc, "lon")
lat <- var.get.nc(nc, "lat")
lat.range <- sort(lat.range)                              #!sort user input values from low to high
lon.range <-sort(lon.range)
lat.index <- which(lat>=lat.range[1]&lat<=lat.range[2])    #! index values within specified range
lon.index <- which(lon>=lon.range[1]&lon<=lon.range[2])    
lat.n <- length(lat.index)                                #!value for count
lon.n <- length(lon.index)
start <- c(lon.index[1], lat.index[1], 1)
count <- c(lon.n, lat.n, NA)                            #! parameter change: 'NA' instead of '-1' to signify entire dimension


# read in the full period of record using aggregated files

data <-var.get.nc(nc, variable = var, start = start, count, unpack=TRUE)    #! argument change: 'variable' instead of 'varid'  
# Output is now a matrix


att.get.nc(nc, "time", "units")
ndims <- file.inq.nc(nc)$ndims
dimnames <- character(ndims)
for(i in seq_len(ndims)) {
  dimnames[i] <- dim.inq.nc(nc, i-1)$name
}

swe_rst <- raster(here::here("data/original/clim_lab/annual_maximum_swe_CTRL.nc"))
swe_rst <- as(swe_rst, "SpatRaster")
plot(swe_rst)