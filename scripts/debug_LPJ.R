rm(list = ls())

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(dplyr)
library(viridis)
library(tidyr)
library(SoilSensitivity)
library(oceanmap)
library(ncdf4)

file <- "/home/femeunier/Downloads/1901APR.h5"
nc <- nc_open(file)
lat <- ncvar_get(nc,"lat")
lon <- ncvar_get(nc,"lon")
pres <- ncvar_get(nc,"tmp")

nc_close(nc)

df <- data.frame(lat = as.vector(lat),
                 lon = as.vector(lon),
                 pres = as.vector(pres[1,,])) %>% filter(!is.na(pres))


# file <- "/home/femeunier/Downloads/CRUNCEP.1901.nc"
# nc <- nc_open(file)
#
# lat <- ncvar_get(nc,"latitude")
# lon <- ncvar_get(nc,"longitude")
# pres <- ncvar_get(nc,"northward_wind")
#
# nc_close(nc)
#
# df <- data.frame(lat = sort((rep(lat,nrow(pres))),decreasing = TRUE),
#                  lon = (rep(lon,ncol(pres))),
#                  pres = as.vector(pres[,,1])) %>% filter(!is.na(pres))


world <- ne_countries(scale = "medium", returnclass = "sf")

Cols <- c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")
jet.colors <- colorRampPalette(Cols)

ggplot(data = df) +
  geom_raster(aes(x=lon, y = lat, fill = pres)) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-90, -30), ylim = c(-20, 20), expand = FALSE) +
  labs(x = "",y = "") +
  theme_bw()


##############################################################################################################

rm(list=ls())
library(lubridate)
library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)
library(viridis)
require(ncdf4)

# First download CRUNCEP.1901.nc from the HPC data VO:  data/gent/gvo000/gvo00074/ED_common_data/met/CRUNCEP/
crufile <- "/home/femeunier/Downloads/CRUNCEP.1901_inv.nc"

# Open
nc <- nc_open(crufile)

# Read dimensions
Lon <- ncvar_get(nc, "longitude")
Lat <- ncvar_get(nc, "latitude")

# Fix
# Lat <- sort(ncvar_get(nc, "latitude"),decreasing = TRUE)
time <- ncvar_get(nc, "time")

# Read rainfall
rain.matrix <- ncvar_get(nc, "precipitation_flux")

# Close nc file
nc_close(nc)

# Convert into dataframe. If you know a cleaner way to do this in R, please let me know!
dimnames(rain.matrix) <- list(Lon,Lat,time)
rain    <- as.data.frame.table(rain.matrix,responseName = "Rain")
rain.df <- rain %>% mutate(Lon = as.numeric(as.character(Var1)),
                           Lat = as.numeric(as.character(Var2)),
                           Tim = as.numeric(as.character(Var3)))

# Some additional processing
rain.df %<>% dplyr::select(Lon,Lat,Tim,Rain)
rain.df %<>% mutate(Tim=as.POSIXct(Tim*24*60*60, origin = "1901-01-01", tz="UTC"))

# Convert to MAP, the quick and dirty way
rain.avg <- rain.df %>% group_by(Lon,Lat) %>% summarize(Rain=mean(Rain)*60*60*24*365) %>% ungroup()


library(rnaturalearth)
world <- ne_countries(scale = "medium", returnclass = "sf")


# And plot
ggplot(rain.avg %>% drop_na()) +
  geom_raster(aes(Lon,Lat,fill=Rain)) + scale_fill_viridis() + labs(title="CRU-NCEP drivers mod",fill="MAP") +
  geom_sf(data = world,
              fill = NA) +
  coord_sf(xlim = c(-90, -30), ylim = c(-20, 20), expand = FALSE) +
  theme_bw()
