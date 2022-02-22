rm(list = ls())

library(ncdf4)
library(dplyr)
library(reshape2)
library(ggplot2)
library("rnaturalearth")
library("rnaturalearthdata")
library(ggExtra)
library(matlab)
library(RColorBrewer)

ncfile <- "./data/CRUNCEP.1901_inv.nc"
nc <- nc_open(ncfile)
lons <- ncvar_get(nc,"longitude")
lats <- ncvar_get(nc,"latitude")
map <- apply(as.array(ncvar_get(nc,"precipitation_flux")),c(1,2),sum)*3600*6
mat <- apply(as.array(ncvar_get(nc,"air_temperature")),c(1,2),mean) - 273.15

nc_close(nc)

mat.df <- melt(mat) %>% rename(lat = Var2,
                                lon = Var1) %>% mutate(lat = lats[lat],
                                                       lon = lons[lon],
                                                       value = value)

map.df <- melt(map) %>% rename(lat = Var2,
                               lon = Var1) %>% mutate(lat = lats[lat],
                                                      lon = lons[lon],
                                                      value = value)

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_raster(data = mat.df,
              aes(x=lon, y = lat, fill = value),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-84.5, -30.5), ylim = c(-19.5, 15.5), expand = FALSE) +
  scale_fill_gradientn(colours = fliplr(brewer.pal(7,"RdYlGn")),na.value = "white") +
  labs(fill = "MAT (°C)", x = "",y = "") +
  labs(x = "",y = "") +
  theme_bw()

ggsave(filename = "./Figures/MAT.png",
       plot = last_plot(),width = 12,height = 7, unit = "cm",
       dpi = 300)

ggplot() +
  geom_raster(data = map.df,
              aes(x=lon, y = lat, fill = value),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-84.5, -30.5), ylim = c(-19.5, 15.5), expand = FALSE) +
  scale_fill_gradientn(colours = (brewer.pal(7,"Blues")),na.value = "white") +
  labs(fill = "MAP (mm)", x = "",y = "") +
  labs(x = "",y = "") +
  theme_bw()

ggsave(filename = "./Figures/MAP.png",
       plot = last_plot(),width = 12,height = 7, unit = "cm",
       dpi = 300)
