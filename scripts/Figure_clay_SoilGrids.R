rm(list = ls())

library(SoilSensitivity)
library(lattice)
library(raster)
library(purrr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(rnaturalearth)
library(rnaturalearthdata)

rc <- brick(file.path(".","maps", "soilgrid_top.soc_mean.grd"),expand = TRUE)
# rc <- brick(file.path(".","maps", "soilgrid.gri"),expand = TRUE)

top.sand.mean <- raster::aggregate(rc[[c(1)]],
                                   1 / res(rc)[1] * 0.5,
                                   1 / res(rc)[2] * 0.5,
                                   fun = mean)

map.sand <- top.sand.mean
map.clay <- rc[[2]]

var.in <- as.array(map.sand)
var <- t(as.matrix(map.sand))

clons <- seq(extent(map.sand)[1],extent(map.sand)[2],res(map.sand)[1])
clats <- sort(seq(extent(map.sand)[3],extent(map.sand)[4],res(map.sand)[2]),decreasing = TRUE)
var.df <- melt(var) %>% rename(lat = Var2,
                               lon = Var1) %>% mutate(lat = clats[lat],
                                                      lon = clons[lon],
                                                      value = value)

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = var.df %>% filter(value > 0)) +
  geom_raster(aes(x=lon, y = lat, fill = value*100),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-84.5, -30.5), ylim = c(-19.5, 15.5), expand = FALSE) +
  # scale_fill_gradient2(low = "darkred",mid = "white",high = "#FCE205",na.value = NA) +
  # scale_fill_gradient2(low = "darkred",mid = "white",high = "#E2725B",na.value = NA) +
  scale_fill_gradient2(low = "darkred",mid = "white",high = "black",na.value = NA) +
  ggtitle("SoilGrids map") +
  labs(fill = "SOC", x = "",y = "") +
  theme_bw() +
  labs(x = "",y = "")
  # theme_bw()

ggsave(filename = "./Figures/SOC.png",
       plot = last_plot(),width = 12,height = 7, unit = "cm",
       dpi = 300)

ggplot(data = var.df %>% filter(value > 0)) +
  geom_raster(aes(x=lon, y = lat, fill = value*100),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-84.5, -30.5), ylim = c(-19.5, 15.5), expand = FALSE) +
  # scale_fill_gradient2(low = "darkred",mid = "white",high = "#FCE205",na.value = NA) +
  scale_fill_gradient2(low = "darkred",mid = "white",high = "#E2725B",na.value = NA) +
  ggtitle("") +
  theme_bw()
  labs(fill = "Clay (%)", x = "",y = "") +
  labs(x = "",y = "")
# theme_bw()



