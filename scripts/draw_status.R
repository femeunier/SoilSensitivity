rm(list = ls())

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(dplyr)
library(viridis)
library(tidyr)
library(SoilSensitivity)

system2("rsync",paste("-avz",
                      "hpc:/data/gent/vo/000/gvo00074/felicien/R/status.all.RDS",
                      "/home/femeunier/Documents/projects/SoilSensitivity/outputs/"))

df <- readRDS(file = "/home/femeunier/Documents/projects/SoilSensitivity/outputs/status.all.RDS")

ED_REG_LATMIN = -19.5
ED_REG_LATMAX =  13.5
ED_REG_LONMIN = -84.5
ED_REG_LONMAX = -30.5

GRID_RES = 1

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = df) +
  geom_raster(aes(x=lon, y = lat, fill = as.factor(status)),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-84.5, -30.5), ylim = c(-19.5, 15.5), expand = FALSE) +
  labs(x = "",y = "") +
  facet_wrap(~ scenario) +
  theme_bw()

df %>% group_by(scenario,status) %>% summarise(N = length(lat))
df %>% group_by(status) %>% summarise(N = length(lat))

ggplot(data = df %>% filter(scenario == "SoilGrids_mean")) +
  geom_raster(aes(x=lon, y = lat, fill = final.year),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-84.5, -30.5), ylim = c(-19.5, 15.5), expand = FALSE) +
  labs(x = "",y = "") +
  theme_bw()

df %>% group_by(scenario) %>% summarise(fyear = mean(final.year,na.rm = TRUE),
                                        fyear.m = median(final.year,na.rm = TRUE),
                                        fyear.min = min(final.year,na.rm = TRUE),
                                        fyear.max = max(final.year,na.rm = TRUE))

