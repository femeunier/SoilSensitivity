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
                      "hpc:/data/gent/vo/000/gvo00074/felicien/R/df_SoilSens_dyn.RDS",
                      "/home/femeunier/Documents/projects/SoilSensitivity/outputs/"))

df <- readRDS(file = "/home/femeunier/Documents/projects/SoilSensitivity/outputs/df_SoilSens_dyn.RDS") %>%
  group_by(lat,lon) %>% mutate(group.id = group_indices())

ids <- unique(df$group.id)
N = min(100,length(ids))

ggplot(data = df %>% filter(group.id %in% sample(ids,N))) +
  geom_line(aes(x = year, y = AGB, color = as.factor(scenario),group = interaction(scenario,group.id))) +
  theme_bw()


ED_REG_LATMIN = -19.5
ED_REG_LATMAX =  13.5
ED_REG_LONMIN = -84.5
ED_REG_LONMAX = -30.5

GRID_RES = 1

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = df  %>% filter(lat<=ED_REG_LATMAX,lat>=ED_REG_LATMIN,lon<=ED_REG_LONMAX,lon>=ED_REG_LONMIN,
                             scenario == "SoilGrids_min",year == 2010)) +
  geom_tile(aes(x=lon, y = lat, fill = AGB),width = 1,height = 1,alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-84.5, -30.5), ylim = c(-19.5, 15.5), expand = FALSE) +
  # scale_fill_gradient2(low = "darkred",
  #                      mid = "darkgrey",
  #                      high = "darkgreen",
  #                      na.value = "red") +
  labs(x = "",y = "") +
  theme_bw()

ggplot(data = df %>% filter(year == 1950)) +
  geom_density(aes(x = LAI,color = as.factor(scenario))) +
  theme_bw()

df.wide <- df %>% pivot_wider(names_from = c(scenario),
                              values_from = c(AGB,AGB.tree,LAI,LAI.tree,clay,sand)) %>% filter(year == 1950)

ggplot(data = df.wide  %>% filter(lat<=ED_REG_LATMAX,lat>=ED_REG_LATMIN,lon<=ED_REG_LONMAX,lon>=ED_REG_LONMIN)) +
  geom_raster(aes(x=lon, y = lat, fill = (clay_SoilGrids_max - clay_SoilGrids_min)),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-84.5, -30.5), ylim = c(-19.5, 15.5), expand = FALSE) +
  labs(x = "",y = "") +
  theme_bw()
