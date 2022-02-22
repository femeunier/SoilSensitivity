rm(list = ls())

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(dplyr)
library(viridis)
library(tidyr)
library(SoilSensitivity)

ED_REG_LATMIN = -19.5
ED_REG_LATMAX =  13.5
ED_REG_LONMIN = -84.5
ED_REG_LONMAX = -30.5

GRID_RES = 1

world <- ne_countries(scale = "medium", returnclass = "sf")

system2("rsync",paste("-avz",
                      "hpc:/data/gent/vo/000/gvo00074/felicien/R/Outputs_ORCHIDEE.RDS",
                      "/home/femeunier/Documents/projects/SoilSensitivity/outputs/"))

tempDF <- readRDS(file = "/home/femeunier/Documents/projects/SoilSensitivity/outputs/Outputs_ORCHIDEE.RDS")
veg.frac <- tempDF %>% filter(variable == "veg.cover")
other.vars <- tempDF %>% filter(variable != "veg.cover")

all.vars <- other.vars %>% left_join(veg.frac %>% rename(veg_cover = value) %>% dplyr::select(lon,lat,pft,month,veg_cover,year,scenario),
                                     by = c("lon","lat","pft","month","year","scenario"))

df <- all.vars %>%
  filter(month == 1,
         variable == "AGB") %>%
  group_by(lon,lat,month,scenario) %>%
  summarise(value = sum(value*veg_cover),
            .groups = "keep")

ggplot(data = df  %>% filter(lat<=ED_REG_LATMAX,lat>=ED_REG_LATMIN,lon<=ED_REG_LONMAX,lon>=ED_REG_LONMIN)) +
  geom_raster(aes(x=lon, y = lat, fill = value),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-84.5, -30.5), ylim = c(-19.5, 15.5), expand = FALSE) +
  scale_fill_gradient2(low = "darkred",mid = "darkgrey",high = "darkgreen",na.value = "white") +
  labs(x = "",y = "") +
  facet_wrap(~ scenario) +
  theme_bw()
