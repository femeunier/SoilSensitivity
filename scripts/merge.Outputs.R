rm(list = ls())

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(dplyr)
library(viridis)
library(tidyr)
library(SoilSensitivity)
library(raster)
library(wesanderson)

ED_REG_LATMIN = -19.5
ED_REG_LATMAX =  13.5
ED_REG_LONMIN = -84.5
ED_REG_LONMAX = -30.5

GRID_RES = 1

world <- ne_countries(scale = "medium", returnclass = "sf")

###########################################################################################################################################
# ORCHIDEE

system2("rsync",paste("-avz",
                      "hpc:/data/gent/vo/000/gvo00074/felicien/R/Outputs_ORCHIDEE.RDS",
                      "/home/femeunier/Documents/projects/SoilSensitivity/outputs/"))

tempDF <- readRDS(file = "/home/femeunier/Documents/projects/SoilSensitivity/outputs/Outputs_ORCHIDEE.RDS")
veg.frac <- tempDF %>% filter(variable == "veg.cover")
other.vars <- tempDF %>% filter(variable != "veg.cover")

all.vars <- other.vars %>% left_join(veg.frac %>% rename(veg_cover = value) %>% dplyr::select(lon,lat,pft,month,veg_cover,year,scenario),
                                     by = c("lon","lat","pft","month","year","scenario"))

ORCHIDEE <- all.vars %>%
  group_by(lon,lat,scenario,pft,variable) %>%
  summarise(value = mean(value),
            veg_cover = mean(veg_cover),
            .groups = "keep") %>%
  group_by(lon,lat,scenario,variable) %>%
  summarise(value = weighted.mean(value,veg_cover),
            .groups = "keep") %>%
  filter(lat<=ED_REG_LATMAX,
         lat>=ED_REG_LATMIN,
         lon<=ED_REG_LONMAX,
         lon>=ED_REG_LONMIN)

# ggplot(data = ORCHIDEE  %>% filter(variable == "AGB")) +
#   geom_raster(aes(x=lon, y = lat, fill = value),alpha = 0.3) +
#   geom_sf(data = world,fill = NA) +
#   coord_sf(xlim = c(-84.5, -30.5), ylim = c(-19.5, 15.5), expand = FALSE) +
#   scale_fill_gradient2(low = "darkred",mid = "darkgrey",high = "darkgreen",na.value = "white") +
#   labs(x = "",y = "") +
#   facet_wrap(~ scenario) +
#   theme_bw()


###########################################################################################################################################
# LPJ

system2("rsync",paste("-avz",
                      "hpc:/data/gent/vo/000/gvo00074/felicien/R/Outputs_LPJ.RDS",
                      "/home/femeunier/Documents/projects/SoilSensitivity/outputs/"))

LPJ <- readRDS(file = "/home/femeunier/Documents/projects/SoilSensitivity/outputs/Outputs_LPJ.RDS") %>%
  filter(lat<=ED_REG_LATMAX,
         lat>=ED_REG_LATMIN,
         lon<=ED_REG_LONMAX,
         lon>=ED_REG_LONMIN)

# ggplot(data = LPJ  %>% filter(variable == "AGB")) +
#   geom_raster(aes(x=lon, y = lat, fill = value),alpha = 0.3) +
#   geom_sf(data = world,fill = NA) +
#   coord_sf(xlim = c(-84.5, -30.5), ylim = c(-19.5, 15.5), expand = FALSE) +
#   scale_fill_gradient2(low = "darkred",mid = "darkgrey",high = "darkgreen",na.value = "white") +
#   labs(x = "",y = "") +
#   facet_wrap(~ scenario) +
#   theme_bw()

###########################################################################################################################################
# ED2

system2("rsync",paste("-avz",
                      "hpc:/data/gent/vo/000/gvo00074/felicien/R/Outputs_ED2.RDS",
                      "/home/femeunier/Documents/projects/SoilSensitivity/outputs/"))

ED2 <- readRDS(file = "/home/femeunier/Documents/projects/SoilSensitivity/outputs/Outputs_ED2.RDS") %>%
  dplyr::select("lat","lon","scenario","GPP","NPP","LAI","biomass","fsw","AGB","soilC") %>% pivot_longer(cols = -c("lat","lon","scenario"),
                                                                                                 names_to = "variable",
                                                                                                 values_to = "value")

# ggplot(data = ED2  %>% filter(variable == "AGB")) +
#   geom_raster(aes(x=lon, y = lat, fill = value),alpha = 0.3) +
#   geom_sf(data = world,fill = NA) +
#   coord_sf(xlim = c(-84.5, -30.5), ylim = c(-19.5, 15.5), expand = FALSE) +
#   scale_fill_gradient2(low = "darkred",mid = "darkgrey",high = "darkgreen",na.value = "white") +
#   labs(x = "",y = "") +
#   facet_wrap(~ scenario) +
#   theme_bw()


###########################################################################################################################################
# All

all.models <- bind_rows(list(ORCHIDEE %>% mutate(model = "ORCHIDEE") %>% mutate(lat = lat + 0.25,
                                                                                lon = lon - 0.25),
                             ED2 %>% mutate(model = "ED2"),
                             LPJ %>% mutate(model = "LPJ-GUESS")))

ggplot(data = all.models  %>% filter(variable == "soilC")) +
  geom_raster(aes(x=lon, y = lat, fill = value)) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-84.5, -30.5), ylim = c(-19.5, 15.5), expand = FALSE) +
  scale_fill_gradient2(low = "darkred",mid = "darkgrey",high = "darkgreen",na.value = "white") +
  labs(x = "",y = "") +
  facet_grid(model ~ scenario) +
  theme_bw()


saveRDS(all.models,file = "./outputs/OPhistorical.RDS")


ggplot(data = all.models  %>% filter(variable == "soilC")) +
  geom_boxplot(aes(y = value, x = model, fill = scenario)) +
  theme_bw()

all.models %>% filter(variable == "soilC") %>% group_by(model, scenario) %>% summarise(median(value,na.rm = TRUE))
