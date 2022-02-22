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
                      "hpc:/data/gent/vo/000/gvo00074/felicien/R/df_SoilSens_analysis.RDS",
                      "/home/femeunier/Documents/projects/SoilSensitivity/outputs/"))

df <- readRDS(file = "/home/femeunier/Documents/projects/SoilSensitivity/outputs/df_SoilSens_analysis.RDS") %>%
  group_by(lat,lon,scenario) %>% mutate(group.id = cur_group_id())

ED_REG_LATMIN = -19.5
ED_REG_LATMAX =  13.5
ED_REG_LONMIN = -84.5
ED_REG_LONMAX = -30.5

GRID_RES = 1

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = df  %>% filter(lat<=ED_REG_LATMAX,lat>=ED_REG_LATMIN,lon<=ED_REG_LONMAX,lon>=ED_REG_LONMIN,
                             year == 2010)) +
  geom_raster(aes(x=lon, y = lat, fill = AGB),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-84.5, -30.5), ylim = c(-19.5, 15.5), expand = FALSE) +
  scale_fill_gradient2(low = "darkred",mid = "darkgrey",high = "darkgreen",na.value = "red") +
  labs(x = "",y = "") +
  facet_wrap(~ scenario) +
  theme_bw()

IDs <- df %>% pull(group.id)


N = 25
ggplot(data = df %>% filter(group.id %in% sample(IDs,N))) +
  geom_line(aes(x = year,y = AGB, group = interaction(scenario,lat,lon),color = scenario)) +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw()


df_wide <- df %>% mutate(scenario = sub(".*\\_", "", scenario)) %>% filter(year == max(df$year)) %>% dplyr::select(-group.id) %>%
  pivot_wider(names_from = scenario,
              values_from = -c(lat,lon,scenario,year))

df_wide <- df_wide %>% mutate(AGB.change = AGB_mean - AGB_min,
                              AGB.change.rel = AGB.change/AGB_mean,
                              LAI.change = LAI_mean - LAI_max,
                              LAI.change.rel = LAI.change/LAI_mean)

ggplot(data = df) +
  geom_density(aes(x = AGB,color = as.factor(scenario))) +
  theme_bw()

ggplot(data = df) +
  geom_density(aes(x = clay,color = as.factor(scenario))) +
  # geom_density(aes(x = sand,color = as.factor(scenario)),lty = 2) +
  # geom_density(aes(x = 1-clay-sand,color = as.factor(scenario)),lty = 3) +
  theme_bw()


ggplot(data = df_wide) +
  geom_point(aes(x = (clay_max - clay_min)/clay_max, y = (LAI_max - LAI_min)),color = 'black') +
  geom_point(aes(x = (clay_max - clay_min)/clay_max, y = (LAI_max - LAI_mean)),color = 'red') +
  theme_bw()


ggplot(data = df_wide) +
  geom_point(aes(x = (clay_mean - clay_min), y = 100*(AGB_mean - AGB_min)/AGB_mean),color = 'black') +
  geom_point(aes(x = (clay_mean - clay_max), y = 100*(AGB_mean - AGB_max)/AGB_mean),color = 'red') +
  geom_point(aes(x = (clay_mean - clay_min), y = 100*(LAI_mean - LAI_min)/LAI_mean),color = 'black', shape = 2) +
  geom_point(aes(x = (clay_mean - clay_max), y = 100*(LAI_mean - LAI_max)/LAI_mean),color = 'red', shape = 2) +
  theme_bw()


ggplot(data = df_wide) +
  geom_point(aes(x = clay_mean - clay_min, y = LAI_mean - LAI_min),color = 'black') +
  geom_point(aes(x = clay_mean - clay_max, y = LAI_mean - LAI_max),color = 'red') +
  theme_bw()

ggplot(data = df_wide) +
  geom_point(aes(x = clay_mean - clay_min, y = AGB_mean - AGB_min),color = 'blue') +
  geom_point(aes(x = clay_mean - clay_max, y = AGB_mean - AGB_max),color = 'red') +
  # geom_hline(yintercept = c(mean(df_wide$AGB_mean),
  #                           mean(df_wide$AGB_min),
  #                           mean(df_wide$AGB_max)),color = c("black","blue","red")) +
  geom_hline(yintercept = c(mean(df_wide$AGB_mean - mean(df_wide$AGB_min)),
                            mean(df_wide$AGB_mean - mean(df_wide$AGB_max))),color = c("blue","red")) +
  theme_bw()

