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

# Models
all.models <- readRDS(file = "./outputs/OPhistorical.RDS")
SoilC <- all.models  %>% filter(variable == "soilC")

threshold <- 30

# data
rc <- raster("./maps/soilgrid_ocs.grd")
data <- raster::aggregate(rc,
                          1 / res(rc)[1],
                          1 / res(rc)[2],fun = mean)
df.data <- as.data.frame(data,xy = TRUE) %>% rename(lon = x,lat = y) %>% mutate(model = "SoilGrids") %>% rename(value = ocs_0.30) %>% filter(value > 0)

pal <- wes_palette("Darjeeling1", 3, type = c("discrete"))

ED_REG_LATMIN = -19.5
ED_REG_LATMAX =  13.5
ED_REG_LONMIN = -84.5
ED_REG_LONMAX = -30.5

GRID_RES = 1

world <- ne_countries(scale = "medium", returnclass = "sf")


# Aggregate
all.models2plotlow <- SoilC %>% filter(model == "ED2",
                                           scenario == "SoilGrids_mean")

all.models2plot  <- SoilC %>% filter(model == "ED2")

for (cmodel in c("ORCHIDEE","LPJ-GUESS")){

  for (cscenar in c("SoilGrids_min","SoilGrids_max","SoilGrids_mean")){
    ORC <- SoilC %>% filter(model == cmodel,
                                scenario == cscenar) %>% ungroup() %>% dplyr::select(lon,lat,value)
    dfr <- rasterFromXYZ(ORC)
    dfrlow <- aggregate(dfr,fact = 2)
    all.models2plot <- bind_rows(list(all.models2plot,
                                      as.data.frame(dfrlow,xy = TRUE) %>% rename(lon = x,lat = y) %>% mutate(model = cmodel,
                                                                                                             scenario = cscenar)))
  }

  dfr <- rasterFromXYZ(ORC)
  dfrlow <- aggregate(dfr,fact = 2)

  all.models2plotlow <- bind_rows(list(all.models2plotlow,
                                       as.data.frame(dfrlow,xy = TRUE) %>% rename(lon = x,lat = y) %>% mutate(model = cmodel)))

}

all.models2plotlow <- all.models2plotlow %>% mutate(model = factor(model,levels = c("ORCHIDEE","ED2","LPJ-GUESS"))) %>%
  mutate(value = case_when(value >= threshold ~ threshold,
                           TRUE ~ value))
all.models2plot <- all.models2plot %>% mutate(model = factor(model,levels = c("ORCHIDEE","ED2","LPJ-GUESS")))

ggplot(data = df.data) +
  geom_raster(aes(x=lon, y = lat, fill = value)) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-84.5, -25.5), ylim = c(-19.5, 18.5), expand = FALSE) +
  scale_fill_gradient2(low = "darkred",mid = "white",high = "darkgreen",na.value = "transparent",
                       limits = c(0,threshold)) +
  ggtitle("") +
  labs(fill = "Soil Carbon (kgC/m²)", x = "",y = "") +
  theme_bw()

ggsave(filename = "./Figures/map.SoilGrids.png",
       plot = last_plot(),width = 15,height = 6, unit = "cm",
       dpi = 300)


ggplot(data = all.models2plotlow) +
  geom_raster(aes(x=lon, y = lat, fill = value)) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-84.5, -25.5), ylim = c(-19.5, 18.5), expand = FALSE) +
  scale_fill_gradient2(low = "darkred",mid = "white",high = "darkgreen",na.value = "transparent",
                       limits = c(0,threshold)) +
  ggtitle("") +
  labs(fill = "Soil Carbon (kgC/m²)", x = "",y = "") +
  facet_wrap(~ model,nrow = 1) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())

ggsave(filename = "./Figures/map.model.png",
       plot = last_plot(),width = 22.5,height = 6, unit = "cm",
       dpi = 300)


SoilC.data <- bind_rows(list(all.models2plot,
                        df.data %>% mutate(model = "ORCHIDEE",
                                           scenario = "Data"),
                        df.data %>% mutate(model = "ED2",
                                           scenario = "Data"),
                        df.data %>% mutate(model = "LPJ-GUESS",
                                           scenario = "Data"))) %>% mutate(model = factor(model,levels = c("ORCHIDEE","ED2","LPJ-GUESS")))
ggplot(data = SoilC.data) +
  geom_density(aes(x = value, color = scenario), fill = NA) +
  scale_colour_manual(values = c("black",pal)) +
  scale_x_continuous(limits = c(0,40)) +
  facet_wrap(~ model) +
  theme_bw() +
  labs(x = "Soil Carbon (kgC/m²)", y = "Density", color = "Scenario") +
  theme(text = element_text(size = 24),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(color = FALSE)

SoilC.data %>% filter(scenario != "Data") %>%
  group_by(scenario,model) %>%
  summarise(value.m = mean(value, na.rm = TRUE),
            .groups = "keep") %>%
  pivot_wider(values_from = value.m,
              names_from = scenario) %>%
  mutate(diff.rel = -100*(SoilGrids_max - SoilGrids_min)/SoilGrids_min)

SoilC.data %>% mutate(scenario = as.character(scenario)) %>%
  mutate(scenario.type = case_when(scenario == "Data" ~ "data",
                                   TRUE ~ "model")) %>%
  group_by(model,scenario.type) %>%
  summarise(m = mean(value,na.rm = TRUE),
            std = sd(value,na.rm = TRUE))

ggsave(filename = "./Figures/density.SoilGrids_model.png",
       plot = last_plot(),width = 22.5,height = 6, unit = "cm",
       dpi = 300)

ggplot(data = SoilC.data %>% filter(scenario == "Data", model == "ED2")) +
  geom_density(aes(x = value, color = scenario), fill = NA) +
  scale_colour_manual(values = c("black")) +
  scale_x_continuous(limits = c(0,40)) +
  facet_wrap(~ model) +
  theme_bw() +
  labs(x = "Soil Carbon (kgC/m²)", y = "Density", color = "Scenario") +
  theme(legend.position = c(0.9,0.85),
        text = element_text(size = 24),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(color = FALSE)

ggsave(filename = "./Figures/density.SoilGrids.png",
       plot = last_plot(),width = 7.5,height = 6, unit = "cm",
       dpi = 300)
