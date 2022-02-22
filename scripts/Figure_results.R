rm(list = ls())

library(wesanderson)
library(tidyverse)
library(raster)
library(cowplot)

maps <- readRDS(file = "./outputs/output_maps.RDS")


ref <- stack("./data/AGBavit_SAM.gri")
ED_REG_LATMIN = -19.5
ED_REG_LATMAX =  13.5
ED_REG_LONMIN = -84.5
ED_REG_LONMAX = -30.5

var.in <- as.array(ref)
var <- t(as.matrix(var.in[,,1]))

clons <- seq(extent(ref)[1],extent(ref)[2],res(ref)[1])
clats <- sort(seq(extent(ref)[3],extent(ref)[4],res(ref)[2]),decreasing = TRUE)
var.df <- melt(var) %>% rename(lat = Var2,
                               lon = Var1) %>% mutate(lat = clats[lat],
                                                      lon = clons[lon],
                                                      value = value/20) %>% filter(lat<=ED_REG_LATMAX,lat>=ED_REG_LATMIN,lon<=ED_REG_LONMAX,lon>=ED_REG_LONMIN)

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = var.df %>% filter(value > 0)) +
  geom_raster(aes(x=lon, y = lat, fill = value),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-84.5, -30.5), ylim = c(-19.5, 15.5), expand = FALSE) +
  # scale_fill_gradient2(low = "darkred",mid = "white",high = "#E2725B",na.value = NA) +
  ggtitle("") +
  labs(fill = "AGB (kgC/m²)", x = "",y = "") +
  labs(x = "",y = "")
# theme_bw()

reference <- bind_rows(list(var.df %>% mutate(model = "ORCHIDEE"),
                            var.df %>% mutate(model = "ED2"),
                            var.df %>% mutate(model = "LPJ-GUESS"))) %>%
  mutate(model = factor(model,levels = c("ORCHIDEE","ED2","LPJ-GUESS")))

pal <- wes_palette("Darjeeling1", 3, type = c("discrete"))

ggplot(data = maps  %>% filter(lat<=ED_REG_LATMAX,lat>=ED_REG_LATMIN,lon<=ED_REG_LONMAX,lon>=ED_REG_LONMIN,
                               var == "AGB")) +
  geom_density(aes(x = value,color = scenario)) +
  geom_density(data = reference,
               aes(x = value), color = "black") +
  facet_wrap(~ model) +
  theme_bw() +
  labs(x = "AGB (kgC/m²)", y = "Density", color = "Scenario") +
  scale_color_manual(values = pal) +
  theme(text = element_text(size = 20),
        legend.position = c(0.9,0.85)) +
  guides(fill = FALSE)

ggsave(filename = "./Figures/AGB.density.png",
       plot = last_plot(),width = 30,height = 14, unit = "cm",
       dpi = 300)

ggplot(data = maps  %>% filter(lat<=ED_REG_LATMAX,lat>=ED_REG_LATMIN,lon<=ED_REG_LONMAX,lon>=ED_REG_LONMIN,
                               var == "AGB")) +
  geom_density(aes(x = value,color = scenario),show.legend = FALSE) +
  facet_wrap(~ model) +
  theme_bw() +
  labs(x = "", y = "", color = "Scenario") +
  scale_y_continuous(breaks = c()) +
  scale_color_manual(values = pal) +
  theme(text = element_text(size = 20),
        legend.position = c(0.9,0.85),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  guides(fill = FALSE)

ggsave(filename = "./Figures/AGB.density_nodata.png",
       plot = last_plot(),width = 9,height = 4, unit = "cm",
       dpi = 300)


ggplot(data = maps %>% filter(lat<=ED_REG_LATMAX,lat>=ED_REG_LATMIN,lon<=ED_REG_LONMAX,lon>=ED_REG_LONMIN,
                               var %in% c("sand","clay"))) +
  geom_density(aes(x = 100*value,color = scenario, fill = NA), alpha = 0.2) +
  facet_grid(~ var) +
  theme_bw() +
  labs(x = "Soil fraction (%)", y = "Density", color = "Scenario") +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  theme(text = element_text(size = 20),
        legend.position = c(0.85,0.78)) +
  guides(fill = FALSE, color = FALSE)

ggsave(filename = "./Figures/soil.density.png",
       plot = last_plot(),width = 20,height = 12, unit = "cm",
       dpi = 300)

maps %>% filter(lat<=ED_REG_LATMAX,lat>=ED_REG_LATMIN,lon<=ED_REG_LONMAX,lon>=ED_REG_LONMIN,
                var %in% c("clay")) %>% group_by(scenario) %>% summarise(v.m = mean(value,na.rm = TRUE))


maps2 <- readRDS(file = "./outputs/output_maps2.RDS")

fsw.plot <- ggplot(data = maps2,
       aes(x = scenario , y = fsw,color = scenario)) +
  geom_boxplot() +
  facet_wrap(~ model) +
  labs(x = "", y = "Soil moisture stress(-)") +
  scale_color_manual(values = pal) +
  scale_y_continuous(breaks = seq(0,1,0.25),labels = c()) +
  theme_minimal() +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  coord_flip() +
  guides(color = FALSE)

fsw.plot

ggsave(filename = "./Figures/fsw.png",
       plot = fsw.plot,width = 30,height = 10, unit = "cm",
       dpi = 300)


maps.mod <- maps %>% mutate(value = case_when(var == "clay" & value > 0.3 ~ 0.3,
                                              TRUE ~ value))
ggplot(data = maps  %>% filter(lat<=ED_REG_LATMAX,lat>=ED_REG_LATMIN,lon<=ED_REG_LONMAX,lon>=ED_REG_LONMIN,
                                           var %in% c("clay"), model == "ED2")) +
  geom_raster(aes(x=lon, y = lat, fill = 100*value),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-84.5, -30.5), ylim = c(-19.5, 15.5), expand = FALSE) +
  # scale_fill_gradient2(low = "darkred",mid = "white",high = "#FCE205",na.value = NA) +
  scale_fill_gradient2(low = "darkred",mid = "white",high = "#E2725B",na.value = NA) +
  labs(x = "",y = "") +
  facet_grid(~ scenario) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  labs(x = "",y = "",fill = "Clay content (%)")

ggsave(filename = "./Figures/map_clay.png",
       plot = last_plot(),width = 40,height = 15, unit = "cm",
       dpi = 300)

summary(maps  %>% filter(lat<=ED_REG_LATMAX,lat>=ED_REG_LATMIN,lon<=ED_REG_LONMAX,lon>=ED_REG_LONMIN,
                        var %in% c("sand"), model == "ED2", scenario == "Mean clay") %>%
          mutate(value = 100*value) %>% pull(value))

ggplot(data = maps2,
       aes(x = fsw , y = GPP,color = scenario)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~ model) +
  labs(y = "GPP (kgC/m²/yr)",
       x = "Soil moisture stress(-)",
       color = "Scenario") +
  scale_color_manual(values = pal) +
  theme_bw() +
  theme(text = element_text(size = 20),
        panel.spacing.x = unit(2, "lines"),
        legend.position = c(0.08,0.75))

ggsave(filename = "./Figures/fsw_agb.png",
       plot = last_plot(),width = 40,height = 15, unit = "cm",
       dpi = 300)

df0 <- data.frame(lat = NA,
                  lon = NA,
                  scenario = c("Max. clay","Mean clay","Min. clay"),
                  GPP = 0,
                  fsw = 1)

hull <- bind_rows(list(maps2,
                       df0 %>% mutate(model = "ORCHIDEE"),
                       df0 %>% mutate(model = "ED2"),
                       df0 %>% mutate(model = "LPJ-GUESS")
                       ))%>% group_by(scenario,model) %>% filter(!is.na(fsw)) %>%
  slice(chull(fsw, GPP)) %>% mutate(model = factor(model,levels = c("ORCHIDEE","ED2","LPJ-GUESS")))


triangle <- ggplot(data = maps2,
       aes(x = fsw , y = GPP,color = scenario)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~ model) +
  geom_polygon(data = hull,
               mapping = aes(fill = scenario),alpha = 0.02) +
  labs(y = "GPP (kgC/m²/yr)",
       x = "",
       color = "Scenario") +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  scale_x_continuous(breaks = seq(0,1,0.25)) +
  theme_bw() +
  theme(text = element_text(size = 20),
        panel.spacing.x = unit(2, "lines"),
        legend.position = c(0.08,0.75)) +
  guides(fill = FALSE,point = FALSE, color = FALSE)

triangle

maps2 %>% group_by(scenario,model) %>% summarise(fsw.m = mean(fsw,na.rm = TRUE)) %>% pivot_wider(values_from = fsw.m,
                                                                                                 names_from = scenario) %>%
  mutate(rel.change1 = 100*(`Mean clay` - `Max. clay`)/`Mean clay`,
         rel.change2 = -100*(`Mean clay` - `Min. clay`)/`Mean clay`,
         rel.change3 = -100*(`Max. clay` - `Min. clay`)/`Mean clay`)

maps2 %>% group_by(scenario,model) %>% summarise(gpp.m = mean(GPP,na.rm = TRUE)) %>% pivot_wider(values_from = gpp.m,
                                                                                                 names_from = scenario) %>%
  mutate(rel.change1 = 100*(`Mean clay` - `Max. clay`)/`Mean clay`,
         rel.change2 = -100*(`Mean clay` - `Min. clay`)/`Mean clay`,
         rel.change3 = -100*(`Max. clay` - `Min. clay`)/`Mean clay`)

ggsave(filename = "./Figures/fsw_agb_CH.png",
       plot = triangle,width = 30,height = 10, unit = "cm",
       dpi = 300)


plot_grid(triangle,
          fsw.plot,align = 'hv',ncol = 1,rel_heights = c(3,1))

ggsave(filename = "./Figures/map.Boxplot.png",
       plot = last_plot(),width = 30,height = 20, unit = "cm",
       dpi = 300)


#################################################################################################################################

ggplot(data = maps  %>% filter(lat<=ED_REG_LATMAX,lat>=ED_REG_LATMIN,lon<=ED_REG_LONMAX,lon>=ED_REG_LONMIN,
                               var %in% c("AGB"), scenario == "Mean clay")) +
  geom_raster(aes(x=lon, y = lat, fill = value)) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-84.5, -30.5), ylim = c(-19.5, 15.5), expand = FALSE) +
  # scale_fill_gradient2(low = "darkred",mid = "white",high = "#FCE205",na.value = NA) +
  # scale_fill_gradient2(low = "darkred",mid = "white",high = "#E2725B",na.value = NA) +
  scale_fill_gradient2(low = "darkred",mid = "white",high = "darkgreen",na.value = NA,limits = c(0,30)) +
  labs(x = "",y = "") +
  facet_grid(~ model) +
  theme_bw() +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  labs(x = "",y = "",fill = "AGB (kgC/m²)")

ggsave(filename = "./Figures/maps.plot_spinup.png",
       plot = last_plot(),width = 45,height = 30, unit = "cm",
       dpi = 300)
