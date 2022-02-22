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
library(reshape2)
library(cowplot)
library(quantreg)

ED_REG_LATMIN = -19.5
ED_REG_LATMAX =  13.5
ED_REG_LONMIN = -84.5
ED_REG_LONMAX = -30.5

all.models <- readRDS("./outputs/OPhistorical.RDS")

###############################################################################################################################################################
# Soil maps
df.soil <- readRDS(file = "/home/femeunier/Documents/projects/SoilSensitivity/outputs/df_SoilSens_analysis.RDS") %>% dplyr::select(scenario,lat,lon,sand,clay)

df.all <- all.models %>% left_join(df.soil,
                                   by = c("scenario","lat","lon")) %>% filter(!is.na(sand))

###############################################################################################################################################################

pal <- wes_palette("Darjeeling1", 3, type = c("discrete"))

ref <- stack("./data/AGBavit_SAM.gri")

var.in <- as.array(ref)
var <- t(as.matrix(var.in[,,1]))

clons <- seq(extent(ref)[1],extent(ref)[2],res(ref)[1])
clats <- sort(seq(extent(ref)[3],extent(ref)[4],res(ref)[2]),decreasing = TRUE)
var.df <- melt(var) %>% rename(lat = Var2,
                               lon = Var1) %>% mutate(lat = clats[lat],
                                                      lon = clons[lon],
                                                      value = value/20) %>% filter(lat<=ED_REG_LATMAX,lat>=ED_REG_LATMIN,
                                                                                   lon<=ED_REG_LONMAX,lon>=ED_REG_LONMIN)
reference <- bind_rows(list(var.df %>% mutate(model = "ORCHIDEE"),
                            var.df %>% mutate(model = "ED2"),
                            var.df %>% mutate(model = "LPJ-GUESS"))) %>%
  mutate(model = factor(model,levels = c("ORCHIDEE","ED2","LPJ-GUESS")))


###################################################################################################################################
# AGB density distribution

world <- ne_countries(scale = "medium", returnclass = "sf")

all.models2plot <- bind_rows(list(all.models,
                                  var.df %>% mutate(variable = "AGB",
                                                    model = "Data",
                                                    scenario = "SoilGrids_mean")))
all.models2plot$model <- factor(all.models2plot$model,levels = c("Data","ORCHIDEE","ED2","LPJ-GUESS"))
all.models2plot <- all.models2plot %>% mutate(value = case_when(variable == "AGB" & value >= 30 ~ 30,
                                                                TRUE ~ value))

all.models2plotlow <- all.models2plot %>% filter(model == "ED2",
                                                 scenario == "SoilGrids_mean",
                                                 variable == "AGB")

for (cmodel in c("ORCHIDEE","Data","LPJ-GUESS")){

  ORC <- all.models2plot %>% filter(model == cmodel,
                                    scenario == "SoilGrids_mean",
                                    variable == "AGB") %>% ungroup() %>% dplyr::select(lon,lat,value)

  dfr <- rasterFromXYZ(ORC)
  dfrlow <- aggregate(dfr,fact = 2)

  all.models2plotlow <- bind_rows(list(all.models2plotlow,
                                       as.data.frame(dfrlow,xy = TRUE) %>% rename(lon = x,lat = y) %>% mutate(model = cmodel)))
}


all.models2plotlow$model = factor(all.models2plotlow$model,levels = c("Data","ORCHIDEE","ED2","LPJ-GUESS"))

ggplot.map1 <- ggplot(data = all.models2plotlow  %>% filter(lat<=ED_REG_LATMAX,lat>=ED_REG_LATMIN,lon<=ED_REG_LONMAX,lon>=ED_REG_LONMIN,
                                                            model == "Data")) +
  geom_raster(aes(x=lon, y = lat, fill = value)) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-84.5, -25.5), ylim = c(-19.5, 18.5), expand = FALSE) +
  scale_fill_gradient2(low = "darkred",mid = "white",high = "darkgreen",na.value = NA,limits = c(0,30)) +
  ggtitle("") +
  labs(fill = "AGB (kgC/m²)", x = "",y = "") +
  labs(x = "",y = "") +
  facet_wrap(~ model,nrow = 1) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())

ggplot.map1

ggsave(filename = "./Figures/maps.plot.png",
       plot = last_plot(),width = 15,height = 10, unit = "cm",
       dpi = 300)


ggplot.map2 <- ggplot(data = all.models2plotlow  %>% filter(lat<=ED_REG_LATMAX,lat>=ED_REG_LATMIN,lon<=ED_REG_LONMAX,lon>=ED_REG_LONMIN,
                                                            model != "Data")) +
  geom_raster(aes(x=lon, y = lat, fill = value)) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-84.5, -25.5), ylim = c(-19.5, 18.5), expand = FALSE) +
  scale_fill_gradient2(low = "darkred",mid = "white",high = "darkgreen",na.value = NA) +
  ggtitle("") +
  labs(fill = "AGB (kgC/m²)", x = "",y = "") +
  labs(x = "",y = "") +
  facet_wrap(~ model,nrow = 1) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())

ggplot.map2

all.models2plotlow %>% group_by(model) %>% summarise(m = mean(value,na.rm = TRUE),
                                                     std = sd(value,na.rm = TRUE))

all.models2plotlow %>% mutate(type = case_when(value <= 8 ~ "non-forest",
                                               TRUE ~ "forest")) %>%
  group_by(model,type) %>% summarise(m = mean(value,na.rm = TRUE),
                                     std = sd(value,na.rm = TRUE)) %>%
  arrange(type)

ggsave(filename = "./Figures/maps.plot2.png",
       plot = last_plot(),width = 30,height = 20, unit = "cm",
       dpi = 300)


ggplot(data = var.df) +
  geom_density(aes(x = value)) +
  scale_x_continuous(limits=c(0,40)) +
  scale_y_continuous(limits=c(0,0.15)) +
  theme_bw() +
  labs(x = "AGB (kgC/m²)", y = "Density", color = "Scenario") +
  scale_color_manual(values = pal) +
  theme(legend.position = c(0.9,0.85),
        text = element_text(size = 24),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(color = FALSE)


var.df %>% mutate(type = case_when(value <= 8 ~ "non-forest",
                                   TRUE ~ "forest")) %>%
  group_by(type) %>% summarise(m = mean(value,na.rm = TRUE),
                               std = sd(value,na.rm = TRUE),
                               .groups = "keep")


ggsave(filename = "./Figures/density.Avitabile.png",
       plot = last_plot(),width = 7.5,height = 6, unit = "cm",
       dpi = 300)

ggplot(data = all.models2plot  %>% filter(lat<=ED_REG_LATMAX,lat>=ED_REG_LATMIN,lon<=ED_REG_LONMAX,lon>=ED_REG_LONMIN,
                                          variable == "AGB",
                                          scenario == "SoilGrids_mean")) +
  geom_raster(aes(x=lon, y = lat, fill = value),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-84.5, -25.5), ylim = c(-19.5, 18.5), expand = FALSE) +
  scale_fill_gradient2(low = "darkred",mid = "white",high = "darkgreen",na.value = NA) +
  ggtitle("") +
  labs(fill = "AGB (kgC/m²)", x = "",y = "") +
  labs(x = "",y = "") +
  facet_wrap(~ model,nrow = 1) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())



ggplot(data = all.models  %>% filter(lat<=ED_REG_LATMAX,lat>=ED_REG_LATMIN,lon<=ED_REG_LONMAX,lon>=ED_REG_LONMIN,
                                     variable == "AGB")) +
  stat_ecdf(aes(x = value,color = scenario),
            geom = "step") +
  stat_ecdf(data = reference,
            aes(x = value), color = "black",
            geom = "step") +
  facet_wrap(~ model) +
  theme_bw() +
  labs(x = "AGB (kgC/m²)", y = "Density", color = "Scenario") +
  scale_color_manual(values = pal) +
  theme(text = element_text(size = 20),
        legend.position = c(0.9,0.2)) +
  guides(fill = FALSE)

all.models  %>% filter(lat<=ED_REG_LATMAX,lat>=ED_REG_LATMIN,lon<=ED_REG_LONMAX,lon>=ED_REG_LONMIN,
                       variable == "AGB") %>%
  group_by(scenario,model) %>% summarise(value.m = mean(value,na.rm = TRUE),
                                        .groups = "keep") %>%
  pivot_wider(values_from = value.m,
              names_from = scenario) %>%
  mutate(diff.rel = 100*(SoilGrids_min - SoilGrids_max)/SoilGrids_min)

###################################################################################################################################
# Fsw boxplots

df.all.wide <- df.all %>% pivot_wider(names_from = "variable",
                                      values_from = "value")

ggplot(data = df.all.wide) +
  geom_boxplot(aes(x = scenario,y = fsw,color = scenario)) +
  facet_wrap(~ model) +
  theme_bw()

###################################################################################################################################
#


df2plot <- df.all.wide %>% group_by(model,scenario) %>% mutate(rel.GPP = GPP/max(GPP),
                                                               rel.AGB = AGB/max(AGB))

pal <- wes_palette("Darjeeling1", 3, type = c("discrete"))

df2plot$model <- factor(df2plot$model,levels = c("ORCHIDEE","ED2","LPJ-GUESS"))

df0 <- data.frame(lat = NA,
                  lon = NA,
                  scenario = c("SoilGrids_max","SoilGrids_mean","SoilGrids_min"),
                  GPP = 0,
                  fsw = 1)

# Convex hull
hull <- bind_rows(list(df2plot,
                       df0 %>% mutate(model = "ORCHIDEE"),
                       df0 %>% mutate(model = "ED2"),
                       df0 %>% mutate(model = "LPJ-GUESS"))) %>% group_by(scenario,model) %>% filter(!is.na(fsw)) %>%
  slice(chull(fsw, GPP)) %>% mutate(model = factor(model,levels = c("ORCHIDEE","ED2","LPJ-GUESS")))


fsws <- seq(0,1,length.out = 1000)

tau = 0.95
multi_rqfit <- df2plot %>% group_by(model,scenario) %>% summarise(rq = pmax(0,coef(rq(GPP ~ fsw,tau = c(tau)))[1] + coef(rq(GPP ~ fsw,tau = c(tau)))[2] * fsws),
                                                                  .groups = 'keep') %>%
  mutate(fsw = fsws)



triangle <-
  ggplot(data = df2plot,
       aes(x = fsw,y = GPP,color = scenario)) +
  geom_point(alpha = 0.4) +
  facet_wrap(~ model) +
  # geom_polygon(data = hull,
  #              mapping = aes(fill = scenario),alpha = 0.02) +
  geom_line(data = multi_rqfit,
            mapping = aes(x = fsw, y = rq, color = scenario)) +
  labs(y = "GPP (kgC/m²/yr)",
       x = "",
       color = "Scenario") +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  scale_x_continuous(breaks = seq(0,1,0.25)) +
  scale_y_continuous(limits = c(0,8)) +
  theme_bw() +
  theme(text = element_text(size = 20),
        panel.spacing.x = unit(2, "lines"),
        legend.position = c(0.08,0.75),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  guides(fill = FALSE,point = FALSE, color = FALSE)


df2plot$scenario <- factor(df2plot$scenario)
levels(df2plot$scenario) <- c("Max. clay","Mean clay","Min. clay")


fsw.plot <- ggplot(data = df2plot,
       aes(x = scenario , y = fsw,color = scenario)) +
  geom_boxplot() +
  facet_wrap(~ model) +
  labs(x = "", y = "Soil moisture stress (-)") +
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

plot_grid(triangle,
          fsw.plot,align = 'hv',ncol = 1,rel_heights = c(3,1))

ggsave(filename = "./Figures/map.Boxplot.png",
       plot = last_plot(),width = 30,height = 20, unit = "cm",
       dpi = 300)


file <- "/home/femeunier/Documents/projects/SoilSensitivity/data/SAM_modis_mean.gri"
data <- brick(file)
plot(data)

ED_REG_LATMIN = -19.5
ED_REG_LATMAX =  13.5
ED_REG_LONMIN = -84.5
ED_REG_LONMAX = -30.5

data.df <- as.data.frame(data,xy = TRUE)  %>%
  filter(y<=ED_REG_LATMAX,y>=ED_REG_LATMIN,x<=ED_REG_LONMAX,x>=ED_REG_LONMIN) %>% mutate(GPP = 365*index_3)

# boxplot(data.df$value)

df2plot.data <- bind_rows(list(df2plot %>% dplyr::select(scenario,model,GPP),
                               data.df %>% dplyr::select(GPP) %>% mutate(scenario = "MODIS",
                                                                         model = "ORCHIDEE"),
                               data.df %>% dplyr::select(GPP) %>% mutate(scenario = "MODIS",
                                                                         model = "ED2"),
                               data.df %>% dplyr::select(GPP) %>% mutate(scenario = "MODIS",
                                                                         model = "LPJ-GUESS")))

df2plot.data$scenario <- factor(df2plot.data$scenario,levels = c("Max. clay","Mean clay","Min. clay","MODIS"))
df2plot.data$model <- factor(df2plot.data$model,levels = c("ORCHIDEE","ED2","LPJ-GUESS"))


GPPplot <- ggplot(data = df2plot.data,
       aes(x = scenario , y = GPP,color = scenario)) +
  geom_boxplot() +
  facet_wrap(~ model) +
  labs(x = "", y = "") +
  scale_color_manual(values = c(pal,"black"),labels = c()) +
  scale_x_discrete(labels = c()) +
  scale_y_continuous(labels = c()) +
  theme_minimal() +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(color = FALSE)

df2plot.data %>% mutate(scenario = as.character(scenario)) %>%
  mutate(scenario.type = case_when(scenario == "MODIS" ~ "data",
                                   TRUE ~ "model")) %>%
  group_by(model,scenario.type) %>%
  summarise(m = mean(GPP,na.rm = TRUE),
            std = sd(GPP,na.rm = TRUE))

ggsave(filename = "./Figures/Boxplot.GPP.png",
       plot = last_plot(),width = 20,height = 20, unit = "cm",
       dpi = 300)
