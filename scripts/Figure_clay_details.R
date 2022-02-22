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
library(wesanderson)
library(cowplot)

rc <- brick(file.path(".","maps", "soilgrid.grd"),expand = TRUE)

top.sand.mean <- raster::aggregate(rc[[c(2)]],
                                   1 / res(rc)[1] * 0.5,
                                  fun = mean)

map.sand <- top.sand.mean
map.clay <- rc[[2]]

df.clay <- data.frame(clay = as.vector(map.clay),
                      sand = as.vector( rc[[1]]))

df.clay.NA <- df.clay %>% mutate(clay = case_when(clay == 0 & sand == 0 ~ NA_real_,
                                                  TRUE ~ clay),
                                 sand = case_when(is.na(clay)  ~ NA_real_,
                                                  TRUE ~ sand)) %>% filter(!is.na(clay))
summary(df.clay.NA$clay)

df.clay.all <- bind_rows(list(data.frame(clay = as.vector( raster::aggregate(rc[[c(2)]],
                                                               1 / res(rc)[1] * 0.5,
                                                               fun = mean)),
                                         sand = as.vector( raster::aggregate(rc[[c(1)]],
                                                                             1 / res(rc)[1] * 0.5,
                                                                             fun = mean))) %>% mutate(type = 'mean'),
                              data.frame(clay = as.vector( aggregates(rc[[c(2)]],
                                                                      rc[[c(2)]],
                                                                      1 / res(rc)[1] * 0.5,
                                                                      1 / res(rc)[2] * 0.5,
                                                              probs = 0.01)),
                                         sand = as.vector( aggregates(rc[[c(2)]],
                                                                      rc[[c(1)]],
                                                                      1 / res(rc)[1] * 0.5,
                                                                      1 / res(rc)[2] * 0.5,
                                                                      probs = 0.01)))  %>% mutate(type = 'min'),
                              data.frame(clay = as.vector( aggregates(rc[[c(2)]],
                                                                      rc[[c(2)]],
                                                                      1 / res(rc)[1] * 0.5,
                                                                      1 / res(rc)[2] * 0.5,
                                                                      probs = 0.99)),
                                         sand = as.vector( aggregates(rc[[c(2)]],
                                                                      rc[[c(1)]],
                                                                      1 / res(rc)[1] * 0.5,
                                                                      1 / res(rc)[2] * 0.5,
                                                                      probs = 0.99))) %>% mutate(type = 'max')))


df.clay.all <- df.clay.all %>% group_by(type) %>% mutate(N = 1:length(sand))
sea.mask <- df.clay.all %>% filter(type == "mean",
                                   clay == 0, sand == 0) %>% pull(N)

pal <- wes_palette("Darjeeling1", 3, type = c("discrete"))

clay.dis <- ggplot() +
  geom_density(data = df.clay.all %>% filter(!(N %in% sea.mask)),
               aes(x = clay*100,
                   color = type)) +
  geom_density(data = df.clay %>% filter(!(sand == 0 & clay == 0)),
               aes(x = clay*100),
               color = "black") +
  scale_color_manual(values = pal) +
  labs(x = "Clay fraction (%)",y = "Density") +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  guides(color = "none")


sand.dis <- ggplot() +
  geom_density(data = df.clay.all %>% filter(!(N %in% sea.mask)),
               aes(x = sand*100,
                   color = type)) +
  geom_density(data = df.clay %>% filter(!(sand == 0 & clay == 0)),
               aes(x = sand*100),
               color = "black") +
  scale_color_manual(values = pal) +
  labs(x = "Sand fraction (%)",y = "") +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  guides(color = "none")

plot_grid(clay.dis,sand.dis,align = "hv",nrow = 1)

ggsave(filename = "./Figures/clay_sand_dis.png",
       plot = last_plot(),width = 36,height = 15, unit = "cm",
       dpi = 300)

df.clay.all %>% filter(!(N %in% sea.mask)) %>%
  group_by(type) %>% summarise(m.sand = median(sand*100))

# High resolution
world <- ne_countries(scale = "medium", returnclass = "sf")
var.tmp <- t(as.matrix(map.clay))

clons <- seq(extent(map.clay)[1],extent(map.clay)[2],res(map.clay)[1])
clats <- sort(seq(extent(map.clay)[3],extent(map.clay)[4],res(map.clay)[2]),decreasing = TRUE)
var.df.high <- melt(var.tmp) %>% rename(lat = Var2,
                                   lon = Var1) %>% mutate(lat = clats[lat],
                                                          lon = clons[lon],
                                                          value = value)


ggplot(data = var.df.high) +
  geom_raster(aes(x=lon, y = lat, fill = 100*(value))) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-84.5, -30.5), ylim = c(-19.5, 15.5), expand = FALSE) +
  scale_fill_gradient2(low = "darkred",mid = "white",high = "#E2725B",na.value = NA, limits = c(0,50)) +
  ggtitle("") +
  labs(x = "",y = "", fill = "Clay content (%)") +
  theme_bw() +
  theme(text = element_text(size = 20))

ggsave(filename = "./Figures/clay_high.png",
       plot = last_plot(),width = 36,height = 20, unit = "cm",
       dpi = 300)


# low resolution
var.tmp <- t(as.matrix(map.sand))

clons <- seq(extent(map.sand)[1],extent(map.sand)[2],res(map.sand)[1])
clats <- sort(seq(extent(map.sand)[3],extent(map.sand)[4],res(map.sand)[2]),decreasing = TRUE)
var.df <- melt(var.tmp) %>% rename(lat = Var2,
                                   lon = Var1) %>% mutate(lat = clats[lat],
                                                          lon = clons[lon],
                                                          value = value)

ggplot() +
  geom_density(data = df.clay,
               aes(x = clay)) +
  geom_density(data = var.df,
               aes(x = value),col = 'red') +
  theme_bw()

# high resolution
var.tmp <- t(as.matrix(map.clay))
var.tmp2 <- t(as.matrix(rc[[1]]))

clons <- seq(extent(map.clay)[1],extent(map.clay)[2],res(map.clay)[1])
clats <- sort(seq(extent(map.clay)[3],extent(map.clay)[4],res(map.clay)[2]),decreasing = TRUE)
var.df.high <- bind_rows(list(melt(var.tmp) %>% rename(lat = Var2,
                                                       lon = Var1) %>% mutate(lat = clats[lat],
                                                               lon = clons[lon],
                                                               value = value,
                                                               var = "clay"),
                              melt(var.tmp2) %>% rename(lat = Var2,
                                                       lon = Var1) %>% mutate(lat = clats[lat],
                                                                              lon = clons[lon],
                                                                              value = value,
                                                                              var = "sand"))) %>% pivot_wider(names_from = "var",
                                                                                                              values_from = "value") %>% rename(value = clay)

top.sand.var <- raster::aggregate(rc[[c(2)]],
                                       1 / res(rc)[1],
                                       1 / res(rc)[2],
                                       fun = var,na.rm = TRUE)

clons <- seq(extent(top.sand.var)[1],extent(top.sand.var)[2],res(top.sand.var)[1])
clats <- sort(seq(extent(top.sand.var)[3],extent(top.sand.var)[4],res(top.sand.var)[2]),decreasing = TRUE)

var.df.find <- melt(t(as.matrix(top.sand.var))) %>% rename(lat = Var2,
                                                  lon = Var1) %>% mutate(lat = clats[lat],
                                                                         lon = clons[lon],
                                                                         value = value)

var.df.find %>% filter(value == max(value,na.rm = TRUE))

var.df.high <- var.df.high %>% mutate(value = case_when(value == 0 & sand == 0 ~ NA_real_,
                                                        TRUE ~ value))

ED_REG_LATMIN = -2.5
ED_REG_LATMAX = ED_REG_LATMIN + 1
ED_REG_LONMIN = -55.5
ED_REG_LONMAX = ED_REG_LONMIN + 1


var.df.high %>% filter(lat<ED_REG_LATMAX,lat>=ED_REG_LATMIN,lon<ED_REG_LONMAX,lon>=ED_REG_LONMIN) %>% arrange(value)
var.df.high %>% filter(lat<ED_REG_LATMAX,lat>=ED_REG_LATMIN,lon<ED_REG_LONMAX,lon>=ED_REG_LONMIN) %>% arrange(desc(value))

hist(var.df.high %>% filter(lat<ED_REG_LATMAX,lat>=ED_REG_LATMIN,lon<ED_REG_LONMAX,lon>=ED_REG_LONMIN) %>% pull(value))

ggplot(data = var.df.high %>% filter(lat<ED_REG_LATMAX,lat>=ED_REG_LATMIN,lon<ED_REG_LONMAX,lon>=ED_REG_LONMIN)) +
  geom_raster(aes(x=lon, y = lat, fill = value*100)) +
  coord_sf(xlim = c(ED_REG_LONMIN,ED_REG_LONMAX), ylim = c(ED_REG_LATMIN, ED_REG_LATMAX), expand = FALSE) +
  scale_fill_gradient2(low = "darkred",mid = "white",high = "#E2725B",na.value = NA, limits = c(0,50)) +
  ggtitle("") +
  labs(x = "",y = "", fill = "clay fraction (%)") +
  theme_bw() +
  theme(text = element_text(size = 20))

ggsave(filename = "./Figures/clay_details.png",
       plot = last_plot(),width = 36,height = 20, unit = "cm",
       dpi = 300)


hist(var.df.high %>% filter(lat<ED_REG_LATMAX,lat>=ED_REG_LATMIN,lon<ED_REG_LONMAX,lon>=ED_REG_LONMIN) %>% pull(value)*100)


ggplot(data = var.df.high %>% filter(lat<ED_REG_LATMAX,lat>=ED_REG_LATMIN,lon<ED_REG_LONMAX,lon>=ED_REG_LONMIN)) +
  geom_density(aes(x = value*100)) +
  geom_density(data = df.clay.NA,
               aes(x = clay*100), col = "red") +
  labs(x = "clay fraction (%)", y = 'Density') +
  theme_bw() +
  theme(text = element_text(size = 20))

ggsave(filename = "./Figures/clay_density.png",
       plot = last_plot(),width = 15,height = 10, unit = "cm",
       dpi = 300)

summary(var.df.high %>% filter(lat<ED_REG_LATMAX,lat>=ED_REG_LATMIN,lon<ED_REG_LONMAX,lon>=ED_REG_LONMIN) %>% pull(value))



ggplot(data = var.df) +
  geom_raster(aes(x=lon, y = lat, fill = 100*(value))) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-84.5, -30.5), ylim = c(-19.5, 15.5), expand = FALSE) +
  scale_fill_gradient2(low = "darkred",mid = "white",high = "#E2725B",na.value = NA, limits = c(0,50)) +
  ggtitle("") +
  labs(x = "",y = "", fill = "Clay content (%)") +
  theme_bw() +
  theme(text = element_text(size = 20))

ggsave(filename = "./Figures/clay.png",
       plot = last_plot(),width = 36,height = 20, unit = "cm",
       dpi = 300)


sqrt(var(var.df.high %>% filter(lat<=ED_REG_LATMAX,lat>=ED_REG_LATMIN,lon<=ED_REG_LONMAX,lon>=ED_REG_LONMIN) %>% pull(value)))


ggplot(data = var.df %>% filter(lat<ED_REG_LATMAX,lat>=ED_REG_LATMIN,lon<ED_REG_LONMAX,lon>=ED_REG_LONMIN)) +
  geom_density(aes(x = value*100)) +
  labs(x = "clay content (%)", y = 'Density') +
  theme_bw() +
  theme(text = element_text(size = 20))


hist(as.vector(rc[[c(2)]]))
