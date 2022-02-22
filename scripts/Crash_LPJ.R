rm(list = ls())

library(ggplot2)
library(dplyr)
library(SoilSensitivity)
library(raster)
library(rnaturalearth)
library(rnaturalearthdata)
library(reshape2)
library(RColorBrewer)

sands <- seq(0,1,length.out = 500)
clays <- seq(0,1,length.out = 500)

df.OP <- data.frame()
for (isand in seq(1,length(sands))){
  for (iclay in seq(1,length(clays))){

    if ((sands[isand] + clays[iclay])>1){
      next()
    }

    SP <- get_soilproperties(sand = sands[isand], clay = clays[iclay], model="LPJ-GUESS")
    df.OP <- bind_rows(list(df.OP,
                            data.frame(sand = sands[isand], clay = clays[iclay],
                                       WP = SP$theta_wp,sat = SP$theta_sat,FC = SP$theta_fc)))
  }
}


# df.OP %>% mutate(diff = sat - FC) %>% filter(diff <= 0)
#
# SP <- get_soilproperties(sand = 0.00742, clay = 0.00972, model="LPJ-GUESS")
# get_soilproperties(sand = 0.01, clay = 0.01, model="LPJ-GUESS")

ggplot(data = df.OP %>% filter(FC >= sat)) +
  geom_tile(aes(x = sand,y = clay, fill = as.numeric(sat - FC))) +
  scale_fill_distiller(palette = "Reds") +
  labs(x = "Sand fraction", y = "Clay fraction", fill = expression(theta[sat] - theta[FC])) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() +
  theme(text = element_text(size = 20))

ggsave(plot = last_plot(),
       dpi = 300, width = 10, height = 7,filename = "./Figures/Crash.PLJ.png")

df.map <- readRDS("./outputs/spinup/ORC/ORCHIDEE_soilmap/output_min.RDS")

# rc <- brick(file.path(".","maps", "soilgrid.grd"),expand = TRUE)
#
#
# df.OP.sum <-  df.OP %>% filter(sat < FC) %>%
#   group_by(sand) %>% summarise(clay.m = max(clay))
#
#
# alpha = 0.01
#
# top.sand.mean <- aggregates(
#   raster1 = rc[[c(2)]],
#   raster2 = rc[[c(1)]],
#   res1 = 1 / res(rc)[1] * 0.5,
#   res2 = 1 / res(rc)[1] * 0.5,
#   probs = alpha)
#
# top.clay.mean <- aggregates(
#   raster1 = rc[[c(2)]],
#   raster2 = rc[[c(2)]],
#   res1 = 1 / res(rc)[1] * 0.5,
#   res2 = 1 / res(rc)[1] * 0.5,
#   probs = alpha)
#
# # pos.sea <- (top.sand.mean==0)& (top.clay.mean ==0)
# #
# # top.sand.mean[pos.sea] <- NA
# # top.clay.mean[pos.sea] <- NA
#
# crash <- top.sand.mean
#
# for (irow in seq(1,nrow(df.OP.sum) - 1)){
#   pos <- top.sand.mean >= as.numeric(df.OP.sum[irow,"sand"]) & top.sand.mean < as.numeric(df.OP.sum[irow + 1,"sand"]) &
#     top.clay.mean <= as.numeric(df.OP.sum[irow,"clay.m"])
#   crash[pos] <- 1000
# }
#
# crash[crash<1000] <- NA
# lons <- seq(extent(crash)[1],extent(crash)[2],0.5)
# lats <- seq(extent(crash)[4],extent(crash)[3],-0.5)
# var.in <- as.array(crash)
# var <- t(base::as.matrix(apply(var.in,c(1,2),sum),dimnames = list(lon,lat)))
#
# var.df <- melt(var) %>% rename(lat = Var2,
#                                lon = Var1) %>% mutate(lat = lats[lat],
#                                                       lon = lons[lon],
#                                                       value = value)
#
# world <- ne_countries(scale = "medium", returnclass = "sf")
#
# ggplot(data = var.df) +
#   geom_raster(aes(x=lon, y = lat, fill = value),alpha = 0.3) +
#   geom_sf(data = world,fill = NA) +
#   coord_sf(xlim = c(-84.5, -30.5), ylim = c(-19.5, 15.5), expand = FALSE) +
#   scale_fill_gradient2(low = "darkred",mid = "darkgrey",high = "darkgreen",na.value = "red") +
#   labs(x = "",y = "") +
#   theme_bw()


clay.ref <- stack(paste0("./maps/soilgrid_top.clay_mean_resampled.gri"))
sand.ref <- stack(paste0("./maps/soilgrid_top.sand_mean_resampled.gri"))

pos.sea <- clay.ref == 0 & sand.ref == 0

clay.in <- paste0("./maps/soilgrid_top.clay_min_resampled.gri")
sand.in <- paste0("./maps/soilgrid_top.sand_min_resampled.gri")

raster.in <- stack(clay.in)
raster.in[pos.sea == 1] <- NA
# raster.in <- aggregate(raster.in,fact = 2)

raster.in2 <- stack(sand.in)
raster.in2[pos.sea== 1] <- NA
# raster.in2 <- aggregate(raster.in,fact = 2)


clay.in <- as.array(raster.in)
sand.in <- as.array(raster.in2)

clay <- t(base::as.matrix(clay.in[,,1],dimnames = list(lon,lat)))
sand <- t(base::as.matrix(sand.in[,,1],dimnames = list(lon,lat)))

clats <- sort(seq(extent(raster.in)[3]+0.25,extent(raster.in)[4],0.5),decreasing = TRUE)
clons <- seq(extent(raster.in)[1]+0.25,extent(raster.in)[2],0.5)
clay.df <- melt(clay) %>% rename(lat = Var2,
                                 lon = Var1) %>% mutate(lat = clats[lat],
                                                        lon = clons[lon],
                                                        value = value)

sand.df <- melt(sand) %>% rename(lat = Var2,
                                 lon = Var1) %>% mutate(lat = clats[lat],
                                                        lon = clons[lon],
                                                        value = value)

df.text <- sand.df %>% rename(sand = value) %>%left_join(clay.df %>% rename(clay = value), by = c("lat","lon")) %>%
  mutate(sat = get_soilproperties(sand = sand, clay = clay, model="LPJ-GUESS")[["theta_sat"]],
         FC =  get_soilproperties(sand = sand, clay = clay, model="LPJ-GUESS")[["theta_fc"]])


world <- ne_countries(scale = "medium", returnclass = "sf")

df.text.crash <- df.text %>% filter(sat <= FC)

ggplot(data = df.text.crash %>% filter(sand > 0 & clay > 0)) +
  geom_tile(aes(x=lon, y = lat, fill = sat - FC),alpha = 0.3,na.rm = TRUE) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-84.5, -30.5), ylim = c(-19.5, 15.5), expand = FALSE) +
  labs(x = "",y = "") +
  theme_bw()

ggplot(data = sand.df) +
  geom_raster(aes(x=lon, y = lat, fill = value),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-84.5, -30.5), ylim = c(-19.5, 15.5), expand = FALSE) +
  labs(x = "",y = "") +
  theme_bw()
