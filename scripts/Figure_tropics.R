rm(list = ls())

library("rnaturalearth")
library("rnaturalearthdata")
library(ncdf4)
library(reshape2)

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_sf(color = "darkgrey") +
  coord_sf(xlim = c(-102.15, 70), ylim = c(-20, 20), expand = FALSE) +
  theme_bw() +
  theme(panel.grid.major = element_line(color = gray(.5),
                                        linetype = 3, size = 0.3),
        panel.background = element_rect(fill = "white"))

ggsave(filename = "./Figures/Worldmap.png",
       plot = last_plot(),width = 20,height = 7, unit = "cm",
       dpi = 300)


ncfile <- "./outputs/spinup/ORC/ORCHIDEE_soilmap/soils_param_Mean_halfdeg.nc"

nc <- nc_open(ncfile)

lats <- unique(as.vector(ncvar_get(nc,"nav_lat")))
lons <- unique(as.vector(ncvar_get(nc,"nav_lon")))
soiltext <- ncvar_get(nc,"soiltext")

nc_close(nc)

df <- melt(soiltext) %>% rename(lon = Var1,
                                lat = Var2) %>% mutate(lat = lats[lat],
                                                       lon = lons[lon],
                                                       value = value)

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_sf(data = world, color = "darkgrey") +
  geom_raster(data = df %>% filter(!is.na(value)),
              aes(x=lon, y = lat, fill = as.factor(value)),alpha = 0.3) +
  coord_sf(xlim = c(-102.15, 70), ylim = c(-20, 20), expand = FALSE) +
  # scale_fill_gradientn(colours = fliplr(brewer.pal(7,"RdYlGn")),na.value = "white") +
  labs(fill = "MAT (°C)", x = "",y = "") +
  labs(x = "",y = "") +
  theme_bw() +
  theme(panel.grid.major = element_line(color = gray(.5),
                                        linetype = 3, size = 0.3),
        panel.background = element_rect(fill = "white")) +
  guides(fill = FALSE)

ggsave(filename = "./Figures/Worldmap2.png",
       plot = last_plot(),width = 20,height = 7, unit = "cm",
       dpi = 300)

