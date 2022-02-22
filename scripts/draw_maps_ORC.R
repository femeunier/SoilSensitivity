rm(list = ls())

library(raster)
library(SoilSensitivity)
library(reshape2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(tidyr)

soil.scenarios = c("Smin","Smean","Smax")
soil.scenarios.name = c("SoilGrids_min","SoilGrids_mean","SoilGrids_max")

vars <- c("GPP","LAI","NPP")

file.frac <- "./outputs/spinup/ORC/ORC_spinup_Smin_VEGFRAC.gri"
raster.frac <- stack(file.frac)
frac.array <- as.array(raster.frac)

lons <- seq(extent(raster.frac)[1],extent(raster.frac)[2],0.5)
lats <- seq(extent(raster.frac)[4],extent(raster.frac)[3],-0.5)

df.all <- data.frame()
for (isoil in seq(1,length(soil.scenarios))){
  for (ivar in seq(1,length(vars))){
    file.in <- paste0("./outputs/spinup/ORC/ORC_spinup_",soil.scenarios[isoil],"_",vars[ivar],".gri")
    raster.in <- stack(file.in)
    var.in <- as.array(raster.in)
    var <- t(base::as.matrix(apply(var.in*frac.array,c(1,2),sum),dimnames = list(lon,lat)))

    var.df <- melt(var) %>% rename(lat = Var2,
                                   lon = Var1) %>% mutate(lat = lats[lat],
                                                          lon = lons[lon])

    df.all <- bind_rows(list(df.all,
                             var.df %>% mutate(scenario = soil.scenarios.name[isoil],
                                               var = vars[ivar])))
  }
}

ED_REG_LATMIN = -19.5
ED_REG_LATMAX =  13.5
ED_REG_LONMIN = -84.5
ED_REG_LONMAX = -30.5

GRID_RES = 1

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = df.all  %>% filter(lat<=ED_REG_LATMAX,lat>=ED_REG_LATMIN,lon<=ED_REG_LONMAX,lon>=ED_REG_LONMIN,
                                 var == "LAI")) +
  geom_raster(aes(x=lon, y = lat, fill = value),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-84.5, -30.5), ylim = c(-19.5, 15.5), expand = FALSE) +
  scale_fill_gradient2(low = "darkred",mid = "darkgrey",high = "darkgreen",na.value = NA) +
  labs(x = "",y = "") +
  facet_wrap(~ scenario) +
  theme_bw()


df <- df.all %>% mutate(year = 1950) %>%
  group_by(lat,lon,year,scenario) %>% mutate(group.id = cur_group_id()) %>%
  pivot_wider(names_from = var,
              values_from = value)

df_wide <- df %>% mutate(scenario = sub(".*\\_", "", scenario)) %>% filter(year == max(df$year)) %>% dplyr::select(-group.id) %>%
  pivot_wider(names_from = scenario,
              values_from = -c(lat,lon,scenario,year))

df_wide <- df_wide %>% mutate(AGB.change = GPP_mean - GPP_min,
                              AGB.change.rel = AGB.change/GPP_mean,
                              LAI.change = LAI_mean - LAI_max,
                              LAI.change.rel = LAI.change/LAI_mean)

ggplot(data = df.all %>% filter(var == "LAI")) +
  geom_density(aes(x = value,color = as.factor(scenario))) +
  theme_bw()


