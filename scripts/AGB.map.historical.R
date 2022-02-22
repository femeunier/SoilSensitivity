rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyr)
library(wesanderson)

system2("rsync",paste("-avz",
                      "hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/Outputs_LPJ.RDS",
                      "./outputs/"))

system2("rsync",paste("-avz",
                      "hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/Outputs_ED2.RDS",
                      "./outputs/"))

system2("rsync",paste("-avz",
                      "hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/Outputs_ORCHIDEE.RDS",
                      "./outputs/"))

OP_ED2 <- readRDS("./outputs/Outputs_ED2.RDS") %>% pivot_longer(cols = -c(scenario,year,lat,lon),
                                                                names_to = "variable",
                                                                values_to = "value") %>% dplyr::select(-c(year))
OP_LPJ <- readRDS("./outputs/Outputs_LPJ.RDS")
OP_ORCHIDEE <- readRDS("./outputs/Outputs_ORCHIDEE.RDS") %>% dplyr::select(-c(year))

veg.cover <- OP_ORCHIDEE %>% filter(variable == "veg.cover") %>% rename(veg.cover = value) %>% dplyr::select(-c(variable))

OP_ORCHIDEE.mod <- OP_ORCHIDEE %>% filter(variable != "veg.cover") %>%
  left_join(veg.cover,
            by = c("lon","lat","pft","month","scenario"))

OP_ORCHIDEE.sum <- OP_ORCHIDEE.mod %>% group_by(lon,lat,scenario,variable,month) %>% summarise(value = case_when(all(is.na(value)) ~ NA_real_,
                                                                                                                 TRUE ~ sum(value*veg.cover,na.rm = TRUE)),
                                                                                               .groups = "keep")  %>%
  group_by(lon,lat,scenario,variable) %>% summarise(value = mean(value,na.rm = TRUE),
                                                    .groups = "keep")


OP.all <- bind_rows(list(OP_ORCHIDEE.sum %>% mutate(model = "ORCHIDEE") ,
                         OP_ED2 %>% mutate(model = "ED2"),
                         OP_LPJ %>% mutate(model = "LPJ")))


world <- ne_countries(scale = "medium", returnclass = "sf")

ED_REG_LATMIN = -19.5
ED_REG_LATMAX =  13.5
ED_REG_LONMIN = -84.5
ED_REG_LONMAX = -30.5


OP.all2plot <- OP.all %>% filter(lat<=ED_REG_LATMAX,lat>=ED_REG_LATMIN,lon<=ED_REG_LONMAX,lon>=ED_REG_LONMIN,
                                 variable %in% c("AGB"))



ggplot(data = OP.all) +
  geom_raster(aes(x=lon, y = lat, fill = value),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-84.5, -30.5), ylim = c(-19.5, 15.5), expand = FALSE) +
  scale_fill_gradient2(low = "darkred",mid = "grey",high = "darkgreen",na.value = NA) +
  labs(x = "",y = "") +
  facet_grid(scenario ~ model) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  labs(x = "",y = "",fill = "AGB (kgC/m²)")


# Data
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

data.all <- bind_rows(list(var.df %>% mutate(model = "ORCHIDEE"),
                           var.df %>% mutate(model = "ED2"),
                           var.df %>% mutate(model = "LPJ")))

pal <- wes_palette("Darjeeling1", 3, type = c("discrete"))

ggplot(data = OP.all  %>% filter(lat<=ED_REG_LATMAX,lat>=ED_REG_LATMIN,lon<=ED_REG_LONMAX,lon>=ED_REG_LONMIN,
                                 variable %in% c("AGB"))) +
  geom_density(aes(x = value, color = scenario)) +
  geom_density(data = var.df,
               aes(x = value), color = "black") +
  scale_color_manual(values = pal) +
  facet_wrap(~ model) +
  theme_bw()
