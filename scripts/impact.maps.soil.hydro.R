rm(list = ls())

library(SoilSensitivity)
library(lattice)
library(raster)
library(purrr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)
library(wesanderson)

clay.ref <- stack(paste0("./maps/soilgrid_top.clay_mean_resampled.gri"))
sand.ref <- stack(paste0("./maps/soilgrid_top.sand_mean_resampled.gri"))

pos.sea <- clay.ref == 0 & sand.ref == 0

scenarios <- c("min","mean","max")
models <- c("ED","LPJ-GUESS","ORCHIDEE")

df.text.all <- data.frame()
for (iscenario in seq(1,length(scenarios))){

  clay.in <- paste0("./maps/soilgrid_top.clay_",scenarios[iscenario],"_resampled.gri")
  sand.in <- paste0("./maps/soilgrid_top.sand_",scenarios[iscenario],"_resampled.gri")

  raster.in <- stack(clay.in)
  raster.in[pos.sea == 1] <- NA
  raster.in2 <- stack(sand.in)
  raster.in2[pos.sea == 1] <- NA

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

  df.text <- sand.df %>% rename(sand = value) %>%left_join(clay.df %>% rename(clay = value), by = c("lat","lon"))

  df.text.all <- bind_rows(list(df.text.all,
                                df.text %>% mutate(scenario = scenarios[iscenario])))

}

names.prop <- c("theta_sat","theta_wp","theta_fc","k_sat")

df.text.all.prop <- df.text.all %>% filter(!(is.na(sand)))

sand.all <- df.text.all.prop$sand
clay.all <- df.text.all.prop$clay

df.hydro <- data.frame()

for (imodel in seq(1,length(models))){
  soil.prop <- get_soilproperties(sand = sand.all,
                                  clay = clay.all,
                                  model = models[imodel],
                                  orc_map = "usda")

  df.hydro.model <- data.frame(lat = df.text.all.prop$lat,
                               lon = df.text.all.prop$lon,
                               sand = df.text.all.prop$sand,
                               clay = df.text.all.prop$clay,
                               scenario = df.text.all.prop$scenario)

  for (iprop in seq(1,length(names.prop))){
    df.hydro.model[[names.prop[iprop]]] <- soil.prop[[names.prop[iprop]]]
  }

  df.hydro <- bind_rows(list(df.hydro,
                             df.hydro.model %>% mutate(model = models[imodel])))
}


df.hydro.all <- df.hydro %>% mutate(diff = theta_fc - theta_wp) %>%
  dplyr::select(model,scenario,sand,clay,theta_sat,theta_wp,theta_fc,diff,k_sat) %>%
  pivot_longer(cols = -c(model,scenario),
               values_to = "value",
               names_to = "var") %>%
  mutate(model = case_when(model == "ED" ~ "ED2",
                           TRUE ~ model),
         var = case_when(var == "diff" ~ "θ[FC] - θ[WP]",
                         var == "theta_fc" ~ "θ[FC]",
                         var == "theta_wp" ~ "θ[WP]",
                         var == "k_sat" ~ "k[sat]",
                         TRUE ~ var),
         scenario = case_when(scenario == "max" ~ "Max. clay",
                              scenario == "mean" ~ "Mean clay",
                              scenario == "min" ~ "Min. clay")) %>%
  mutate(model = factor(model,levels = c("ORCHIDEE","ED2","LPJ-GUESS")),
         var = factor(var,levels = c("θ[FC]","θ[WP]","θ[FC] - θ[WP]","k[sat]")))

df2plot <- df.hydro.all %>% filter(var %in% c("θ[FC]","θ[WP]","θ[FC] - θ[WP]"))
data.sum <- df2plot %>% group_by(model,var,scenario) %>% summarise(m = mean(value))

pal <- wes_palette("Darjeeling1", 3, type = c("discrete"))

maps.hydro.RDS <- readRDS("./outputs/maps.hydro.RDS") %>% filter(depth == 0)

df.ref <- maps.hydro.RDS %>% dplyr::select(theta_fc,theta_wp,Ks) %>% rename(k_sat = Ks) %>%
  mutate(diff = theta_fc - theta_wp,
         k_sat = k_sat/86400/100) %>%
  pivot_longer(cols = c("theta_fc","theta_wp","diff","k_sat"),
               names_to = "var",
               values_to = "value") %>% filter(!is.na(value)) %>%
  mutate(var = case_when(var == "diff" ~ "θ[FC] - θ[WP]",
                         var == "theta_fc" ~ "θ[FC]",
                         var == "theta_wp" ~ "θ[WP]",
                         var == "k_sat" ~ "k[sat]",
                         TRUE ~ var)) %>%
  mutate(var = factor(var,levels = c("θ[FC]","θ[WP]","θ[FC] - θ[WP]","k[sat]")),
         model = "ED2",scenario = "Reference")

df.ref1 <- df.ref %>% mutate(model = "LPJ-GUESS")
df.ref2 <- df.ref %>% mutate(model = "ORCHIDEE")

df2plot.final <- bind_rows(list(df.hydro.all,
                                df.ref,
                                df.ref1,
                                df.ref2)) %>% filter(var %in% c("θ[FC]","θ[WP]","θ[FC] - θ[WP]")) %>%
  mutate(model = factor(model,levels = c("ORCHIDEE","ED2","LPJ-GUESS")))

ggplot(data = df2plot.final) +
  geom_density(data = df2plot.final %>% filter(scenario != "Reference"),
               mapping = aes(x = 100*value, color = scenario),fill = NA,
               show.legend = FALSE) +
  geom_density(data = df2plot.final %>% filter(scenario == "Reference"),
               mapping = aes(x = 100*value, color = NA,fill = scenario),alpha = 0.5,
               show.legend = FALSE) +
  geom_point(data = data.sum,
             aes(x = 100*m, y = 0,color = scenario)) +
  facet_grid(var ~ model, scales = "free",labeller = label_parsed) +
  scale_color_manual(values = c(pal,"darkgrey")) +
  scale_fill_manual(values = c("darkgrey",pal)) +
  # scale_x_log10() +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.position = c(0.75,0.87)) +
  labs(x = "water content (%)",y = "Density", color = "Scenario")

ggsave(filename = "./Figures/hydro_prop.png",
       plot = last_plot(),width = 30,height = 20, unit = "cm",
       dpi = 300)


maps.hydro.RDS <- readRDS("./outputs/maps.hydro.RDS")

df.ref <- maps.hydro.RDS %>% dplyr::select(theta_fc,theta_wp,Ks) %>% rename(k_sat = Ks) %>%
  mutate(diff = theta_fc - theta_wp,
         k_sat = k_sat/86400/100) %>%
  pivot_longer(cols = c("theta_fc","theta_wp","diff","k_sat"),
               names_to = "var",
               values_to = "value") %>% filter(!is.na(value)) %>%
  mutate(var = case_when(var == "diff" ~ "θ[FC] - θ[WP]",
                         var == "theta_fc" ~ "θ[FC]",
                         var == "theta_wp" ~ "θ[WP]",
                         var == "k_sat" ~ "k[sat]",
                         TRUE ~ var)) %>%
  mutate(var = factor(var,levels = c("θ[FC]","θ[WP]","θ[FC] - θ[WP]","k[sat]")),
         model = "ED2",scenario = "Reference")

df2plot.final2 <- bind_rows(list(df.hydro.all,
                                 df.ref)) %>% filter(var %in% c("k[sat]")) %>%
  mutate(model = factor(model,levels = c("ORCHIDEE","ED2","LPJ-GUESS")))

df2plot.final2plot <- df2plot.final2 %>%
  mutate(scenario = case_when(scenario == "Reference" ~ "Montzka et al., 2017",
                              TRUE ~ scenario))

ggplot(data = df2plot.final2plot %>% filter(model == "ED2")) +
  geom_density(aes(x = log10(value), color = scenario), alpha = 0.3, fill = NA) +
  scale_color_manual(values = c(pal,"darkgrey")) +
  scale_fill_manual(values = c("darkgrey",pal)) +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.position = c(0.85,0.8)) +
  labs(x = expression(log[10](k[sat])),y = "Density", color = "")

ggsave(filename = "./Figures/hydro_prop_ksat.png",
       plot = last_plot(),width = 30,height = 20, unit = "cm",
       dpi = 300)


ggplot(data = df.hydro.all %>% filter(var %in% c("k[sat]"))) +
  geom_density(aes(x = value, color = scenario), alpha = 0.3, fill = NA) +
  facet_wrap(~ model, scales = "free") +
  scale_x_log10() +
  theme_bw()

ggplot(data = df.hydro) +
  geom_density(aes(x = theta_fc, color = scenario), alpha = 0.3, fill = NA) +
  geom_density(aes(x = theta_wp, color = scenario), alpha = 0.3,fill = NA,linetype = 2) +
  facet_wrap(~ model, scales = "free") +
  # scale_x_log10() +
  theme_bw()

ggplot(data = df.hydro) +
  geom_density(aes(x = theta_fc - theta_wp, fill = scenario), alpha = 0.3) +
  facet_wrap(~ model, scales = "free") +
  # scale_x_log10() +
  theme_bw()


ggplot(data = df.hydro) +
  geom_density(aes(x = k_sat, fill = scenario), alpha = 0.3) +
  facet_wrap(~ model, scales = "free") +
  scale_x_log10() +
  theme_bw()


df.hydro %>% group_by(model,scenario) %>%
  summarise(mean(k_sat),
            mean(theta_sat - theta_wp),
            N = length(k_sat))

df.hydro.wide <- df.hydro %>% pivot_wider(values_from = c(names.prop,"sand","clay"),
                                          names_from = scenario) %>%
  mutate(theta_sat_diff = theta_sat_max - theta_sat_min,
         theta_wp_diff  = theta_wp_max - theta_wp_min,
         theta_fc_diff  = theta_fc_max - theta_fc_min,
         k_sat_diff     = k_sat_max - k_sat_min,
         sand_diff      = sand_max - sand_min,
         clay_diff      = clay_max - clay_min) %>%
  dplyr::select(c(lat,lon,model,theta_sat_diff,theta_wp_diff,theta_fc_diff,k_sat_diff,sand_diff,clay_diff)) %>%
  pivot_longer(cols = -c(lat,lon,model),
               values_to = "value",
               names_to = 'var') %>%
  mutate(var = sub("\\|.*", "", var))

ggplot(data = df.hydro.wide) +
  geom_density(aes(x = value, fill = model), alpha = 0.2) +
  facet_wrap(~var,scales = "free") +
  theme_bw()
