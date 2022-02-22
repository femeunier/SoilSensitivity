rm(list = ls())

library(raster)
library(SoilSensitivity)
library(reshape2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(tidyr)

########################################################################################

world <- ne_countries(scale = "medium", returnclass = "sf")

ED_REG_LATMIN = -19.5
ED_REG_LATMAX =  13.5
ED_REG_LONMIN = -84.5
ED_REG_LONMAX = -30.5

########################################################################################
# LPJ

scenarios <- c("output_mean","output_max","output_min")
soil.scenarios.name <- c("SoilGrids_mean","SoilGrids_max","SoilGrids_min")

var.names <- c("GPP","NPP","LAI","biomass","fsw","AGB","sand","clay") # ED2
vars.LPJ <- c("agpp.out","anpp.out","lai.out","cpool.out","wscal.out","cpool.out","Sand","Clay") # LPJ
vars.ORCH <- c("GPP","NPP","LAI","TOTAL_M","MOISTRESS","AGB","top.sand","top.clay") # ORCHIDEE

df.all.LPJ <- data.frame()
for (isoil in seq(1,length(scenarios))){
  for (ivar in seq(1,length(vars.LPJ))){


    if (vars.LPJ[ivar] %in% c("Sand","Clay")){
      file.in <- paste0("./outputs/spinup/LPJ/",scenarios[isoil],"/driver.out")
    } else {
      file.in <- paste0("./outputs/spinup/LPJ/",scenarios[isoil],"/",vars.LPJ[ivar])
    }

    if (!file.exists(file.in)){
      next()
    }

    if (vars.LPJ[ivar] == "mwcont_upper.out"){
      data <- read.table(file.in,header =TRUE,stringsAsFactors=FALSE) %>%
        mutate(Total = rowMeans(select(., -c("Lon","Lat","Year")))) %>%
                 dplyr::select(c(Lon,Lat,Year,Total)) %>% filter(Year == 1910) %>%
                 rename(lat = Lat,
                        lon = Lon,
                        value = Total) %>% dplyr::select(lat,lon,value)

      data2 <- read.table(paste0("./outputs/spinup/LPJ/",scenarios[isoil],"/","mwcont_lower.out"),header =TRUE,stringsAsFactors=FALSE) %>%
        mutate(Total = rowMeans(select(., -c("Lon","Lat","Year")))) %>%
        dplyr::select(c(Lon,Lat,Year,Total)) %>% filter(Year == 1910) %>%
        rename(lat = Lat,
               lon = Lon,
               value = Total) %>% dplyr::select(lat,lon,value)

      data <- data %>% mutate(value = (1/3*data$value + 2/3*data2$value))

    } else if (vars.LPJ[ivar] == "cpool.out" & var.names[ivar] == "AGB"){
      data <- read.table(file.in,header =TRUE,stringsAsFactors=FALSE) %>%
        dplyr::select(c(Lon,Lat,Year,VegC)) %>% filter(Year == 1910) %>% mutate(AGB = 0.7*VegC) %>%
        rename(lat = Lat,
               lon = Lon,
               value = AGB) %>% dplyr::select(lat,lon,value)

    } else if (vars.LPJ[ivar] == "cpool.out"){
      data <- read.table(file.in,header =TRUE,stringsAsFactors=FALSE) %>%
        dplyr::select(c(Lon,Lat,Year,VegC)) %>% filter(Year == 1910) %>%
        rename(lat = Lat,
               lon = Lon,
               value = VegC) %>% dplyr::select(lat,lon,value)

    }  else if (vars.LPJ[ivar] == "wscal.out"){

      data <- read.table(file.in,header =TRUE,stringsAsFactors=FALSE) %>%
        filter(Year == 1910)

      data.wscal <- data %>% dplyr::select(-c(Lon,Lat,Year))

      lai <- read.table(paste0("./outputs/spinup/LPJ/",scenarios[isoil],"/","lai.out"),header =TRUE,stringsAsFactors=FALSE) %>%
        filter(Year == 1910) %>% dplyr::select(-c(Lon,Lat,Year,Total))

      data <- data %>% mutate(value = rowSums(lai*data.wscal)/rowSums(lai)) %>%
        rename(lon = Lon,lat = Lat) %>%
        dplyr::select(c(lat,lon,value))


    } else if (vars.LPJ[ivar] %in% c("Sand","Clay")){
      data <- read.table(file.in,header =TRUE,stringsAsFactors=FALSE) %>%
        dplyr::select(c(Lon,Lat,Year,vars.LPJ[ivar])) %>% filter(Year == 1910) %>%
        rename(lat = Lat,
               lon = Lon,
               value = vars.LPJ[ivar]) %>% dplyr::select(lat,lon,value)

    } else{
      data <- read.table(file.in,header =TRUE,stringsAsFactors=FALSE) %>%
        dplyr::select(c(Lon,Lat,Year,Total)) %>% filter(Year == 1910) %>%
        rename(lat = Lat,
               lon = Lon,
               value = Total) %>% dplyr::select(lat,lon,value)
    }

    # ggplot(data = data) +
    #   geom_raster(aes(x=lon, y = lat, fill = value),alpha = 0.3) +
    #   geom_sf(data = world,fill = NA) +
    #   coord_sf(xlim = c(-84.5, -30.5), ylim = c(-19.5, 15.5), expand = FALSE) +
    #   scale_fill_gradient2(low = "darkred",mid = "darkgrey",high = "darkgreen",na.value = NA) +
    #   labs(x = "",y = "") +
    #   theme_bw()

    df.all.LPJ <- bind_rows(list(df.all.LPJ,
                                 data %>% mutate(scenario = soil.scenarios.name[isoil],
                                                 var = var.names[ivar])))

  }
}



#######################################################################################
# ORCHIDEE

soil.scenarios = c("Smin","Smean","Smax")
soil.map.names = c("min","mean","max")
soil.scenarios.name = c("SoilGrids_min","SoilGrids_mean","SoilGrids_max")

vars <- var.names
fac.ORC <- c(1/1000*365,1/1000*365,1,1/1000,1,1/1000,1,1)

file.frac <- "./outputs/spinup/ORC/ORC_spinup_Smin_VEGFRAC.gri"
raster.frac <- stack(file.frac)
frac.array <- as.array(raster.frac)

lons <- seq(extent(raster.frac)[1],extent(raster.frac)[2],0.5)
lats <- seq(extent(raster.frac)[4],extent(raster.frac)[3],-0.5)

df.all.ORC <- data.frame()
for (isoil in seq(1,length(soil.scenarios))){
  for (ivar in seq(1,length(vars.ORCH))){

    if (vars.ORCH[ivar] %in% c("top.sand","top.clay")){
      file.in <- paste0("./maps/soilgrid_",vars.ORCH[ivar],"_",soil.map.names[isoil],"_resampled.gri")
    } else if (vars.ORCH[ivar] == "AGB") {
      file.in <- paste0("./outputs/spinup/ORC/ORC_spinup_",soil.scenarios[isoil],"_LEAF_M.gri")
    } else {
      file.in <- paste0("./outputs/spinup/ORC/ORC_spinup_",soil.scenarios[isoil],"_",vars.ORCH[ivar],".gri")
    }


    if (!file.exists(file.in)){
      next()
    }

    raster.in <- stack(file.in)
    var.in <- as.array(raster.in)

    if (vars.ORCH[ivar] %in% c("top.sand","top.clay")){

      var <- t(base::as.matrix(var.in[,,1],dimnames = list(lon,lat)))

      clats <- sort(seq(extent(raster.in)[3]+0.25,extent(raster.in)[4],0.5),decreasing = TRUE)
      clons <- seq(extent(raster.in)[1]+0.25,extent(raster.in)[2],0.5)
      var.df <- melt(var) %>% rename(lat = Var2,
                                     lon = Var1) %>% mutate(lat = clats[lat],
                                                            lon = clons[lon],
                                                            value = value)

    } else if (vars.ORCH[ivar] == "AGB"){

      var.in <- as.array(stack(paste0("./outputs/spinup/ORC/ORC_spinup_",soil.scenarios[isoil],"_LEAF_M.gri"))) +
        as.array(stack(paste0("./outputs/spinup/ORC/ORC_spinup_",soil.scenarios[isoil],"_HEART_M_AB.gri"))) +
        as.array(stack(paste0("./outputs/spinup/ORC/ORC_spinup_",soil.scenarios[isoil],"_SAP_M_AB.gri")))

      var <- t(base::as.matrix(apply(var.in*frac.array,c(1,2),sum),dimnames = list(lon,lat)))

      var.df <- melt(var) %>% rename(lat = Var2,
                                     lon = Var1) %>% mutate(lat = lats[lat],
                                                            lon = lons[lon],
                                                            value = fac.ORC[ivar]*value)

      # ggplot(data = var.df) +
      #   geom_raster(aes(x=lon, y = lat, fill = value),alpha = 0.3) +
      #   geom_sf(data = world,fill = NA) +
      #   coord_sf(xlim = c(-84.5, -30.5), ylim = c(-19.5, 15.5), expand = FALSE) +
      #   labs(x = "",y = "") +
      #   theme_bw()




    } else {
      var <- t(base::as.matrix(apply(var.in*frac.array,c(1,2),sum),dimnames = list(lon,lat)))

      var.df <- melt(var) %>% rename(lat = Var2,
                                     lon = Var1) %>% mutate(lat = lats[lat],
                                                            lon = lons[lon],
                                                            value = fac.ORC[ivar]*value)

    }



    df.all.ORC <- bind_rows(list(df.all.ORC,
                                 var.df %>% mutate(scenario = soil.scenarios.name[isoil],
                                                   var = vars[ivar])))

  }
}


#######################################################################################
# ED2

df.all.ED2 <- readRDS(file = "/home/femeunier/Documents/projects/SoilSensitivity/outputs/df_SoilSens_analysis.RDS") %>%
  dplyr::select(c("scenario","lat","lon",vars)) %>% pivot_longer(cols = vars,
                                                                 names_to = "var",
                                                                 values_to = "value")


df.all <- bind_rows(list(df.all.LPJ %>% mutate(model = "LPJ.GUESS"),
                         df.all.ORC %>% mutate(model = "ORCHIDEE"),
                         df.all.ED2 %>% mutate(model = "ED2")))

df.wide <- df.all %>% pivot_wider(names_from = model,
                                  values_from = value)

df.long.omit <- df.wide %>% filter(!is.na(ED2)) %>%
  pivot_longer(cols = c("LPJ.GUESS","ORCHIDEE","ED2"),
               names_to = "model",
               values_to = "value")

df.wide.all <- df.long.omit %>% pivot_wider(names_from = var,
                                            values_from = value)

df.wide.ED2 <- df.wide.all %>% filter(model == "ED2")
df.wide.LPJ <- df.wide.all %>% filter(model == "LPJ.GUESS")
df.wide.ORC <- df.wide.all %>% filter(model == "ORCHIDEE")
df.wide.var <- bind_rows(list(df.wide.ED2,
                              df.wide.LPJ,
                              df.wide.ORC))

# df.sum <- df.all %>% group_by(scenario,model,var) %>% summarise(m = mean(value,na.rm = TRUE)) %>% arrange(var,scenario)
# df.sum %>% filter(var == "fsw")
# df.sum %>% filter(var == "clay")

df.long.omit.sum <- df.long.omit %>% group_by(scenario,model,var) %>% summarise(m = mean(value,na.rm = TRUE)) %>% arrange(var,scenario)
df.long.omit.sum %>% filter(var == "fsw")
df.long.omit.sum %>% filter(var == "clay")
df.long.omit.sum %>% filter(var == "AGB")

saveRDS(df.wide.var %>% mutate(model = case_when(model == "LPJ.GUESS" ~ "LPJ-GUESS",
                                                 TRUE ~ model),
                               scenario = case_when(scenario == "SoilGrids_mean" ~ "Mean clay",
                                                    scenario == "SoilGrids_max" ~ "Max. clay",
                                                    scenario == "SoilGrids_min" ~ "Min. clay")) %>%
          mutate(model = factor(model,levels = c("ORCHIDEE","ED2","LPJ-GUESS"))),
        file = "./outputs/output_maps2.RDS")

df.long.omit.mod <- df.long.omit %>% mutate(model = case_when(model == "LPJ.GUESS" ~ "LPJ-GUESS",
                                                                        TRUE ~ model),
                                            scenario = case_when(scenario == "SoilGrids_mean" ~ "Mean clay",
                                                                 scenario == "SoilGrids_max" ~ "Max. clay",
                                                                 scenario == "SoilGrids_min" ~ "Min. clay")) %>%
  mutate(model = factor(model,levels = c("ORCHIDEE","ED2","LPJ-GUESS")))


saveRDS(df.long.omit.mod,file = "./outputs/output_maps.RDS")

#######################################################################################
# plots

ggplot(data = df.long.omit.mod  %>% filter(lat<=ED_REG_LATMAX,lat>=ED_REG_LATMIN,lon<=ED_REG_LONMAX,lon>=ED_REG_LONMIN,
                                 var %in% c("AGB"))) +
  geom_raster(aes(x=lon, y = lat, fill = value),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-84.5, -30.5), ylim = c(-19.5, 15.5), expand = FALSE) +
  scale_fill_gradient2(low = "darkred",mid = "grey",high = "darkgreen",na.value = NA) +
  labs(x = "",y = "") +
  facet_grid(scenario ~ model) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  labs(x = "",y = "",fill = "AGB (kgC/m²)")


ggsave(filename = "./Figures/AGBmap.png",
       plot = last_plot(),width = 50,height = 35, unit = "cm",
       dpi = 300)


df.long.omit.mod.rel <- df.long.omit.mod %>% group_by(model,var,scenario) %>% mutate(value.rel = (value - min(value,na.rm = TRUE))/
                                                                                       (max(value,na.rm = TRUE) - min(value,na.rm = TRUE)))

ggplot(data = df.long.omit.mod.rel  %>% filter(lat<=ED_REG_LATMAX,lat>=ED_REG_LATMIN,lon<=ED_REG_LONMAX,lon>=ED_REG_LONMIN,
                                           var %in% c("AGB"))) +
  geom_raster(aes(x=lon, y = lat, fill = value.rel),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-84.5, -30.5), ylim = c(-19.5, 15.5), expand = FALSE) +
  scale_fill_gradient2(low = "darkred",mid = "grey",high = "darkgreen",na.value = NA) +
  labs(x = "",y = "") +
  facet_grid(scenario ~ model) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  labs(x = "",y = "",fill = "AGB (kgC/m²)")

ggplot(data = df.wide.var,
       aes(x = fsw , y = biomass,color = scenario)) +
  geom_point() +
  facet_wrap(~ model) +
  theme_bw()


ggplot(data = df.wide.var,
       aes(x = scenario , y = fsw,color = scenario)) +
  geom_boxplot() +
  facet_wrap(~ model,scales = "free") +
  theme_bw()


ggplot(data = df.all  %>% filter(lat<=ED_REG_LATMAX,lat>=ED_REG_LATMIN,lon<=ED_REG_LONMAX,lon>=ED_REG_LONMIN,
                                 var == "AGB")) +
  geom_density(aes(x = value,color = scenario)) +
  facet_wrap(~ model) +
  theme_bw()

ggplot(data = df.all  %>% filter(lat<=ED_REG_LATMAX,lat>=ED_REG_LATMIN,lon<=ED_REG_LONMAX,lon>=ED_REG_LONMIN,
                                 var == "AGB")) +
  geom_density(aes(x = value,color  = scenario),fill = NA,position="dodge") +
  facet_wrap(~ model) +
  theme_bw()

ggplot(data = df.wide %>% filter(var == "LAI")) +
  geom_point(aes(x = ORCHIDEE,y = ED2), color = "black") +
  geom_point(aes(x = ORCHIDEE,y = LPJ.GUESS), color = "blue") +
  geom_point(aes(x = ED2,y = LPJ.GUESS), color = "red") +
  theme_bw()


ggplot(data = df.long.omit  %>% filter(lat<=ED_REG_LATMAX,lat>=ED_REG_LATMIN,lon<=ED_REG_LONMAX,lon>=ED_REG_LONMIN,
                                 var == "LAI")) +
  geom_raster(aes(x=lon, y = lat, fill = value),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-84.5, -30.5), ylim = c(-19.5, 15.5), expand = FALSE) +
  scale_fill_gradient2(low = "darkred",mid = "darkgrey",high = "darkgreen",na.value = NA) +
  labs(x = "",y = "") +
  facet_grid(scenario ~ model) +
  theme_bw()

ggplot(data = df.long.omit  %>% filter(lat<=ED_REG_LATMAX,lat>=ED_REG_LATMIN,lon<=ED_REG_LONMAX,lon>=ED_REG_LONMIN)) +
  geom_density(aes(x = value,color = scenario)) +
  facet_wrap(var ~ model,scales = "free",ncol = 3) +
  theme_bw()


ggplot(data = df.long.omit  %>% filter(lat<=ED_REG_LATMAX,lat>=ED_REG_LATMIN,lon<=ED_REG_LONMAX,lon>=ED_REG_LONMIN,
                                       var == "clay")) +
  geom_raster(aes(x=lon, y = lat, fill = value),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-84.5, -30.5), ylim = c(-19.5, 15.5), expand = FALSE) +
  scale_fill_gradient2(low = "darkred",mid = "darkgrey",high = "darkgreen",na.value = NA) +
  labs(x = "",y = "") +
  facet_grid(scenario ~ model) +
  theme_bw()
