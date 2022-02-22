rm(list = ls())

library(raster)
library(reshape2)
library(dplyr)
library(ggplot2)
library(tidyr)

########################################################################################

ED_REG_LATMIN = -19.5
ED_REG_LATMAX =  13.5
ED_REG_LONMIN = -84.5
ED_REG_LONMAX = -30.5

########################################################################################
# LPJ

scenarios <- c("output_mean","output_max","output_min")
soil.scenarios.name <- c("SoilGrids_mean","SoilGrids_max","SoilGrids_min")

var.names <- c("GPP","NPP","LAI","biomass","fsw","AGB","soilC") # ED2
vars.LPJ <- c("agpp.out","anpp.out","lai.out","cpool.out","wscal.out","cpool.out","cpool.out")

directory <- "/data/gent/vo/000/gvo00074/SoilSens_output_LPJGUESS/"

df.all.LPJ <- data.frame()
for (isoil in seq(1,length(scenarios))){
  print(soil.scenarios.name[isoil])
  for (ivar in seq(1,length(vars.LPJ))){
    print(paste0("- ",var.names[ivar]))

    file.in <- file.path(directory,paste0("historical_",scenarios[isoil]),vars.LPJ[ivar])

    if (!file.exists(file.in)){
      next()
    }
    if (vars.LPJ[ivar] == "cpool.out" & var.names[ivar] == "soilC"){
      data <- read.table(file.in,header =TRUE,stringsAsFactors=FALSE) %>%
        dplyr::select(c(Lon,Lat,Year,LitterC,SoilC)) %>% filter(Year == 2010) %>% mutate(soilC = LitterC + SoilC) %>%
        rename(lat = Lat,
               lon = Lon,
               value = soilC) %>% dplyr::select(lat,lon,value)

    } else if (vars.LPJ[ivar] == "cpool.out" & var.names[ivar] == "AGB"){
      data <- read.table(file.in,header =TRUE,stringsAsFactors=FALSE) %>%
        dplyr::select(c(Lon,Lat,Year,VegC)) %>% filter(Year == 2010) %>% mutate(AGB = 0.7*VegC) %>%
        rename(lat = Lat,
               lon = Lon,
               value = AGB) %>% dplyr::select(lat,lon,value)

    } else if (vars.LPJ[ivar] == "cpool.out"){
      data <- read.table(file.in,header =TRUE,stringsAsFactors=FALSE) %>%
        dplyr::select(c(Lon,Lat,Year,VegC)) %>% filter(Year == 2010) %>%
        rename(lat = Lat,
               lon = Lon,
               value = VegC) %>% dplyr::select(lat,lon,value)

    }  else if (vars.LPJ[ivar] == "wscal.out"){

      data <- read.table(file.in,header = TRUE,stringsAsFactors = FALSE) %>%
        filter(Year == 2010)

      data.wscal <- data %>% dplyr::select(-c(Lon,Lat,Year))

      lai <- read.table(file.path(directory,paste0("historical_",scenarios[isoil]),"lai.out"),header = TRUE,stringsAsFactors = FALSE) %>%
        filter(Year == 2010) %>% dplyr::select(-c(Lon,Lat,Year,Total))

      data <- data %>% mutate(value = rowSums(lai*data.wscal)/rowSums(lai)) %>%
        rename(lon = Lon,lat = Lat) %>%
        dplyr::select(c(lat,lon,value))

    } else{
      data <- read.table(file.in,header =TRUE,stringsAsFactors=FALSE) %>%
        dplyr::select(c(Lon,Lat,Year,Total)) %>% filter(Year == 2010) %>%
        rename(lat = Lat,
               lon = Lon,
               value = Total) %>% dplyr::select(lat,lon,value)
    }

    df.all.LPJ <- bind_rows(list(df.all.LPJ,
                                 data %>% mutate(scenario = soil.scenarios.name[isoil],
                                                 variable = var.names[ivar])))

  }
}

saveRDS(df.all.LPJ,file = "./Outputs_LPJ.RDS")

# scp /home/femeunier/Documents/projects/SoilSensitivity/scripts/analyze_OP_LPJ.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
