rm(list = ls())

library(raster)
library(reshape2)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ncdf4)
library(reshape2)

########################################################################################

ED_REG_LATMIN = -19.75
ED_REG_LATMAX =  14.75
ED_REG_LONMIN = -89.75
ED_REG_LONMAX = -30.25

vars.ORCH <- c("GPP","NPP","LAI","TOTAL_M","MOISTRESS","AGB","TOTAL_SOIL_CARB","VEGET_COV_MAX")
var.names <- c("GPP","NPP","LAI","biomass","fsw","AGB","soilC","veg.cover") # ED2

fac.ORC <- c(1/1000*365,1/1000*365,1,1/1000,1,1/1000,1/1000,1)

#######################################################################################
# ORCHIDEE

soil.scenarios = c("Smin","Smean","Smax")
soil.scenarios.name = c("SoilGrids_min","SoilGrids_mean","SoilGrids_max")

directory <- "/data/gent/vo/000/gvo00074/SoilSenv_outputs/historical"

# fac.ORC <- c(1/1000*365,1/1000*365,1,1/1000,1,1/1000,1/1000,1)

df.all.ORC <- data.frame()
for (isoil in seq(1,length(soil.scenarios))){
  print(paste0("",soil.scenarios[isoil]))
  for (ivar in seq(1,length(vars.ORCH))){
    print(paste0("- ",vars.ORCH[ivar]))
    cvar <- vars.ORCH[ivar]
    file.in <- file.path(directory,paste0("ORC_",soil.scenarios[isoil],"_1980-2016.nc"))

    if (!file.exists(file.in)){
      next()
    }

    ncfile <- nc_open(file.in)

    if (ivar == 1){
      all.lats <- ncfile$dim$lat$vals
      all.lons <- ncfile$dim$lon$vals
    }

    if (cvar == "AGB"){
      var <- ncvar_get(nc = ncfile,"LEAF_M") + ncvar_get(nc = ncfile,"HEART_M_AB") + ncvar_get(nc = ncfile,"SAP_M_AB")
    } else{
      var <- ncvar_get(nc = ncfile,cvar)
    }

    var.lon <- melt(var[,,,361:372]) %>% rename(lon = Var1,
                                                lat = Var2,
                                                pft = Var3,
                                                month = Var4) %>% mutate(lon = all.lons[lon],
                                                                         lat = all.lats[lat],
                                                                         year = 2010,
                                                                         variable = var.names[ivar],
                                                                         value = fac.ORC[ivar]*value)

    df.all.ORC <- bind_rows(list(df.all.ORC,
                                 var.lon %>% mutate(scenario = soil.scenarios.name[isoil])))

  }
}

saveRDS(df.all.ORC,file = "./Outputs_ORCHIDEE.RDS")

# scp /home/femeunier/Documents/projects/SoilSensitivity/scripts/analyze_OP_ORCHIDEE.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
