rm(list = ls())

library(ncdf4)
library(reshape2)

scenarios <- c("Smean","Smax","Smin")
soil.scenarios.name <- c("SoilGrids_mean","SoilGrids_max","SoilGrids_min")

time <- 1980 + (1:(37*12) - 1)/12

df.all <- data.frame()

for (iscenar in seq(1,length(scenarios))){

  print(soil.scenarios.name[iscenar])

  file <- paste0("/data/gent/vo/000/gvo00074/SoilSenv_outputs/historical/ORC_",scenarios[iscenar],"_1980-2016.nc")

  nc <- nc_open(file)

  GPP <- ncvar_get(nc,"GPP")
  lon <- ncvar_get(nc,"lon")
  lat <- ncvar_get(nc,"lat")
  veget <- ncvar_get(nc,"veget")
  veget_cover <- ncvar_get(nc,"VEGET_COV_MAX")

  df.GPP <- melt(GPP*veget_cover) %>% mutate(Var1 = lon[Var1],
                                             Var2 = lat[Var2],
                                             Var3 = veget[Var3],
                                             Var4 = time[Var4]) %>%
    rename(lon = Var1,
           lat = Var2,
           PFT = Var3,
           time = Var4)

  df.GPP.select <- df.GPP %>% filter(time >= 2010, time <= 2011) %>% mutate(scenario = soil.scenarios.name[iscenar])

  nc_close(nc)

  df.all <- bind_rows(list(df.all,
                           df.GPP.select))
}

saveRDS(df.all,file = "./Outputs_ORC_PFT.RDS")

# scp /home/femeunier/Documents/projects/SoilSensitivity/scripts/analyze_OP_PFT_ORC.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
