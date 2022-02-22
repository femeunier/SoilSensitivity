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

var.name <- c("GPP") # ED2
var.LPJ <- c("agpp.out")

directory <- "/data/gent/vo/000/gvo00074/SoilSens_output_LPJGUESS/"

df.all.LPJ <- data.frame()
for (isoil in seq(1,length(scenarios))){
  print(soil.scenarios.name[isoil])

  file.in <- file.path(directory,paste0("historical_",scenarios[isoil]),var.LPJ)

  data <- read.table(file.in,header =TRUE,stringsAsFactors=FALSE) %>%
    filter(Year == 2010)

  df.all.LPJ <- bind_rows(list(df.all.LPJ,
                               data %>% mutate(scenario = soil.scenarios.name[isoil],
                                               variable = var.name)))
}

saveRDS(df.all.LPJ,file = "./Outputs_LPJ_PFT.RDS")

# scp /home/femeunier/Documents/projects/SoilSensitivity/scripts/analyze_OP_PFT_LPJ.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
