rm(list = ls())

library(dplyr)
library(tidyr)
library(purrr)
library(rrtm)
library(ED2scenarios)
library(PEcAn.ED2)
library(purrr)
library(ggplot2)
library(ggridges)
library(cowplot)
library(pracma)
library(BayesianTools)
library(raster)
library(rhdf5)
library(stringr)

source("/data/gent/vo/000/gvo00074/felicien/R/h5read_opt.r")

# source("/home/femeunier/Documents/ED2/R-utils/h5read_opt.r")
# h5file <- file.path("/home/femeunier/Documents/projects/SoilSensitivity/outputs/analysis-Q-2009-01-00-000000-g01.h5")
# mymont  = lapply(h5read_opt(h5file),FUN=aperm)

ED_REG_LATMIN = -19.5
ED_REG_LATMAX =  13.5
ED_REG_LONMIN = -84.5
ED_REG_LONMAX = -30.5

GRID_RES = 1

X = seq(ED_REG_LONMIN,ED_REG_LONMAX,GRID_RES)
Y = seq(ED_REG_LATMIN,ED_REG_LATMAX,GRID_RES)

ref_dir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/SoilSensitivity/run"
rundir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/SoilSensitivity/run/grid"
ICdir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/LSliana/IC/"
outdir <- "/kyukon/scratch/gent/vo/000/gvo00074/felicien/SoilSensitivity/out"

scenars <- c("SoilGrids_mean","SoilGrids_min","SoilGrids_max")

land <- readRDS(file.path("maps","landmask.RDS"))

df <- data.frame()

for(ix in seq(1,length(X))){
  for(iy in seq(1,length(Y))){
    clat = Y[iy]; clon = X[ix]

    if (land[which(rownames(land)==clon),which(colnames(land)==clat)] != 0){
      for (iscenar in seq(1,length(scenars))){

        cat("\r progress = ",format((ix/length(X))*100,nsmall=2),"%")

        run_name <- paste0("SoilSens_Amazon_historical_",scenars[iscenar],"_X_",abs(X[ix]),ifelse(X[ix]<0,"W","E"),
                           "_Y_",abs(Y[iy]),ifelse(Y[iy]<0,"S","N"))

        run_ref <- file.path(rundir,run_name)
        out_ref <- file.path(outdir,run_name)

        agb <- lai <- gpp <- npp <- bleaf <- broot <- leaf.resp <- pl.resp <- rh <- total.soil.c <-
          sand <- clay <- structsc <- ssc <- fsc <- fsw <- biomass <- c()
        for (imonth in seq(1,1)){
          h5file <- file.path(out_ref,"analy",paste0("analysis-Q-2010-",sprintf("%02d",imonth),"-00-000000-g01.h5"))
          if (file.exists(h5file)){

            # nc <- nc_open()
            mymont  <- lapply(h5read_opt(h5file),FUN=aperm)

            df.patch <- data.frame(dbh = mymont$DBH,
                                   AGB = mymont$AGB_CO,
                                   pa = rep(1:mymont$NPATCHES_GLOBAL,mymont$PACO_N),
                                   LAI = mymont$LAI_CO,
                                   area = rep(mymont$AREA,mymont$PACO_N),
                                   h = mymont$HITE,
                                   n = mymont$NPLANT,
                                   pft = mymont$PFT) %>% group_by(pa,pft) %>% summarise(AGB = sum(AGB*n),
                                                                                        LAI = sum(LAI),
                                                                                        area = unique(area),
                                                                                        .groups = "keep") %>%
              group_by(pft) %>% summarise(agb = weighted.mean(AGB,area),
                                          lai = weighted.mean(LAI,area),
                                          .groups = "keep")

          }
        }

        df <- bind_rows(list(df,
                             df.patch %>% mutate(scenario = scenars[iscenar],
                                                 year = 2010,
                                                 lat = clat,
                                                 lon = clon,

                                                 sand = mymont$SLXSAND,
                                                 clay = mymont$SLXCLAY)))
      }
    }
  }
}

saveRDS(df,"df_SoilSens_analysis_patch.RDS")

# scp /home/femeunier/Documents/projects/SoilSensitivity/scripts/analyze_patch_SoilSens_analysis.R hpc:/data/gent/vo/000/gvo00074/felicien/R
