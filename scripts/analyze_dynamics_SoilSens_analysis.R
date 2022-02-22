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
# h5file <- file.path("/home/femeunier/Documents/projects/SoilSensitivity/outputs/analysis-Q-1950-01-00-000000-g01.h5")
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
            mymont    = lapply(h5read_opt(h5file),FUN=aperm)

            agb <- c(agb,sum(mymont$AGB_PY))
            biomass <- c(biomass, sum(mymont$BDEAD_PY) + sum(mymont$BALIVE_PY) + sum(mymont$MMEAN_BSTORAGE_PY) + sum(mymont$BSEEDS_PY))
            lai <- c(lai,sum(mymont$MMEAN_LAI_PY))

            gpp <- c(gpp,mymont$MMEAN_GPP_PY)
            npp <- c(npp,mymont$MMEAN_NPP_PY)
            bleaf <- c(bleaf,sum(mymont$MMEAN_BLEAF_PY))
            broot <- c(broot,sum(mymont$MMEAN_BROOT_PY))
            leaf.resp <- c(leaf.resp,mymont$MMEAN_LEAF_RESP_PY)
            pl.resp <- c(pl.resp,mymont$MMEAN_PLRESP_PY)
            rh <- c(rh,mymont$MMEAN_RH_PY)
            total.soil.c <- c(total.soil.c,mymont$MMEAN_STRUCT_SOIL_C_PY + mymont$MMEAN_SLOW_SOIL_C_PY + mymont$MMEAN_FAST_SOIL_C_PY)
            sand <- c(sand,mymont$SLXSAND)
            clay <- c(clay,mymont$SLXCLAY)
            fsw <- c(fsw,mymont$MMEAN_FSW_PY)
            structsc <- c(structsc,mymont$MMEAN_STRUCT_SOIL_C_PY)
            ssc <- c(ssc,mymont$MMEAN_SLOW_SOIL_C_PY)
            fsc <- c(fsc,mymont$MMEAN_FAST_SOIL_C_PY)

          }
        }

        df <- bind_rows(list(df,
                             data.frame(scenario = scenars[iscenar],
                                        year = 2010,
                                        lat = clat,
                                        lon = clon,
                                        AGB = mean(agb),
                                        biomass = mean(biomass),
                                        LAI = mean(lai),
                                        GPP = mean(gpp),
                                        NPP = mean(npp),
                                        Bleaf = mean(bleaf),
                                        Broot = mean(broot),
                                        leaf_resp = mean(leaf.resp),
                                        plant_resp = mean(pl.resp),

                                        het_resp = mean(rh),
                                        total_soil_carb = mean(total.soil.c),
                                        sand = mean(sand),
                                        clay = mean(clay),

                                        fsw = mean(fsw),
                                        StructSC = mean(structsc),
                                        SSC = mean(ssc),
                                        FSC = mean(fsc),
                                        soilC = mean(structsc) + mean(ssc) + mean(fsc))
                             ))
      }
    }
  }
}

saveRDS(df,"Outputs_ED2.RDS")

# scp /home/femeunier/Documents/projects/SoilSensitivity/scripts/analyze_dynamics_SoilSens_analysis.R hpc:/data/gent/vo/000/gvo00074/felicien/R
