rm(list = ls())

library(dplyr)
library(tidyr)
library(PEcAnRTM)
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

all_depths <- c(-8,-7,-6.2,-5.5,-4.9,-4.3,-3.8,-3.3,-2.8,-2.3,-1.8,-1.3,-1,-0.6,-0.3,-0.15)
df <- data.frame()

for(ix in seq(1,length(X))){

  cat("\r progress = ",
      format((ix/length(X))*100,nsmall=2),"%")

  for(iy in seq(1,length(Y))){
    clat = Y[iy]; clon = X[ix]

    if (land[which(rownames(land)==clon),which(colnames(land)==clat)] != 0){

      for (iscenar in seq(1,length(scenars))){

        # run_name <- paste0("SoilSens_Amazon_IC_",scenars[iscenar],"_X_",abs(X[ix]),ifelse(X[ix]<0,"W","E"),
        #                    "_Y_",abs(Y[iy]),ifelse(Y[iy]<0,"S","N"))
        run_name <- paste0("SoilSens_Amazon_historical_",scenars[iscenar],"_X_",abs(X[ix]),ifelse(X[ix]<0,"W","E"),
                           "_Y_",abs(Y[iy]),ifelse(Y[iy]<0,"S","N"))

        run_ref <- file.path(rundir,run_name)
        out_ref <- file.path(outdir,run_name)

        if (!dir.exists(run_ref)) next()

        ed2in <- read_ed2in(file.path(run_ref,"ED2IN"))
        clay <- ed2in$SLXCLAY
        sand <- ed2in$SLXSAND

        details.file <- file.info(list.files(path = file.path(out_ref,"histo"), full.names = TRUE,pattern = ".h5"))

        if (nrow(details.file)>0){
          files.OP.ordered <- details.file[with(details.file, order(as.POSIXct(mtime),decreasing = TRUE)), ]

          fyear <- 2010

          for (year in sort(unique(c(fyear,seq(1910,fyear,50))))){
            h5file <- file.path(dirname(rownames(files.OP.ordered)[1]),paste0("history-S-",year,"-01-01-000000-g01.h5"))
            h5file.name <- basename(h5file)

            if (file.exists(h5file)){
              mymont    = lapply(h5read_opt(h5file),FUN=aperm)
              names(mymont) <- gsub(x = names(mymont), pattern = "\\_", replacement = ".")

              lat = mymont$LATITUDE
              lon = mymont$LONGITUDE
              AGB = sum(mymont$AGB.PY)
              AGB.tree = sum(mymont$AGB.PY[1,,c(2,3,4)])
              LAI = sum(mymont$LAI.PY)
              LAI.tree = sum(mymont$LAI.PY[1,,c(2,3,4)])
              GPP.tree = sum(mymont$LAI.PY[1,,c(2,3,4)])

              df <- bind_rows(list(df,
                                   data.frame(scenario = scenars[iscenar],
                                              lat,lon,
                                              AGB,AGB.tree,
                                              LAI,LAI.tree,
                                              clay,sand,
                                              year)))
            }
          }
        }
      }
    }
  }
}

saveRDS(df,"df_SoilSens_dyn.RDS")

# scp /home/femeunier/Documents/projects/SoilSensitivity/scripts/analyze_dynamics_SoilSens_SG.R hpc:/data/gent/vo/000/gvo00074/felicien/R
