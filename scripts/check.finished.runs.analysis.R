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

ED_REG_LATMIN = -19.5
ED_REG_LATMAX =  13.5
ED_REG_LONMIN = -84.5
ED_REG_LONMAX = -30.5

GRID_RES = 1

fyear = 2010

X = seq(ED_REG_LONMIN,ED_REG_LONMAX,GRID_RES)
Y = seq(ED_REG_LATMIN,ED_REG_LATMAX,GRID_RES)

ref_dir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/SoilSensitivity/run"
rundir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/SoilSensitivity/run/grid"
ICdir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/LSliana/IC/"
outdir <- "/kyukon/scratch/gent/vo/000/gvo00074/felicien/SoilSensitivity/out"

scenars <- c("SoilGrids_mean","SoilGrids_min","SoilGrids_max")

land <- readRDS(file.path("maps","landmask.RDS"))

df.restart <- data.frame()

Nsimuperjob = 60
isimu = 0

list_dir <- list()
ed_exec <- "/user/scratchkyukon/gent/gvo000/gvo00074/felicien/ED2/ED/build/ed_2.1-opt-master-2bb6872"

all_depths <- c(-8,-7,-6.2,-5.5,-4.9,-4.3,-3.8,-3.3,-2.8,-2.3,-1.8,-1.3,-1,-0.6,-0.3,-0.15)

df.temp <- data.frame()

for(ix in seq(1,length(X))){
  for(iy in seq(1,length(Y))){
    clat = Y[iy]; clon = X[ix]

    if (land[which(rownames(land)==clon),which(colnames(land)==clat)] != 0){
      for (iscenar in seq(1,length(scenars))){

        run_name <- paste0("SoilSens_Amazon_historical_",scenars[iscenar],"_X_",abs(X[ix]),ifelse(X[ix]<0,"W","E"),
                           "_Y_",abs(Y[iy]),ifelse(Y[iy]<0,"S","N"))

        run_ref <- file.path(rundir,run_name)
        out_ref <- file.path(outdir,run_name)

        analy.file <- file.path(out_ref,"analy","analysis-Q-2010-12-00-000000-g01.h5")
        histo.file <- file.path(out_ref,"histo","history-S-2010-01-01-000000-g01.h5")

        df.temp <- bind_rows(list(df.temp,
                                  data.frame(name = run_name,
                                             lat = Y[iy],
                                             lon = X[ix],
                                             analy = file.exists(analy.file),
                                             histo = file.exists(histo.file))))

      }
    }
  }
}
