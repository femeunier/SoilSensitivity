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

X = seq(ED_REG_LONMIN,ED_REG_LONMAX,GRID_RES)
Y = seq(ED_REG_LATMIN,ED_REG_LATMAX,GRID_RES)

ref_dir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/SoilSensitivity/run"
rundir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/SoilSensitivity/run/grid"
ICdir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/LSliana/IC/"
outdir <- "/kyukon/scratch/gent/vo/000/gvo00074/felicien/SoilSensitivity/out"

ed2in_ref <- read_ed2in(file.path(ref_dir,"ED2IN"))
scenars <- c("SoilGrids_mean","SoilGrids_min","SoilGrids_max")
cssfile_base <- "Amazon"

land <- readRDS(file.path("maps","landmask.RDS"))

df.restart <- data.frame()

Nsimuperjob = 1
isimu = 0

list_dir <- list()
ed_exec <- "/user/scratchkyukon/gent/gvo000/gvo00074/felicien/ED2/ED/build/ed_2.1-opt-master-2bb6872"

soilgrids_mean <- stack(brick(file.path("maps","soilgrid_top.sand_mean_resampled.grd")),
                        brick(file.path("maps","soilgrid_top.clay_mean_resampled.grd")))

soilgrids_min <- stack(brick(file.path("maps","soilgrid_top.sand_min_resampled.grd")),
                       brick(file.path("maps","soilgrid_top.clay_min_resampled.grd")))

soilgrids_max <- stack(brick(file.path("maps","soilgrid_top.sand_max_resampled.grd")),
                       brick(file.path("maps","soilgrid_top.clay_max_resampled.grd")))
all_depths <- c(-8,-7,-6.2,-5.5,-4.9,-4.3,-3.8,-3.3,-2.8,-2.3,-1.8,-1.3,-1,-0.6,-0.3,-0.15)

for(ix in seq(1,length(X))){
  for(iy in seq(1,length(Y))){
    clat = Y[iy]; clon = X[ix]

    if (land[which(rownames(land)==clon),which(colnames(land)==clat)] != 0){

      for (iscenar in seq(1,length(scenars))){


        run_name <- paste0("SoilSens_Amazon_IC_",scenars[iscenar],"_X_",abs(X[ix]),ifelse(X[ix]<0,"W","E"),
                             "_Y_",abs(Y[iy]),ifelse(Y[iy]<0,"S","N"))


        run_origin <- file.path(rundir,run_name)
        out_origin <- file.path(outdir,run_name)

        histo_origin <- file.path(out_origin,"histo")

        for (year in seq(1902,2010,10)){
            file2delete <- file.path(histo_origin,paste0("history-S-",year,"-",sprintf("%02d",month),"-01-000000-g01.h5"))
            if (file.exists(file2delete)){
              system2("rm",file2delete)
          }
        }
      }
    }
  }
}

# scp /home/femeunier/Documents/projects/SoilSensitivity/scripts/delete_some_files.R hpc:/data/gent/vo/000/gvo00074/felicien/R
