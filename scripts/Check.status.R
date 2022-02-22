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

scenars <- c("SoilGrids_mean","SoilGrids_min","SoilGrids_max")

land <- readRDS(file.path("maps","landmask.RDS"))

status.all <- data.frame()

for(ix in seq(1,length(X))){
  for(iy in seq(1,length(Y))){
    clat = Y[iy]; clon = X[ix]

    if (land[which(rownames(land)==clon),which(colnames(land)==clat)] != 0){

      for (iscenar in seq(1,length(scenars))){

        run_name <- paste0("SoilSens_Amazon_IC_",scenars[iscenar],"_X_",abs(X[ix]),ifelse(X[ix]<0,"W","E"),
                           "_Y_",abs(Y[iy]),ifelse(Y[iy]<0,"S","N"))

        run_ref <- file.path(rundir,run_name)
        out_ref <- file.path(outdir,run_name)

        ed2in <- read_ed2in(file.path(run_ref,"ED2IN"))
        lat <- ed2in$POI_LAT
        lon <- ed2in$POI_LON

        status.file <- file.path(run_ref,"status.txt")
        if (!file.exists(status.file)){
          status <- "Not started"
        } else {
          lines <- readLines(status.file)
          if (lines[length(lines)] == "SUCCESS"){
            status <- "Success"
          } else if (lines[length(lines)] == "ERROR"){
            status <- "Error"
          } else {
            status <- "Started"
          }
        }

        if (status %in% c("Error","Started")){
          details.file <- file.info(list.files(path = file.path(out_ref,"histo"), full.names = TRUE,pattern = ".h5"))
          files.OP.ordered <- details.file[with(details.file, order(as.POSIXct(mtime),decreasing = TRUE)), ]
          fyear <- as.numeric(str_split(basename(rownames(files.OP.ordered)[1]),pattern = "-")[[1]][3])

        } else if (status == "Success") {
          fyear <- 1930
        } else {
          fyear <-1900
        }

        df.status <- data.frame(scenario = scenars[iscenar],
                                lat,lon,
                                status,final.year = fyear)
        status.all <- bind_rows(list(status.all,
                                     df.status))
      }
    }
  }
}

saveRDS(status.all,"status.all.RDS")

# scp /home/femeunier/Documents/projects/SoilSensitivity/scripts/Check.status.R hpc:/data/gent/vo/000/gvo00074/felicien/R
