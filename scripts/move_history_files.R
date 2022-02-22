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


for(ix in seq(1,length(X))){
  for(iy in seq(1,length(Y))){
    clat = Y[iy]; clon = X[ix]

    if (land[which(rownames(land)==clon),which(colnames(land)==clat)] != 0){

      for (iscenar in seq(1,length(scenars))){

        run_name <- paste0("SoilSens_Amazon_IC_",scenars[iscenar],"_X_",abs(X[ix]),ifelse(X[ix]<0,"W","E"),
                           "_Y_",abs(Y[iy]),ifelse(Y[iy]<0,"S","N"))

        run_ref <- file.path(rundir,run_name)
        out_ref <- file.path(outdir,run_name)

        # details.file <- file.info(list.files(path = file.path(out_ref,"histo"), full.names = TRUE,pattern = ".h5"))
        # files.OP.ordered <- details.file[with(details.file, order(as.POSIXct(mtime),decreasing = TRUE)), ]
        # fyear <- as.numeric(str_split(basename(rownames(files.OP.ordered)[1]),pattern = "-")[[1]][3])


        histo.file <- file.path(out_ref,"histo","history-S-2061-01-01-000000-g01.h5")

        if (file.exists(histo.file)){
            print(run_name)
            for (iyear in seq(2011,2061)){

              fin <- paste0(out_ref,"/histo/","history-S-",iyear,"-01-01-000000-g01.h5")
              fout <- paste0(out_ref,"/histo/","history-S-",(iyear - 111),"-01-01-000000-g01.h5")

              system2("mv",paste(fin,fout))
            }
        }


        # if (fyear == 2061){
        #   print(run_name)
        #   for (iyear in seq(2011,2061)){
        #
        #     fin <- paste0(out_ref,"/histo/","history-S-",iyear,"-01-01-000000-g01.h5")
        #     fout <- paste0(out_ref,"/histo/","history-S-",(iyear - 111),"-01-01-000000-g01.h5")
        #
        #     system2("mv",paste(fin,fout))
        #   }
        # }
      }
    }
  }
}


# scp /home/femeunier/Documents/projects/SoilSensitivity/scripts/move_history_files.R hpc:/data/gent/vo/000/gvo00074/felicien/R
