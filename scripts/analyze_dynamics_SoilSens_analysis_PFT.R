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

        gpp <- c()
        for (imonth in seq(1,1)){
          h5file <- file.path(out_ref,"analy",paste0("analysis-Q-2010-",sprintf("%02d",imonth),"-00-000000-g01.h5"))

          if (file.exists(h5file)){

            # nc <- nc_open()
            mymont  = lapply(h5read_opt(h5file),FUN=aperm)

            gpp <- mymont$MMEAN_GPP_CO
            area <- rep(mymont$AREA,mymont$PACO_N)
            nplant <- mymont$NPLANT
            patch <- rep(1:mymont$NPATCHES_GLOBAL,mymont$PACO_N)
            pft <- mymont$PFT

            if (length(gpp) >0){
              df.pft <- data.frame(patch,area,pft,nplant,gpp) %>% group_by(patch,pft) %>% summarise(gpp = sum(gpp*nplant),
                                                                                                    area = area[1],
                                                                                                    .groups = "keep") %>%
                group_by(pft) %>% summarise(gpp = weighted.mean(gpp,area),
                                            .groups = 'keep')



              df <- bind_rows(list(df,
                                   df.pft %>% mutate(scenario = scenars[iscenar],
                                                     lat = clat,
                                                     lon = clon,
                                                     year = 2010,
                                                     month = imonth)))
            }
          }
        }
      }
    }
  }
}

saveRDS(df,"df_SoilSens_analysis_PFT.RDS")

# scp /home/femeunier/Documents/projects/SoilSensitivity/scripts/analyze_dynamics_SoilSens_analysis_PFT.R hpc:/data/gent/vo/000/gvo00074/felicien/R

# system2("rsync",paste("-avz",
#                       "hpc:/kyukon/scratch/gent/vo/000/gvo00074/felicien/SoilSensitivity/out/SoilSens_Amazon_historical_SoilGrids_mean_X_84.5W_Y_10.5N/analy/analysis-Q-2010-01-00-000000-g01.h5",
#                       "./outputs/"))
