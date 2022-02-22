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

fyear = 1901

X = seq(ED_REG_LONMIN,ED_REG_LONMAX,GRID_RES)
Y = seq(ED_REG_LATMIN,ED_REG_LATMAX,GRID_RES)

ref_dir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/SoilSensitivity/run"
rundir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/SoilSensitivity/run/grid"
ICdir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/LSliana/IC/"
outdir <- "/kyukon/scratch/gent/vo/000/gvo00074/felicien/SoilSensitivity/out"

scenars <- c("SoilGrids_mean","SoilGrids_min","SoilGrids_max")

land <- readRDS(file.path("maps","landmask.RDS"))

df.restart <- data.frame()

Nsimuperjob = 5
isimu = 0

list_dir <- list()
ed_exec <- "/user/scratchkyukon/gent/gvo000/gvo00074/felicien/ED2/ED/build/ed_2.1-opt-master-2bb6872"

all_depths <- c(-8,-7,-6.2,-5.5,-4.9,-4.3,-3.8,-3.3,-2.8,-2.3,-1.8,-1.3,-1,-0.6,-0.3,-0.15)

df.temp <- data.frame()

for(ix in seq(1,length(X),1)){
  for(iy in seq(1,length(Y),1)){
    clat = Y[iy]; clon = X[ix]

    if (land[which(rownames(land)==clon),which(colnames(land)==clat)] != 0){
      for (iscenar in seq(1,length(scenars))){

        run_name <- paste0("SoilSens_Amazon_historical_",scenars[iscenar],"_X_",abs(X[ix]),ifelse(X[ix]<0,"W","E"),
                           "_Y_",abs(Y[iy]),ifelse(Y[iy]<0,"S","N"))

        run_ref <- file.path(rundir,run_name)
        out_ref <- file.path(outdir,run_name)

        if (file.exists(file.path(out_ref,"histo","history-S-2009-01-01-000000-g01.h5"))){
          next()
        }

        isimu <- isimu + 1

        if(!dir.exists(run_ref)) dir.create(run_ref)
        if(!dir.exists(out_ref)) dir.create(out_ref)
        if(!dir.exists(file.path(out_ref,"analy"))) dir.create(file.path(out_ref,"analy"))
        if(!dir.exists(file.path(out_ref,"histo"))) dir.create(file.path(out_ref,"histo"))

        run_origin <- file.path(rundir,paste0("SoilSens_Amazon_IC_",scenars[iscenar],"_X_",abs(X[ix]),ifelse(X[ix]<0,"W","E"),
                                              "_Y_",abs(Y[iy]),ifelse(Y[iy]<0,"S","N")))
        out_origin <- file.path(outdir,paste0("SoilSens_Amazon_IC_",scenars[iscenar],"_X_",abs(X[ix]),ifelse(X[ix]<0,"W","E"),
                                              "_Y_",abs(Y[iy]),ifelse(Y[iy]<0,"S","N")))

        system2("cp",paste(file.path(run_origin,"ED2IN"),
                           file.path(run_ref,"ED2IN")))
        system2("cp",paste(file.path(run_origin,"config.xml"),
                           file.path(run_ref,"config.xml")))

        system2("cp",paste(file.path(out_origin,"histo","history-S-1950-01-01-000000-g01.h5"),
                           file.path(out_origin,"histo","history-S-1901-01-01-000000-g01.h5")))


        ed2in <- read_ed2in(file.path(run_ref,"ED2IN"))

        # ED2IN
        ed2in_scenar <- ed2in
        ed2in_scenar$IEDCNFGF <- file.path(run_ref,"config.xml")
        ed2in_scenar$FFILOUT = file.path(out_ref,"analy","analysis")
        ed2in_scenar$SFILOUT = file.path(out_ref,"histo","history")
        ed2in_scenar$POI_LAT <- Y[iy]
        ed2in_scenar$POI_LON <- X[ix]

        history.file <- file.path(out_origin,"histo","history-S-1901-01-01-000000-g01.h5")
        if (file.exists(history.file)){
          ed2in_scenar$RUNTYPE <- "HISTORY"
          ed2in_scenar$IED_INIT_MODE <- 6
          ed2in_scenar$SFILIN <- file.path(out_origin,"histo","history")
          ed2in_scenar$ITIMEH <- 0
          ed2in_scenar$IDATEH <- 1
          ed2in_scenar$IMONTHH <- 1
          ed2in_scenar$IYEARH <- fyear
          ed2in_scenar$IMETAVG <- 3

          # Output year/month
          ed2in_scenar$IMONTHZ = 1
          ed2in_scenar$IYEARA = 1901
          ed2in_scenar$IYEARZ = 2010
          ed2in_scenar$IQOUTPUT = 0

          ed2in_scenar$ED_MET_DRIVER_DB = "/data/gent/vo/000/gvo00074/ED_common_data/met/CRUNCEP/ED2_CO2/ED_MET_DRIVER_HEADER"

          ed2in_scenar$METCYC1 = 1901
          ed2in_scenar$METCYCF = 2010

          ed2in_scenar$FRQSTATE = 12

          write_ed2in(ed2in_scenar,filename = file.path(run_ref,"ED2IN"))

          if (isimu == 1){
            isfirstjob = TRUE
            dir_joblauncher = run_ref
            list_dir[[run_name]] = run_ref
          } else{
            isfirstjob = FALSE
          }

          # job.sh
          write_joblauncher_noR_status(file =  file.path(dir_joblauncher,"job_history.sh"),
                                       nodes = 1,ppn = 18,mem = 16,walltime = 72,
                                       prerun = "ml purge ; ml UDUNITS/2.2.26-intel-2018a R/3.4.4-intel-2018a-X11-20180131 HDF5/1.10.1-intel-2018a; ulimit -s unlimited",
                                       CD = run_ref,
                                       ed_exec = ed_exec,
                                       ED2IN = "ED2IN",
                                       firstjob = isfirstjob,
                                       CD.main = dir_joblauncher,
                                       remove = FALSE)


          if (isimu == Nsimuperjob){
            isimu = 0
          }
        } else {

          df.temp <- bind_rows(list(df.temp,
                                    data.frame(run = run_name)))
        }
      }
    }
  }
}

dumb <- write_bash_submission(file = file.path(rundir,"all_jobs_history.sh"),
                              list_files = list_dir,
                              job_name = "job_history.sh")

# scp /home/femeunier/Documents/projects/SoilSensitivity/scripts/restart_history_runs.R hpc:/data/gent/vo/000/gvo00074/felicien/R
