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

Nsimuperjob = 4
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

        run_ref <- file.path(rundir,run_name)
        out_ref <- file.path(outdir,run_name)

        ed2in <- read_ed2in(file.path(run_ref,"ED2IN"))

        # Check status
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


        if (status %in% c("Started","Error")){ # Change ED2IN and restart

          isimu <- isimu + 1

          details.file <- file.info(list.files(path = file.path(out_ref,"histo"), full.names = TRUE,pattern = ".h5"))
          files.OP.ordered <- details.file[with(details.file, order(as.POSIXct(mtime),decreasing = TRUE)), ]
          fyear <- as.numeric(str_split(basename(rownames(files.OP.ordered)[1]),pattern = "-")[[1]][3])

          if (nrow(details.file)>0){
            ed2in$RUNTYPE <- "HISTORY"
            ed2in$IED_INIT_MODE <- 6
            ed2in$SFILIN <- file.path(out_ref,"histo","history")
            ed2in$ITIMEH <- 0
            ed2in$IDATEH <- 1
            ed2in$IMONTHH <- 1
            ed2in$IYEARH <- fyear
            ed2in$IMETAVG <- 3
          } else{
            # ED2IN
            ed2in <- ed2in_ref

            ed2in$IEDCNFGF <- file.path(run_ref,"config.xml")
            ed2in$FFILOUT = file.path(out_ref,"analy","analysis")
            ed2in$SFILOUT = file.path(out_ref,"histo","history")
            ed2in$POI_LAT <- Y[iy]
            ed2in$POI_LON <- X[ix]

            ed2in$SFILIN <- file.path(ICdir,
                                             paste0(cssfile_base,".lat",sprintf("%.4f",Y[iy]),"lon",sprintf("%.4f",X[ix])))

            if (file.exists(paste0(ed2in$SFILIN,".css"))){
              css <- read.table(paste0(ed2in$SFILIN,".css"),header = TRUE)
              ed2in$IED_INIT_MODE <- 6
              ed2in$INCLUDE_THESE_PFT <- sort(unique(css[,6]))
            } else{
              ed2in$IED_INIT_MODE <- 0
              ed2in$INCLUDE_THESE_PFT <- 1
            }

            ed2in$RUNTYPE <- "INITIAL"
            ed2in$ITIMEH <- 0
            ed2in$IDATEH <- 1
            ed2in$IMONTHH <- 1
            ed2in$IYEARH <- 1900
            ed2in$IMETAVG <- 3


            ## Scenars
            # Soil properties
            if (scenars[iscenar] == "SoilGrids_mean") {

              ed_exec <- "/user/scratchkyukon/gent/gvo000/gvo00074/felicien/ED2/ED/build/ed_2.1-opt-master-2bb6872"

              e <- extent(max(ED_REG_LONMIN,clon-0.05),
                          min(ED_REG_LONMAX,clon+0.05),
                          max(ED_REG_LATMIN,clat-0.05),
                          min(ED_REG_LATMAX,clat+0.05))

              ed2in$SLZ <- all_depths

              ed2in$SLXSAND = raster::extract(soilgrids_mean[[1]], e)
              ed2in$SLXCLAY = raster::extract(soilgrids_mean[[2]], e)

            } else if (scenars[iscenar] == "SoilGrids_min") {

              ed_exec <- "/user/scratchkyukon/gent/gvo000/gvo00074/felicien/ED2/ED/build/ed_2.1-opt-master-2bb6872"

              e <- extent(clon-0.05,clon+0.05,clat-0.05,clat+0.05)

              ed2in$SLZ <- all_depths

              ed2in$SLXSAND = raster::extract(soilgrids_min[[1]], e)
              ed2in$SLXCLAY = raster::extract(soilgrids_min[[2]], e)

            } else if (scenars[iscenar] == "SoilGrids_max") {

              ed_exec <- "/user/scratchkyukon/gent/gvo000/gvo00074/felicien/ED2/ED/build/ed_2.1-opt-master-2bb6872"

              e <- extent(clon-0.05,clon+0.05,clat-0.05,clat+0.05)

              ed2in$SLZ <- all_depths

              ed2in$SLXSAND = raster::extract(soilgrids_max[[1]], e)
              ed2in$SLXCLAY = raster::extract(soilgrids_max[[2]], e)

            }
          }

          write_ed2in(ed2in,filename = file.path(run_ref,"ED2IN"))

          if (isimu == 1){
            isfirstjob = TRUE
            dir_joblauncher = run_ref
            list_dir[[run_name]] = run_ref
          } else{
            isfirstjob = FALSE
          }

          # job.sh
          write_joblauncher_noR_status(file =  file.path(dir_joblauncher,"job.sh"),
                                       nodes = 1,ppn = 18,mem = 16,walltime = 24,
                                       prerun = "ml purge ; ml UDUNITS/2.2.26-intel-2018a R/3.4.4-intel-2018a-X11-20180131 HDF5/1.10.1-intel-2018a; ulimit -s unlimited",
                                       CD = run_ref,
                                       ed_exec = ed_exec,
                                       ED2IN = "ED2IN",
                                       firstjob = isfirstjob,
                                       CD.main = dir_joblauncher)


          if (isimu == Nsimuperjob){
            isimu = 0
          }

          df.temp <- data.frame(status = status,
                                dir = run_ref)

          df.restart <- bind_rows(list(df.restart,
                                   df.temp))

        } else if (status == "Not started") { # It never started

          isimu <- isimu + 1

          if (isimu == 1){
            isfirstjob = TRUE
            dir_joblauncher = run_ref
            list_dir[[run_name]] = run_ref
          } else{
            isfirstjob = FALSE
          }

          # job.sh
          write_joblauncher_noR_status(file =  file.path(dir_joblauncher,"job.sh"),
                                       nodes = 1,ppn = 18,mem = 16,walltime = 24,
                                       prerun = "ml purge ; ml UDUNITS/2.2.26-intel-2018a R/3.4.4-intel-2018a-X11-20180131 HDF5/1.10.1-intel-2018a; ulimit -s unlimited",
                                       CD = run_ref,
                                       ed_exec = ed_exec,
                                       ED2IN = "ED2IN",
                                       firstjob = isfirstjob,
                                       CD.main = dir_joblauncher)


          if (isimu == Nsimuperjob){
            isimu = 0
          }

          df.temp <- data.frame(status = status,
                                dir = run_ref)

          df.restart <- bind_rows(list(df.restart,
                                   df.temp))
        }
      }
    }
  }
}

dumb <- write_bash_submission(file = file.path(rundir,"all_jobs_restart.sh"),
                              list_files = list_dir,
                              job_name = "job.sh")

# scp /home/femeunier/Documents/projects/SoilSensitivity/scripts/restart_SoilSens.R hpc:/data/gent/vo/000/gvo00074/felicien/R
