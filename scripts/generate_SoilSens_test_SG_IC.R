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

ed2in <- read_ed2in(file.path(ref_dir,"ED2IN"))
ed2in$IMONTHA = 1
ed2in$IDATEA = 1
ed2in$IYEARA = 1900

ed2in$IMONTHZ = 2
ed2in$IDATEZ = 1
ed2in$IYEARZ = 1930

cssfile_base <- "Amazon"

ed2in$IMETAVG = -1

list_dir <- list()
# load landmask here
land <- readRDS(file.path("maps","landmask.RDS"))

Nsimuperjob = 5
isimu = 0

defaults <- list()
PREFIX_XML <- "<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n"
# Default settings
settings <- list(model = list(revision = "git",
                              config.header = NULL),
                 pfts = list(pft = list(num = 1,
                                        ed2_pft_number = 1,
                                        name = "saccharum"),
                             pft = list(num = 2,
                                        ed2_pft_number = 2,
                                        name = "Early"),
                             pft = list(num = 3,
                                        ed2_pft_number = 3,
                                        name = "Mid"),
                             pft = list(num = 4,
                                        ed2_pft_number = 4,
                                        name = "Late")))

# Config
config <- list()
config[["saccharum"]] <- unlist(list(num = 1))
config[["Early"]] <- unlist(list(num = 2))
config[["Mid"]] <- unlist(list(num = 3))
config[["Late"]] <- unlist(list(num = 4,
                                wood_Kmax = 0.01))

scenars <- c("SoilGrids_mean","SoilGrids_min","SoilGrids_max")

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

        isimu = isimu + 1

        run_ref <- file.path(rundir,run_name)
        out_ref <- file.path(outdir,run_name)

        if(!dir.exists(run_ref)) dir.create(run_ref)
        if(!dir.exists(out_ref)) dir.create(out_ref)
        if(!dir.exists(file.path(out_ref,"analy"))) dir.create(file.path(out_ref,"analy"))
        if(!dir.exists(file.path(out_ref,"histo"))) dir.create(file.path(out_ref,"histo"))

        # ED2IN
        ed2in_scenar <- ed2in
        ed2in_scenar$IEDCNFGF <- file.path(run_ref,"config.xml")
        ed2in_scenar$FFILOUT = file.path(out_ref,"analy","analysis")
        ed2in_scenar$SFILOUT = file.path(out_ref,"histo","history")
        ed2in_scenar$POI_LAT <- Y[iy]
        ed2in_scenar$POI_LON <- X[ix]


        ed2in_scenar$SFILIN <- file.path(ICdir,
                                         paste0(cssfile_base,".lat",sprintf("%.4f",Y[iy]),"lon",sprintf("%.4f",X[ix])))

        if (file.exists(paste0(ed2in_scenar$SFILIN,".css"))){
          css <- read.table(paste0(ed2in_scenar$SFILIN,".css"),header = TRUE)
          ed2in_scenar$IED_INIT_MODE <- 6
          ed2in_scenar$INCLUDE_THESE_PFT <- sort(unique(css[,6]))
        } else{
          ed2in_scenar$IED_INIT_MODE <- 0
          ed2in_scenar$INCLUDE_THESE_PFT <- 1
        }


        ## Scenars
        # Soil properties
        if (scenars[iscenar] == "SoilGrids_mean") {

          ed_exec <- "/user/scratchkyukon/gent/gvo000/gvo00074/felicien/ED2/ED/build/ed_2.1-opt-master-2bb6872"

          e <- extent(max(ED_REG_LONMIN,clon-0.05),
                      min(ED_REG_LONMAX,clon+0.05),
                      max(ED_REG_LATMIN,clat-0.05),
                      min(ED_REG_LATMAX,clat+0.05))

          ed2in_scenar$SLZ <- all_depths

          ed2in_scenar$SLXSAND = raster::extract(soilgrids_mean[[1]], e)
          ed2in_scenar$SLXCLAY = raster::extract(soilgrids_mean[[2]], e)

        } else if (scenars[iscenar] == "SoilGrids_min") {

          ed_exec <- "/user/scratchkyukon/gent/gvo000/gvo00074/felicien/ED2/ED/build/ed_2.1-opt-master-2bb6872"

          e <- extent(clon-0.05,clon+0.05,clat-0.05,clat+0.05)

          ed2in_scenar$SLZ <- all_depths

          ed2in_scenar$SLXSAND = raster::extract(soilgrids_min[[1]], e)
          ed2in_scenar$SLXCLAY = raster::extract(soilgrids_min[[2]], e)

        } else if (scenars[iscenar] == "SoilGrids_max") {

          ed_exec <- "/user/scratchkyukon/gent/gvo000/gvo00074/felicien/ED2/ED/build/ed_2.1-opt-master-2bb6872"

          e <- extent(clon-0.05,clon+0.05,clat-0.05,clat+0.05)

          ed2in_scenar$SLZ <- all_depths

          ed2in_scenar$SLXSAND = raster::extract(soilgrids_max[[1]], e)
          ed2in_scenar$SLXCLAY = raster::extract(soilgrids_max[[2]], e)

        }

        write_ed2in(ed2in_scenar,filename = file.path(run_ref,"ED2IN"))

        # config
        config_simu <- config
        # Modify config if needed
        for (ipft in seq(1,length(config_simu))){
          if (!(config[[ipft]]["num"] %in% ed2in_scenar$INCLUDE_THESE_PFT)){
            config_simu[[names(config)[ipft]]] <- NULL
          }
        }
        xml <- write.config.xml.ED2(defaults = defaults,
                                    settings = settings,
                                    trait.values = config_simu)

        XML::saveXML(xml, file = file.path(run_ref,"config.xml"), indent = TRUE,
                     prefix = PREFIX_XML)

        if (isimu == 1){
          isfirstjob = TRUE
          dir_joblauncher = run_ref
          list_dir[[run_name]] = run_ref
        } else{
          isfirstjob = FALSE
        }

        # job.sh
        write_joblauncher_noR_status(file =  file.path(dir_joblauncher,"job.sh"),
                                     nodes = 1,ppn = 18,mem = 16,walltime = 12,
                                     prerun = "ml purge ; ml UDUNITS/2.2.26-intel-2018a R/3.4.4-intel-2018a-X11-20180131 HDF5/1.10.1-intel-2018a; ulimit -s unlimited",
                                     CD = run_ref,
                                     ed_exec = ed_exec,
                                     ED2IN = "ED2IN",
                                     firstjob = isfirstjob,
                                     CD.main = dir_joblauncher)


        if (isimu == Nsimuperjob){
          isimu = 0
        }
      }
    }
  }
}

dumb <- write_bash_submission(file = file.path(rundir,"all_jobs_IC.sh"),
                              list_files = list_dir,
                              job_name = "job.sh")

# scp /home/femeunier/Documents/projects/SoilSensitivity/scripts/generate_SoilSens_test_SG_IC.R hpc:/data/gent/vo/000/gvo00074/felicien/R

