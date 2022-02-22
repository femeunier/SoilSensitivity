rm(list = ls())

library(sf)
library(dplyr)
library(tidyr)
library(ncdf4)


years = 1901:2010
var <- tibble::tribble(~DAP.name, ~CF.name, ~units, "tair",
                       "air_temperature", "Kelvin", "lwdown", "surface_downwelling_longwave_flux_in_air",
                       "W/m2", "press", "air_pressure", "Pascal", "swdown",
                       "surface_downwelling_shortwave_flux_in_air", "W/m2",
                       "uwind", "eastward_wind", "m/s", "vwind", "northward_wind",
                       "m/s", "qair", "specific_humidity", "g/g", "rain", "precipitation_flux",
                       "kg/m2/s")


for (iyear in seq(1,length(years))){
  print(years[iyear])

  file <- paste0("/data/gent/vo/000/gvo00074/ED_common_data/met/CRUNCEP/CRUNCEP.",years[iyear],".nc")

  nc <- nc_open(file)
  var.list <- nc$var

  for (ivar in seq(1,length(var))){
    var.list[[ivar]]$dim[[2]]$vals <- sort(nc$dim$latitude$vals,decreasing = TRUE)
  }

  lons.in <-  nc$dim$longitude$vals
  lats.in <-  nc$dim$latitude$vals
  ntime <- length(nc$dim$time$vals)

  loc <- ncdf4::nc_create(filename =  paste0("/data/gent/vo/000/gvo00074/ED_common_data/met/CRUNCEP/CRUNCEP.",years[iyear],"_inv.nc"),
                          vars = var.list,
                          verbose = TRUE)

  dat.list <- list()
  for (j in seq_len(nrow(var))) {


    dat.list[[j]] <- PEcAn.utils::retry.func(ncdf4::ncvar_get(nc,
                                                              as.character(var$CF.name[j]),
                                                              c(1,1,1),
                                                              c(length(lons.in),length(lats.in),ntime)),
                                             maxErrors = maxErrors,
                                             sleep = sleep)

    ncdf4::ncvar_put(nc = loc, varid = as.character(var$CF.name[j]),
                     vals = dat.list[[j]])
  }

  ncdf4::nc_close(loc)
  ncdf4::nc_close(nc)

}

# scp /home/femeunier/Documents/projects/SoilSensitivity/scripts/invert_lat_coordinates_CRUNCEP.R hpc:/data/gent/vo/000/gvo00074/felicien/R


