rm(list = ls())

library(SoilSensitivity)
library(lattice)
library(raster)

ED_REG_LATMIN = -20
ED_REG_LATMAX = 15
ED_REG_LONMIN = -85
ED_REG_LONMAX = -30

Delta_X = 2
Delta_Y = 2

X = seq(ED_REG_LONMIN,ED_REG_LONMAX-Delta_X,Delta_X)
Y = seq(ED_REG_LATMIN,ED_REG_LATMAX-Delta_Y,Delta_Y)


depths <- c("0-5")

vars <- c("bdod")
grid <- expand.grid(lon=X, lat=Y)

files <- list() ; files_not_downloaded <- list() ; compt <- 1

for (i in seq(1,nrow(grid))){
  clat = grid[i,"lat"];  clon = grid[i,"lon"]

  print(paste(c(clat,clon),collapse = " "))

  cstack <- list()

  for (idepth in seq(1,length(depths))){
    for (ivar in seq(1,length(vars))){

      varname <- paste(vars[ivar],depths[idepth],sep = "_")

      cvar <- tryCatch(download.soilGrid.psims (var = vars[ivar],
                                                depth = depths[idepth],
                                                product = "mean",
                                                long.min = as.character(clon),
                                                long.max = as.character(clon + Delta_X),
                                                lat.min = as.character(clat),
                                                lat.max = as.character(clat + Delta_Y),
                                                resam.pix=240),
                       error = function(err){NA})

      if (!is.na(cvar[1])){
        r.cvar <- raster(cvar/1000,
                         xmn=clon, xmx=clon+Delta_X,
                         ymn=clat, ymx=clat+Delta_Y,
                         crs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

        cstack[[varname]] <- r.cvar
      } else {
        files_not_downloaded[[compt]] <- paste(varname,clat,clon,sep = "_")
        compt <- compt + 1
      }
    }
  }

  files[[i]] <- stack(cstack)
}


N = length(files)
for (i in seq(1,length(files_not_downloaded))){

  all <- strsplit(files_not_downloaded[[i]],"_")[[1]]

  currentvar <- all[1]
  cdepth <- all[2]
  clon <- as.numeric(all[4])
  clat <- as.numeric(all[3])

  pos <- which(grid$lat == clat & grid$lon == clon)

  cvar <- tryCatch(download.soilGrid.psims (var = currentvar,
                                            depth = cdepth,
                                            product = "mean",
                                            long.min = as.character(clon),
                                            long.max = as.character(clon + Delta_X),
                                            lat.min = as.character(clat),
                                            lat.max = as.character(clat + Delta_Y),
                                            resam.pix=240),
                   error = function(err){NA})
  r.cvar <- raster(cvar/1000,
                   xmn=clon, xmx=clon+Delta_X,
                   ymn=clat, ymx=clat+Delta_Y,
                   crs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  files[[pos]]  <- stack(files[[pos]],r.cvar)
}

m <- do.call(merge, files)
names(m) <- names(cstack)
values(m) <- values(m)/10/1000
plot(m)

rf <- writeRaster(m, filename=file.path(".","maps", "soilgrid_soc.grd"))
