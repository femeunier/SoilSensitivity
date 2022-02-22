rm(list = ls())
library(ncdf4)

# orc<-c("Sa","LoSa","SaLo","SiLo","Si","Lo","SaClLo","SiClLo","ClLo","SaCl","SiCl","Cl")
names <- c("Mean","Max","Min")
names.out <- c("output_mean","output_max","output_min")

ED_REG_LATMIN = -19.5
ED_REG_LATMAX =  13.5
ED_REG_LONMIN = -84.5
ED_REG_LONMAX = -30.5

for (iname in seq(1,length(names))){

  soilmap <- paste0("/home/femeunier/Documents/projects/SoilSensitivity/outputs/spinup/ORC/ORCHIDEE_soilmap/soils_param_",names[iname],"_halfdeg.nc")

  nc <- nc_open(soilmap)
  ST  <- ncvar_get(nc,"soiltext")
  lat  <- ncvar_get(nc,"nav_lat")
  lon  <- ncvar_get(nc,"nav_lon")
  nc_close(nc)


  pos <- which(lat >= ED_REG_LATMIN & lat <= ED_REG_LATMAX & lon >= ED_REG_LONMIN & lon <= ED_REG_LONMAX)
  lat.select <- lat[pos]
  lon.select <- lon[pos]
  ST.select <- ST[pos]

  df <- data.frame(lat = lat.select,
                   lon = lon.select,
                   soil.text = ST.select)

  world <- ne_countries(scale = "medium", returnclass = "sf")

  ggplot(data = df) +
    geom_raster(aes(x=lon, y = lat, fill = soil.text),alpha = 0.3) +
    geom_sf(data = world,fill = NA) +
    coord_sf(xlim = c(-84.5, -30.5), ylim = c(-19.5, 15.5), expand = FALSE) +
    scale_fill_gradient2(low = "darkred",mid = "darkgrey",high = "darkgreen",na.value = "white") +
    labs(x = "",y = "") +
    theme_bw()

  saveRDS(object = df,
          file = paste0("/home/femeunier/Documents/projects/SoilSensitivity/outputs/spinup/ORC/ORCHIDEE_soilmap/",names.out[iname],".RDS"))

}
