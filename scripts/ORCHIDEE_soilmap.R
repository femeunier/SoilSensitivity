rm(list=ls())
library(RNetCDF)
library(raster)
library(soiltexture)
workdir<-"/home/orchidee02/mpeau/SoilSensitivity_maps/"
setwd(workdir)

# scenario mean
Top_Mean_Clay<-raster(paste0("soilgrid_top.clay_mean_resampled.grd"))
Top_Mean_Sand<-raster(paste0("soilgrid_top.sand_mean_resampled.grd"))

#scecnario min
Top_Min_Clay<-raster(paste0("soilgrid_top.clay_min_resampled.grd"))
Top_Min_Sand<-raster(paste0("soilgrid_top.sand_min_resampled.grd"))

# scenario max
Top_Max_Clay<-raster(paste0("soilgrid_top.clay_max_resampled.grd"))
Top_Max_Sand<-raster(paste0("soilgrid_top.sand_max_resampled.grd"))


# Compute usda class from texture

apply.usda<-function(clay,sand){

orc<-c("Sa","LoSa","SaLo","SiLo","Si","Lo","SaClLo","SiClLo","ClLo","SaCl","SiCl","Cl")


out.mask<-clay*0+9999
clay<-values(clay)*100
sand<-values(sand)*100
silt<-100-clay-sand

out.vec<-rep(NA,length(clay))
pos<-which(!is.na(clay))
clay<-na.omit(clay)
sand<-na.omit(sand)
silt<-na.omit(silt)
usda.class<-soiltexture::TT.points.in.classes( tri.data = data.frame(CLAY=clay,
                                                                         SILT=silt,
                                                                         SAND=sand)
                                                   ,class.sys   = "USDA.TT",PiC.type    = "t")
usda.fac<-factor(usda.class,levels=orc)

out.vec[pos]<-as.numeric(usda.fac)
values(out.mask)<-out.vec
return(out.mask)
}

na.mask<-Top_Mean_Clay
na.mask[values(na.mask)==0]<-NA

USDA_Mean<-apply.usda(clay=Top_Mean_Clay,sand=Top_Mean_Sand)
USDA_Min<-apply.usda(clay=Top_Min_Clay,sand=Top_Min_Sand)
USDA_Max<-apply.usda(clay=Top_Max_Clay,sand=Top_Max_Sand)

USDA_Mean[is.na(values(na.mask))]<-NA
USDA_Min[is.na(values(na.mask))]<-NA
USDA_Max[is.na(values(na.mask))]<-NA

USDA_Mean<-extend(USDA_Mean,extent(c(-180,180,-90,90)))
USDA_Min<-extend(USDA_Min,extent(c(-180,180,-90,90)))
USDA_Max<-extend(USDA_Max,extent(c(-180,180,-90,90)))

USDA_Mean_mat<-t(apply(raster::as.matrix(USDA_Mean),2,rev))
USDA_Min_mat<-t(apply(raster::as.matrix(USDA_Min),2,rev))
USDA_Max_mat<-t(apply(raster::as.matrix(USDA_Max),2,rev))

# Insert new data in netcdf map
#system(paste0("cp ",workdir,"ORCHIDEE_nc/soils_param_SoilGrids_halfdeg.nc ",workdir,"ORCHIDEE_nc/soils_param_Mean_halfdeg.nc"))
#system(paste0("cp ",workdir,"ORCHIDEE_nc/soils_param_SoilGrids_halfdeg.nc ",workdir,"ORCHIDEE_nc/soils_param_Min_halfdeg.nc"))
#system(paste0("cp ",workdir,"ORCHIDEE_nc/soils_param_SoilGrids_halfdeg.nc ",workdir,"ORCHIDEE_nc/soils_param_Max_halfdeg.nc"))



ORCmap_halfdeg<-open.nc(paste0(workdir,"ORCHIDEE_nc/soils_param_Mean_halfdeg.nc"),write=T)
att.put.nc(ORCmap_halfdeg, "soiltext", "missing_value", "NC_DOUBLE", -9999)
var.put.nc(ncfile=ORCmap_halfdeg, variable="soiltext",data=USDA_Mean_mat)
close.nc(ORCmap_halfdeg)

ORCmap_halfdeg<-open.nc(paste0(workdir,"ORCHIDEE_nc/soils_param_Min_halfdeg.nc"),write=T)
att.put.nc(ORCmap_halfdeg, "soiltext", "missing_value", "NC_DOUBLE", -9999)
var.put.nc(ncfile=ORCmap_halfdeg, variable="soiltext",data=USDA_Min_mat)
close.nc(ORCmap_halfdeg)

ORCmap_halfdeg<-open.nc(paste0(workdir,"ORCHIDEE_nc/soils_param_Max_halfdeg.nc"),write=T)
att.put.nc(ORCmap_halfdeg, "soiltext", "missing_value", "NC_DOUBLE", -9999)
var.put.nc(ncfile=ORCmap_halfdeg, variable="soiltext",data=USDA_Max_mat)
close.nc(ORCmap_halfdeg)








