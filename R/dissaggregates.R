dissaggregates <- function(raster.in){

  # Land.mask
  ladmask.file <- readRDS("./maps/landmask.RDS")
  landmask <- (SoilSensitivity::rotate(apply(ladmask.file,2, rev)))
  LM <- raster(landmask)
  extent(LM) <- c(min(as.numeric(rownames(ladmask.file))),max(as.numeric(rownames(ladmask.file)))+0.5,
                  min(as.numeric(colnames(ladmask.file))),max(as.numeric(colnames(ladmask.file)))+0.5)

  AGB.raster <- raster.in
  AGB.raster2 <- disaggregate(AGB.raster,fac = res(AGB.raster)/res(LM),method = "")

  LM.cropped <- crop(LM,AGB.raster2)

  f <- focal(AGB.raster2, w=matrix(1,nrow=5, ncol=5), fun=mean, NAonly=TRUE, na.rm=TRUE,pad = TRUE)

  raster.out = mask(f,LM.cropped,maskvalue=0)

  return(raster.out)
}
