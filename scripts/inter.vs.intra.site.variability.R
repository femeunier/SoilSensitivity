rm(list = ls())

library(SoilSensitivity)
library(lattice)
library(raster)
library(purrr)
library(tidyr)
library(ggplot2)
library(dplyr)

rc <- brick(file.path(".","maps", "soilgrid.grd"),expand = TRUE)
pos.sea <- rc[[1]]==0 & rc[[2]]==0

layer.select = 2

rc[[layer.select]][pos.sea] <- NA

aggregs <- c(0.1,0.25,0.5,1,2,2.5,4,5,6,8,10,20)
aggregs <- c(0.05,0.1,0.25,0.5,1,2.5,5)
aggregs <- c(res(rc)[1])*c(2,5,10,20,50,100,200,500,1000)

intersite.variability <- intrasite.variability <- c()
intersite.variability.cv <- intrasite.variability.cv <- c()

for (iaggreg in seq(1,length(aggregs))){

  cat(iaggreg/length(aggregs))

  top.sand.mean <- raster::aggregate(rc[[c(layer.select)]],
                                     1 / res(rc)[1] * aggregs[iaggreg],
                                     1 / res(rc)[2] * aggregs[iaggreg],
                                     fun = mean,na.rm = TRUE)

  intersite.variability[iaggreg] <- sqrt(var(as.array(top.sand.mean),na.rm = TRUE))
  intersite.variability.cv[iaggreg] <- sqrt(var(as.array(top.sand.mean),na.rm = TRUE))/
    mean(as.array(top.sand.mean),na.rm = TRUE)

  top.sand.var <- raster::aggregate(rc[[c(layer.select)]],
                                     1 / res(rc)[1] * aggregs[iaggreg],
                                     1 / res(rc)[2] * aggregs[iaggreg],
                                     fun = var,na.rm = TRUE)

  top.sand.cv <- sqrt(top.sand.var)/top.sand.mean

  intrasite.variability[iaggreg] <- mean(as.array(sqrt(top.sand.var)),na.rm = TRUE)
  intrasite.variability.cv[iaggreg] <- mean(as.array(top.sand.cv),na.rm = TRUE)
}

aggregs <- c(res(rc)[1],aggregs)
intersite.variability <- c(sqrt(var(as.array(rc[[layer.select]]),na.rm = TRUE)),intersite.variability)
intrasite.variability <- c(0,intrasite.variability)
intersite.variability.cv <- c(sqrt(var(as.array(rc[[layer.select]]),na.rm = TRUE))/
                                mean(as.array(rc[[layer.select]]),na.rm = TRUE),intersite.variability.cv)
intrasite.variability.cv <- c(0,intrasite.variability.cv)

par(mfrow = c(1,1))
plot(aggregs,intersite.variability*100,type = 'l',log = '',ylim = 100*c(0,0.10),xlim = c(0,5),
     xlab = "Spatial resolution (°)", ylab = "Variability (% clay)",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(aggregs,intrasite.variability*100,col = 'red')

plot(aggregs,intersite.variability.cv,type = 'l',log = '',ylim = c(0,0.25),xlim = c(0,5))
lines(aggregs,intrasite.variability.cv,col = 'red')

# plot(intersite.variability,type = 'l',log = 'x',ylim = c(0,0.5))
# lines(intrasite.variability,col = 'red')
# plot(intersite.variability.cv,type = 'l',log = 'x',ylim = c(0,0.5))
# lines(intrasite.variability.cv,col = 'red')
