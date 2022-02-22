rm(list = ls())

library(rhdf5)
library(dplyr)

source("/home/femeunier/Documents/ED2/R-utils/h5read_opt.r")

h5file <- file.path("/home/femeunier/Documents/projects/SoilSensitivity","outputs",
                    "history-S-1904-01-01-000000-g01.h5")

mymont    = lapply(h5read_opt(h5file),FUN=aperm)
names(mymont) <- gsub(x = names(mymont), pattern = "\\_", replacement = ".")

sum(mymont$LAI.CO)


for (i in seq(10889053,10889103)){
  system2("qdel",i)
}
