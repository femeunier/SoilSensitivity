rm(list = ls())

library(rhdf5)
library(dplyr)

source("/home/femeunier/Documents/ED2/R-utils/h5read_opt.r")

year = 2009
system2("rsync",paste("-avz",paste0("hpc:/kyukon/scratch/gent/vo/000/gvo00074/felicien/SoilSensitivity/out/SoilSens_Amazon_IC_SoilGrids_mean_X_54.5W_Y_9.5S/analy/analysis-Q-",year,"-01-00-000000-g01.h5"),
                      "./outputs"))

h5file <- file.path("/home/femeunier/Documents/projects/SoilSensitivity/outputs",
                    paste0("analysis-Q-",year,"-01-00-000000-g01.h5"))

mymont    = lapply(h5read_opt(h5file),FUN=aperm)
names(mymont) <- gsub(x = names(mymont), pattern = "\\_", replacement = ".")


bdeadconow        = mymont$BDEADA + mymont$BDEADB
btimberconow      = mymont$BTIMBER
bleafconow        = mymont$MMEAN.BLEAF.CO

bcrootconow       = mymont$BSAPWOODB + mymont$BDEADB
bstemconow        = mymont$BSAPWOODA + mymont$BDEADA
brootconow        = bfrootconow + bcrootconow
baliveconow       = bleafconow + bfrootconow + bsapwoodconow + bbarkconow
bstorageconow     = mymont$MMEAN.BSTORAGE.CO
bseedsconow       = mymont$BSEEDS.CO
byieldconow       = mymont$BYIELD.CO
biomassconow      = baliveconow + bstorageconow + bseedsconow + bdeadconow

mymont$BDEADA + mymont$BDEADB



