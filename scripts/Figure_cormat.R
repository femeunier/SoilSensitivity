# rm(list = ls())
#
# library(SoilSensitivity)
# library(lattice)
# library(raster)
# library(purrr)
# library(tidyr)
# library(ggplot2)
# library(dplyr)
# library(reshape2)
# library(rnaturalearth)
# library(rnaturalearthdata)
# library(wesanderson)
# library(soiltexture)
# library(ggtern)
#
# dat<-data.frame(replicate(2,sample(c("A", "B", "C","D"), size = 100, replace=TRUE)))
#
# trans.matrix <- function(X, prob=T)
# {
#   tt <- table( c(X[,-ncol(X)]), c(X[,-1]) )
#   if(prob) tt <- tt / rowSums(tt)
#   tt
# }
#
# trans.matrix(as.matrix(dat))
# trf <- table(data.frame(ref=dat$X1,mod=dat$X2))
# trf_prob <- round(trf / rowSums(trf),2)
#
# get_lower_tri<-function(cormat){
#   cormat[upper.tri(cormat)] <- NA
#   return(cormat)
# }
# # Get upper triangle of the correlation matrix
# get_upper_tri <- function(cormat){
#   cormat[lower.tri(cormat)]<- NA
#   return(cormat)
# }
#
# df2plot_low <- melt(get_lower_tri(trf_prob),na.rm = TRUE)
# df2plot_high <- melt(get_upper_tri(trf_prob),na.rm = TRUE)
#
# ggplot(df2plot_low, aes(ref, mod, fill = value)) +
#   geom_tile(color = "white") +
#   scale_fill_gradient2(
#     low = "blue",
#     high = "red",
#     mid = "white",
#     midpoint = 0,
#     limit = c(-1, 1),
#     space = "Lab",
#     name = "Pearson\nCorrelation",
#     na.value = "transparent"
#   ) +
#   theme_minimal() +
#   coord_fixed() +
#   scale_y_discrete(position = "right") +
#   theme(
#     text = element_text(size = 20),
#     axis.text.x = element_text(
#       angle = 45,
#       vjust = 1,
#       size = 12,
#       hjust = 1
#     ),
#     axis.title.x = element_blank(),
#     axis.title.y = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.border = element_blank(),
#     panel.background = element_blank(),
#     axis.ticks = element_blank()
#   )
#
# ggplot(df2plot_high, aes(ref, mod, fill = value)) +
#   geom_tile(color = "white") +
#   scale_fill_gradient2(
#     low = "blue",
#     high = "red",
#     mid = "white",
#     midpoint = 0,
#     limit = c(-1, 1),
#     space = "Lab",
#     name = "Pearson\nCorrelation",
#     na.value = "transparent"
#   ) +
#   theme_minimal() +
#   coord_fixed() +
#   scale_x_discrete(position = "top") +
#   theme(
#     text = element_text(size = 20),
#     axis.title.x = element_blank(),
#     axis.title.y = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.border = element_blank(),
#     panel.background = element_blank(),
#     axis.ticks = element_blank()) +
#   guides(fill = FALSE)

##############################################################################################################
# Transition matrix

rm(list = ls())

library(SoilSensitivity)
library(lattice)
library(raster)
library(purrr)
library(tidyr)
library(ggtern)
library(ggplot2)
library(dplyr)
library(reshape2)
library(rnaturalearth)
library(rnaturalearthdata)
library(wesanderson)
library(soiltexture)
library(cowplot)

data("USDA")
detach("package:ggtern", unload=TRUE)
detach("package:ggplot2", unload=TRUE)
library(ggplot2)

get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

trans.matrix <- function(X, prob=TRUE)
{
  tt <- table( c(X[,-ncol(X)]), c(X[,-1]) )
  if(prob) tt <- tt / rowSums(tt)
  tt
}

trans.matrix2 <- function(X, prob=T){
  # myStates <- sort(unique(c(X[[1]], X[[2]])))
  myStates <- c("Clay","Sandy Clay","Sandy Clay Loam","Sandy Loam","Loamy Sand","Sand","Clay Loam","Loam","Silt Loam","Silty Clay","Silty Clay Loam","Silt")
  lenSt <- length(myStates)

  currState <- match(X[[1]], myStates)
  nextState <- match(X[[2]], myStates)
  transMat <- matrix(NA, lenSt, lenSt)

  summary <- data.frame(currState,nextState) %>% group_by(currState,nextState) %>% summarise(N = length(currState),
                                                                                             .groups = "keep")

  transMat[cbind(summary$currState, summary$nextState)] <- summary$N

  if (prob){
    transMat <- transMat/rowSums(transMat)
  }
  # transMat[is.na(transMat)] <- 0
  colnames(transMat) <- rownames(transMat) <- myStates

  return(transMat)
}


rc <- brick(file.path(".","maps", "soilgrid.grd"),expand = TRUE)

map.clay <- rc[[2]]

df.clay <- data.frame(clay = as.vector(map.clay),
                      sand = as.vector( rc[[1]]))
df.clay.all <- bind_rows(list(data.frame(clay = as.vector( raster::aggregate(rc[[c(2)]],
                                                                             1 / res(rc)[1] * 0.5,
                                                                             1 / res(rc)[2] * 0.5,
                                                                             fun = mean)),
                                         sand = as.vector( raster::aggregate(rc[[c(1)]],
                                                                             1 / res(rc)[1] * 0.5,
                                                                             1 / res(rc)[2] * 0.5,
                                                                             fun = mean))) %>% mutate(type = 'mean'),
                              data.frame(clay = as.vector( aggregates(rc[[c(2)]],
                                                                      rc[[c(2)]],
                                                                      1 / res(rc)[1] * 0.5,
                                                                      1 / res(rc)[2] * 0.5,
                                                                      probs = 0.01)),
                                         sand = as.vector( aggregates(rc[[c(2)]],
                                                                      rc[[c(1)]],
                                                                      1 / res(rc)[1] * 0.5,
                                                                      1 / res(rc)[2] * 0.5,
                                                                      probs = 0.01)))  %>% mutate(type = 'min'),
                              data.frame(clay = as.vector( aggregates(rc[[c(2)]],
                                                                      rc[[c(2)]],
                                                                      1 / res(rc)[1] * 0.5,
                                                                      1 / res(rc)[2] * 0.5,
                                                                      probs = 0.99)),
                                         sand = as.vector( aggregates(rc[[c(2)]],
                                                                      rc[[c(1)]],
                                                                      1 / res(rc)[1] * 0.5,
                                                                      1 / res(rc)[2] * 0.5,
                                                                      probs = 0.99))) %>% mutate(type = 'max')))

df.clay.all <- df.clay.all %>% group_by(type) %>% mutate(N = 1:length(sand))


df.clay.all.renamed <- df.clay.all %>% mutate(SAND = 100*sand,
                                              CLAY = 100*clay,
                                              SILT = (1 - sand - clay)*100)

sea.mask <- df.clay.all %>% filter(type == "mean",
                                   clay == 0, sand == 0) %>% pull(N)

df.clay.all.renamed.masked <- df.clay.all.renamed %>% filter(!(N %in% sea.mask))


soil.type = TT.points.in.classes(tri.data = as.data.frame(df.clay.all.renamed.masked %>% ungroup() %>% dplyr::select(SAND,CLAY,SILT)),
                                 class.sys = "USDA.TT",
                                 PiC.type = "t" )

df.correspondence <- data.frame(long.label = levels(USDA$Label),
                                short = c("Cl","SaCl","SaClLo","SaLo","LoSa","Sa","ClLo","Lo","SiLo","SiCl","SiClLo","Si"))

soil.type.long <- as.character(df.correspondence$long.label[match(soil.type,df.correspondence$short)])

df.clay.all.renamed.masked.mutate <- df.clay.all.renamed.masked %>% ungroup() %>% mutate(soil = soil.type.long) %>% dplyr::select(N,type,soil) %>%
  pivot_wider(values_from = soil,
              names_from = type)

# saveRDS(object = df.clay.all.renamed.masked.mutate,"./outputs/df.clay.all.renamed.masked.mutate.RDS")
# df.clay.all.renamed.masked.mutate <- readRDS("./outputs/df.clay.all.renamed.masked.mutate.RDS")


trf_min <- trans.matrix2(X = df.clay.all.renamed.masked.mutate %>% dplyr::select(mean,min),prob = FALSE)
trf_min_prob <- round(trf_min / rowSums(trf_min),2)

df2plot_min <- melt((trf_min),na.rm = FALSE) %>% mutate(Var1 = rownames(trf_min_prob)[Var1],
                                                       Var2 = colnames(trf_min_prob)[Var2])

# df2plot_min$Var1 <- factor(df2plot_min$Var1,unique(USDA$Label))
# df2plot_min$Var2 <- factor(df2plot_min$Var2,unique(USDA$Label))

trf_max <- trans.matrix2(X = df.clay.all.renamed.masked.mutate %>% dplyr::select(mean,max),prob = FALSE)
trf_max_prob <- round(trf_max / rowSums(trf_max),2)

df2plot_max <- melt((trf_max),na.rm = FALSE) %>% mutate(Var1 = rownames(trf_max_prob)[Var1],
                                                            Var2 = colnames(trf_max_prob)[Var2])

# df2plot_max$Var1 <- factor(df2plot_max$Var1,unique(USDA$Label))
# df2plot_max$Var2 <- factor(df2plot_max$Var2,unique(USDA$Label))


pal <- wes_palette("Darjeeling1", 3, type = c("discrete"))

ggplot.min <- ggplot(data = df2plot_min,
       mapping = aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "grey") +
  scale_fill_gradient2(
    low = "blue",
    high = pal[1],
    mid = "white",
    midpoint = 0,
    limits = c(0,1000),
    space = "Lab",
    na.value = "lightgrey"
  ) +
  theme_bw() +
  labs(x = "",y = "Min clay") +
  scale_x_discrete(labels = c(),expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  coord_fixed() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        text = element_text(size = 20),
        panel.grid = element_line(colour = "grey"),
        axis.ticks = element_line(colour = "grey"),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm"))) +
  guides(fill = "none")

ggplot.max <- ggplot(data = df2plot_max,
       mapping = aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "grey") +
  scale_fill_gradient2(
    low = "blue",
    high = pal[1],
    mid = "white",
    midpoint = 0,
    space = "Lab",
    limits = c(0,1000),
    na.value = "lightgrey"
  ) +
  theme_bw() +
  labs(x = "Mean clay",y = "Max clay") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  coord_fixed() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        text = element_text(size = 20),
        panel.grid = element_line(colour = "grey"),
        axis.ticks = element_line(colour = "grey"),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm"))) +
  guides(fill = "none")

df2plot_min %>% filter(Var1 == Var2) %>% pull(value) %>% sum(na.rm = T) /
  df2plot_min %>% pull(value) %>% sum(na.rm = T)

df2plot_max %>% filter(Var1 == Var2) %>% pull(value) %>% sum(na.rm = T) /
  df2plot_max %>% pull(value) %>% sum(na.rm = T)

plot_grid(ggplot.min,ggplot.max,align = "hv",nrow = 2)

ggsave(filename = "./Figures/Figure.soil.change.png",
       plot = last_plot(),width = 20,height = 40, unit = "cm",
       dpi = 300)


ggsave(filename = "./Figures/Figure.soil.change_legend.png",
       plot =  ggplot(data = df2plot_max,
                      mapping = aes(x = Var1, y = Var2, fill = value)) +
         geom_tile(color = "grey") +
         scale_fill_gradient2(
           low = "blue",
           high = pal[1],
           mid = "white",
           midpoint = 0,
           space = "Lab",
           limits = c(0,1000),
           na.value = "lightgrey"
         ) +
         theme_bw() +
         labs(x = "Mean clay",y = "Max clay", fill = "Frequency (%)") +
         scale_x_discrete(expand = c(0,0)) +
         scale_y_discrete(expand = c(0,0)) +
         coord_fixed() +
         theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
               text = element_text(size = 20),
               panel.grid = element_line(colour = "grey"),
               axis.ticks = element_line(colour = "grey"),
               axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
               axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm"))) ,width = 20,height = 40, unit = "cm",
       dpi = 300)

