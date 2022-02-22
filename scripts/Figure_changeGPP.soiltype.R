
rm(list = ls())

library(SoilSensitivity)
library(lattice)
library(raster)
library(purrr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(rnaturalearth)
library(rnaturalearthdata)
library(wesanderson)
library(soiltexture)
library(cowplot)


rc <- brick(file.path(".","maps", "soilgrid.grd"),expand = TRUE)

map.clay <- rc[[2]]

df.clay.all <- bind_rows(list(cbind(as.data.frame(raster::aggregate(rc[[c(2)]],
                                                                         1 / res(rc)[1] * 0.5,
                                                                         1 / res(rc)[2] * 0.5,
                                                                         fun = mean),
                                                       xy = TRUE) %>% rename(clay = clay_0.5,
                                                                             lon = x,
                                                                             lat = y),
                                         as.data.frame(raster::aggregate(rc[[c(1)]],
                                                                         1 / res(rc)[1] * 0.5,
                                                                         1 / res(rc)[2] * 0.5,
                                                                         fun = mean),
                                                       xy = FALSE) %>% rename(sand = sand_0.5)) %>% mutate(type = 'mean'),
                              cbind(as.data.frame( aggregates(rc[[c(2)]],
                                                              rc[[c(2)]],
                                                              1 / res(rc)[1] * 0.5,
                                                              1 / res(rc)[2] * 0.5,
                                                              probs = 0.01),
                                                   xy = TRUE)  %>% rename(clay = clay_0.5,
                                                                          lon = x,
                                                                          lat = y),
                                    as.data.frame( aggregates(rc[[c(2)]],
                                                              rc[[c(1)]],
                                                              1 / res(rc)[1] * 0.5,
                                                              1 / res(rc)[2] * 0.5,
                                                              probs = 0.01))) %>% rename(sand = clay_0.5) %>% mutate(type = 'min'),
                         cbind(as.data.frame( aggregates(rc[[c(2)]],
                                                         rc[[c(2)]],
                                                         1 / res(rc)[1] * 0.5,
                                                         1 / res(rc)[2] * 0.5,
                                                         probs = 0.99),
                                              xy = TRUE)  %>% rename(clay = clay_0.5,
                                                                     lon = x,
                                                                     lat = y),
                               as.data.frame( aggregates(rc[[c(2)]],
                                                         rc[[c(1)]],
                                                         1 / res(rc)[1] * 0.5,
                                                         1 / res(rc)[2] * 0.5,
                                                         probs = 0.99))) %>% rename(sand = clay_0.5) %>% mutate(type = 'max')))

df.clay.all <- df.clay.all %>% group_by(type) %>% mutate(N = 1:length(sand))
df.clay.all.renamed <- df.clay.all %>% mutate(SAND = 100*sand,
                                              CLAY = 100*clay,
                                              SILT = (1 - sand - clay)*100)

sea.mask <- df.clay.all %>% filter(type == "mean",
                                   clay == 0, sand == 0) %>% pull(N)

df.clay.all.renamed.masked <- df.clay.all.renamed %>% filter(!(N %in% sea.mask))

library(ggtern)
data("USDA")
detach("package:ggtern", unload=TRUE)
detach("package:cowplot", unload=TRUE)
detach("package:ggplot2", unload=TRUE)
library(ggplot2)
library(cowplot)



soil.type = TT.points.in.classes(tri.data = as.data.frame(df.clay.all.renamed.masked %>% ungroup() %>% dplyr::select(SAND,CLAY,SILT)),
                                 class.sys = "USDA.TT",
                                 PiC.type = "t" )

df.correspondence <- data.frame(long.label = levels(USDA$Label),
                                short = c("Cl","SaCl","SaClLo","SaLo","LoSa","Sa","ClLo","Lo","SiLo","SiCl","SiClLo","Si"))

soil.type.long <- as.character(df.correspondence$long.label[match(soil.type,df.correspondence$short)])

df.clay.all.renamed.masked.mutate <- df.clay.all.renamed.masked %>% ungroup() %>% mutate(soil = soil.type.long) %>% dplyr::select(lat,lon,,type,soil) %>%
  pivot_wider(values_from = soil,
              names_from = type) %>%
  mutate(lat = lat + 0.25,
         lon = lon + 0.25)   # To check!!!
# GPP.models.wide %>% group_by(model) %>% summarise(lat.min = min(lat),lat.max = max(lat),lon.min = min(lon),lon.max = max(lon))
df.clay.all.renamed.masked.mutate %>% summarise(lat.min = min(lat),lat.max = max(lat),lon.min = min(lon),lon.max = max(lon))

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

trf_min <- trans.matrix2(X = df.clay.all.renamed.masked.mutate %>% dplyr::select(mean,min),prob = FALSE)
df2plot_min <- melt((trf_min),na.rm = FALSE) %>% mutate(Var1 = rownames(trf_min)[Var1],
                                                        Var2 = colnames(trf_min)[Var2])

trf_max <- trans.matrix2(X = df.clay.all.renamed.masked.mutate %>% dplyr::select(mean,max),prob = FALSE)
df2plot_max <- melt((trf_max),na.rm = FALSE) %>% mutate(Var1 = rownames(trf_max)[Var1],
                                                        Var2 = colnames(trf_max)[Var2])


all.models <- readRDS("./outputs/OPhistorical.RDS")
GPP.models <- all.models %>% filter(variable == "GPP",
                                    !is.na(value))
GPP.models.wide <- GPP.models %>% pivot_wider(values_from = value,
                                              names_from = scenario) %>% ungroup() %>% dplyr::select(-c(variable))

model.vs.soil <- GPP.models.wide %>% left_join(df.clay.all.renamed.masked.mutate,
                                               by = c("lon","lat")) %>% mutate(transition1 = paste(mean,min,sep = ":"),
                                                                               transition2 = paste(mean,max,sep = ":"))


myStates <- c("Clay","Sandy Clay","Sandy Clay Loam","Sandy Loam","Loamy Sand","Sand","Clay Loam","Loam","Silt Loam","Silty Clay","Silty Clay Loam","Silt")
lenSt <- length(myStates)

# currState <- match(rc[[1]], myStates)
# nextState <- match(rc[[2]], myStates)

model.vs.soil.summ <- model.vs.soil %>% group_by(model,transition1,transition2) %>% summarise(diff_min = mean(SoilGrids_mean - SoilGrids_min,na.rm = TRUE),
                                                                                  diff_min_rel = 100*mean((SoilGrids_mean - SoilGrids_min)/SoilGrids_mean,na.rm = TRUE),
                                                                                  diff_max = mean(SoilGrids_mean - SoilGrids_max,na.rm = TRUE),
                                                                                  diff_max_rel = 100*mean((SoilGrids_mean - SoilGrids_max)/SoilGrids_mean,na.rm = TRUE),
                                                            .groups = "keep") %>%
  mutate(mean = gsub(":.*", "", transition1),
         min = gsub(".*:", "", transition1),
         mean2 = gsub(":.*", "", transition2),
         max = gsub(".*:", "", transition2),
         mean_pos = match(mean,myStates),
         min_pos = match(min,myStates),
         mean_pos = match(mean,myStates),
         max_pos = match(max,myStates)) %>% filter(!is.na(mean_pos))

threshold = 100

for (cmodel in c("ED2","ORCHIDEE","LPJ-GUESS")){
  transMat_min <- trf_min*0
  transMat_max <- trf_max*0

  cmodel.vs.soil.summ <- model.vs.soil.summ %>% filter(model == cmodel)

  transMat_min[cbind(cmodel.vs.soil.summ$mean_pos, cmodel.vs.soil.summ$min_pos)] <- cmodel.vs.soil.summ$diff_min_rel
  transMat_max[cbind(cmodel.vs.soil.summ$mean_pos, cmodel.vs.soil.summ$max_pos)] <- cmodel.vs.soil.summ$diff_max_rel

  transMat_min[transMat_min < (-threshold)] <- -threshold
  transMat_max[transMat_max < (-threshold)] <- -threshold
  transMat_min[transMat_min > (threshold)] <- threshold
  transMat_max[transMat_max > (threshold)] <- threshold


    df2plot_min <- melt((transMat_min)) %>% mutate(Var1 = rownames(transMat_min)[Var1],
                                                   Var2 = colnames(transMat_min)[Var2])

    df2plot_max <- melt((transMat_max)) %>% mutate(Var1 = rownames(transMat_max)[Var1],
                                                                Var2 = colnames(transMat_max)[Var2])

    ggplot.min <- ggplot(data = df2plot_min,
         mapping = aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(color = "grey") +
    scale_fill_gradient2(
      low = "blue",
      high = "red",
      mid = "white",
      midpoint = 0,
      space = "Lab",
      na.value = "lightgrey"
    ) +
    theme_bw() +
    labs(x = "",y = "Min clay") +
      scale_x_discrete(labels = c(),expand = c(0,0)) +
      scale_y_discrete(labels = c(),expand = c(0,0)) +
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
      high = "red",
      mid = "white",
      midpoint = 0,
      space = "Lab",
      na.value = "lightgrey",limits = c(-1,1)*threshold) +
    theme_bw() +
    labs(x = "Mean clay",y = "Max clay") +
      scale_x_discrete(expand = c(0,0)) +
      scale_y_discrete(labels = c(),expand = c(0,0)) +
    coord_fixed() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          text = element_text(size = 20),
          panel.grid = element_line(colour = "grey"),
          axis.ticks = element_line(colour = "grey"),
          axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
          axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm"))) +
    guides(fill = "none")

    plot_grid(ggplot.min,ggplot.max,align = "hv",nrow = 2)

    ggsave(filename = paste0("./Figures/Figure.soil.change",cmodel,".png"),
           plot = plot_grid(ggplot.min,ggplot.max,align = "hv",nrow = 2),width = 20,height = 40, unit = "cm",
           dpi = 300)

    # print(c(cmodel,
    #         df2plot_min %>% filter(Var1 == Var2) %>% summarise(value.m = mean(value,na.rm = TRUE)) %>% pull(value.m),
    #         df2plot_max %>% filter(Var1 == Var2) %>% summarise(value.m = mean(value,na.rm = TRUE))  %>% pull(value.m)))

    print(c(cmodel,
            df2plot_min %>% filter(Var1 != Var2) %>% summarise(value.m = mean(value,na.rm = TRUE)) %>% pull(value.m),
            df2plot_max %>% filter(Var1 != Var2) %>% summarise(value.m = mean(value,na.rm = TRUE))  %>% pull(value.m)))

}


ggplot(data = df2plot_max,
       mapping = aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "grey") +
  scale_fill_gradient2(
    low = "blue",
    high = "red",
    mid = "white",
    midpoint = 0,
    space = "Lab",
    na.value = "lightgrey",limits = c(-1,1)*threshold) +
  theme_bw() +
  labs(x = "Mean clay",y = "Max clay", fill = "Relative change \r\n of GPP (%)") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(labels = c(),expand = c(0,0)) +
  coord_fixed() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        text = element_text(size = 20),
        legend.title.align =  0.5,
        panel.grid = element_line(colour = "grey"),
        axis.ticks = element_line(colour = "grey"),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")))


ggsave(filename = paste0("./Figures/legend",cmodel,".png"),
       plot = last_plot(),width = 20,height = 40, unit = "cm",
       dpi = 300)


