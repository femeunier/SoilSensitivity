rm(list = ls())

library(SoilSensitivity)
library(lattice)
library(raster)
library(purrr)
library(tidyr)
# library(ggplot2)
library(dplyr)
library(reshape2)
library(rnaturalearth)
library(rnaturalearthdata)
library(wesanderson)
library(soiltexture)
library(ggtern)
library(compositions)
library(rlang)

StatDensityTern2 <-
  ggproto(
    "StatDensityTern2",
    StatDensityTern,
    compute_group = function(
      self, data, scales, na.rm = FALSE, n = 100, h = NULL,
      bdl = 0, bdl.val = NA, contour = TRUE, base = "ilr", expand = 0.5,
      weight = NULL, bins = NULL, binwidth = NULL, breaks = NULL
    ) {
      if (!c(base) %in% c("identity", "ilr"))
        stop("base must be either identity or ilr", call. = FALSE)
      raes = self$required_aes
      data[raes] = suppressWarnings(compositions::acomp(data[raes]))
      data[raes][data[raes] <= bdl] = bdl.val[1]
      data = remove_missing(data, vars = self$required_aes, na.rm = na.rm,
                            name = "StatDensityTern", finite = TRUE)
      if (ggplot2:::empty(data))
        return(data.frame())
      coord = coord_tern()
      f = get(base, mode = "function")
      fInv = get(sprintf("%sInv", base), mode = "function")
      if (base == "identity")
        data = tlr2xy(data, coord, inverse = FALSE, scale = TRUE)
      h = h %||% ggtern:::estimateBandwidth(base, data[which(colnames(data) %in%
                                                               raes)])
      if (length(h) != 2)
        h = rep(h[1], 2)
      if (base != "identity" && diff(h) != 0)
        warning("bandwidth 'h' has different x and y bandwiths for 'ilr', this may (probably will) introduce permutational artifacts depending on the ordering",
                call. = FALSE)
      data[raes[1:2]] = suppressWarnings(f(as.matrix(data[which(colnames(data) %in%
                                                                  raes)])))
      expand = if (length(expand) != 2)
        rep(expand[1], 2)
      else expand
      rngxy = range(c(data$x, data$y))
      rngx = scales:::expand_range(switch(base, identity = coord$limits$x,
                                          rngxy), expand[1])
      rngy = scales:::expand_range(switch(base, identity = coord$limits$y,
                                          rngxy), expand[2])
      dens = ggtern:::kde2d.weighted(data$x, data$y, h = h, n = n, lims = c(rngx,
                                                                            rngy), w = data$weight)

      # Here be relevant changes ------------------------------------------------


      df = data.frame(expand.grid(x = dens$x, y = dens$y),
                      z = as.vector(dens$z) * nrow(data),
                      group = data$group[1])

      # Here end relevant changes -----------------------------------------------


      if (contour) {
        df = StatContour$compute_panel(df, scales, bins = bins,
                                       binwidth = binwidth, breaks = breaks)
      }
      else {
        names(df) <- c("x", "y", "density", "group")
        df$level <- 1
        df$piece <- 1
      }
      if (base == "identity")
        df = tlr2xy(df, coord, inverse = TRUE, scale = TRUE)
      df[raes] = suppressWarnings(fInv(as.matrix(df[which(colnames(df) %in%
                                                            raes)])))
      df
    }
  )


approveupdate <- c(ggtern:::.approvedstat, "density_tern2" = "StatDensityTern2")
assignInNamespace(".approvedstat", approveupdate, pos = "package:ggtern")


rc <- brick(file.path(".","maps", "soilgrid.grd"),expand = TRUE)

top.sand.mean <- raster::aggregate(rc[[c(2)]],
                                   1 / res(rc)[1] * 0.5,
                                   1 / res(rc)[2] * 0.5,fun = mean)

map.sand <- top.sand.mean
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



soil_data <- data.frame(
  soil= c("a", "b", "c", "d"),
  sand = c(15, 18, 57, 32),
  silt = c(52, 70, 8, 26),
  clay = c(33, 12, 35, 42),
  om = c(1, 3, 4, 11),
  bd = c(1.33, 1.38, 1.76, 1.15)
)

data(USDA)

USDA_text <- USDA  %>% group_by(Label) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)
detach("package:ggtern", unload=TRUE)
detach("package:ggplot2", unload=TRUE)

library(ggplot2)

ggplot(data = USDA, aes(y = Clay,
                        x = Sand,
                        z = Silt)) +
  geom_polygon(,
               alpha = 0.0,
               size = 0.5,
               color = "black") +
  # coord_tern(L = "x", T = "y", R = "z") +

  geom_text(data = USDA_text,
            aes(label = Label),
            color = 'black',
            size = 2) +
  stat_density_tern(data = df.clay.all,
                    aes(
                      x = sand * 100,
                      y = clay * 100,
                      z = (1 - sand - clay) * 100,
                      alpha = ..level.., fill = type
                    ),
                    geom = 'polygon',
                    bins = 10,
                    color = "red") +
  theme_showarrows() +
  theme_clockwise() +
  theme(text = element_text(family = "Helvetica")) +
  guides(fill = FALSE, color = FALSE)


df <- data.frame(X = c(runif(150, 0.7, 1),runif(50, 0, 0.3)),
                 Y = c(runif(150, 0, 0.3),runif(50, 0, 0.3)),
                 Z = c(runif(150, 0, 0.5),runif(50, 0.5, 1)),
                 D = c(rep("A", 150), rep("B", 50)))



df.clay.all <- df.clay.all %>% group_by(type) %>% mutate(N = 1:length(sand))
sea.mask <- df.clay.all %>% filter(type == "mean",
                                   clay == 0, sand == 0) %>% pull(N)

ggtern(data = df.clay.all  %>% filter(!(N %in% sea.mask)),
       aes(
         x = sand * 100,
         y = clay * 100,
         z = (1 - sand - clay) * 100
       )) +
  coord_tern(L = "x", T = "y", R = "z") +
  geom_polygon(data = USDA, aes(x = Sand,
                                y = Clay,
                                z = Silt),
               alpha = 0.0,
               size = 0.5,
               color = "black") +
  geom_text(data = USDA_text,
            aes(x = Sand,
                y = Clay,
                z = Silt,
                label = Label),
            color = 'black',
            size = 2) +
  geom_polygon(
    aes(fill = type),
    stat = "DensityTern2",
    breaks = seq(100, 1000000, by = 10000),
    color = NA,
    alpha = 0.4
  ) +
  labs(x = "Sand (%)",y = "Clay (%)", z = "Silt (%)") +
  # facet_wrap(~ type) +
  # geom_point(alpha = 0.5) +
  scale_colour_manual(values = c("tomato3", "turquoise4")) +
  theme_showarrows() +
  theme_clockwise() +
  theme(text = element_text(family = "Helvetica",
                            size = 20)) +
  guides(alpha = FALSE, fill = FALSE)


pal <- wes_palette("Darjeeling1", 3, type = c("discrete"))

ggplot(data = USDA, aes(y = Clay,
                        x = Sand,
                        z = Silt)) +
  coord_tern(L = "x", T = "y", R = "z") +
  geom_polygon(data = df.clay.all  %>% filter(!(N %in% sea.mask)),
               mapping = aes(
                 x = sand * 100,
                 y = clay * 100,
                 z = (1 - sand - clay) * 100,
                 fill = type),
    stat = "DensityTern2",
    breaks = seq(0, 1000000, by = 10000),
    color = NA,
    alpha = 0.6
  ) +
  geom_text(data = USDA_text,
            aes(x = Sand,
                y = Clay,
                z = Silt,
                label = Label),
            color = 'black',
            size = 3) +
  geom_polygon(aes(fill = Label),
               alpha = 0.0,
               size = 0.5,
               color = "black") +
  labs(x = "Sand (%)",y = "Clay (%)", z = "Silt (%)") +
  # facet_wrap(~ type) +
  # geom_point(alpha = 0.5) +
  # scale_colour_manual(values = c("tomato3", "turquoise4")) +
  scale_fill_manual(values = c(rep("#000000",4),pal,rep("#000000",8))) +
  theme_showarrows() +
  theme_clockwise() +
  theme(text = element_text(family = "Helvetica",
                            size = 20)) +
  guides(alpha = FALSE, fill = FALSE)

ggsave(filename = "./Figures/Triangle.Soil.png",
       plot = last_plot(),width = 20,height = 20, unit = "cm",
       dpi = 300)
