rm(list = ls())

library(dplyr)
library(tidyr)
library(purrr)
library(RColorBrewer)
library(ggplot2)

# system2("rsync",paste("-avz","hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/df_SoilSens_analysis_PFT.RDS","./outputs"))
# system2("rsync",paste("-avz","hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/Outputs_ORC_PFT.RDS","./outputs/"))
# system2("rsync",paste("-avz","hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/Outputs_LPJ_PFT.RDS","./outputs"))

OP_ED2 <- readRDS("./outputs/df_SoilSens_analysis_PFT.RDS")

OP_ED2.sum <- OP_ED2 %>% group_by(scenario,lat,lon,pft) %>% summarise(gpp = mean(gpp),
                                                                      .groups = "keep")

OP_ED2.sum.wide <- OP_ED2.sum %>% pivot_wider(names_from = scenario,
                                              values_from = gpp)

OP_ED2.sum.wide.long <- bind_rows(list(OP_ED2.sum.wide %>% rename(mod = SoilGrids_max,
                                                              ref = SoilGrids_mean) %>% dplyr::select(-SoilGrids_min) %>% mutate(scenario = "max"),
                                   OP_ED2.sum.wide %>% rename(mod = SoilGrids_min,
                                                              ref = SoilGrids_mean) %>% dplyr::select(-SoilGrids_max) %>% mutate(scenario = "min")))

ggplot(data = OP_ED2.sum.wide.long %>% filter(!(ref == 0 & mod == 0)),
       aes(x = mod, y = ref)) +
  geom_point(aes(color = scenario),size = 0.05) +
  # stat_density_2d(aes(fill = stat(level)), geom = 'polygon',alpha = 0.5) +
  stat_density_2d(aes(fill = stat(density)), geom = 'raster', contour = FALSE,alpha = 1) +
  scale_fill_viridis_c(name = "density") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  # geom_abline(slope = 1, col = "red") +
  facet_wrap(~ scenario) +
  theme_bw() +
  guides(color = FALSE, fill = FALSE)

##########################################################################################################################
# ORCHIDEE

OP_ORC <- readRDS("./outputs/Outputs_ORC_PFT.RDS") %>%
  filter(time == 2010) %>%
  group_by(scenario,lon,lat)

OP_ORC_wide <- OP_ORC  %>% pivot_wider(values_from = value,
                                       names_from = scenario)

OP_ORC_wide.long <- bind_rows(list(OP_ORC_wide %>% rename(mod = SoilGrids_max,
                                                          ref = SoilGrids_mean) %>% dplyr::select(-SoilGrids_min) %>% mutate(scenario = "max"),
                                   OP_ORC_wide %>% rename(mod = SoilGrids_min,
                                                          ref = SoilGrids_mean) %>% dplyr::select(-SoilGrids_max) %>% mutate(scenario = "min")))

ggplot(data = OP_ORC_wide.long %>% filter(!(ref == 0 & mod == 0)),
       aes(x = mod, y = ref)) +
  geom_point(aes(color = scenario),size = 0.05) +
  # stat_density_2d(aes(fill = stat(level)), geom = 'polygon',alpha = 0.5) +
  stat_density_2d(aes(fill = stat(density)), geom = 'raster', contour = FALSE,alpha = 1) +
  scale_fill_viridis_c(name = "density") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  # geom_abline(slope = 1, col = "red") +
  facet_wrap(~ scenario) +
  theme_bw() +
  guides(color = FALSE, fill = FALSE)

# OP_ORC.sum <- OP_ORC %>% summarise(dominating.PFT = PFT[which.max(value)],
#                                            dominating.GPP = value[which.max(value)],
#                                            total.GPP = sum(value,na.rm = TRUE),
#                                            .groups = "keep")
#
# OP_ORC_wide <- OP_ORC.sum %>% pivot_wider(values_from = c(dominating.PFT,dominating.GPP,total.GPP),
#                                           names_from = scenario)  %>% mutate(diff_max = dominating.PFT_SoilGrids_mean - dominating.PFT_SoilGrids_max,
#                                                                          diff_min = dominating.PFT_SoilGrids_mean - dominating.PFT_SoilGrids_min)
#
# OP_ORC_wide %>% filter(diff_min != 0) %>% nrow() /nrow(OP_ORC_wide)




PFTs <- c("BNE","BINE","BNS","TeNE","TeBS","IBS","TeBE","TrBE","TrIBE","TrBR","C3G","C4G")

OP_LPJ <- readRDS("./outputs/Outputs_LPJ_PFT.RDS")

OP_LPJ.long <- OP_LPJ %>% dplyr::select(-Total)  %>% pivot_longer(cols = all_of(PFTs),
                                                                      names_to = "PFT",
                                                                      values_to = "value")

OP_LPJ.long.wide <- OP_LPJ.long %>% pivot_wider(values_from = value,
                                                names_from = scenario)

OP_LPJ.long.wide.all <- bind_rows(list(OP_LPJ.long.wide %>% rename(mod = SoilGrids_max,
                                                                   ref = SoilGrids_mean) %>% dplyr::select(-SoilGrids_min) %>% mutate(scenario = "max"),
                                   OP_LPJ.long.wide %>% rename(mod = SoilGrids_min,
                                                               ref = SoilGrids_mean) %>% dplyr::select(-SoilGrids_max) %>% mutate(scenario = "min")))

ggplot(data = OP_LPJ.long.wide.all %>% filter(!(ref == 0 & mod == 0)),
       aes(x = mod, y = ref)) +
  geom_point(aes(color = scenario),size = 0.05) +
  # stat_density_2d(aes(fill = stat(level)), geom = 'polygon',alpha = 0.5) +
  stat_density_2d(aes(fill = stat(density)), geom = 'raster', contour = FALSE,alpha = 1) +
  scale_fill_viridis_c(name = "density") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  # geom_abline(slope = 1, col = "red") +
  facet_wrap(~ scenario) +
  theme_bw() +
  guides(color = FALSE, fill = FALSE)

# OP_LPJ <- OP_LPJ %>%
#   mutate(dominating.GPP = pmax(BNE,BINE,BNS,TeNE,TeBS,IBS,TeBE,TrBE,TrIBE,TrBR,C3G,C4G)) %>% pivot_longer(cols = c(BNE,BINE,BNS,TeNE,TeBS,IBS,TeBE,TrBE,TrIBE,TrBR,C3G,C4G,Total),
#                                                                         names_to = "PFT",
#                                                                         values_to = "GPP") %>% dplyr::select(-variable) %>%
#   mutate(dominating.PFT = ifelse(GPP == dominating.GPP,PFT,"")) %>%
#   pivot_wider(names_from = PFT,
#               values_from = GPP) %>% group_by(Lon,Lat,scenario) %>%
#   summarise(dominating.GPP = mean(dominating.GPP),
#             dominating.PFT = dominating.PFT[2],
#             BNE = sum(BNE,na.rm = TRUE),
#             BINE = sum(BINE,na.rm = TRUE),
#             BNS = sum(BNS,na.rm = TRUE),
#             TeNE = sum(TeNE,na.rm = TRUE),
#             TeBS = sum(TeBS,na.rm = TRUE),
#             IBS = sum(IBS,na.rm = TRUE),
#             TeBE = sum(TeBE,na.rm = TRUE),
#             TrBE = sum(TrBE,na.rm = TRUE),
#             TrIBE = sum(TrIBE,na.rm = TRUE),
#             TrBR = sum(TrBR,na.rm = TRUE),
#             C3G = sum(C3G,na.rm = TRUE),
#             C4G = sum(C4G,na.rm = TRUE),
#             Total = sum(Total,na.rm = TRUE),
#             .groups = "keep") %>%
#   dplyr::select(Lon,Lat,scenario,dominating.GPP,dominating.PFT,Total) %>%
#   pivot_wider(values_from = c(dominating.PFT,dominating.GPP,Total),
#               names_from = scenario)


OP.all <- bind_rows(list(OP_ED2.sum.wide.long %>% mutate(pft = as.character(pft)) %>% mutate(model = "ED2"),
                         OP_ORC_wide.long %>% rename(pft = PFT) %>% dplyr::select(-time) %>% mutate(pft = as.character(pft),
                                                                                                    ref = 1/1000*365*ref,
                                                                                                    mod = 1/1000*365*mod,
                                                                                                    model = "ORCHIDEE") %>%
                           filter(!is.na(ref)),
                         OP_LPJ.long.wide.all %>% filter(variable == "GPP") %>% dplyr::select(-c(Year,variable)) %>% rename(lat = Lat,
                                                                                                                            lon = Lon,
                                                                                                                            pft = PFT) %>%
                           mutate(model = "LPJ-GUESS")))


OP.all$model <- factor(OP.all$model,levels = c("ORCHIDEE","ED2","LPJ-GUESS"))

ggplot(data = OP.all %>% filter(!(ref == 0 & mod == 0)),
       aes(x = mod, y = ref)) +
  geom_abline(slope = 1, col = "black",size = 2, linetype = 1) +
  geom_smooth(method = "lm",se = FALSE,color = "darkred",size = 2, linetype = 1,alpha = 1) +
  geom_point(size = 0.1,alpha = 0.4) +
  stat_density_2d(aes(fill = stat(ndensity)), geom = 'raster', contour = FALSE,alpha = 0.5) +
  # scale_fill_viridis_c(name = "density") +
  scale_fill_gradientn(colours=brewer.pal(9,"Greys")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "PFT-level reference GPP \r\n (kgC/m²/yr)", y = "PFT-level scenario GPP \r\n (kgC/m²/yr)") +
  facet_wrap(scenario ~ model,ncol = 3) +
  theme_bw() +
  guides(color = FALSE) +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")))



OP.all %>% filter(!(ref == 0 & mod == 0)) %>%
  group_by(model,scenario) %>% mutate(diff.rel = (ref - mod)/ref,
                                      ref = ref) %>%
  summarise(Ntot = length(ref),
            N = length(ref[diff.rel > 0.1 & ref > 1])) %>%
  mutate(N/Ntot)

OP_ORC <- readRDS("./outputs/Outputs_ORCHIDEE.RDS") %>%
  filter(time == 2010,
         variable == "GPP")

OP.sum <- OP_ORC %>% group_by(lat,lon,pft,scenario) %>% summarise(gpp = mean(value,na.rm = TRUE)) %>%
  group_by(lat,lon,scenario) %>% summarise(gpp = sum(gpp,na.rm = TRUE)) %>% filter(gpp > 0) %>%
  pivot_wider(values_from = gpp,
              names_from = scenario) %>%
  mutate(ref = SoilGrids_mean,
         max = SoilGrids_max,
         min = SoilGrids_min) %>% dplyr::select(lat,lon,ref,min,max) %>%
  pivot_longer(cols = c("max","min"),
               values_to = "mod",
               names_to = "scenario")

OP.all.all <- bind_rows(list(OP.all %>% filter(!(ref == 0 & mod == 0)),
                             OP.all %>% filter(!(ref == 0 & mod == 0)) %>%
                               filter(model != "ORCHIDEE") %>%
                               group_by(scenario,model,lat,lon) %>%
                               summarise(mod = sum(mod,na.rm = TRUE),
                                         ref = sum(ref,na.rm = TRUE),
                                         .groups = "keep") %>% mutate(pft = "Total"),
                             OP.sum %>% mutate(pft = "Total",
                                               model = "ORCHIDEE")))


ggplot(data = OP.all.all %>% filter(pft != "Total"),
       aes(x = mod, y = ref)) +
  geom_abline(slope = 1, col = "black", linetype = 1) +
  geom_smooth(method = "lm",se = FALSE,color = "black", linetype = 2,alpha = 1) +
  geom_point(size = 0.1,alpha = 0.4) +

  geom_point(data = OP.all.all %>% filter(pft == "Total"),
             aes(x = mod, y = ref),
             size = 0.1,alpha = 0.4, color = "red") +

  stat_density_2d(aes(fill = stat(ndensity)), geom = 'raster', contour = FALSE,alpha = 0.5) +
  scale_fill_gradientn(colours=brewer.pal(9,"Greys")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "PFT-level reference GPP \r\n (kgC/m²/yr)", y = "PFT-level scenario GPP \r\n (kgC/m²/yr)") +
  facet_wrap(scenario ~ model,ncol = 3) +
  theme_bw() +
  guides(color = FALSE) +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")))


ggsave(filename = "./Figures/plot.Density.png",
       plot = last_plot(),width = 30,height = 15, unit = "cm",
       dpi = 300)



OP.diff <- OP.all %>% filter(!(ref == 0 & mod == 0)) %>%
  mutate(diff = mod - ref,
         diff.rel = (mod - ref)/ref) %>%
  group_by(scenario, model)

# ggplot(data = OP.diff) +
#   geom_boxplot(aes(x = model,y = diff.rel),outlier.shape = NA) +
#   facet_wrap(~ scenario) +
#   # scale_y_continuous(limits = 0.1*c(-1,1)) +
#   theme_bw()

OP.diff %>%
  summarise(GPP.m = mean(ref,na.rm = TRUE),
            diff.m = mean(diff,na.rm = TRUE),
            diff.low = quantile(abs(diff),0.025,na.rm = TRUE),
            diff.high = quantile(abs(diff),0.975,na.rm = TRUE),
            diff.min = min(abs(diff),na.rm = TRUE),
            diff.max = max(abs(diff),na.rm = TRUE),
            RMSE = sqrt(sum(diff**2,na.rm = TRUE)/length(diff)),
            .groups = "keep")
