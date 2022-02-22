rm(list = ls())

library(wesanderson)
library(tidyverse)
library(tidyr)

maps <- readRDS(file = "./outputs/output_maps.RDS") %>% group_by(model,var,scenario) %>%
  mutate(value.rel = (value - min(value,na.rm = TRUE))/
           (max(value,na.rm = TRUE) - min(value,na.rm = TRUE)))

maps.wide <- maps %>% dplyr::select(-c(value)) %>%
  pivot_wider(names_from = scenario,
              values_from = value.rel)

pal <- wes_palette("Darjeeling1", 3, type = c("discrete"))

ggplot(data = maps.wide %>%
         filter(var %in% c("AGB","GPP","LAI"))) +
  geom_point(aes(x = `Mean clay`,
                 y = `Min. clay`,
                 shape = model),alpha = 0.2, color = pal[3],show.legend = FALSE) +
  geom_point(aes(x = `Mean clay`,
                 y = `Max. clay`,
                 shape = model),alpha = 0.2, color = pal[1]) +
  facet_wrap(~ var) +
  labs(x = "Reference (Mean Clay)", y = "Scenario (Max. clay)") +
  theme_bw() +
  theme(text = element_text(size = 20),
        panel.spacing.x = unit(2, "lines"))


ggplot(data = maps.wide %>%
         filter(var %in% c("AGB","GPP","LAI"))) +
  geom_point(aes(x = `Mean clay`,
                 y = `Min. clay`,
                 shape = var),alpha = 0.2, color = pal[3],show.legend = FALSE) +
  geom_point(aes(x = `Mean clay`,
                 y = `Max. clay`,
                 shape = var),alpha = 0.2, color = pal[1]) +
  facet_wrap(~ model) +
  labs(x = "Reference (Mean Clay)", y = "Scenario (Max. clay)",
       shape = "Variable") +
  theme_bw() +
  theme(text = element_text(size = 20),
        panel.spacing.x = unit(2, "lines"),
        legend.position = c(0.08,0.75))

ggsave(filename = "./Figures/modelvsmodel.png",
       plot = last_plot(),width = 30,height = 10, unit = "cm",
       dpi = 300)
