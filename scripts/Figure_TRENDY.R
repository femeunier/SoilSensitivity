rm(list = ls())

library(stringr)

# CO2
fileCO2 <- "./data/CO2_1700_2019_TRENDYv2020.txt"
dataC02 <- read.table(fileCO2,stringsAsFactors = FALSE)

dataCO2.n <- dataC02 %>% mutate(years = str_sub(V1,7,10),
                                CO2 = as.numeric(str_sub(V1,12,17))) %>% dplyr::select(years,CO2)

ggplot(data = dataCO2.n) +
  geom_rect(aes(xmin = 1901,xmax = 1910,ymin = -Inf,ymax = Inf),fill = "lightgrey",alpha = 0.2) +
  geom_line(aes(x = as.numeric(years), y = CO2)) +
  theme_bw() +
  labs(y = "CO2 (ppm)", x = "") +
  scale_y_continuous(limits = c(250,400)) +
  theme(text = element_text(size = 20))

ggsave(filename = "./Figures/CO2.png",
       plot = last_plot(),width = 12,height = 7, unit = "cm",
       dpi = 300)


ggplot(data = dataCO2.n) +
  geom_rect(aes(xmin = 1901,xmax = 1910,ymin = -Inf,ymax = Inf),fill = "lightgrey",alpha = 0.2) +
  geom_line(aes(x = as.numeric(years), y = CO2)) +
  theme_bw() +
  labs(y = "CO2 (ppm)", x = "") +
  scale_y_continuous(limits = c(250,400)) +
  theme(text = element_text(size = 20))

