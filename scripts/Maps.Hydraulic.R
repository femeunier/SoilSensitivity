rm(list = ls())

library(ncdf4)
library(ggplot2)
library(dplyr)
library(SoilSensitivity)
library(reshape2)

df.all <- data.frame()
names = c("0","5","15","30","60","100","200")

for (i in seq(1,7)){
  file <- paste0("/home/femeunier/Downloads/SoilGrids/Hydraul_Param_SoilGrids_Schaap_sl",i,".nc")

  nc <- nc_open(file)

  tmp.thetas <- ncvar_get(nc,paste0("mean_theta_s_",names[i],"cm"))
  tmp.n <- ncvar_get(nc,paste0("n_fit_",names[i],"cm"))
  tmp.thetar <- ncvar_get(nc,paste0("mean_theta_r_",names[i],"cm"))
  tmp.alpha <- ncvar_get(nc,paste0("alpha_fit_",names[i],"cm"))
  tmp.L <- ncvar_get(nc,paste0("mean_L_",names[i],"cm"))
  tmp.Ks <- ncvar_get(nc,paste0("mean_Ks_",names[i],"cm"))
  lats <- ncvar_get(nc,"latitude")
  lons <- ncvar_get(nc,"longitude")

  nc_close(nc)

  df.tmp <- melt(tmp.thetas) %>% mutate(lat = lats[Var2],
                                        lon = lons[Var1]) %>% rename(thetas = value) %>%
    mutate(n = melt(tmp.n) %>% pull(value),
           thetar = melt(tmp.thetar) %>% dplyr::pull(value),
           alpha = melt(tmp.alpha) %>% dplyr::pull(value),
           L = melt(tmp.L) %>% dplyr::pull(value),
           Ks = melt(tmp.Ks) %>% dplyr::pull(value)) %>% dplyr::select(-c(Var1,Var2))

  df.all <- bind_rows(list(df.all,
                           df.tmp %>% mutate(depth = as.numeric(names[i]))))
}

df.all.hydro <- df.all %>% mutate(theta_fc = theta_h(h = -300,theta_s = thetas,theta_r = thetar,
                                                     alpha = alpha,n = n),
                                  theta_wp = theta_h(h = -1500,theta_s = thetas,theta_r = thetar,
                                                     alpha = alpha,n = n))

saveRDS(object = df.all.hydro,
        file = "./outputs/maps.hydro.RDS")

ggplot(data = df.all.hydro) +
  geom_density(aes(x = theta_fc, fill = as.factor(depth),color = as.factor(depth)),alpha = 0.2) +
  theme_bw()

hist(log10(df.all$Ks/10/86400))


ggplot(data = df.all.hydro %>% filter(depth == 5)) +
  geom_density(aes(x = theta_wp),alpha = 0.2) +
  theme_bw()
