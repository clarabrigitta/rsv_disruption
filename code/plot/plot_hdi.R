library(dplyr)
library(zoo)
library(tidyr)
library(ggplot2)
library(HDInterval)

plot_hdi <- function(traj, traj_infection){
  
  data <- do.call(rbind, traj) %>% 
    as.data.frame() %>% 
    hdi() %>% 
    t() %>% 
    bind_cols(scotland_rate[, c(1, 2, 3)]) %>% 
    cbind(mean = colMeans(do.call(rbind, traj))) %>% 
    bind_cols(do.call(rbind, traj_infection) %>% 
                as.data.frame() %>% 
                hdi() %>% 
                t() %>% 
                cbind(mean_inf = colMeans(do.call(rbind, traj_infection))) %>%
                as.data.frame() %>% 
                rename(lower_inf = lower, upper_inf = upper))
  
  fig <- ggplot(data) +
    geom_line(aes(x = yearmon, y = mean_inf, color = "Infection"), linetype = 1) +
    geom_ribbon(aes(x = yearmon, ymax = upper_inf, ymin = lower_inf, fill = "Infection"), alpha = 0.3, linetype = 0) +
    geom_line(aes(x = yearmon, y = mean, color = "Disease"), linetype = 1) +
    geom_ribbon(aes(x = yearmon, ymax = upper, ymin = lower, fill = "Disease"), alpha = 0.3, linetype = 0) +
    geom_point(aes(x = yearmon, y = count, shape = "Data"), size = 2, colour = "black") +
    scale_color_manual(name = "", values = c("Infection" = "#1E88E5", "Disease" = "#D81B60")) +
    scale_fill_manual(name = "", values = c("Infection" = "#1E88E5", "Disease" = "#D81B60")) +
    scale_shape_manual(name = "", values = c("Data" = 16)) +
    labs(x = "Time (months)", y = "Number of RSV cases") + 
    theme_classic() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          strip.text = element_text(size = 14, face = "bold"),
          legend.text = element_text(size = 12),
          legend.position = "bottom") + 
    facet_wrap(~age)
  
  dir.create(here("output", "figures", "hdi", format(Sys.Date(), "%d%m%Y")))
  ggsave(filename = here("output", "figures", "hdi", format(Sys.Date(), "%d%m%Y"), paste0(n, ".png")), plot = fig, width = 9, height = 5, dpi = 300)
  
}
