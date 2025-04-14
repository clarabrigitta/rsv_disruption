library(dplyr)
library(zoo)
library(tidyr)
library(ggplot2)
library(HDInterval)

plot_hdi <- function(traj, traj_infection){
  
  traj <- do.call(rbind, traj) %>% 
    as.data.frame() %>% 
    hdi() %>% 
    t() %>% 
    bind_cols(scotland_rate[, c(1, 2, 3)]) %>% 
    cbind(median = colMedians(do.call(rbind, traj))) %>% 
    bind_cols(do.call(rbind, traj_infection) %>% 
                as.data.frame() %>% 
                hdi() %>% 
                t() %>% 
                cbind(median_inf = colMedians(do.call(rbind, traj_infection))) %>%
                as.data.frame() %>% 
                rename(lower_inf = lower, upper_inf = upper))
  
  fig <- ggplot(traj) +
    geom_line(aes(x = yearmon, y = median_inf, color = "Infection"), linetype = 1) +
    geom_ribbon(aes(x = yearmon, ymax = upper_inf, ymin = lower_inf, fill = "Infection"), alpha = 0.3, linetype = 0) +
    geom_line(aes(x = yearmon, y = median, color = "Disease"), linetype = 1) +
    geom_ribbon(aes(x = yearmon, ymax = upper, ymin = lower, fill = "Disease"), alpha = 0.3, linetype = 0) +
    geom_point(aes(x = yearmon, y = count, shape = "Scottish Data"), size = 1, colour = "black") +
    scale_color_manual(name = "", values = c("Infection" = "#2A7B43", "Disease" = "#C93842")) +
    scale_fill_manual(name = "", values = c("Infection" = "#2A7B43", "Disease" = "#C93842")) +
    scale_shape_manual(name = "", values = c("Scottish Data" = 16)) +
    labs(x = "Months", y = "Count") + 
    theme_classic() +
    facet_wrap(~age)
  
  dir.create(here("output", "figures", "hdi", format(Sys.Date(), "%d%m%Y")))
  ggsave(filename = here("output", "figures", "hdi", format(Sys.Date(), "%d%m%Y"), paste0(n, ".png")), plot = fig, width = 9, height = 4, dpi = 300)
  
}
