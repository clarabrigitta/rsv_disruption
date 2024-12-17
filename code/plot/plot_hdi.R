library(dplyr)
library(zoo)
library(tidyr)
library(ggplot2)
library(HDInterval)

plot_hdi <- function(traj){
  
  traj <- do.call(rbind, traj) %>% 
    as.data.frame() %>% 
    hdi() %>% 
    t() %>% 
    bind_cols(scotland_rate[, c(1, 2, 5)]) %>% 
    cbind(mean = colMeans(do.call(rbind, traj)))
  
  fig <- ggplot(traj) +
    geom_line(aes(x = yearmon, y = count, color = "Scottish Data")) +
    geom_line(aes(x = yearmon, y = mean, color = "Mean"), linetype = 2) +
    geom_ribbon(aes(x = yearmon, ymax = upper, ymin = lower), alpha = 0.3, linetype = 0) +
    labs(x = "Months", y = "Count") + 
    scale_color_discrete(name = "") +
    theme_classic() +
    facet_wrap(~age)
  
  ggsave(filename = here("output", "figures", "hdi", "test", paste0(n, ".png")), plot = fig, width = 12, height = 8, dpi = 300)
  
}
