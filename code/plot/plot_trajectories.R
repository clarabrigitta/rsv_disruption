library(dplyr)
library(zoo)
library(tidyr)
library(ggplot2)

plot_trajectories <- function(traj){
  
  traj <- do.call(rbind, traj) %>% 
    t() %>% 
    as.data.frame() %>%
    bind_cols(scotland_rate[, 1:2]) %>% 
    pivot_longer(cols = c(1:length(traj)), names_to = "iter", values_to = "count")
  
  fig <- ggplot() + 
    geom_line(data = traj, aes(x = yearmon, y = count, group = iter), alpha = 0.05, color = "darkgrey") +
    geom_line(data = scotland_rate, aes(x = yearmon, y = count), alpha = 0.7, color = "black") +
    theme(legend.position = "none") +
    labs(x = "Months", y = "Count") + 
    theme_classic() +
    facet_wrap(~age)
  
  dir.create(here("output", "figures", "trajectories", format(Sys.Date(), "%d%m%Y")))
  ggsave(filename = here("output", "figures", "trajectories", format(Sys.Date(), "%d%m%Y"), paste0(n, ".png")), plot = fig, width = 12, height = 8, dpi = 300)
  
}