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
  
  fig1 <- ggplot(data) +
    # geom_line(aes(x = yearmon, y = mean_inf, color = "Infection"), linetype = 1) +
    # geom_ribbon(aes(x = yearmon, ymax = upper_inf, ymin = lower_inf, fill = "Infection"), alpha = 0.3, linetype = 0) +
    geom_line(aes(x = yearmon, y = mean, color = "Recorded Disease"), linetype = 1) +
    geom_ribbon(aes(x = yearmon, ymax = upper, ymin = lower, fill = "Recorded Disease"), alpha = 0.3, linetype = 0) +
    geom_point(aes(x = yearmon, y = count, shape = "Data"), size = 2, colour = "black") +
    scale_color_manual(name = "", values = c("Infection" = "#1E88E5", "Recorded Disease" = "#D81B60")) +
    scale_fill_manual(name = "", values = c("Infection" = "#1E88E5", "Recorded Disease" = "#D81B60")) +
    scale_shape_manual(name = "", values = c("Data" = 16)) +
    # scale_y_continuous(transform = "log10") +
    labs(x = "Time (months)", y = "Number of RSV cases") + 
    theme_classic() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          strip.text = element_text(size = 14, face = "bold"),
          legend.text = element_text(size = 12),
          legend.position = "bottom") + 
    facet_wrap(~age)
  
  data <- do.call(rbind, traj) %>% 
    t() %>% 
    as.data.frame() %>% 
    bind_cols(scotland_rate[, c(1, 2, 3)]) %>% 
    left_join(dates) %>% 
    group_by(season, age) %>% 
    summarise(across(c(1:2000, "count"), sum)) %>% 
    rowwise() %>% 
    mutate(lower = hdi(c_across(3:2002))[1],
           upper = hdi(c_across(3:2002))[2],
           mean = mean(c_across(3:2002))) %>% 
    ungroup() %>% 
    select(-(3:2002)) %>% 
    filter(!is.na(season)) %>% 
    pivot_longer(cols = c(count:mean), names_to = "metric", values_to = "value")
  
  fig2 <- ggplot() +
    geom_bar(data = data %>% filter(metric %in% c("count", "mean")), aes(x = season, y = value, fill = metric), stat = "identity", position = "dodge", alpha = 0.9) +
    geom_errorbar(data = data %>% pivot_wider(names_from = metric), aes(x = as.numeric(season) + 0.9 / 4, ymin = lower, ymax = upper), width = 0.3, size = 1) +
    scale_fill_manual(values = c("black", "#D81B60"), labels = c("count" = "Data", "mean" = "Recorded Disease")) +
    labs(x = "Season", y = "Number of RSV cases", fill = "") + 
    theme_classic() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text = element_text(size = 14, face = "bold"),
          legend.text = element_text(size = 12),
          legend.position = "bottom") +
    facet_grid(~age)
  
  fig <- fig1 / fig2 + plot_annotation(tag_levels = "A") + theme(plot.tag = element_text(size = 14))
  
  dir.create(here("output", "figures", "hdi", format(Sys.Date(), "%d%m%Y")))
  ggsave(filename = here("output", "figures", "hdi", format(Sys.Date(), "%d%m%Y"), paste0(n, ".png")), plot = fig, width = 9, height = 9, dpi = 300)
  
}

# test code for annual/seasonal disease counts
traj17 <- readRDS("~/Desktop/rsv_disruption/output/data/trajectories/17112025/traj17.rds")
traj <- traj17
