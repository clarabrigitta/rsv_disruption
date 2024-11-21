library(dplyr)
library(zoo)
library(tidyr)
library(ggplot2)
library(HDInterval)

# plot all trajectories

paramsampler <- ...

# for(paramsampler in c("twoDEzs", "twoMetropolis", "threeDEzs", "threeMetropolis", "fourDEzs", "fourMetropolis", "allDEzs", "allMetropolis")){
# for(paramsampler in c("nocaseimport", "fixedcaseimport", "maternalandinflection", "allinflectionanddecay")){
  traj <- do.call(rbind, get(paste0("traj_", paramsampler))) %>% 
    t() %>% 
    as.data.frame() %>%
    bind_cols(scotland_rate[, 1:2]) %>% 
    pivot_longer(cols = c(1:length(get(paste0("traj_", paramsampler)))), names_to = "iter", values_to = "count")
  
  fig <- ggplot() + 
    geom_line(data = traj, aes(x = yearmon, y = count, group = iter), alpha = 0.05, color = "darkgrey") +
    geom_line(data = scotland_rate, aes(x = yearmon, y = count), alpha = 0.7, color = "black") +
    theme(legend.position = "none") +
    labs(x = "Months", y = "Count") + 
    theme_classic() +
    facet_wrap(~age)
  
  ggsave(filename = paste0("/Users/lsh2301561/Desktop/rsv_disruption/output/figures/trajectories/", paramsampler, ".png"), plot = fig, width = 12, height = 8, dpi = 300)
# } # only for use if loop needed

# -------------------------------------------------------------------------

# plot data vs mean and 95% interval
  
# for(paramsampler in c("burnin", "nocaseimport", "fixedcaseimport", "maternalandinflection", "allinflectionanddecay")){
  traj <- do.call(rbind, get(paste0("traj_", paramsampler))) %>% 
    as.data.frame() %>% 
    hdi() %>% 
    t() %>% 
    bind_cols(scotland_rate[, c(1, 2, 5)]) %>% 
    cbind(mean = colMeans(do.call(rbind, get(paste0("traj_", paramsampler)))))
  
  fighdi <- ggplot(traj) +
    geom_line(aes(x = yearmon, y = count, color = "Scottish Data")) +
    geom_line(aes(x = yearmon, y = mean, color = "Mean"), linetype = 2) +
    geom_ribbon(aes(x = yearmon, ymax = upper, ymin = lower), alpha = 0.3, linetype = 0) +
    labs(x = "Months", y = "Count") + 
    scale_color_discrete(name = "") +
    theme_classic() +
    facet_wrap(~age)
  
  ggsave(filename = paste0("/Users/lsh2301561/Desktop/rsv_disruption/output/figures/trajectories/hdi_", paramsampler, ".png"), plot = fighdi, width = 12, height = 8, dpi = 300)
# } # only for use if loop needed


# -------------------------------------------------------------------------

# plot function shapes
maternal <- ggplot(data.frame(x = c(0, 25)), aes(x = x)) + 
    stat_function(fun = function(x){1/(1+exp(-2*(x-25)))}, aes(colour = "k=2, x0=25")) +
    theme_bw() +
    labs(x = "Months since maternal infection", y = "Immunity level")

waning <- ggplot(data.frame(x = c(0, 48)), aes(x = x)) + 
  stat_function(fun = function(x){1/(1+exp(1*(x-45)))}, aes(colour = "k=1, x0=45")) +
  scale_x_continuous(breaks = seq(0, 48, 4)) +
  theme_bw() +
  labs(x = "Months since birth", y = "Waning immunity")
  
aging <- ggplot(data.frame(x = c(0, 48)), aes(x = x)) + 
  stat_function(fun = function(x){1/(1+exp(2*(x-30)))}, aes(colour = "k=2, x0=40")) +
  scale_x_continuous(breaks = seq(0, 48, 4)) +
  theme_bw() +
  labs(x = "Months since birth", y = "Probability of disease")

grid.arrange(maternal, aging, waning, ncol = 3, nrow = 1)