library(dplyr)
library(zoo)
library(tidyr)
library(ggplot2)

for(paramsampler in c("twoDEzs", "twoMetropolis", "threeDEzs", "threeMetropolis", "fourDEzs", "fourMetropolis", "allDEzs", "allMetropolis")){
  traj <- do.call(rbind, get(paste0("traj_", paramsampler))) %>% 
    t() %>% 
    as.data.frame() %>%
    bind_cols(scotland_rate[, 1:2]) %>% 
    pivot_longer(cols = c(1:40000), names_to = "iter", values_to = "count")
  
  fig <- ggplot() + 
    # geom_line(data = traj, aes(x = yearmon, y = count, group = iter), alpha = 0.3, color = "darkgrey") +
    geom_ribbon
    geom_line(data = scotland_rate, aes(x = yearmon, y = count), alpha = 0.7, color = "black") +
    theme(legend.position = "none") +
    labs(x = "Months", y = "Count") + 
    theme_classic() +
    facet_wrap(~age)
  
  ggsave(filename = paste0("/Users/lsh2301561/Desktop/rsv_disruption/output/figures/trajectories/", paramsampler, ".png"), plot = fig, width = 12, height = 8, dpi = 300)
}

# -------------------------------------------------------------------------
# plots
# plot data vs mean and 95% interval
library(HDInterval)

traj <- do.call(rbind, traj_allDEzs) %>% 
  as.data.frame() %>% 
  hdi() %>% 
  t() %>% 
  bind_cols(scotland_rate[, c(1, 2, 5)]) %>% 
  cbind(mean = colMeans(do.call(rbind, traj_allDEzs)))

ggplot(traj) +
  geom_line(aes(x = yearmon, y = count, color = "Scottish Data")) +
  geom_line(aes(x = yearmon, y = mean, color = "Mean"), linetype = 2) +
  geom_ribbon(aes(x = yearmon, ymax = upper, ymin = lower), alpha = 0.3, linetype = 0) +
  labs(x = "Months", y = "Count") + 
  scale_color_discrete(name = "") +
  theme_classic() +
  facet_wrap(~age)

# test plot to compare model with and without binomial fix
test <- cbind(scotland_rate, 
              model = data_model[, 1],
              fixed = data[, 1])
ggplot(test) +
  geom_line(aes(x = yearmon, y = count, color = "Scottish Data")) +
  geom_line(aes(x = yearmon, y = model, color = "Model (no fix)"), linetype = 1) +
  geom_line(aes(x = yearmon, y = fixed, color = "Model (binomial fix)"), linetype = 1) +
  labs(x = "Months", y = "Count") + 
  scale_color_discrete(name = "") +
  theme_classic() +
  facet_wrap(~age)

# plot mcmc traces and correlation plots
out <- out_allDEzs
plot(out)
correlationPlot(out)

# plot parameter assumptions
theta <- ggplot(data.frame(x = c(0, 25)), aes(x = x)) + 
  stat_function(fun = function(x){1*x}) +
  scale_x_continuous(breaks = seq(0, 25, 5)) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 14)) +
  labs(title = "Maternal Immunity", x = "Immunity Level", y = "Probability of Infection at Birth")
theta_fit <- ggplot(data.frame(x = c(0, 25)), aes(x = x)) + 
  stat_function(fun = function(x){ifelse(theta*x > 1, 1, theta*x)}) +
  scale_x_continuous(breaks = seq(0, 25, 5)) +
  theme_bw() +
  labs(title = "Theta (fit)", x = "Immunity level", y = "Probability of infection")

omega <- ggplot(data.frame(x = c(0, 48)), aes(x = x)) + 
  stat_function(fun = function(x){-1/48*x + 1}) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 14)) +
  scale_x_continuous(breaks = seq(0, 48, 4)) +
  labs(title = "Immunity Waning", x = "Months Since Birth", y = "Proportion of Starting Probability of Immunity")
omega_fit <- ggplot(data.frame(x = c(0, 48)), aes(x = x)) + 
  stat_function(fun = function(x){ifelse(omega*x+1 < 0, 0, omega*x+1)}) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 48, 4)) +
  labs(title = "Omega (fit)", x = "Months since birth", y = "% of immunity")

alpha <- ggplot(data.frame(x = c(0, 48)), aes(x = x)) + 
  stat_function(fun = function(x){-1/48*x+1}) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 14)) +
  scale_x_continuous(breaks = seq(0, 48, 4)) +
  labs(title = "Ageing", x = "Months Since Birth", y = "Probability of Developing Disease")
alpha_fit <- ggplot(data.frame(x = c(0, 48)), aes(x = x)) + 
  stat_function(fun = function(x){ifelse(alpha*x+1 < 0, 0, alpha*x+1)}) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 48, 4)) +
  labs(title = "Alpha (fit)", x = "Months since birth", y = "% of developing disease")

grid.arrange(theta, omega, alpha, theta_fit, omega_fit, alpha_fit, ncol = 3, nrow = 2)
grid.arrange(theta, omega, alpha, ncol = 3, nrow = 1)
