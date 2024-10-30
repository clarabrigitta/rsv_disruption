traj <- do.call(rbind, traj_allMetropolis) %>% 
  t()
cbind(traj, scotland_rate[, 1:2])
as.data.frame() %>% 
  pivot_longer(cols = 1:4, names_to = "age", values_to = "count")

ggplot(traj) +
  geom_line(aes(x = yearmon, y = count, color = "Scottish Data")) +
  # geom_line(aes(x = yearmon, y = universal, color = "Universal Scaling Factor"), linetype = 2) +
  # geom_line(aes(x = yearmon, y = guess, color = "Guess"), linetype = 2) +
  geom_line(aes(x = yearmon, y = model, color = "Model"), linetype = 2) +
  geom_ribbon(aes(x = yearmon, ymax = upper, ymin = lower), alpha = 0.3, linetype = 0) +
  labs(x = "Months", y = "Rate (per 100,000)") + 
  scale_color_discrete(name = "") +
  theme_classic() +
  facet_wrap(~age)

# -------------------------------------------------------------------------
# plots
# plot scotland vs model rate with 95% credible interval parameter values
both <- cbind(scotland_rate, 
              guess = model_function(lambda = 0, theta = 1/25, omega = -1/12, alpha = -1/48, stored_data = save_data)[, 1],
              model = model_function(lambda = exp(-5.124), theta = 0.051, omega = -0.817, alpha = -0.978, stored_data = save_data)[, 1] * 0.2, # omega and alpha should be negative
              lower = model_function(lambda = exp(-9.767), theta = 0.010, omega = -0.973, alpha = -0.999, stored_data = save_data)[, 1] * 0.197,
              upper = model_function(lambda = exp(-1.390), theta = 0.991, omega = -0.035, alpha = -0.855, stored_data = save_data)[, 1] * 0.204)

scot_detect <- cbind(scotland_rate,
                     detection = model_function(lambda = 0, theta = 1/25, omega = -1/12, alpha = -1/48, stored_data = save_data, detect_rate = 0.1)[, 1])

ggplot(both) +
  geom_line(aes(x = yearmon, y = count, color = "Scottish Data")) +
  # geom_line(aes(x = yearmon, y = universal, color = "Universal Scaling Factor"), linetype = 2) +
  # geom_line(aes(x = yearmon, y = guess, color = "Guess"), linetype = 2) +
  geom_line(aes(x = yearmon, y = model, color = "Model"), linetype = 2) +
  geom_ribbon(aes(x = yearmon, ymax = upper, ymin = lower), alpha = 0.3, linetype = 0) +
  labs(x = "Months", y = "Rate (per 100,000)") + 
  scale_color_discrete(name = "") +
  theme_classic() +
  facet_wrap(~age)

ggplot() +
  geom_line(aes(x = c(1:264), y = save_data[[3]], color = "Original")) +
  geom_line(aes(x = c(1:264), y = save_data[[3]] * 0.1,, color = "Scaled")) +
  theme_classic()

# plot mcmc traces
plot(out_uni)
plot(out1)
plot(out2)
plot(out3)
plot(out4)

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
  stat_function(fun = function(x){0.041*x}) +
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
  stat_function(fun = function(x){0.037*x+1}) +
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
  stat_function(fun = function(x){0.037*x+1}) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 48, 4)) +
  labs(title = "Alpha (fit)", x = "Months since birth", y = "% of developing disease")

grid.arrange(theta, omega, alpha, theta_fit, omega_fit, alpha_fit, ncol = 3, nrow = 2)
grid.arrange(theta, omega, alpha, ncol = 3, nrow = 1)
