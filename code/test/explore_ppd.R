# -------------------------------------------------------------------------

# theta prior predictive distribution

n_sim <- 100

theta_prior <- runif(n_sim, 0, 1)

hist(theta_prior)

theta_prior_sim <- list()

for(i in 1:n_sim){
  theta_prior_sim[[i]] <- model_function(lambda = 0, theta = theta_prior[i], omega = -1/48, alpha = -1/48, stored_data = save_data, uni = 1)
}

# plot for <1 year
p <- ggplot()

for(i in 1:n_sim){
  p <- p + geom_line(data = theta_prior_sim[[i]][93:184, ], aes(x = time, y = rate_1), colour = "blue", alpha = 0.3, size = 0.1)
}

p_1 <- p + geom_line(data = scotland_rate %>% mutate(time = rep(c(58:149), 2)) %>% filter(age == "<1 years"), 
                     aes(x = time, y = rate), colour = "black", linetype = 1, size = 0.5) +
  theme_classic() +
  labs(title = "<1 year", x = "Time", y = "Rate")

# plot for 1-4 year
p <- ggplot()

for(i in 1:n_sim){
  p <- p + geom_line(data = theta_prior_sim[[i]][1:92, ], aes(x = time, y = rate_1), colour = "red", alpha = 0.3, size = 0.1)
}

p_2 <- p + geom_line(data = scotland_rate %>% mutate(time = rep(c(58:149), 2)) %>% filter(age == "1-4 years"), 
                     aes(x = time, y = rate), colour = "black", linetype = 1, size = 0.5) +
  theme_classic() +
  labs(title = "1-4 years", x = "Time", y = "Rate")

grid.arrange(p_1, p_2, ncol = 2, nrow = 1)

# -------------------------------------------------------------------------

# omega prior predictive distribution

n_sim <- 100

omega_prior <- runif(n_sim, -1, 1)

omega_prior_sim <- list()

for(i in 1:n_sim){
  omega_prior_sim[[i]] <- model_function(lambda = 0, theta = 1/25, omega = omega_prior[i], alpha = -1/48, stored_data = save_data, uni = 0.1)
}

hist(omega_prior)

# plot for <1 year
p <- ggplot()

for(i in 1:n_sim){
  p <- p + geom_line(data = omega_prior_sim[[i]][93:184, ], aes(x = time, y = rate_1), colour = "blue", alpha = 0.3, size = 0.1)
}

p_1 <- p + geom_line(data = scotland_rate %>% mutate(time = rep(c(58:149), 2)) %>% filter(age == "<1 years"), 
                   aes(x = time, y = rate), colour = "black", linetype = 1, size = 0.5) +
  theme_classic() +
  labs(title = "<1 year", x = "Time", y = "Rate")

# plot for 1-4 year
p <- ggplot()

for(i in 1:n_sim){
  p <- p + geom_line(data = omega_prior_sim[[i]][1:92, ], aes(x = time, y = rate_1), colour = "red", alpha = 0.3, size = 0.1)
}

p_2 <- p + geom_line(data = scotland_rate %>% mutate(time = rep(c(58:149), 2)) %>% filter(age == "1-4 years"), 
                   aes(x = time, y = rate), colour = "black", linetype = 1, size = 0.5) +
  theme_classic() +
  labs(title = "1-4 years", x = "Time", y = "Rate")

grid.arrange(p_1, p_2, ncol = 2, nrow = 1)

# -------------------------------------------------------------------------

# alpha prior predictive distribution

n_sim <- 100

alpha_prior <- runif(n_sim, -1, 0)

alpha_prior_sim <- list()

for(i in 1:n_sim){
  alpha_prior_sim[[i]] <- model_function(lambda = 0, theta = 1/25, omega = -1/48, alpha = alpha_prior[i], stored_data = save_data, uni = 1)
}

hist(alpha_prior)

# plot for <1 year
p <- ggplot()

for(i in 1:n_sim){
  p <- p + geom_line(data = alpha_prior_sim[[i]][93:184, ], aes(x = time, y = rate_1), colour = "blue", alpha = 0.5, size = 0.1)
}

p_1 <- p + geom_line(data = scotland_rate %>% mutate(time = rep(c(58:149), 2)) %>% filter(age == "<1 years"), 
                     aes(x = time, y = rate), colour = "black", linetype = 1, size = 0.5) +
  theme_classic() +
  labs(title = "<1 year", x = "Time", y = "Rate")

# plot for 1-4 year
p <- ggplot()

for(i in 1:n_sim){
  p <- p + geom_line(data = alpha_prior_sim[[i]][1:92, ], aes(x = time, y = rate_1), colour = "red", alpha = 0.5, size = 0.1)
}

p_2 <- p + geom_line(data = scotland_rate %>% mutate(time = rep(c(58:149), 2)) %>% filter(age == "1-4 years"), 
                     aes(x = time, y = rate), colour = "black", linetype = 1, size = 0.5) +
  theme_classic() +
  labs(title = "1-4 years", x = "Time", y = "Rate")

grid.arrange(p_1, p_2, ncol = 2, nrow = 1)
