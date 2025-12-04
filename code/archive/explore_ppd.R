# lambda prior predictive distribution

n_sim <- 100

lambda_prior <- runif(n_sim, -10, 0)

hist(lambda_prior)

lambda_prior_sim <- list()

for(i in 1:n_sim){
  lambda_prior_sim[[i]] <- model_function(lambda = exp(lambda_prior[i]), theta = 1/25, omega = -1/48, alpha = -1/48, stored_data = save_data)
}

# plot for <1 year
p <- ggplot()

for(i in 1:n_sim){
  p <- p + geom_line(data = lambda_prior_sim[[i]][93:184, ], aes(x = time, y = `1`), colour = "blue", alpha = 0.3, size = 0.1)
}

lambda_1 <- p + geom_line(data = scotland_rate %>% mutate(time = rep(c(58:149), 2)) %>% filter(age == "<1 years"), 
                         aes(x = time, y = count), colour = "black", linetype = 1, size = 0.5) +
  theme_classic() +
  labs(title = "A. <1 year", x = "Time", y = "Count")

# plot for 1-4 year
p <- ggplot()

for(i in 1:n_sim){
  p <- p + geom_line(data = lambda_prior_sim[[i]][1:92, ], aes(x = time, y = `1`), colour = "red", alpha = 0.3, size = 0.1)
}

lambda_2 <- p + geom_line(data = scotland_rate %>% mutate(time = rep(c(58:149), 2)) %>% filter(age == "1-4 years"), 
                         aes(x = time, y = count), colour = "black", linetype = 1, size = 0.5) +
  theme_classic() +
  labs(title = "1-4 years", x = "Time", y = "Count")

lambda_fig <- grid.arrange(lambda_1, lambda_2, ncol = 2, nrow = 1)
# -------------------------------------------------------------------------

# theta prior predictive distribution

n_sim <- 100

theta_prior <- runif(n_sim, 0, 1)

hist(theta_prior)

theta_prior_sim <- list()

for(i in 1:n_sim){
  theta_prior_sim[[i]] <- model_function(lambda = 0, theta = theta_prior[i], omega = -1/48, alpha = -1/48, stored_data = save_data)
}

# plot for <1 year
p <- ggplot()

for(i in 1:n_sim){
  p <- p + geom_line(data = theta_prior_sim[[i]][93:184, ], aes(x = time, y = `1`), colour = "blue", alpha = 0.3, size = 0.1)
}

theta_1 <- p + geom_line(data = scotland_rate %>% mutate(time = rep(c(58:149), 2)) %>% filter(age == "<1 years"), 
                     aes(x = time, y = count), colour = "black", linetype = 1, size = 0.5) +
  theme_classic() +
  labs(title = "B. <1 year", x = "Time", y = "Count")

# plot for 1-4 year
p <- ggplot()

for(i in 1:n_sim){
  p <- p + geom_line(data = theta_prior_sim[[i]][1:92, ], aes(x = time, y = `1`), colour = "red", alpha = 0.3, size = 0.1)
}

theta_2 <- p + geom_line(data = scotland_rate %>% mutate(time = rep(c(58:149), 2)) %>% filter(age == "1-4 years"), 
                     aes(x = time, y = count), colour = "black", linetype = 1, size = 0.5) +
  theme_classic() +
  labs(title = "1-4 years", x = "Time", y = "Count")

theta_fig <- grid.arrange(theta_1, theta_2, ncol = 2, nrow = 1)

# -------------------------------------------------------------------------

# omega prior predictive distribution

n_sim <- 100

omega_prior <- runif(n_sim, 0, 1)

omega_prior_sim <- list()

for(i in 1:n_sim){
  omega_prior_sim[[i]] <- model_function(lambda = 0, theta = 1/25, omega = omega_prior[i], alpha = -1/48, stored_data = save_data)
}

hist(omega_prior)

# plot for <1 year
p <- ggplot()

for(i in 1:n_sim){
  p <- p + geom_line(data = omega_prior_sim[[i]][93:184, ], aes(x = time, y = `1`), colour = "blue", alpha = 0.3, size = 0.1)
}

omega_1 <- p + geom_line(data = scotland_rate %>% mutate(time = rep(c(58:149), 2)) %>% filter(age == "<1 years"), 
                   aes(x = time, y = count), colour = "black", linetype = 1, size = 0.5) +
  theme_classic() +
  labs(title = "C. <1 year", x = "Time", y = "Count")

# plot for 1-4 year
p <- ggplot()

for(i in 1:n_sim){
  p <- p + geom_line(data = omega_prior_sim[[i]][1:92, ], aes(x = time, y = `1`), colour = "red", alpha = 0.3, size = 0.1)
}

omega_2 <- p + geom_line(data = scotland_rate %>% mutate(time = rep(c(58:149), 2)) %>% filter(age == "1-4 years"), 
                   aes(x = time, y = count), colour = "black", linetype = 1, size = 0.5) +
  theme_classic() +
  labs(title = "1-4 years", x = "Time", y = "Count")

omega_fig <- grid.arrange(omega_1, omega_2, ncol = 2, nrow = 1)

# -------------------------------------------------------------------------

# alpha prior predictive distribution

n_sim <- 100

alpha_prior <- runif(n_sim, 0, 1)

alpha_prior_sim <- list()

for(i in 1:n_sim){
  alpha_prior_sim[[i]] <- model_function(lambda = 0, theta = 1/25, omega = -1/48, alpha = alpha_prior[i], stored_data = save_data)
}

hist(alpha_prior)

# plot for <1 year
p <- ggplot()

for(i in 1:n_sim){
  p <- p + geom_line(data = alpha_prior_sim[[i]][93:184, ], aes(x = time, y = `1`), colour = "blue", alpha = 0.5, size = 0.1)
}

alpha_1 <- p + geom_line(data = scotland_rate %>% mutate(time = rep(c(58:149), 2)) %>% filter(age == "<1 years"), 
                     aes(x = time, y = count), colour = "black", linetype = 1, size = 0.5) +
  theme_classic() +
  labs(title = "D. <1 year", x = "Time", y = "Count")

# plot for 1-4 year
p <- ggplot()

for(i in 1:n_sim){
  p <- p + geom_line(data = alpha_prior_sim[[i]][1:92, ], aes(x = time, y = `1`), colour = "red", alpha = 0.5, size = 0.1)
}

alpha_2 <- p + geom_line(data = scotland_rate %>% mutate(time = rep(c(58:149), 2)) %>% filter(age == "1-4 years"), 
                     aes(x = time, y = count), colour = "black", linetype = 1, size = 0.5) +
  theme_classic() +
  labs(title = "1-4 years", x = "Time", y = "Count")

grid.arrange(alpha_1, alpha_2, ncol = 2, nrow = 1)

# -------------------------------------------------------------------------

prob_prior <- runif(n_sim, 0, 0.5)
delta_prior <- runif(n_sim, 0, 0.05)
theta_prior <- runif(n_sim, 0.5, 5)

prior_sim <- list()

for(i in 1:n_sim){
  prior_sim[[i]] <- model_function(lambda = exp(lambda_prior[i]), theta = theta_prior[i], omega = omega_prior[i], alpha = alpha_prior[i], stored_data = save_data, delta = delta_prior[i])
  prior_sim[[i]][, 1] <- prior_sim[[i]][, 1] * prob_prior[i]
}

# plot for <1 year
p <- ggplot()

for(i in 1:n_sim){
  p <- p + geom_line(data = prior_sim[[i]][93:184, ], aes(x = time, y = `1`), colour = "blue", alpha = 0.5, size = 0.1)
}

all_1 <- p + geom_line(data = scotland_rate %>% mutate(time = rep(c(58:149), 2)) %>% filter(age == "<1 years"), 
                         aes(x = time, y = count), colour = "black", linetype = 1, size = 0.5) +
  theme_classic() +
  labs(title = "D. <1 year", x = "Time", y = "Count")

# plot for 1-4 year
p <- ggplot()

for(i in 1:n_sim){
  p <- p + geom_line(data = prior_sim[[i]][1:92, ], aes(x = time, y = `1`), colour = "red", alpha = 0.5, size = 0.1)
}

all_2 <- p + geom_line(data = scotland_rate %>% mutate(time = rep(c(58:149), 2)) %>% filter(age == "1-4 years"), 
                         aes(x = time, y = count), colour = "black", linetype = 1, size = 0.5) +
  theme_classic() +
  labs(title = "1-4 years", x = "Time", y = "Count")

grid.arrange(all_1, all_2, ncol = 2, nrow = 1)

# combined plot
grid.arrange(lambda_1, lambda_2,
             theta_1, theta_2,
             omega_1, omega_2, 
             alpha_1, alpha_2,
             ncol = 2,
             nrow = 4)

# -------------------------------------------------------------------------

for(upper in c(3:6)){
u <- upper
  
n_sim <- 1000

prior <- runif(n_sim, 0, u)

prior_df <- data.frame(prior = prior)

assign(paste0("histogram", u), ggplot(prior_df) +
  geom_histogram(aes(prior), fill = "#69b3a2", alpha = 0.7) +
  theme_bw() +
  labs(x = " ", y = "Frequency", title = paste0(LETTERS[u-2], ".", " upper limit = ", u)) +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14)))

x_vals <- seq(0, 48, length.out = 500)

data <- matrix(nrow = 500, ncol = length(prior))

for(r in 1:length(prior)){
  data[, r] <- 1 - (1 / (1 + exp(-as.numeric(prior[r]) * (x_vals - 3.5))))
}

assign(paste0("ppd", u), data %>% 
         as.data.frame() %>% 
         mutate(months = x_vals) %>% 
         pivot_longer(cols = 1:1000, names_to = "iter", values_to = "value") %>% 
         ggplot() +
         geom_line(aes(x = months, y = value, group = iter), colour = "#69b3a2", size = 0.1, alpha = 0.2) +
         labs(x = "Age (months)", y = "Waning immunity", title = " ") +
         theme_bw() +
         theme(axis.text=element_text(size=12),
               axis.title=element_text(size=14)) +
         xlim(0, 48))
}

waning_ppd <- grid.arrange(histogram3, ppd3, histogram4, ppd4, histogram5, ppd5, histogram6, ppd6, ncol = 2, nrow = 4)

ggsave(filename = here("output", "figures", paste0("waning_ppd", ".png")), plot = waning_ppd, width = 8, height = 8, dpi = 300)
