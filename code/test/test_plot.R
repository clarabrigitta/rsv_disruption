data <- readRDS(file = "/Users/lsh2301561/Desktop/rsv_disruption/output/data/prefitting/prefitted_model_functionised_exponential.rds")

# plot rate of exposure
plot_ly() %>%
  add_trace(data = data %>% 
              select(time_calendar, month, rate) %>% 
              distinct(),
            x = ~time_calendar,
            y = ~rate,
            type = "scatter",
            mode = "lines",
            text = ~month,
            hovertemplate = paste('<b>Month</b>: %{text}',
                                  '<br><b>Rate</b>: %{y}',
                                  '<extra></extra>')) %>%
  layout(xaxis = list(title = "Calendar month",
                      range = list(37, 120),
                      tickvals = seq(37, 120, 3),
                      ticktext = rep(c("Jan", "Apr", "Jul", "Oct"), 7),
                      tickmode = "array",
                      tickangle = -45),
         shapes = lockdown)

# plot probability of infection or disease
data %>% 
  select(time_calendar, time_birth, month, prob_dis, level) %>% 
  mutate(level = factor(level, levels = 1:25)) %>% 
  # plot_ly() %>% 
  # add_trace(x = ~time_birth,
  #           split = ~level,
  #           group = ~level,
  #           y = ~prob_dis,
  #           type = "scatter",
  #           mode = "lines")
  ggplot() +
  geom_line(aes(x = time_birth, y = prob_dis, colour = level)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Time since birth",
       y = "Probability")

# plot ratio
data %>% 
  select(-time) %>% 
  mutate(age = case_when(time_birth  < 12 ~ "<1",
                         time_birth >= 12 & time_birth  <= 48 ~ "1-4")) %>%
  group_by(time_calendar, month, year, yearmon, date, rate, age) %>% 
  summarise(infected = sum(infected),
            disease = sum(disease)) %>% 
  ungroup() %>% 
  mutate(ratio = disease/infected) %>% 
  filter(year >= 2015, year <= 2029) %>% 
  plot_ly() %>% 
  add_trace(x = ~time_calendar,
            y = ~ratio,
            split = ~age,
            colour = ~age,
            type = "scatter",
            mode = "line",
            text = ~yearmon
  ) %>%
  layout(xaxis = list(title = "Calendar month",
                      range = list(37, 216),
                      tickvals = seq(37, 216, 6),
                      ticktext = rep(c("Jan", "Jul"), 15),
                      tickmode = "array",
                      tickangle = -45),
         yaxis = list(title = "Ratio",
                      range = list(0, 1),
                      tickmode = "linear",
                      tick0 = 0,
                      dtick = 0.2,
                      tickformat = "digits",
                      side = "left"),
         shapes = lockdown)

# plot rate 
rate_reference %>% 
  plot_ly() %>%
  add_trace(x = ~time,
            y = ~rate,
            type = "scatter",
            mode = "lines",
            text = ~month,
            hovertemplate = paste('<b>Month</b>: %{text}',
                                  '<br><b>Rate</b>: %{y}',
                                  '<extra></extra>')) %>%
  layout(xaxis = list(title = "Calendar month",
                      range = list(37, 120),
                      tickvals = seq(37, 120, 3),
                      ticktext = rep(c("Jan", "Apr", "Jul", "Oct"), 7),
                      tickmode = "array",
                      tickangle = -45),
         shapes = lockdown)

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
