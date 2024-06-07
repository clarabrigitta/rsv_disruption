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

# plot ratio of disease/infection
data %>% 
  select(time_calendar, time_birth, month, prob_inf, prob_dis, level) %>% 
  mutate(ratio = prob_dis/prob_inf,
         level = factor(level, levels = 1:25)) %>% 
  # plot_ly() %>% 
  # add_trace(x = ~time_birth,
  #           y = ~ratio,
  #           split = ~level,
  #           type = "scatter",
  #           mode = "lines")
ggplot() +
  geom_line(aes(x = time_birth, y = ratio, colour = level)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Time since birth",
       y = "Ratio")
