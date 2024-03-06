## plot outputs of births model
## load libraries
library(dplyr)
library(ggplot2)
library(plotly)
library(viridisLite)
library(stringr)

# load data
births <- readRDS("./output/data/births.rds")

# plot rate of exposure
births %>% 
  filter(!is.na(rate)) %>% 
  plot_ly() %>% 
  add_trace(x = ~time,
            y = ~rate,
            type = "scatter",
            mode = "lines",
            text = ~month,
            hovertemplate = paste('<b>Month</b>: %{text}',
                                  '<br><b>Rate</b>: %{y}',
                                  '<extra></extra>')) %>% 
  layout(xaxis = list(title = "Time",
                      tickmode = "array",
                      ticktext = list("January", "July", "January", "July", "January", "July", "January"),
                      tickvals = list(1, 7, 13, 19, 25, 31, 37)),
         yaxis = list(title = "Rate of Exposure"))

# plot probability of infection
births %>% 
  filter(!is.na(rate)) %>% 
  plot_ly() %>% 
  add_trace(x = ~time,
            y = ~prob_inf,
            split = ~level,
            type = "scatter",
            mode = "lines",
            text = ~month,
            hovertemplate = paste('<b>Month</b>: %{text}',
                                  '<br><b>Probability</b>: %{y}',
                                  '<extra></extra>')) %>% 
  layout(xaxis = list(title = "Time",
                      tickmode = "array",
                      ticktext = list("January", "July", "January", "July", "January", "July", "January"),
                      tickvals = list(1, 7, 13, 19, 25, 31, 37)),
         yaxis = list(title = "Probability of Infection"),
         legend = list(title = list(text = "Immunity Level")))

# plot probability of disease
births %>% 
  filter(!is.na(rate)) %>% 
  plot_ly() %>% 
  add_trace(x = ~time,
            y = ~prob_dis,
            split = ~level,
            type = "scatter",
            mode = "lines",
            text = ~month,
            hovertemplate = paste('<b>Month</b>: %{text}',
                                  '<br><b>Probability</b>: %{y}',
                                  '<extra></extra>')) %>% 
  layout(xaxis = list(title = "Time",
                      tickmode = "array",
                      ticktext = list("January", "July", "January", "July", "January", "July", "January"),
                      tickvals = list(1, 7, 13, 19, 25, 31, 37)),
         yaxis = list(title = "Probability of Disease"),
         legend = list(title = list(text = "Immunity Level")))

# plot monthly infections
births %>% 
  plot_ly() %>% 
  add_trace(x = ~time,
            y = ~infected,
            split = ~level,
            colour = ~level,
            type = "scatter",
            mode = "lines",
            text = ~month,
            hovertemplate = paste('<b>Month</b>: %{text}',
                                  '<br><b>Count</b>: %{y}',
                                  '<extra></extra>')) %>% 
  layout(xaxis = list(title = "Time",
                      tickmode = "array",
                      ticktext = list("January", "July", "January", "July", "January", "July", "January"),
                      tickvals = list(1, 7, 13, 19, 25, 31, 37)),
         yaxis = list(title = "Count",
                      range = list(0, 7000)),
         legend = list(title = list(text = "Immunity Level")),
         title = "January Babies")

# plot monthly count of disease
births %>% 
  plot_ly() %>% 
  add_trace(x = ~time,
            y = ~disease,
            split = ~level,
            colour = ~level,
            type = "scatter",
            mode = "lines",
            text = ~month,
            hovertemplate = paste('<b>Month</b>: %{text}',
                                  '<br><b>Count</b>: %{y}',
                                  '<extra></extra>')) %>% 
  layout(xaxis = list(title = "Time",
                      tickmode = "array",
                      ticktext = list("January", "July", "January", "July", "January", "July", "January"),
                      tickvals = list(1, 7, 13, 19, 25, 31, 37)),
         yaxis = list(title = "Count",
                      range = list(0, 7000)),
         legend = list(title = list(text = "Immunity Level")),
         title = "January Babies")

# plot of cumulative proportion of babies infected
births %>% 
  filter(level == "total") %>% 
  mutate(cum_sum = cumsum(infected),
         prop = (cum_sum/61942)*100) %>% 
  ggplot() +
  geom_line(aes(x = time, y = prop)) +
  scale_x_continuous(breaks = seq(1, nrow(births), 6), labels = c(rep(c("January", "July"), rep), "January")) +
  theme_bw() +
  labs(x = "",
       y = "Proportion Infected (%)")

births %>% 
  filter(level == "total") %>% 
  mutate(cum_sum = cumsum(infected),
         prop = (cum_sum/61942)*100) %>% 
  plot_ly() %>% 
  add_trace(x = ~time,
            y = ~prop,
            type = "scatter",
            mode = "lines",
            text = ~month,
            hovertemplate = paste('<b>Month</b>: %{text}',
                                  '<br><b>Proportion Infected</b>: %{y}',
                                  '<extra></extra>')) %>% 
  layout(xaxis = list(title = "Time",
                      tickmode = "array",
                      ticktext = list("January", "July", "January", "July", "January", "July", "January"),
                      tickvals = list(1, 7, 13, 19, 25, 31, 37)),
         yaxis = list(title = "Proportion Infected (%)",
                      range = list(0, 100)))
