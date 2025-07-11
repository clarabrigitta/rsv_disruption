## plot outputs of women model
## load libraries
library(dplyr)
library(ggplot2)
library(plotly)
library(viridisLite)
library(stringr)

# load data
women <- readRDS("./output/data/women/women.rds")
women.long <- readRDS("./output/data/women/women_long.rds")
women_long_disrupt <- readRDS("./output/data/women/women_long_disrupt.rds")

# parameters used in model
n_interest <- 24 # number of months of interest for history of infection/ duration of immunity
rep <- 75 # number of years to model

# plot infection status proportions
women.long %>% 
  filter(time > 12*60) %>%
  ggplot() +
  geom_bar(aes(x = time, y = proportion, fill = infection), position = "fill", stat = "identity") +
  # geom_line(aes(x = time, y = rate*10), linetype = "solid", colour = "red") +
  scale_fill_manual(values = c("lightgrey", "darkgrey", viridis(n_interest))) +
  scale_x_continuous(breaks = seq(1, nrow(women), 6), labels = c(rep(c("January", "July"), rep), "January")) +
  # scale_y_continuous(sec.axis = sec_axis(~./10, name = "Rate of Infection")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        # axis.title.y.right = element_text(color = "red"),
        # axis.text.y.right = element_text(color = "red"),
        # axis.ticks.y.right = element_line(color = "red"),
        # axis.line.y.right = element_line(color = "red")
  ) +
  labs(x = "Time",
       y = "Proportion",
       fill = "Infection Status")

# plot infection status heatmap
women.long %>% 
  filter(time > 12*60) %>%
  filter(!infection %in% c("susceptible_naive", "susceptible_reinf")) %>%
  ggplot() +
  geom_tile(aes(x = time, y = infection, fill = proportion)) +
  scale_x_continuous(breaks = seq(1, nrow(women), 6), labels = c(rep(c("January", "July"), rep), "January")) +
  scale_y_discrete(limits = rev) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  labs(x = "Time",
       y = "Time Since Infection",
       fill = "Proportion (%)")

# plot monthly infections with rate of infection double axis
women %>% 
  filter(time > 12*60) %>%
  mutate(I = lead(I1, 1)) %>% 
  ggplot() +
  geom_line(aes(x = time, y = I)) +
  geom_line(aes(x = time, y = rate*500000), linetype = "dashed", colour = "blue") +
  scale_x_continuous(breaks = seq(1, nrow(women), 6), labels = c(rep(c("January", "July"), rep), "January")) +
  scale_y_continuous(name = "Infection Count",
                     sec.axis = sec_axis(~./500000, name = "Rate of Infection")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y.right = element_text(color = "blue"),
        axis.text.y.right = element_text(color = "blue"),
        axis.ticks.y.right = element_line(color = "blue"),
        axis.line.y.right = element_line(color = "blue")) +
  labs(x = "Time")

# plot annual incidence proportions
women %>%
  filter(!is.na(month)) %>% 
  mutate(year = rep(1:rep, each = 12),
         I0 = lead(I1)) %>% 
  group_by(year) %>% 
  summarise(infected = sum(I0)) %>% 
  ungroup() %>% 
  filter(year > 60) %>%
  ggplot(aes(x = year, y = infected)) +
  scale_x_continuous(breaks = seq(1, nrow(women), 6), labels = c(rep(c("January", "July"), rep), "January")) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Year",
       y = "Annual Incidence")

# plot annual incidence proportions (monthly view)
women %>%
  filter(!is.na(month)) %>%
  tail(-6) %>% # changing years to be jul-jul instead of jan-jan
  head(-6) %>% 
  mutate(year = rep(1:(rep-1), each = 12), # rep decreased by 1 for jul-jul year
         I0 = lead(I1)) %>% 
  group_by(year) %>% 
  mutate(cumsum = cumsum(I0)) %>% 
  mutate(year = as.character(year)) %>% 
  filter(time > 12*60) %>%
  ggplot(aes(x = time, y = cumsum, fill = year)) +
  scale_x_continuous(breaks = seq(1, nrow(women), 6), labels = c(rep(c("January", "July"), rep), "January")) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(option = "D") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "Time",
       y = "Cumulative Incidence")

# plot infection history/ immunity level proportions

women.prop %>%
  filter(disruption == 3) %>% 
  ggplot() +
  # geom_bar(aes(x = month, y = proportion, fill = infection), position = "fill", stat = "identity") +
  # scale_fill_manual(values = c("lightgrey", "darkgrey", viridis(n_interest))) +
  geom_bar(aes(x = month, y = proportion, fill = level), position = "fill", stat = "identity") +
  scale_fill_manual(values = viridis(4)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Month",
       y = "Proportion",
       fill = "Immunity Level")
