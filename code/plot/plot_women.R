## plot outputs of women model
## load libraries
library(dplyr)
library(ggplot2)
library(plotly)
library(viridisLite)
library(stringr)

# load data
women <- readRDS("./output/data/women.rds")
women.long <- readRDS("./output/data/women_long.rds")

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

# determine monthly average infection status proportions from 60th year onwards for immunity classification of mothers
women.prop <- women.long %>% 
  filter(time > (12*60)) %>% 
  group_by(month, infection) %>% 
  summarise(across(c("count", "proportion"), mean, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(month = factor(month, levels = month.abb)) %>% 
  filter(infection != "susceptible_naive") %>% 
  mutate(level = as.factor(case_when(infection %in% str_c(rep("I", 6), 1:6) ~ 1,
                                     infection %in% str_c(rep("I", 9), 7:15) ~ 2,
                                     infection %in% str_c(rep("I", 9), 16:24) ~ 3,
                                     infection == "susceptible_reinf" ~ 4))) %>% 
  group_by(month, level) %>% 
  summarise(across(c("count", "proportion"), sum, na.rm = TRUE)) %>% 
  ungroup()

saveRDS(women.prop, file = "./output/data/women_prop.rds")

women.prop %>%
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
