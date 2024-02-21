# load libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(viridisLite)
library(plotly)

# history of infection model in women, calculate infection status in 1,000,000 women based on monthly infection rate
n_interest <- 24 # number of months of interest for history of infection
rep <- 75 # number of years
rate_scale <- 5 # scaling rate of infection for exploration
disruption <- FALSE # disruption or no disruption?
n_burn <- 65 # burn-in period for disruption

women <- as.data.frame(matrix(NA, 12*rep, 5+n_interest))
colnames(women) <- c("time", "month", "susceptible_naive", "susceptible_reinf", "rate", str_c(rep("I", n_interest), 1:n_interest))

women <- women %>% mutate(month = rep(month.abb, rep),
                          time = 1:nrow(women),
                          # rate = 0.05,
                          # rate = ifelse(month %in% c(month.abb[11:12], month.abb[1:3]), 0.10, 0.05),
                          rate = (case_when(month == month.abb[1] ~ 0.015,
                                           month %in% month.abb[2:3] ~ 0.005,
                                           month %in% month.abb[4:8] ~ 0.000,
                                           month == month.abb[9] ~ 0.010,
                                           month == month.abb[10] ~ 0.020,
                                           month %in% month.abb[11:12] ~ 0.045))*rate_scale)

if(disruption == TRUE){
  women[(12*n_burn+3):(12*(n_burn+1)+3), "rate"] <- 0 # setting disruption after 25 year burn-in, 0 between March
  # women[(12*n_burn+3):(12*(n_burn+1)+8), "rate"] <- as.data.frame(c(rep(0, 13), seq(0, 0.008, 0.002))) # option 1: setting disruption after 25 year burn-in, 0 between March then gradual increase until September
  # women[(12*n_burn+3):(12*(n_burn+1)+8), "rate"] <- as.data.frame(c(rep(0, 13), rep(0.002, 3), rep(0.005, 2))) # option 2: setting disruption after 25 year burn-in, 0 between March then step-wise following restrictions lifting until September
  # women[(12*n_burn+3):(12*(n_burn+1)+8), "rate"] <- as.data.frame(c(rep(0, 13), rep(0.01, 2), rep(0, 3))) # option 3: setting disruption after 25 year burn-in, 0 between March then increase + decrease
}

# initial state
women[1, "susceptible_naive"] <- 1000000
# women[1, "I1"] <- 200000
# women[1, "I2"] <- 100000
# women[1, "I24"] <- 1000000 # problem when susceptible naive is 0 at beginning


for (row in 1:nrow(women)) {
  women[row + 1, "I1"] <-   ifelse(is.na(women[row, "susceptible_reinf"]), women[row, "susceptible_naive"] * women[row, "rate"], (women[row, "susceptible_naive"] * women[row, "rate"]) + (women[row, "susceptible_reinf"] * women[row, "rate"]))
  women[row + 1, "susceptible_naive"] <- women[row, "susceptible_naive"] - (women[row, "susceptible_naive"] * women[row, "rate"])
  women[row + 1, "susceptible_reinf"] <- ifelse(is.na(women[row, "susceptible_reinf"]), women[row, paste0("I", n_interest)], women[row, "susceptible_reinf"] - (women[row, "susceptible_reinf"] * women[row, "rate"]) + women[row, paste0("I", n_interest)])

  for (month in 2:n_interest){
    if(!is.na(women[row, paste0("I", month-1)])){
      women[row + 1, paste0("I", month)] <- women[row, paste0("I", month-1)]
    } else {women[row + 1, paste0("I", month)] <- NA}
  }
}

# reshaping data for plotting
women.long <- women %>% 
  filter(!is.na(month)) %>% 
  mutate(sum = rowSums(across(c("susceptible_naive", "susceptible_reinf", "I1":paste0("I", n_interest))), na.rm = TRUE)) %>% 
  pivot_longer(c("I1":paste0("I", n_interest), "susceptible_naive", "susceptible_reinf"), names_to = "infection", values_to = "count") %>% 
  mutate(infection = factor(infection, levels = c("susceptible_naive", "susceptible_reinf", rev(str_c(rep("I", n_interest), 1:n_interest)))),
         proportion = count/sum)

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
  
# plot monthly infections
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

# reshaping data for cumulative annual incidence
women.annual.v1 <- women %>%
  filter(!is.na(month)) %>% 
  mutate(year = rep(1:rep, each = 12),
         I0 = lead(I1)) %>% 
  group_by(year) %>% 
  summarise(infected = sum(I0)) %>% 
  ungroup()

women.annual.v2 <- women %>%
  filter(!is.na(month)) %>%
  tail(-6) %>% # changing years to be jul-jul instead of jan-jan
  head(-6) %>% 
  mutate(year = rep(1:(rep-1), each = 12), # rep decreased by 1 for jul-jul year
         I0 = lead(I1)) %>% 
  group_by(year) %>% 
  mutate(cumsum = cumsum(I0))

# plot annual incidence proportions
women.annual.v1 %>% 
  filter(year > 60) %>%
  ggplot(aes(x = year, y = infected)) +
  scale_x_continuous(breaks = seq(1, nrow(women), 6), labels = c(rep(c("January", "July"), rep), "January")) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Year",
       y = "Annual Incidence")

women.annual.v2 %>% 
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