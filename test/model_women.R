# load libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)
library(scales)
library(stringr)
library(viridisLite)
library(plotly)

# history of infection model in women, calculate infection status in 1,000,000 women based on monthly infection rate
n_interest <- 24 # number of months of interest for history of infection
rep <- 75 # number of years
rate_scale <- 1 # scaling rate of infection for exploration

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

# plot infection status in women
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

# cumulative annual incidence and proportion
women.annual <- women %>%
  filter(!is.na(month)) %>% 
  mutate(year = rep(1:rep, each = 12),
         I0 = lead(I1),
         susceptible_total = rowSums(across(c("susceptible_naive", "susceptible_reinf")), na.rm = TRUE)) %>% 
  group_by(year) %>% 
  summarise(infected = sum(I0),
            susceptible = sum(susceptible_total)) %>% 
  ungroup() %>% 
  mutate(annual_rate = infected/susceptible)

# plot annual incidence proportion
women.annual %>% 
  ggplot(aes(x = year, y = infected)) +
  scale_x_continuous(breaks = seq(0, 80, 5)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Year",
       y = "Annual Incidence")


# plotting susceptible naive women
# women %>% 
#   ggplot() +
#   geom_line(aes(x = time, y = susceptible_naive)) +
#   scale_x_continuous(breaks = seq(1, nrow(women), 6), labels = c(rep(c("January", "July"), rep), "January")) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   labs(x = "Time",
#        y = "Count")
  
# plotly plot
# women.long %>%
#   plot_ly() %>%
#   add_trace(x = ~time,
#             y = ~proportion,
#             type = "bar",
#             color = ~infection) %>%
#   layout(barmode = "stack")

# history of infection model with disruption
n_burn <- 65 # burn-in period

women.disrupt <- women
women.disrupt[(12*n_burn+3):(12*(n_burn+1)+3), "rate"] <- 0 # setting disruption after 25 year burn-in, 0 between March
# women.disrupt[(12*n_burn+3):(12*(n_burn+1)+8), "rate"] <- as.data.frame(c(rep(0, 13), seq(0, 0.008, 0.002))) # option 1: setting disruption after 25 year burn-in, 0 between March then gradual increase until September
# women.disrupt[(12*n_burn+3):(12*(n_burn+1)+8), "rate"] <- as.data.frame(c(rep(0, 13), rep(0.002, 3), rep(0.005, 2))) # option 2: setting disruption after 25 year burn-in, 0 between March then step-wise following restrictions lifting until September
# women.disrupt[(12*n_burn+3):(12*(n_burn+1)+8), "rate"] <- as.data.frame(c(rep(0, 13), rep(0.01, 2), rep(0, 3))) # option 3: setting disruption after 25 year burn-in, 0 between March then increase + decrease
women.disrupt[, c("susceptible_naive", "susceptible_reinf", str_c(rep("I", n_interest), 1:n_interest))] <- NA

# initial state
women.disrupt[1, "susceptible_naive"] <- 1000000
# women.disrupt[1, "I1"] <- 200000
# women.disrupt[1, "I2"] <- 100000


for (row in 1:nrow(women.disrupt)) {
  women.disrupt[row + 1, "I1"] <-   ifelse(is.na(women.disrupt[row, "susceptible_reinf"]), women.disrupt[row, "susceptible_naive"] * women.disrupt[row, "rate"], (women.disrupt[row, "susceptible_naive"] * women.disrupt[row, "rate"]) + (women.disrupt[row, "susceptible_reinf"] * women.disrupt[row, "rate"]))
  women.disrupt[row + 1, "susceptible_naive"] <- women.disrupt[row, "susceptible_naive"] - (women.disrupt[row, "susceptible_naive"] * women.disrupt[row, "rate"])
  women.disrupt[row + 1, "susceptible_reinf"] <- ifelse(is.na(women.disrupt[row, "susceptible_reinf"]), women.disrupt[row, paste0("I", n_interest)], women.disrupt[row, "susceptible_reinf"] - (women.disrupt[row, "susceptible_reinf"] * women.disrupt[row, "rate"]) + women.disrupt[row, paste0("I", n_interest)])
  
  for (month in 2:n_interest){
    if(!is.na(women.disrupt[row, paste0("I", month-1)])){
      women.disrupt[row + 1, paste0("I", month)] <- women.disrupt[row, paste0("I", month-1)]
    } else {women.disrupt[row + 1, paste0("I", month)] <- NA}
  }
}

# reshaping data for plotting
women.disrupt.long <- women.disrupt %>% 
  mutate(sum = rowSums(across(c("susceptible_naive", "susceptible_reinf", "I1":paste0("I", n_interest))), na.rm = TRUE)) %>% 
  pivot_longer(c("I1":paste0("I", n_interest), "susceptible_naive", "susceptible_reinf"), names_to = "infection", values_to = "count") %>% 
  mutate(infection = factor(infection, levels = c("susceptible_naive", "susceptible_reinf", rev(str_c(rep("I", n_interest), 1:n_interest)))),
         proportion = count/sum)

# plot infection status in women
women.disrupt.long %>% 
  filter(time > 12*60) %>%
  ggplot() +
  geom_bar(aes(x = time, y = proportion, fill = infection), position = "fill", stat = "identity") +
  geom_vline(aes(xintercept = 12*n_burn+3), color = "red") +
  geom_vline(aes(xintercept = 12*(n_burn+1)+3), color = "red") +
  scale_fill_manual(values = c("lightgrey", "darkgrey", viridis(n_interest))) +
  scale_x_continuous(breaks = seq(1, nrow(women.disrupt), 6), labels = c(rep(c("January", "July"), rep), "January")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Time",
       y = "Proportion",
       fill = "Infection Status")

# plot monthly infections
women.disrupt %>% 
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

# cumulative annual incidence proportion
women.disrupt.annual <- women.disrupt %>%
  filter(!is.na(month)) %>% 
  mutate(year = rep(1:rep, each = 12),
         I0 = lead(I1),
         susceptible_total = rowSums(across(c("susceptible_naive", "susceptible_reinf")), na.rm = TRUE)) %>% 
  group_by(year) %>% 
  summarise(infected = sum(I0),
            susceptible = sum(susceptible_total)) %>% 
  ungroup() %>% 
  mutate(annual_rate = infected/susceptible)

# plot annual incidence proportion
women.disrupt.annual %>% 
  ggplot() +
  scale_x_continuous(breaks = seq(1, 80, 2)) +
  geom_line(aes(x = year, y = annual_rate)) +
  theme_bw() +
  labs(x = "Year",
       y = "Annual Incidence Proportion")

# # determine average infectious status proportions from 7th year onwards
# women.prop <- women.long %>% 
#   filter(!time %in% c(1:72)) %>% 
#   group_by(month, infection) %>% 
#   summarise(across(c("count", "proportion"), mean, na.rm = TRUE))
# 
# women.prop %>% 
#   filter(!is.na(month)) %>% 
#   mutate(month = factor(month, levels = month.abb)) %>% 
#   ggplot() + 
#   geom_bar(aes(x = month, y = proportion, fill = infection), position = "fill", stat = "identity") +
#   scale_fill_manual(values = c("grey", viridis(24))) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   labs(x = "Month",
#        y = "Proportion",
#        fill = "Infection Status")
# 
# # assign susceptibility to babies born
# # grouping infection status into 3
# women.prop <- women.prop %>% 
#   filter(!is.na(month)) %>% 
#   mutate(group = ifelse(infection %in% c("susceptible", str_c(rep("I", 8), 1:8)), 1,
#                         ifelse(infection %in% str_c(rep("I", 8), 9:16), 2, 3))) %>% 
#   group_by(month, group) %>% 
#   summarise(proportion = sum(proportion))
# 
# infection <- data %>%
#   select(c("year":"births")) %>% 
#   left_join(women.prop) %>% 
#   mutate(susceptibility = floor(births*proportion),
#          inf_normal = ifelse(month %in% c("Nov", "Dec", "Jan", "Feb", "Mar"), 0.3, 0.05),
#          inf_pandemic = ifelse(year == 2020 & month %in% c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") | year == 2021 & month %in% c("Jan", "Feb", "Mar"), 0, inf_normal)) %>% 
#   mutate(inf_normal = ifelse(group == 1, inf_normal*0.5, inf_normal),
#          inf_pandemic = ifelse(group == 1, inf_pandemic*0.5, inf_pandemic))