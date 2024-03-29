## history of infection model in women, calculate infection status in 1,000,000 women based on monthly rate of exposure/infection
## load libraries
library(dplyr)
library(tidyr)
library(stringr)

# setting parameters
n_interest <- 24 # number of months of interest for history of infection/ duration of immunity
rep <- 75 # number of years to model
rate_scale <- 5 # scaling rate of infection for exploration
disruption <- FALSE # disruption (TRUE) or no disruption (FALSE)?
n_burn <- 65 # burn-in period before disruption

# setting up data frame with months and rates
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

# run model
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

# save model output
ifelse(disruption == FALSE, saveRDS(women.long, file = "./output/data/women/women.rds"), saveRDS(women.long, file = "./output/data/women/women_disrupt.rds"))

# reshaping data to become long for plotting
women.long <- women %>% 
  filter(!is.na(month)) %>% 
  mutate(sum = rowSums(across(c("susceptible_naive", "susceptible_reinf", "I1":paste0("I", n_interest))), na.rm = TRUE)) %>% 
  pivot_longer(c("I1":paste0("I", n_interest), "susceptible_naive", "susceptible_reinf"), names_to = "infection", values_to = "count") %>% 
  mutate(infection = factor(infection, levels = c("susceptible_naive", "susceptible_reinf", rev(str_c(rep("I", n_interest), 1:n_interest)))),
         proportion = count/sum)

# save long model output
ifelse(disruption == FALSE, saveRDS(women.long, file = "./output/data/women/women_long.rds"), saveRDS(women.long, file = "./output/data/women/women_long_disrupt.rds"))

# determine monthly average infection status proportions from 60th year onwards for immunity classification of mothers
women.long <- readRDS("./output/data/women/women_long.rds")
women_long_disrupt <- readRDS("./output/data/women/women_long_disrupt.rds")

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

women.prop <- bind_rows(women.long %>% 
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
                          ungroup() %>% 
                          mutate(disruption = 0),
                        women_long_disrupt %>% 
                          filter(time > 12*65 & time <= 12*66) %>% 
                          mutate(month = factor(month, levels = month.abb)) %>% 
                          filter(infection != "susceptible_naive") %>% 
                          mutate(level = as.factor(case_when(infection %in% str_c(rep("I", 6), 1:6) ~ 1,
                                                             infection %in% str_c(rep("I", 9), 7:15) ~ 2,
                                                             infection %in% str_c(rep("I", 9), 16:24) ~ 3,
                                                             infection == "susceptible_reinf" ~ 4))) %>% 
                          group_by(month, level) %>% 
                          summarise(across(c("count", "proportion"), sum, na.rm = TRUE)) %>% 
                          ungroup() %>% 
                          mutate(disruption = 1),
                        women_long_disrupt %>% 
                          filter(time > 12*66 & time <= 12*67) %>% 
                          mutate(month = factor(month, levels = month.abb)) %>% 
                          filter(infection != "susceptible_naive") %>% 
                          mutate(level = as.factor(case_when(infection %in% str_c(rep("I", 6), 1:6) ~ 1,
                                                             infection %in% str_c(rep("I", 9), 7:15) ~ 2,
                                                             infection %in% str_c(rep("I", 9), 16:24) ~ 3,
                                                             infection == "susceptible_reinf" ~ 4))) %>% 
                          group_by(month, level) %>% 
                          summarise(across(c("count", "proportion"), sum, na.rm = TRUE)) %>% 
                          ungroup() %>% 
                          mutate(disruption = 2),
                        women_long_disrupt %>% 
                          filter(time > 12*67 & time <= 12*68) %>% 
                          mutate(month = factor(month, levels = month.abb)) %>% 
                          filter(infection != "susceptible_naive") %>% 
                          mutate(level = as.factor(case_when(infection %in% str_c(rep("I", 6), 1:6) ~ 1,
                                                             infection %in% str_c(rep("I", 9), 7:15) ~ 2,
                                                             infection %in% str_c(rep("I", 9), 16:24) ~ 3,
                                                             infection == "susceptible_reinf" ~ 4))) %>% 
                          group_by(month, level) %>% 
                          summarise(across(c("count", "proportion"), sum, na.rm = TRUE)) %>% 
                          ungroup() %>% 
                          mutate(disruption = 3))

saveRDS(women.prop, file = "./output/data/women_prop.rds")