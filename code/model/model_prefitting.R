library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(zoo)

# -------------------------------------------------------------------------
# explore reintroduction of rate

# setting parameters
n_interest <- 24 # number of months of interest for history of infection/ duration of immunity
rep <- 75 # number of years to model
rate_scale <- 4 # scaling rate of infection for exploration
disruption <- TRUE # disruption (TRUE) or no disruption (FALSE)?
n_burn <- 65 # burn-in period before disruption

# -------------------------------------------------------------------------

# MODELING MOTHERS

# pick one of the options below out of 3

# 1. baseline scenario: rate is arbitrary

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
}

# reintroduction of rate
women <- women %>% 
  mutate(rate = ifelse(time > 12*(n_burn+1)+3 & time <= 12*(n_burn+3)+3, rate * 0.5, rate)) # reducing rate by half for subsequent 2 years after end of disruption

# 2. alternative scenario: change in mobility informing rate of exposure

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
                                            month %in% month.abb[11:12] ~ 0.045))*rate_scale) %>% 
  left_join(readRDS("./output/data/prefitting/mobility.rds") %>% select(time, value)) %>% 
  mutate(value = ifelse(is.na(value), 1, value),
         rate = rate * value) %>% 
  select(-value)

# 3. alternative scenario: 2017 onwards rate shape is informed by scottish data but scaled to make sense

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
                                            month %in% month.abb[11:12] ~ 0.045))*rate_scale) %>% 
  left_join(readRDS("./output/data/prefitting/rate_scot.rds") %>% select(rate_scot, time), by = join_by(`time`)) %>% 
  mutate(rate = ifelse(is.na(rate_scot), rate, rate_scot)) %>% 
  select(-rate_scot)

# -------------------------------------------------------------------------

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

# reshaping data to become long for plotting
women.long <- women %>% 
  filter(!is.na(month)) %>% 
  mutate(sum = rowSums(across(c("susceptible_naive", "susceptible_reinf", "I1":paste0("I", n_interest))), na.rm = TRUE)) %>% 
  pivot_longer(c("I1":paste0("I", n_interest), "susceptible_naive", "susceptible_reinf"), names_to = "infection", values_to = "count") %>% 
  mutate(infection = factor(infection, levels = c("susceptible_naive", "susceptible_reinf", rev(str_c(rep("I", n_interest), 1:n_interest)))),
         proportion = count/sum)

# save long model output
saveRDS(women.long, file = "./output/data/women/women_rate_prefitting.rds")

# -------------------------------------------------------------------------

# MODELING BABIES

# data preparation

# load birth data
birth_data <- read_excel("./data/births_pregnancies.xlsx", sheet = "All") %>%
  mutate(year_month = paste(year, month, sep = "_"),
         date = as.Date(as.yearmon(`year_month`, "%Y_%b")),) %>%
  select(year, month, date, births) %>% 
  filter(!is.na(births)) %>% 
  slice(1:(12*10)) %>% 
  mutate(time = 1:nrow(.))

# create reference table for rates to join
rate_reference <- readRDS("./output/data/women/women_rate_prefitting.rds") %>% 
  select(time, month, rate) %>% 
  distinct() %>% 
  filter(time <= 12*75, time > 12*60) %>% 
  mutate(month = factor(month, levels = month.abb),
         time = 1:nrow(.)) %>% 
  cross_join(data.frame(level = 1:4)) %>% 
  mutate(level = factor(level, levels = 1:4))

# create 10 years of infection history/immunity split
immunity_split <- readRDS("./output/data/women/women_rate_prefitting.rds") %>% 
  filter(time <= 12*70, time > 12*60) %>% 
  mutate(month = factor(month, levels = month.abb)) %>% 
  filter(infection != "susceptible_naive") %>% 
  mutate(level = as.factor(case_when(infection %in% str_c(rep("I", 6), 1:6) ~ 1,
                                     infection %in% str_c(rep("I", 9), 7:15) ~ 2,
                                     infection %in% str_c(rep("I", 9), 16:24) ~ 3,
                                     infection == "susceptible_reinf" ~ 4))) %>% 
  group_by(time, rate, month, level) %>% 
  summarise(across(c("count", "proportion"), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(time = rep(1:(12*10), each = 4)) %>% 
  # combining with birth data
  select(-count) %>% 
  left_join(birth_data %>% select(month, births, time)) %>% 
  mutate(susceptible = proportion * births)

# -------------------------------------------------------------------------
# model continuous monthly births, each for 3 months

# nesting data set to model
model_continuous_births <- immunity_split %>% 
  nest(.by = time)

# model
for(n in 1:nrow(model_continuous_births)){
  data <- model_continuous_births[[2]][[n]] %>% 
    mutate(time = n) %>% 
    right_join(rate_reference) %>% 
    filter(time %in% c(n:(n+12*3-1))) %>% 
    # distinguish between calendar months and months since birth
    rename(time_calendar = time) %>% 
    mutate(time_birth = rep(1:36, each = 4)) %>% 
    # set probability of infection/disease based on immunity level - choose one of the options.
    # option 1
    mutate(prob_inf = case_when(level == 1 ~ 0,
                                level == 2 ~ 0.5,
                                level == 3 ~ 0.5,
                                level == 4 ~ 1),
           prob_dis = case_when(level == 1 ~ 0,
                                level == 2 ~ 0,
                                level == 3 ~ 0.5,
                                level == 4 ~ 1)) %>%
  # -------------------------------------------------------------------------
  # explore probability of infection and disease
  # option 2
  # mutate(prob_inf = case_when(level == 1 ~ 0,
  #                             level == 2 ~ 0.5,
  #                             level == 3 ~ 0.75,
  #                             level == 4 ~ 1),
  #        prob_dis = case_when(level == 1 ~ 0,
  #                             level == 2 ~ 0.5,
  #                             level == 3 ~ 0.75,
  #                             level == 4 ~ 1)) %>% 
  # option 3
  # mutate(prob_inf = case_when(level == 1 ~ 0.25,
  #                             level == 2 ~ 0.5,
  #                             level == 3 ~ 0.75,
  #                             level == 4 ~ 1),
  #        prob_dis = case_when(level == 1 ~ 0,
  #                             level == 2 ~ 0.25,
  #                             level == 3 ~ 0.50,
  #                             level == 4 ~ 0.75)) %>%
  # explore changing the shape of waning and aging - choose one of the options.
    # option 1
    # introduce waning immunity to probability of infection
    mutate(waning = case_when(time_birth <= 6 ~ 1,
                              time_birth > 6 & time_birth <= 12 ~ 0.5,
                              time_birth > 12 & time_birth <= 24 ~ 0.2,
                              time_birth > 24 ~ 0),
           prob_inf = 1 - ((1 - prob_inf) * waning)) %>%
    # introduce aging to probability of disease
    mutate(aging = case_when(time_birth <= 6 ~ 1,
                             time_birth > 6 & time_birth <= 12 ~ 0.5,
                             time_birth > 12 & time_birth <= 24 ~ 0.2,
                             time_birth > 24 ~ 0),
           prob_dis = prob_dis * aging) %>%
# -------------------------------------------------------------------------
    # # option 2
    # # introduce waning immunity to probability of infection
    # mutate(waning = 1-time_birth/36,
    #      prob_inf = 1 - ((1 - prob_inf) * waning)) %>%
    # # introduce aging to probability of disease
    # mutate(aging = 1-time_birth/36,
    #        prob_dis = prob_dis * aging) %>%
    # # option 3
    # # introduce waning immunity to probability of infection
    # mutate(waning = 1*0.9^time_birth,
    #      prob_inf = 1 - ((1 - prob_inf) * waning)) %>%
    # # introduce aging to probability of disease
    # mutate(aging = 1*0.9^time_birth,
    #        prob_dis = prob_dis * aging) %>%
    # determine initial number of infected and disease
    mutate(infected = susceptible * rate * prob_inf,
           disease = infected * prob_dis) %>% 
    group_by(level) %>% 
    nest()
  
  for(lev in 1:4){
    subdata <- data[[2]][[lev]]
    
    for(row in 1:nrow(subdata)){
      subdata[row, "infected"] <- subdata[row, "susceptible"] * subdata[row, "rate"] * subdata[row, "prob_inf"]
      subdata[row, "disease"] <- subdata[row, "infected"] * subdata[row, "prob_dis"]
      subdata[row + 1, "susceptible"] <- subdata[row, "susceptible"] - subdata[row, "infected"]
    }
    
    data[[2]][[lev]] <- subdata
  }
  
  data <- data %>% 
    unnest() %>%
    filter(!is.na(month)) %>%
    mutate(`<6` = ifelse(time_birth < 6, 1, 0),
           `6-12` = ifelse(time_birth >= 6 & time_birth < 12, 1, 0),
           `12-24` = ifelse(time_birth >= 12 & time_birth < 24, 1, 0),
           `24-36` = ifelse(time_birth >= 24 & time_birth <= 36, 1, 0))
  
  model_continuous_births[[2]][[n]] <- data
}

output <- model_continuous_births %>% unnest()

saveRDS(output, file = "./output/data/prefitted_model.rds") # add suffix (_...) as necessary (for rate: scot, mobility; for shapes: linear, exponential)


# -------------------------------------------------------------------------

# TEST

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
         yaxis = list(title = "Rate of exposure"))