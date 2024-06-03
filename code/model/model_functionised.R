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

# pick one of the options below

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
         proportion = count/sum) %>% 
  mutate(infection = str_remove(infection, "I"),
         infection = replace(infection, infection == "susceptible_reinf", 25)) %>% 
  filter(infection %in% c(1:25)) %>% 
  mutate(infection = as.numeric(infection))

# save long model output
saveRDS(women.long, file = "./output/data/women/women_rate_functionised.rds")

# -------------------------------------------------------------------------

# MODELING BABIES

# data preparation

# load birth data
# # england
# birth_data <- read_excel("./data/births_pregnancies.xlsx", sheet = "All") %>%
#   mutate(year_month = paste(year, month, sep = "_"),
#          date = as.Date(as.yearmon(`year_month`, "%Y_%b")),) %>%
#   select(year, month, date, births) %>% 
#   filter(!is.na(births)) %>% 
#   slice(1:(12*10)) %>% 
#   mutate(time = 1:nrow(.))

# scotland
birth_data <- read_excel("./data/births-time-series-22-bt.3.xlsx", skip = 3) %>% 
  head(-7) %>% 
  select(1:14) %>% 
  select(-Total) %>% 
  rename(year = Year) %>% 
  pivot_longer(cols = `Jan`:`Dec`, names_to = "month", values_to = "births") %>% 
  filter(year >= 2012, year <= 2021) %>% 
  mutate(month = rep(month.abb, 10),
         year_month = paste(year, month, sep = "_"),
         date = as.Date(as.yearmon(`year_month`, "%Y_%b"))) %>%
  select(year, month, date, births) %>% 
  mutate(time = 1:nrow(.))

# create reference table for rates to join
rate_reference <- readRDS("./output/data/women/women_rate_functionised.rds") %>% 
  select(time, month, rate, infection) %>% 
  rename(level = infection) %>% 
  filter(time <= 12*75, time > 12*60) %>% 
  mutate(month = factor(month, levels = month.abb),
         time = rep(1:n_distinct(time), each = 25))

# create 10 years of infection history/immunity split
immunity_split <- readRDS("./output/data/women/women_rate_functionised.rds") %>% 
  filter(time <= 12*70, time > 12*60) %>% 
  mutate(month = factor(month, levels = month.abb)) %>% 
  rename(level = infection) %>% 
  group_by(time, rate, month, level) %>% 
  summarise(across(c("count", "proportion"), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(time = rep(1:(12*10), each = 25)) %>% 
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
    filter(time %in% c(n:(n+12*4-1))) %>% 
    # distinguish between calendar months and months since birth
    rename(time_calendar = time) %>% 
    mutate(time_birth = rep(1:48, each = 25)) %>% 
    # set probability of infection/disease based on immunity level - choose one of the options.
    # # option 1: step-wise
    # mutate(prob_inf = case_when(level <=6 ~ 0,
    #                             level >6 & level <=15 ~ 0.5,
    #                             level >15 & level <=24  ~ 0.5,
    #                             level >24 ~ 1),
    #        prob_dis = case_when(level <=6 ~ 0,
    #                             level >6 & level <=15  ~ 0,
    #                             level >15 & level <=24 ~ 0.5,
    #                             level >24 ~ 1)) %>%
    # -------------------------------------------------------------------------
  # explore probability of infection and disease
  # # option 2: linear
  # mutate(prob_inf = 1/25*level,
  #        prob_dis = 0.0526*level - 0.3158) %>%
  # option 3: exponential
  mutate(prob_inf = 0.0441*exp(0.1248*level),
         prob_dis = 0.0194*exp(0.1577*level)) %>%
  # explore changing the shape of waning and aging - choose one of the options.
  # option 1: step-wise
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
  # # option 2: linear
  # # introduce waning immunity to probability of infection
  # mutate(waning = 1-time_birth/36,
  #      prob_inf = 1 - ((1 - prob_inf) * waning)) %>%
  # # introduce aging to probability of disease
  # mutate(aging = 1-time_birth/36,
  #        prob_dis = prob_dis * aging) %>%
  # # option 3: exponential
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
  
  for(lev in 1:25){
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
           `12-24` = ifelse(time_birth >= 12 & time_birth <= 24, 1, 0),
           `24-36` = ifelse(time_birth >= 24 & time_birth <= 36, 1, 0),
           `36-48` = ifelse(time_birth >= 36 & time_birth <= 48, 1, 0))
  
  model_continuous_births[[2]][[n]] <- data
}

output <- model_continuous_births %>% unnest()

saveRDS(output, file = "./output/data/prefitting/prefitted_model_functionised_exponential.rds") # add suffix (_...) as necessary (for rate: scot, mobility; for shapes: linear, exponential)


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
         shapes = lockdown)


# -------------------------------------------------------------------------

# match model output to data

test <- readRDS(file = "./output/data/prefitting/prefitted_model_functionised_linear.rds") %>% 
  select(-time) %>% 
  mutate(age = case_when(time_birth  < 12 ~ "<1",
                         time_birth >= 12 & time_birth  <= 48 ~ "1-4")) %>%
  group_by(time_calendar, month, rate, age) %>% 
  summarise(infected = sum(infected),
            disease = sum(disease)) %>% 
  ungroup() %>% 
  pivot_longer(cols = c("infected", "disease"), names_to = "type", values_to = "count") %>%
  left_join(population) %>% 
  mutate(rate = (count/population)*100000) %>% 
  # visualise
  filter(type == "disease") %>% 
  plot_ly() %>%
  add_trace(
    x = ~time_calendar,
    y = ~rate,
    split = ~age,
    colour = ~age,
    type = "scatter",
    mode = "line",
    text = ~month
  ) %>%
  layout(xaxis = list(title = "Calendar month",
                      range = list(37, 120),
                      tickvals = seq(37, 120, 3),
                      ticktext = rep(c("Jan", "Apr", "Jul", "Oct"), 7),
                      tickmode = "array",
                      tickangle = -45),
         yaxis = list(title = "Rate (per 100,000)",
                      # range = list(0, 15000),
                      # tickmode = "linear",
                      # tick0 = 0,
                      # dtick = 2000,
                      # tickformat = "digits",
                      side = "left"),
         shapes = lockdown)         
