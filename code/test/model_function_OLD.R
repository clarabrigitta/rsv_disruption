library(dplyr)
library(tidyr)
library(stringr)
library(zoo)
library(readxl)
library(purrr)

# -------------------------------------------------------------------------
# creating fixed data to use in model function

# starting matrix for modelling women
n_interest <- 24 # number of months of interest for history of infection/ duration of immunity
rep <- 30 # number of years to model
n_burn <- 20 # burn-in period before disruption

women_mat <- as.data.frame(matrix(0, 12*rep, 5+n_interest))
colnames(women_mat) <- c("time", "month", "rate", "susceptible_naive", "susceptible_reinf", str_c(rep("I", n_interest), 1:n_interest))

women_mat <- women_mat %>% mutate(month = rep(1:12, rep),
                                  time = 1:nrow(women_mat),
                                  rate = (case_when(month == 1 ~ 0.06,
                                                    month == 2 | month == 3 ~ 0.02,
                                                    month >= 4 & month <= 8 ~ 0.000,
                                                    month == 9 ~ 0.04,
                                                    month == 10 ~ 0.08,
                                                    month == 11 | month == 12 ~ 0.18))) %>% 
  select(-month)

women_mat <- apply(as.matrix(women_mat), c(1, 2), as.numeric)

# helper data frame for dates and rates (without any disruption) to model babies
dates <- as.data.frame(matrix(NA, 12*(rep+4), 6))
colnames(dates) <- c("time", "month", "month_num", "year", "yearmon", "date")

dates <- dates %>% 
  mutate(time = 1:nrow(dates),
         month = rep(month.abb, nrow(dates)/12),
         month_num = rep(1:12, rep+4),
         year = rep(c(2000:(2000+(nrow(dates)/12)-1)), each =12),
         yearmon = as.yearmon(paste(year, month, sep = "_"), "%Y_%b"),
         date = as.Date(yearmon),
         rate = (case_when(month == month.abb[1] ~ 0.06,
                           month %in% month.abb[2:3] ~ 0.02,
                           month %in% month.abb[4:8] ~ 0,
                           month == month.abb[9] ~ 0.04,
                           month == month.abb[10] ~ 0.08,
                           month %in% month.abb[11:12] ~ 0.18))) %>% 
  filter(year >= 2012) %>% 
  mutate(time = 1:n_distinct(time)) %>%
  left_join(data.frame(level = rep(1:25, 264),
                       time = rep(1:264, each = 25)))

# matrix for Scottish birth data (2012 - 2029)
birth_data <- read_excel("./data/births-time-series-22-bt.3.xlsx", skip = 3) %>%
  head(-7) %>%
  select(1:14) %>%
  select(-Total) %>%
  rename(year = Year, Jun = June, Jul = July, Sep = Sept) %>%
  pivot_longer(cols = `Jan`:`Dec`, names_to = "month", values_to = "births") %>%
  filter(year >= 2012) %>%
  mutate(year = as.numeric(year),
         yearmon = as.yearmon(paste(year, month, sep = "_"), "%Y_%b"),
         date = as.Date(yearmon)) %>%
  right_join(dates %>% select(-c(rate, level)) %>% filter(year <= 2029) %>% distinct()) %>%
  # extrapolate births beyond 2022 (linear regression)
  mutate(births = ifelse(is.na(births), -8.131*time + 4903.856, births)) %>% 
  select(c(time, births))

# vector for population size of <1 and 1-4 in Scotland
population <- read_excel("./data/mid-year-pop-est-22-data.xlsx", sheet = "Table 1", skip = 3) %>%
  filter(`Area name` == "Scotland",
         Sex == "Persons") %>%
  select(1:10) %>%
  select(-`All ages`) %>%
  pivot_longer(cols = `0`:`4`, names_to = "age_year", values_to = "population") %>%
  mutate(age = ifelse(age_year == 0, "<1", "1-4")) %>%
  group_by(age) %>%
  summarise(population = sum(population)) %>% 
  select(population) %>% 
  pull()

# empty matrix to model babies for 4 years
empty <- as.data.frame(matrix(0, 48, 10))
colnames(empty) <- c("time_calendar", "time_birth", "level", "rate", "prob_inf", "waning", "aging", "susceptible", "infected", "disease")

# vector of monthly rates
rate_vector <- dates %>% select(-level) %>% distinct() %>% select(rate) %>% pull()

# put all data into a list
save_data <- list(women_mat, birth_data, population, empty, rate_vector)

# -------------------------------------------------------------------------

# beginning of model function

model_function <- function(lambda, theta, omega, alpha, stored_data){
  
  print(Sys.time())
  
  # matrix key:
  # 1 = time
  # 2 = rate
  # 3 - 28 = infection history status
  women <- stored_data[[1]]
  
  # subject rate to disruption factor lambda 
  women[243:255, 2] <-  women[243:255, 2] * lambda
  
  # initial state
  women[1, 3] <- 1000000
  
  # model women
  for (row in 3:nrow(women)) {
    # map_dbl(2:nrow(women),
    #     function(x) {})
    
    women[row - 1, 5] <- women[row - 2, 3] * women[row - 2, 2] + women[row - 2, 4] * women[row - 2, 2]
    women[row - 1, 3] <- women[row - 2, 3] - women[row - 2, 3] * women[row - 2, 2]
    women[row - 1, 4] <- women[row - 2, 4] - women[row - 2, 4] * women[row - 2, 2] + women[row - 2, 28]
    women[row, c(6:28, 4)] <- women[row - 1, 5:28]
    
    # <- c(women[row - 1. month_at:last_month], c(1:))
    # for (month in rev(2:24)){
    #     women[row, 30-month] <- women[row - 1, 29-month]
    # }
  }
  
  # create link between mothers and babies (infection history/immunity split)
  women <- cbind(women, sum = 1000000)
  women <- women[145:360, ] # selecting for 2012-2029 to match time period of birth data
  women[, 1] <- 1:216 # re-labeling time to match birth data
  
  immunity_split <- as.data.frame(women) %>% 
    pivot_longer(3:28, names_to = "infection", values_to = "count") %>% 
    mutate(proportion = count/sum) %>% 
    mutate(infection = str_remove(infection, "I"),
           infection = replace(infection, infection == "susceptible_reinf", 25)) %>% 
    filter(infection %in% c(1:25)) %>% 
    mutate(infection = as.numeric(infection)) %>% 
    rename(level = infection) %>% 
    select(-c(sum, count)) %>% 
    # combining with birth data
    left_join(stored_data[[2]][, c("births", "time")], by = "time") %>% 
    mutate(susceptible = proportion * births) %>% 
    select(-c(proportion, births, rate)) %>% 
    arrange(level)
  
  print(Sys.time())
  
  ## model babies
  # apply lambda to rate vector
  stored_data[[5]][99:111] <- stored_data[[5]][99:111] * lambda
  
  # nesting data set to model
  births <- immunity_split %>% 
    nest(.by = time)
  
  for(n in 1:216){
    data <- births[[2]][[n]] %>% 
      nest(.by = level)
    
    for(lev in 1:25){
      subdata <- stored_data[[4]] %>% mutate(time_calendar = n:(n+12*4-1),
                                             time_birth = 1:48,
                                             level = lev,
                                             rate = stored_data[[5]][n:(n+12*4-1)],
                                             prob_inf = theta * level,
                                             waning = omega * time_birth + 1,
                                             aging = alpha * time_birth + 1)
      
      # initial state
      subdata[1, "susceptible"] <- data[[2]][[lev]]$susceptible
      
      for(row in 1:48){
        subdata[row, "infected"] <- subdata[row, "susceptible"] * subdata[row, "rate"] * (1 - ((1 - subdata[row, "prob_inf"]) * subdata[row, "waning"]))
        subdata[row, "disease"] <- subdata[row, "susceptible"] * subdata[row, "rate"] * (1 - ((1 - subdata[row, "prob_inf"]) * subdata[row, "waning"])) * subdata[row, "aging"]
        subdata[row + 1, "susceptible"] <- subdata[row, "susceptible"] - subdata[row, "infected"]
      }
      
      data[[2]][[lev]] <- subdata[1:48, ]
    }
    
    births[[2]][[n]] <- data %>% unnest()
    
  }
  
  ## convert to desired output form
  output <- births %>% 
    unnest(cols = c(data)) %>% 
    select(-c(time, level)) %>% 
    rename(level = level1) %>% 
    mutate(age = case_when(time_birth  < 12 ~ "<1",
                           time_birth >= 12 & time_birth  <= 48 ~ "1-4")) %>%
    group_by(time_calendar, age) %>% 
    summarise(disease = sum(disease)) %>% 
    ungroup() %>% 
    mutate(population = case_when(age == "<1" ~ stored_data[[4]][2],
                                  age == "1-4" ~ stored_data[[4]][1]),
           rate = (disease/population)*100000) %>% 
    filter(time_calendar >= 58 & time_calendar <= 149) %>% 
    arrange(age)
  
  print(Sys.time())
  
  return(output)
  
}

# -------------------------------------------------------------------------
## OLDER VERSION

library(dplyr)
library(tidyr)
library(stringr)

# -------------------------------------------------------------------------
# creating fixed data frames to use in model function

# starting data frame for modelling women
n_interest <- 24 # number of months of interest for history of infection/ duration of immunity
rep <- 30 # number of years to model
n_burn <- 20 # burn-in period before disruption

women_mat <- as.data.frame(matrix(0, 12*rep, 5+n_interest))
colnames(women_mat) <- c("time", "month", "susceptible_naive", "susceptible_reinf", "rate", str_c(rep("I", n_interest), 1:n_interest))

women_mat <- women_mat %>% mutate(month = rep(month.abb, rep),
                                  time = 1:nrow(women_mat),
                                  rate = (case_when(month == month.abb[1] ~ 0.06,
                                                    month %in% month.abb[2:3] ~ 0.02,
                                                    month %in% month.abb[4:8] ~ 0,
                                                    month == month.abb[9] ~ 0.04,
                                                    month == month.abb[10] ~ 0.08,
                                                    month %in% month.abb[11:12] ~ 0.18)))

women_mat <- apply(as.matrix(women_mat), c(1, 2), as.numeric)

# create data frame for monthly births in scotland from 2012 and extrapolate 
birth_data <- read_excel("./data/births-time-series-22-bt.3.xlsx", skip = 3) %>% 
  head(-7) %>% 
  select(1:14) %>% 
  select(-Total) %>% 
  rename(year = Year) %>% 
  pivot_longer(cols = `Jan`:`Dec`, names_to = "month", values_to = "births") %>% 
  filter(year >= 2012) %>% 
  mutate(year = as.numeric(year),
         yearmon = as.yearmon(paste(year, month, sep = "_"), "%Y_%b"),
         date = as.Date(yearmon)) %>% 
  right_join(rate_reference %>% select(-c(rate, level)) %>% filter(year <= 2029) %>% distinct()) %>%
  # extrapolate births beyond 2022 (linear regression)
  mutate(births = ifelse(is.na(births), -8.131*time + 4903.856, births))

# scottish population data by age
population <- read_excel("./data/mid-year-pop-est-22-data.xlsx", sheet = "Table 1", skip = 3) %>% 
  filter(`Area name` == "Scotland",
         Sex == "Persons") %>% 
  select(1:10) %>% 
  select(-`All ages`) %>% 
  pivot_longer(cols = `0`:`4`, names_to = "age_year", values_to = "population") %>% 
  mutate(age = ifelse(age_year == 0, "<1", "1-4")) %>% 
  group_by(age) %>% 
  summarise(population = sum(population))

# But all data into a list
data <- list(women_mat)

# -------------------------------------------------------------------------

# beginning of model function

model_function <- function(lambda, theta, omega, alpha, data){
  # get everything out of the data list
  women <- data[[1]]
  
  # subject rate to disruption factor lambda 
  women[243:255, "rate"] <-  women[243:255, "rate"] * lambda
  
  # initial state
  women[1, "susceptible_naive"] <- 1000000
  
  # run model
  # NEEDS TO BE CALCUATED
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
    mutate(infection = as.numeric(infection)) %>% 
    left_join(readRDS("./output/data/dates.rds")) # NONO
  
  ## data preparation
  
  # create reference table for rates to join
  rate_reference <- women.long %>% 
    select(time, month, year, yearmon, date, rate, infection) %>% 
    rename(level = infection) %>% 
    filter(time <= 12*75, time > 12*57) %>% 
    mutate(month = factor(month, levels = month.abb),
           time = rep(1:n_distinct(time), each = 25)) %>% 
    # adding extra years to model beyond 2029
    bind_rows(data.frame(level = rep(c(1:25), 48), 
                         time = rep(c(217:(217 + 12*4 - 1)), each = 25),
                         month = rep(month.abb, 4, each = 25),
                         year = rep(2029:2032, each = 12*25)) %>% 
                mutate(yearmon = as.yearmon(paste(year, month, sep = "_"), "%Y_%b"),
                       date = as.Date(yearmon),
                       rate = (case_when(month == month.abb[1] ~ 0.06,
                                         month %in% month.abb[2:3] ~ 0.02,
                                         month %in% month.abb[4:8] ~ 0.000,
                                         month == month.abb[9] ~ 0.04,
                                         month == month.abb[10] ~ 0.08,
                                         month %in% month.abb[11:12] ~ 0.18))))
  
  
  
  # create 10 years of infection history/immunity split
  immunity_split <- women.long %>%
    filter(time <= 12*75, time > 12*57) %>% 
    mutate(month = factor(month, levels = month.abb)) %>% 
    rename(level = infection) %>% 
    group_by(time, month, year, yearmon, date, rate, level) %>% 
    summarise(across(c("count", "proportion"), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(time = rep(1:n_distinct(time), each = 25)) %>% 
    # combining with birth data
    select(-count) %>% 
    left_join(birth_data) %>% 
    mutate(susceptible = proportion * births)
  
  ## model babies
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
      # option: linear
      mutate(prob_inf = theta * level) %>%
      # option: linear
      # introduce waning immunity to probability of infection
      mutate(waning = omega * time_birth + 1) %>%
      # introduce aging for disease progression
      mutate(aging = alpha * time_birth + 1) %>%
      # determine initial number of infected and disease
      mutate(infected = susceptible * rate * (1 - (1 - prob_inf) * waning),
             disease = susceptible * rate * (1 - (1 - prob_inf) * waning) * aging) %>% 
      group_by(level) %>% 
      nest()
    
    for(lev in 1:25){
      subdata <- data[[2]][[lev]]
      
      for(row in 1:nrow(subdata)){
        subdata[row, "infected"] <- subdata[row, "susceptible"] * subdata[row, "rate"] * (1 - ((1 - subdata[row, "prob_inf"]) * subdata[row, "waning"]))
        subdata[row, "disease"] <- subdata[row, "susceptible"] * subdata[row, "rate"] * (1 - ((1 - subdata[row, "prob_inf"]) * subdata[row, "waning"])) * subdata[row, "aging"]
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
  
  ## convert to desired output form
  output <- model_continuous_births %>% 
    unnest() %>% 
    select(-time) %>% 
    mutate(age = case_when(time_birth  < 12 ~ "<1",
                           time_birth >= 12 & time_birth  <= 48 ~ "1-4")) %>%
    group_by(time_calendar, month, year, yearmon, date, rate, age) %>% 
    summarise(infected = sum(infected),
              disease = sum(disease)) %>% 
    ungroup() %>% 
    pivot_longer(cols = c("infected", "disease"), names_to = "type", values_to = "count") %>%
    left_join(population) %>% 
    mutate(rate = (count/population)*100000)
  
  return(output)
  
}
