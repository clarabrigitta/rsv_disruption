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

birth_data <- as.matrix(birth_data)

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

empty <- as.matrix(empty)

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
    pivot_longer(4:29, names_to = "infection", values_to = "count") %>% 
    mutate(proportion = count/sum) %>% 
    mutate(infection = str_remove(infection, "I"),
           infection = replace(infection, infection == "susceptible_reinf", 25)) %>% 
    filter(infection %in% c(1:25)) %>% 
    mutate(infection = as.numeric(infection)) %>% 
    rename(month_num = month, level = infection) %>% 
    select(-c(sum, count)) %>% 
    # combining with birth data
    left_join(stored_data[[3]][, c("births", "time")], by = "time") %>% 
    mutate(susceptible = proportion * births) %>% 
    select(-c(proportion, births, month_num, rate)) %>% 
    arrange(level)
  
  print(Sys.time())
  
  ## model babies
  # apply lambda to rate vector
  stored_data[[6]][99:111] <- stored_data[[6]][99:111] * lambda
  
  # nesting data set to model
  births <- immunity_split %>% 
    nest(.by = time)
  
  for(n in 1:216){
    data <- births[[2]][[n]] %>% 
      nest(.by = level)
    
    for(lev in 1:25){
      subdata <- stored_data[[5]] %>% mutate(time_calendar = n:(n+12*4-1),
                                           time_birth = 1:48,
                                           level = lev,
                                           rate = stored_data[[6]][n:(n+12*4-1)],
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
    mutate(population = case_when(age == "<1" ~ stored_data[[4]][1],
                                  age == "1-4" ~ stored_data[[4]][2]),
           rate = (disease/population)*100000) %>% 
    filter(time_calendar >= 58 & time_calendar <= 149) %>% 
    arrange(age)
  
  print(Sys.time())
  
  return(output)
  
}
