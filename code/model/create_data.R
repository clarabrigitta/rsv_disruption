library(dplyr)
library(tidyr)
library(stringr)
library(zoo)
library(readxl)
library(purrr)

# -------------------------------------------------------------------------
# creating fixed data to use in model function

# n_interest: number of months of interest for history of infection/ duration of immunity
# rep: number of years to model
# n-burn: burn-in period before disruption

create_data <- function(n_interest, rep = 30, n_burn = 20){
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
  
  # helper data frame for Scottish birth data (2012 - 2029)
  birth_data <- read_excel(here("data", "births-time-series-22-bt.3.xlsx"), skip = 3) %>%
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
    select(births) %>%
    pull()
  
  # starting matrix for modelling women
  women_mat <- as.data.frame(matrix(0, 12*rep, 5+n_interest))
  colnames(women_mat) <- c("time", "month", "rate", "susceptible_naive", "susceptible_reinf", str_c(rep("I", n_interest), 1:n_interest))
  
  women_mat <- women_mat %>% mutate(month = rep(1:12, rep),
                                    time = 1:nrow(women_mat),
                                    rate = case_when(month == 1 ~ 0.06,
                                                     month == 2 | month == 3 ~ 0.02,
                                                     month >= 4 & month <= 8 ~ 0.000,
                                                     month == 9 ~ 0.04,
                                                     month == 10 ~ 0.08,
                                                     month == 11 | month == 12 ~ 0.18),
                                    births = 0) %>%
    select(-month)
  
  women_mat[145:360, "births"] <- birth_data # combining monthly birth data with women matrix
  
  women_mat <- apply(as.matrix(women_mat), c(1, 2), as.numeric)
  
  # vector for population size of <1 and 1-4 in Scotland
  population <- read_excel(here("data", "mid-year-pop-est-22-data.xlsx"), sheet = "Table 1", skip = 3) %>%
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
  empty <- as.data.frame(matrix(0, 48, 4+n_interest))
  colnames(empty) <- c("time_calendar", "rate", "susceptible_reinf", str_c(rep("I", n_interest), 1:n_interest), "births") # , "time_birth", "prob_inf", "waning", "aging", "susceptible", "infected", "disease")
  empty <- cbind(empty, time_birth = 1:48)
  empty <- as.matrix(empty)
  
  # vector of monthly rates
  rate_vector <- dates %>% select(-level) %>% distinct() %>% select(rate) %>% pull()
  
  # vector of levels
  level <- c(n_interest+1, 1:n_interest)
  
  # scotland counts
  # weekly rate of laboratory confirmed cases by age and pathogen aggregated to monthly
  count <- read.csv(here("data", "respiratory_age_20240515.csv")) %>%
    mutate(date = as.Date(as.character(WeekBeginning), "%Y%m%d")) %>% 
    filter(Pathogen == "Respiratory syncytial virus",
           AgeGroup %in% c("<1 years", "1-4 years")) %>% 
    rename(age = AgeGroup,
           rate = RatePer100000) %>%
    mutate(yearmon = as.yearmon(date)) %>% 
    group_by(age, yearmon) %>% 
    summarise(rate = sum(rate)) %>% 
    ungroup() %>% 
    # to calculate counts
    mutate(population = ifelse(age == "<1 years", 47186, 200551),
           count = rate / 100000 * population) %>% 
    select(count) %>% 
    round(digits = 0) %>%
    as.matrix()
  
  # put all data into a list
  save_data <- list(women_mat, empty, rate_vector, population, level, count)
  
  return(save_data)
}