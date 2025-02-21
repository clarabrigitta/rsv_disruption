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

create_data <- function(n_interest, rep = 30, factor){
  
  # helper data frame for dates and rates (without any disruption) to model babies - 2010 until 2024
  dates <- as.data.frame(matrix(NA, 12*(rep+4), 6)) # add 4 years to account for modelling children until 4 years old
  colnames(dates) <- c("time", "month", "month_num", "year", "yearmon", "date")
  
  dates <- dates %>%
    mutate(time = 1:nrow(dates),
           month = rep(month.abb, nrow(dates)/12),
           month_num = rep(1:12, rep+4),
           year = rep(c(1995:(1995+(nrow(dates)/12)-1)), each =12), # model 30 years: 1995-2024
           yearmon = as.yearmon(paste(year, month, sep = "_"), "%Y_%b"),
           date = as.Date(yearmon),
           rate = (case_when(month == month.abb[1] ~ 0.06,
                             month %in% month.abb[2:3] ~ 0.02,
                             month %in% month.abb[4:8] ~ 0,
                             month == month.abb[9] ~ 0.04,
                             month == month.abb[10] ~ 0.08,
                             month %in% month.abb[11:12] ~ 0.18))) %>% # up until here to keep track of time/year when modelling mothers
    mutate(rate = rate * factor) %>%
    filter(year >= 2010) %>%
    mutate(time = 1:n_distinct(time)) %>%
    left_join(data.frame(level = rep(1:25, 228),
                         time = rep(1:228, each = 25))) # model 15yrs of births (2010-2024)
  
  # monthly birth occurrences data (spans 1995 jan - 2024 oct)
  birth_data <- read_excel(here("data", "monthly-births-october-24-tabs.xlsx"), sheet = "Table_3", skip = 4) %>%
    filter(`NHS Board area` == "Scotland") %>%
    filter(Year >= 1995) %>% # start at 1995 to match time/year when modelling mothers (full dataset starts 1991)
    select(-c(`NHS Board area`, `Column1`)) %>%
    rename(year = Year, month = Month, births = `Births occurring`) %>%
    mutate(yearmon = as.yearmon(paste(year, month, sep = "_"), "%Y_%B"),
           date = as.Date(yearmon),
           birth_month = format(as.Date(date, format = "%Y-%B-%d"), "%m")) %>%
    arrange(yearmon)
  
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
                                    births = 0,
                                    birth_month = 0) %>%
    mutate(rate = rate * factor) %>%
    select(-month)
  
  women_mat[1:358, "births"] <- birth_data[, "births"] # combining monthly birth data with women matrix, 358 because birth data only goes up until october
  women_mat[1:358, "birth_month"] <- birth_data[, "birth_month"] # combining monthly birth data with women matrix, 358 because birth data only goes up until october
  
  women_mat <- apply(as.matrix(women_mat), c(1, 2), as.numeric)
  
  # empty matrix to model babies for 4 years
  empty <- as.data.frame(matrix(0, 48, 5+n_interest))
  colnames(empty) <- c("time_calendar", "rate", "susceptible_reinf", str_c(rep("I", n_interest), 1:n_interest), "births", "birth_month") # , "time_birth", "prob_inf", "waning", "aging", "susceptible", "infected", "disease")
  empty <- cbind(empty, time_birth = 1:48)
  empty <- as.matrix(empty)
  
  # vector of monthly rates
  rate_vector <- dates %>% select(-level) %>% distinct() %>% select(rate) %>% pull()
  
  # vector of levels
  level <- c(n_interest+1, 1:n_interest)
  
  # put all data into a list
  save_data <- list(women_mat, empty, rate_vector, level)
  
  return(save_data)
}