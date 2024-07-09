library(dplyr)
library(tidyr)
library(stringr)
library(zoo)
library(readxl)
library(purrr)

# -------------------------------------------------------------------------
# creating fixed data to use in model function

#fixed parameters
n_interest <- 24 # number of months of interest for history of infection/ duration of immunity
rep <- 30 # number of years to model
n_burn <- 20 # burn-in period before disruption

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
empty <- as.data.frame(matrix(0, 48, 28))
colnames(empty) <- c("time_calendar", "rate", "susceptible_reinf", str_c(rep("I", n_interest), 1:n_interest), "births") # , "time_birth", "prob_inf", "waning", "aging", "susceptible", "infected", "disease")
empty <- cbind(empty, time_birth = 1:48)
empty <- as.matrix(empty)

# vector of monthly rates
rate_vector <- dates %>% select(-level) %>% distinct() %>% select(rate) %>% pull()

# vector of levels
level <- c(25, 1:24)

# put all data into a list
save_data <- list(women_mat, empty, rate_vector, population, level)

# -------------------------------------------------------------------------

# beginning of model function

model_function <- function(lambda, theta, omega, alpha, stored_data){
  
  print(Sys.time())
  
  # matrix key:
  # 1 = time
  # 2 = rate
  # 3 - 28 = infection history status
  # 29 = births
  women <- stored_data[[1]]
  
  # subject rate to disruption factor lambda 
  women[243:255, 2] <-  women[243:255, 2] * lambda
  
  # initial state
  women[1, 3] <- 1000000
  
  ## model women
  for (row in 3:nrow(women)) {
    # map_dbl(3:nrow(women),
    #     function(x) {})
    
    women[row - 1, 5] <- women[row - 2, 3] * women[row - 2, 2] + women[row - 2, 4] * women[row - 2, 2]
    women[row - 1, 3] <- women[row - 2, 3] - women[row - 2, 3] * women[row - 2, 2]
    women[row - 1, 4] <- women[row - 2, 4] - women[row - 2, 4] * women[row - 2, 2] + women[row - 2, 28]
    women[row, c(6:28, 4)] <- women[row - 1, 5:28]
    
  }
  
  # create link between mothers and babies (infection history/immunity split)
  women <- women[145:360, ] # selecting for 2012-2029 to match time period of birth data
  women[, 1] <- 1:216 # re-labeling time to match birth data
  
  # calculate proportion of women in each infection history status per month
  women[, 3:28] <- women[, 3:28]/1000000
  babies <- women[, -3] # remove susceptible_naive column since negligible in later time steps
  
  # calculate number of babies born with an immunity profile based on births and proportion
  babies[, 3:27] <- babies[, 3:27] * babies[, 28]
  
  print(Sys.time())
  
  ## model babies
  # apply lambda to rate vector
  # stored_data[[4]][99:111] <- stored_data[[4]][99:111] * lambda
  # matrix key:
  # 1 = time_calendar
  # 2 = rate
  # 3 - 27 = immunity levels
  # 28 = births
  # 29 = time_birth
  # 30 = aging
  # 31 = waning
  # 32 = infected
  # 33 = disease
  
  data <- map(1:nrow(babies),
          function(x){
            
            subdata <- empty
            subdata[1, 1:28] <- babies[x, ]
            subdata[, 1] <- x:(x+12*4-1)
            subdata[, 2] <- stored_data[[3]][x:(x+12*4-1)]
            subdata <- cbind(subdata,
                          waning = omega * subdata[, 29] + 1,
                          aging = alpha * subdata[, 29] + 1,
                          infected = 0,
                          disease = 0)
            
            for(month in 2:48){
              subdata[month, 3:27] <- subdata[month - 1, 3:27] # susceptible babies to next time step
              subdata[month - 1, 3:27] <- subdata[month - 1, 3:27] * subdata[month - 1, 2] * (1 - ((1 - (theta * stored_data[[5]])) * subdata[month - 1, 30])) # calculate number of babies infected in each immunity level
              subdata[month - 1, 32] <- sum(subdata[month - 1, 3:27]) # total number of infections at that time step
              subdata[month, 3:27] <- subdata[month, 3:27] - subdata[month - 1, 3:27] # deduct infected from susceptible
              subdata[month - 1, 3:27] <- subdata[month - 1, 3:27] * subdata[month - 1, 31] # calculate number of babies that develop disease in each immunity level
              subdata[month - 1, 33] <- sum(subdata[month - 1, 3:27]) # total number of babies that develop disease at that time step
            }
            
            return(subdata)
            
          })
  
  print(Sys.time())
  
  data <- do.call(rbind, data)[, c(1, 29, 33)] # unlist map output
  data <- cbind(data, age = 0)
  data[data[, 2] > 11, 4] <- 1 # 0 = <1, 1 = 1-4 
  data <- tapply( data[, 3], list(data[, 1],  data[, 4]), sum) # calculate counts of disease per age group per month
  data <- cbind(data,
                rate_0 = 0,
                rate_1 = 0)
  data[, 3] <- data[, 1]/stored_data[[4]][2]*100000 # calculate rates for <1
  data[, 4] <- data[, 2]/stored_data[[4]][1]*100000 # calculate rates for 1-4
  data <- cbind(data[, 3:4],
                time = 1:263)
  data <- rbind(data[58:149, c(1, 3)], data[58:149, 2:3]) # selecting times to match Scottish rate data
  
  print(Sys.time())
  
  return(data)
  
}
