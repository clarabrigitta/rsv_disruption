library(dplyr)
library(tidyr)
library(stringr)
library(zoo)
library(readxl)
library(purrr)
library(matrixStats)
library(data.table)

# changing create_data.R and model_function.R 

# -------------------------------------------------------------------------
# creating fixed data to use in model function with extended births february 2025 onwards

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
    # left_join(data.frame(level = rep(1:25, 228),
    #                      time = rep(1:228, each = 25))) %>%  # model 15yrs of births (2010-2024)
    mutate(season = cut(as.numeric(yearmon), 
                        breaks = as.yearmon(c("Jul 2017", "Jul 2018", "Jul 2019", "Jul 2020", "Jul 2021", "Jul 2022", "Jul 2023", "Jul 2024", "Jul 2025", "Jul 2026", "Jul 2027", "Jul 2028")), 
                        labels = c("2017-18", "2018-19", "2019-20", "2020-21", "2021-22", "2022-23", "2023-24", "2024-25", "2025-26", "2026-27", "2027-28"), 
                        right = FALSE)) %>% 
    rename(time_calendar = time)
  
  # monthly birth occurrences data (spans 1995 jan - 2025 feb)
  birth_data <- read_excel(here("data", "monthly-births-february-2025.xlsx"), sheet = "Table_3", skip = 4) %>%
    filter(`NHS Board area` == "Scotland") %>%
    filter(Year >= 1995) %>% # start at 1995 to match time/year when modelling mothers (full dataset starts 1991)
    select(-`NHS Board area`)%>% 
    rename(year = Year, month = Month, births = `Births occurring`) %>%
    mutate(yearmon = as.yearmon(paste(year, month, sep = "_"), "%Y_%B"),
           date = as.Date(yearmon),
           birth_month = as.numeric(format(as.Date(date, format = "%Y-%B-%d"), "%m"))) %>%
    arrange(yearmon)
  
  birth_average <- birth_data %>% 
    filter(year >= 2020) %>% 
    group_by(month) %>% 
    summarise(average = floor(mean(births)))

  birth_extended <- bind_rows(birth_data, 
                              dates %>% 
                                select(-c(rate, month_num)) %>% 
                                distinct() %>% filter(yearmon > "Feb 2025") %>% 
                                mutate(month = month.name[match(month, month.abb)])) %>% 
    mutate(birth_month = as.numeric(format(as.Date(date, format = "%Y-%B-%d"), "%m"))) %>% 
    left_join(birth_average, by = "month") %>% 
    mutate(births = coalesce(births, average)) %>% 
    select(-average)
  
  birth_data <- birth_extended
  
  # starting matrix for modelling women
  women_mat <- as.data.frame(matrix(0, 12*(rep+4), 5+n_interest))
  colnames(women_mat) <- c("time", "month", "rate", "susceptible_naive", "susceptible_reinf", str_c(rep("I", n_interest), 1:n_interest))
  
  women_mat <- women_mat %>% mutate(month = rep(1:12, rep+4),
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
  
  women_mat[1:408, "births"] <- birth_data[, "births"] # combining monthly birth data with women matrix, data goes up to feb 2025 but extending to dec 2028 (408)
  women_mat[1:408, "birth_month"] <- birth_data[, "birth_month"] # combining monthly birth data with women matrix, data goes up to feb 2025 but extending to dec 2028 (408)

  women_mat <- apply(as.matrix(women_mat), c(1, 2), as.numeric)
  
  # empty matrix to model babies for 4 years
  empty <- as.data.frame(matrix(0, 49, 6+n_interest))
  colnames(empty) <- c("time_calendar", "rate", "susceptible_reinf", str_c(rep("I", n_interest), 1:n_interest), "vaccinated", "births", "birth_month") # , "time_birth", "prob_inf", "waning", "aging", "susceptible", "infected", "disease")
  empty <- cbind(empty, time_birth = 1:49)
  empty <- as.matrix(empty)
  
  # vector of monthly rates
  rate_vector <- dates %>% distinct() %>% select(rate) %>% pull()
  
  # vector of levels
  level <- c(n_interest+1, 1:n_interest, 2) # change to 3 if vaccinated at month 6, otherwise 2 if vaccinated at month 7
  
  # put all data into a list
  save_data <- list(women_mat, empty, rate_vector, level)
  
  return(save_data)
}

save_data <- create_data(n_interest = duration, rep = 30, factor = combinations[[n]]$factor)

# -------------------------------------------------------------------------
# load posteriors

out <- readRDS(here("output", "data", "parameters", "15032025*", paste0("out", n, ".rds")))

posterior <- getSample(out, thin = 100)
posterior <- posterior[1:2000, ]
fixed <- matrix(combinations[[n]]$fixed[!combinations[[n]]$ind],
                nrow = nrow(posterior), 
                ncol = sum(!combinations[[n]]$ind),
                byrow = TRUE,
                dimnames = list(NULL, combinations[[n]]$name[!combinations[[n]]$ind]))
posterior <- cbind(posterior, fixed)

# -------------------------------------------------------------------------
# create functions for projection

create_projection_babies <- function(lambda, stored_data, delta, n_interest){
  
  # matrix key:
  # 1 = time
  # 2 = rate
  # 3 - 28 = infection history status
  # 29 = births
  women <- stored_data[[1]]
  
  # adding base case import rate to exposure rate
  women[, 2] <- women[, 2] + delta
  
  # subject rate to disruption factor lambda
  women[303:315, 2] <-  women[303:315, 2] * lambda # period corresponding to March 2020 - March 2021 = 315 (January 2021  = 313)
  
  # initial state
  women[1, 3] <- 1000000
  
  ## model women
  for (row in 3:nrow(women)) {
    # map_dbl(3:nrow(women),
    #     function(x) {})
    
    women[row - 1, 5] <- women[row - 2, 3] * women[row - 2, 2] + women[row - 2, 4] * women[row - 2, 2]
    women[row - 1, 3] <- women[row - 2, 3] - women[row - 2, 3] * women[row - 2, 2]
    women[row - 1, 4] <- women[row - 2, 4] - women[row - 2, 4] * women[row - 2, 2] + women[row - 2, 4+n_interest]
    women[row, c(6:(4+n_interest), 4)] <- women[row - 1, 5:(4+n_interest)]
    
  }
  
  # create link between mothers and babies (infection history/immunity split)
  women <- women[181:408, ] # selecting for 2010 onwards to model babies (15years 1995-2010 to model mothers as "burn-in")
  women[, 1] <- 1:228 # re-labeling time (modelling 2010-2028)
  
  # calculate proportion of women in each infection history status per month
  women[, 3:(4+n_interest)] <- women[, 3:(4+n_interest)]/1000000
  babies <- women[, -3] # remove susceptible_naive column since negligible in later time steps
  
  # adding vaccination compartment to babies
  babies <- cbind(
    babies[, 1:((4+n_interest) - 1), drop = FALSE],
    vaccinated = rep(0, nrow(babies)),
    babies[, (4+n_interest):ncol(babies), drop = FALSE]
  )
  
  # calculate number of babies born to vaccinated mothers based on vaccination rate (September 2024 onwards)
  babies[177:228, (4+n_interest)] <- babies[177:228, (5+n_interest)] * 0.4
  
  # calculate number of babies born with an immunity profile based on births and proportion
  babies[, 3:(3+n_interest)] <- babies[, 3:(3+n_interest)] * (babies[, 5+n_interest] - babies[, 4+n_interest])
  
  return(babies)
  
}

create_projection_birth_month <- function(lambda, theta1, theta2, omega1, omega2, alpha1, alpha2, stored_data, delta, n_interest){
  # matrix key:
  # 1 = time
  # 2 = rate
  # 3 - 28 = infection history status
  # 29 = births
  women <- stored_data[[1]]
  
  # adding base case import rate to exposure rate
  women[, 2] <- women[, 2] + delta
  
  # subject rate to disruption factor lambda
  women[303:315, 2] <-  women[303:315, 2] * lambda # period corresponding to March 2020 - March 2021 = 315 (January 2021  = 313)
  
  # initial state
  women[1, 3] <- 1000000
  
  ## model women
  for (row in 3:nrow(women)) {
    # map_dbl(3:nrow(women),
    #     function(x) {})
    
    women[row - 1, 5] <- women[row - 2, 3] * women[row - 2, 2] + women[row - 2, 4] * women[row - 2, 2]
    women[row - 1, 3] <- women[row - 2, 3] - women[row - 2, 3] * women[row - 2, 2]
    women[row - 1, 4] <- women[row - 2, 4] - women[row - 2, 4] * women[row - 2, 2] + women[row - 2, 4+n_interest]
    women[row, c(6:(4+n_interest), 4)] <- women[row - 1, 5:(4+n_interest)]
    
  }
  
  # create link between mothers and babies (infection history/immunity split)
  women <- women[181:408, ] # selecting for 2010 onwards to model babies (15years 1995-2010 to model mothers as "burn-in")
  women[, 1] <- 1:228 # re-labeling time (modelling 2010-2028)
  
  # calculate proportion of women in each infection history status per month
  women[, 3:(4+n_interest)] <- women[, 3:(4+n_interest)]/1000000
  babies <- women[, -3] # remove susceptible_naive column since negligible in later time steps
  
  # adding vaccination compartment to babies
  babies <- cbind(
    babies[, 1:((4+n_interest) - 1), drop = FALSE],
    vaccinated = rep(0, nrow(babies)),
    babies[, (4+n_interest):ncol(babies), drop = FALSE]
  )
  
  # calculate number of babies born to vaccinated mothers based on vaccination rate (September 2024 onwards)
  babies[177:228, (4+n_interest)] <- babies[177:228, (5+n_interest)] * 0.4
  
  # calculate number of babies born with an immunity profile based on births and proportion
  babies[, 3:(3+n_interest)] <- babies[, 3:(3+n_interest)] * (babies[, 5+n_interest] - babies[, 4+n_interest])
  
  ## model babies
  # matrix key:
  # 1 = time_calendar
  # 2 = rate
  # 3 - 28 = immunity levels
  # 29 = births
  # 30 = birth_month
  # 31 = time_birth
  # 32 = waning
  # 33 = aging
  # 34 = infected
  # 35 = disease
  
  # adding base case import rate to exposure rate
  stored_data[[3]] <- stored_data[[3]] + delta
  
  # apply lambda to rate vector
  stored_data[[3]][123:135] <- stored_data[[3]][123:135] * lambda # period corresponding to March 2020 - March 2021 = 135 (January 2021 = 133)
  
  data <- map(1:nrow(babies),
              function(x){
                
                subdata <- stored_data[[2]]
                subdata[1, 1:(6+n_interest)] <- babies[x, ]
                subdata[, 30] <- babies[x, 30]
                subdata[, 1] <- x:(x+12*4)
                subdata[, 2] <- stored_data[[3]][x:(x+12*4)]
                subdata <- cbind(subdata,
                                 waning = 1/(1 + exp(omega1 * (subdata[, 7+n_interest]-omega2))),
                                 aging = 1/(1 + exp(alpha1 * (subdata[, 7+n_interest]-alpha2))),
                                 infected = 0,
                                 disease = 0)
                start_inf <- 1/(1 + exp(-theta1 * (stored_data[[4]]-theta2))) # starting probability of infection given maternal immunity
                
                for(month in 2:49){
                  subdata[month, 3:(4+n_interest)] <- subdata[month - 1, 3:(4+n_interest)] # susceptible babies to next time step
                  subdata[month - 1, 3:(4+n_interest)] <- subdata[month - 1, 3:(4+n_interest)] * subdata[month - 1, 2] * (1 - ((1 - start_inf) * subdata[month - 1, n_interest+8])) # calculate number of babies infected in each immunity level
                  subdata[month - 1, n_interest+10] <- sum(subdata[month - 1, 3:(4+n_interest)]) # total number of infections at that time step
                  subdata[month, 3:(4+n_interest)] <- subdata[month, 3:(4+n_interest)] - subdata[month - 1, 3:(4+n_interest)] # deduct infected from susceptible
                  subdata[month - 1, 3:(4+n_interest)] <- subdata[month - 1, 3:(4+n_interest)] * subdata[month - 1, n_interest+9] # calculate number of babies that develop disease in each immunity level
                  subdata[month - 1, n_interest+11] <- sum(subdata[month - 1, 3:(4+n_interest)]) # total number of babies that develop disease at that time step
                }
                
                subdata <- subdata[-nrow(subdata), ]
                
                return(subdata)
                
              })
  
  data <- do.call(rbind, data) # unlist map output, keep all columns
  data <- data[data[, "time_calendar"] >= 82 & data[, "time_calendar"] <= 228, ] # keeping oct 2016 (when scottish data starts) - dec 2028
  
  return(data)
  
}

# -------------------------------------------------------------------------
# to be used if we want to calculate rates (need denominator of population)

projection_babies <- create_projection_babies(lambda = exp(-4.3),
                                              stored_data = save_data,
                                              delta = 0.0075,
                                              n_interest = duration) %>% 
  as.data.frame() %>% 
  pivot_longer(cols = c(`susceptible_reinf`:`vaccinated`), names_to = "last_exp") %>% 
  mutate(last_exp = recode(last_exp, "susceptible_reinf" = ">24", "I24" = "24", "I23" = "23", "I22" = "22", "I21" = "21", "I20" = "20", "I19" = "19", "I18" = "18",
                           "I17" = "17", "I16" = "16", "I15" = "15", "I14" = "14", "I13" = "13", "I12" = "12", "I11" = "11", "I10" = "10", "I9" = "9", "I8" = "8",
                           "I7" = "7", "I6" = "6", "I5" = "5", "I4" = "4", "I3" = "3", "I2" = "2", "I1" = "1", "vaccinated" = "V")) %>% 
  mutate(level = case_when(last_exp %in% c(4:24, ">24") ~ "no immunity",
                           last_exp %in% c(1:3, "V") ~ "immunity")) %>% 
  rename(time_calendar = time) %>% 
  left_join(dates[, c("time_calendar", "season")]) %>% 
  filter(!is.na(season)) %>% 
  group_by(season, level, births) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  group_by(season, level) %>% 
  summarise(births = sum(births),
            value = sum(value)) %>% 
  mutate(perc = value/births*100)

# -------------------------------------------------------------------------
# run model
projection_birth_month <- mclapply(1:nrow(posterior),
                             function(r){
                               output <- create_projection_birth_month(lambda = exp(posterior[r, "disruption"]),
                                                                       theta1 = posterior[r, "inf_imm1"], theta2 = posterior[r, "inf_imm2"],
                                                                       omega1 = posterior[r, "waning1"], omega2 = posterior[r, "waning2"],
                                                                       alpha1 = posterior[r, "aging1"], alpha2 = posterior[r, "aging2"],
                                                                       stored_data = save_data,
                                                                       delta = 0.0075,
                                                                       n_interest = duration)
                               output[, c(3:28, 35)] <- output[, c(3:28, 35)] * posterior[r, "detection"]
                               return(output)
                             },
                             mc.cores = 4) 

# -------------------------------------------------------------------------
# data wrangling to plot vaccination scenario (split)
  
data <- lapply(projection_birth_month, function(x) {
  as.data.frame(x) %>%
    select(-c("waning", "aging", "infected", "disease", "births", "rate")) %>% 
    pivot_longer(cols = c(`susceptible_reinf`:`vaccinated`), names_to = "last_exp") %>% 
    mutate(last_exp = recode(last_exp, "susceptible_reinf" = ">24", "I24" = "24", "I23" = "23", "I22" = "22", "I21" = "21", "I20" = "20", "I19" = "19", "I18" = "18",
                             "I17" = "17", "I16" = "16", "I15" = "15", "I14" = "14", "I13" = "13", "I12" = "12", "I11" = "11", "I10" = "10", "I9" = "9", "I8" = "8",
                             "I7" = "7", "I6" = "6", "I5" = "5", "I4" = "4", "I3" = "3", "I2" = "2", "I1" = "1", "vaccinated" = "V")) %>% 
    mutate(level = case_when(last_exp %in% c(1:24, ">24") ~ "unvaccinated",
                             last_exp == "V" ~ "vaccinated")) %>% 
    left_join(dates[, c("time_calendar", "season")]) %>% 
    filter(!is.na(season)) %>% 
    group_by(season, time_birth, level) %>% 
    summarise(disease = sum(value))
  })

split <- rbindlist(data)

split <- split %>% 
  group_by(season, time_birth, level) %>%
  summarise(mean = mean(disease),
            lower = hdi(disease)[[1]],
            upper = hdi(disease)[[2]]) %>%
  filter((level == "unvaccinated") | 
           (level == "vaccinated" & !season %in% c("2017-18", "2018-19", "2019-20", "2020-21", "2021-22", "2022-23", "2023-24"))) %>% 
  mutate(mean = ifelse(level == "vaccinated" & mean == 0, NA, mean))

# -------------------------------------------------------------------------
# data wrangling to plot vaccination scenario (total)

data <- lapply(projection_birth_month, function(x) {
  as.data.frame(x) %>%
    select(-c("waning", "aging", "infected", "disease", "births", "rate")) %>% 
    pivot_longer(cols = c(`susceptible_reinf`:`vaccinated`), names_to = "last_exp") %>% 
    left_join(dates[, c("time_calendar", "season")]) %>% 
    filter(!is.na(season)) %>% 
    group_by(season, time_birth) %>% 
    summarise(disease = sum(value))
})

total <- rbindlist(data)

total <- total %>% 
  group_by(season, time_birth) %>% 
  summarise(mean = mean(disease),
            lower = hdi(disease)[[1]],
            upper = hdi(disease)[[2]])

# -------------------------------------------------------------------------
# running no vaccination scenario

# counterfactual (no vaccination, vaccination rate = 0)
create_projection_babies <- function(lambda, stored_data, delta, n_interest){
  
  # matrix key:
  # 1 = time
  # 2 = rate
  # 3 - 28 = infection history status
  # 29 = births
  women <- stored_data[[1]]
  
  # adding base case import rate to exposure rate
  women[, 2] <- women[, 2] + delta
  
  # subject rate to disruption factor lambda
  women[303:315, 2] <-  women[303:315, 2] * lambda # period corresponding to March 2020 - March 2021 = 315 (January 2021  = 313)
  
  # initial state
  women[1, 3] <- 1000000
  
  ## model women
  for (row in 3:nrow(women)) {
    # map_dbl(3:nrow(women),
    #     function(x) {})
    
    women[row - 1, 5] <- women[row - 2, 3] * women[row - 2, 2] + women[row - 2, 4] * women[row - 2, 2]
    women[row - 1, 3] <- women[row - 2, 3] - women[row - 2, 3] * women[row - 2, 2]
    women[row - 1, 4] <- women[row - 2, 4] - women[row - 2, 4] * women[row - 2, 2] + women[row - 2, 4+n_interest]
    women[row, c(6:(4+n_interest), 4)] <- women[row - 1, 5:(4+n_interest)]
    
  }
  
  # create link between mothers and babies (infection history/immunity split)
  women <- women[181:408, ] # selecting for 2010 onwards to model babies (15years 1995-2010 to model mothers as "burn-in")
  women[, 1] <- 1:228 # re-labeling time (modelling 2010-2028)
  
  # calculate proportion of women in each infection history status per month
  women[, 3:(4+n_interest)] <- women[, 3:(4+n_interest)]/1000000
  babies <- women[, -3] # remove susceptible_naive column since negligible in later time steps
  
  # adding vaccination compartment to babies
  babies <- cbind(
    babies[, 1:((4+n_interest) - 1), drop = FALSE],
    vaccinated = rep(0, nrow(babies)),
    babies[, (4+n_interest):ncol(babies), drop = FALSE]
  )
  
  # calculate number of babies born to vaccinated mothers based on vaccination rate (September 2024 onwards)
  babies[177:228, (4+n_interest)] <- babies[177:228, (5+n_interest)] * 0
  
  # calculate number of babies born with an immunity profile based on births and proportion
  babies[, 3:(3+n_interest)] <- babies[, 3:(3+n_interest)] * (babies[, 5+n_interest] - babies[, 4+n_interest])
  
  return(babies)
  
}

create_projection_birth_month <- function(lambda, theta1, theta2, omega1, omega2, alpha1, alpha2, stored_data, delta, n_interest){
  # matrix key:
  # 1 = time
  # 2 = rate
  # 3 - 28 = infection history status
  # 29 = births
  women <- stored_data[[1]]
  
  # adding base case import rate to exposure rate
  women[, 2] <- women[, 2] + delta
  
  # subject rate to disruption factor lambda
  women[303:315, 2] <-  women[303:315, 2] * lambda # period corresponding to March 2020 - March 2021 = 315 (January 2021  = 313)
  
  # initial state
  women[1, 3] <- 1000000
  
  ## model women
  for (row in 3:nrow(women)) {
    # map_dbl(3:nrow(women),
    #     function(x) {})
    
    women[row - 1, 5] <- women[row - 2, 3] * women[row - 2, 2] + women[row - 2, 4] * women[row - 2, 2]
    women[row - 1, 3] <- women[row - 2, 3] - women[row - 2, 3] * women[row - 2, 2]
    women[row - 1, 4] <- women[row - 2, 4] - women[row - 2, 4] * women[row - 2, 2] + women[row - 2, 4+n_interest]
    women[row, c(6:(4+n_interest), 4)] <- women[row - 1, 5:(4+n_interest)]
    
  }
  
  # create link between mothers and babies (infection history/immunity split)
  women <- women[181:408, ] # selecting for 2010 onwards to model babies (15years 1995-2010 to model mothers as "burn-in")
  women[, 1] <- 1:228 # re-labeling time (modelling 2010-2028)
  
  # calculate proportion of women in each infection history status per month
  women[, 3:(4+n_interest)] <- women[, 3:(4+n_interest)]/1000000
  babies <- women[, -3] # remove susceptible_naive column since negligible in later time steps
  
  # adding vaccination compartment to babies
  babies <- cbind(
    babies[, 1:((4+n_interest) - 1), drop = FALSE],
    vaccinated = rep(0, nrow(babies)),
    babies[, (4+n_interest):ncol(babies), drop = FALSE]
  )
  
  # calculate number of babies born to vaccinated mothers based on vaccination rate (September 2024 onwards)
  babies[177:228, (4+n_interest)] <- babies[177:228, (5+n_interest)] * 0
  
  # calculate number of babies born with an immunity profile based on births and proportion
  babies[, 3:(3+n_interest)] <- babies[, 3:(3+n_interest)] * (babies[, 5+n_interest] - babies[, 4+n_interest])
  
  ## model babies
  # matrix key:
  # 1 = time_calendar
  # 2 = rate
  # 3 - 28 = immunity levels
  # 29 = births
  # 30 = birth_month
  # 31 = time_birth
  # 32 = waning
  # 33 = aging
  # 34 = infected
  # 35 = disease
  
  # adding base case import rate to exposure rate
  stored_data[[3]] <- stored_data[[3]] + delta
  
  # apply lambda to rate vector
  stored_data[[3]][123:135] <- stored_data[[3]][123:135] * lambda # period corresponding to March 2020 - March 2021 = 135 (January 2021 = 133)
  
  data <- map(1:nrow(babies),
              function(x){
                
                subdata <- stored_data[[2]]
                subdata[1, 1:(6+n_interest)] <- babies[x, ]
                subdata[, 30] <- babies[x, 30]
                subdata[, 1] <- x:(x+12*4)
                subdata[, 2] <- stored_data[[3]][x:(x+12*4)]
                subdata <- cbind(subdata,
                                 waning = 1/(1 + exp(omega1 * (subdata[, 7+n_interest]-omega2))),
                                 aging = 1/(1 + exp(alpha1 * (subdata[, 7+n_interest]-alpha2))),
                                 infected = 0,
                                 disease = 0)
                start_inf <- 1/(1 + exp(-theta1 * (stored_data[[4]]-theta2))) # starting probability of infection given maternal immunity
                
                for(month in 2:49){
                  subdata[month, 3:(4+n_interest)] <- subdata[month - 1, 3:(4+n_interest)] # susceptible babies to next time step
                  subdata[month - 1, 3:(4+n_interest)] <- subdata[month - 1, 3:(4+n_interest)] * subdata[month - 1, 2] * (1 - ((1 - start_inf) * subdata[month - 1, n_interest+8])) # calculate number of babies infected in each immunity level
                  subdata[month - 1, n_interest+10] <- sum(subdata[month - 1, 3:(4+n_interest)]) # total number of infections at that time step
                  subdata[month, 3:(4+n_interest)] <- subdata[month, 3:(4+n_interest)] - subdata[month - 1, 3:(4+n_interest)] # deduct infected from susceptible
                  subdata[month - 1, 3:(4+n_interest)] <- subdata[month - 1, 3:(4+n_interest)] * subdata[month - 1, n_interest+9] # calculate number of babies that develop disease in each immunity level
                  subdata[month - 1, n_interest+11] <- sum(subdata[month - 1, 3:(4+n_interest)]) # total number of babies that develop disease at that time step
                }
                
                subdata <- subdata[-nrow(subdata), ]
                
                return(subdata)
                
              })
  
  data <- do.call(rbind, data) # unlist map output, keep all columns
  data <- data[data[, "time_calendar"] >= 82 & data[, "time_calendar"] <= 228, ] # keeping oct 2016 (when scottish data starts) - dec 2028
  
  return(data)
  
}

# run model with no vaccination
novacc_birth_month <- mclapply(1:nrow(posterior),
                                   function(r){
                                     output <- create_projection_birth_month(lambda = exp(posterior[r, "disruption"]),
                                                                             theta1 = posterior[r, "inf_imm1"], theta2 = posterior[r, "inf_imm2"],
                                                                             omega1 = posterior[r, "waning1"], omega2 = posterior[r, "waning2"],
                                                                             alpha1 = posterior[r, "aging1"], alpha2 = posterior[r, "aging2"],
                                                                             stored_data = save_data,
                                                                             delta = 0.0075,
                                                                             n_interest = duration)
                                     output[, c(3:28, 35)] <- output[, c(3:28, 35)] * posterior[r, "detection"]
                                     return(output)
                                   },
                                   mc.cores = 4) 
# -------------------------------------------------------------------------
# data wrangling vaccination scenario to calculate cumulative disease 

vaccination <- lapply(seq_along(projection_birth_month), function(i) {
  as.data.frame(projection_birth_month[[i]])[, c("time_calendar", "time_birth", "disease")] %>%
    filter(time_birth <= 6) %>% 
    group_by(time_calendar) %>% 
    summarise(disease = sum(disease)) %>%
    filter(time_calendar >= 175) %>% 
    arrange(time_calendar) %>%
    mutate(cum = cumsum(disease),
           iter = i)
  })
vaccination <- rbindlist(vaccination)
vaccination <- rename(vaccination, disease_vacc = disease, cum_vacc = cum)

# data wrangling no vaccination scenario to calculate cumulative disease
counterfactual <- lapply(seq_along(novacc_birth_month), function(i) {
  as.data.frame(novacc_birth_month[[i]])[, c("time_calendar", "time_birth", "disease")] %>%
    filter(time_birth <= 6) %>% 
    group_by(time_calendar) %>% 
    summarise(disease = sum(disease)) %>%
    filter(time_calendar >= 175) %>% 
    arrange(time_calendar) %>%
    mutate(cum = cumsum(disease),
           iter = i)
  })
counterfactual <- rbindlist(counterfactual)
counterfactual <- rename(counterfactual, disease_novacc = disease, cum_novacc = cum)

# -------------------------------------------------------------------------
# percentage reduction
season_reduction <- cbind(vaccination, counterfactual[, -c("time_calendar", "iter")])
season_reduction <- season_reduction %>% 
  left_join(dates %>% select(-rate), by = join_by(time_calendar)) %>% 
  filter(!is.na(season)) %>% 
  group_by(season, iter) %>% 
  summarise(disease_novacc = sum(disease_novacc),
            disease_vacc = sum(disease_vacc),
            .groups = "drop") %>% 
  mutate(perc = ((disease_novacc - disease_vacc)/disease_novacc)*100) %>% 
  group_by(season) %>% 
  summarise(mean = mean(perc, na.rm = TRUE),
            lower = hdi(perc)[[1]],
            upper = hdi(perc)[[2]])