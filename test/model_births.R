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

# load data and calculate over 2yrs, under 2yrs, pregnancy trimester counts and proportions
data <- read_excel("./data/births_pregnancies.xlsx", sheet = "All") %>%
  mutate(year_month = paste(year, month, sep = "_"),
         date = as.Date(as.yearmon(`year_month`, "%Y_%b")),) %>%
  select(year, month, date, births) %>% 
  mutate(rate = case_when(month == month.abb[1] ~ 0.015,
                          month %in% month.abb[2:3] ~ 0.005,
                          month %in% month.abb[4:8] ~ 0.000,
                          month == month.abb[9] ~ 0.010,
                          month == month.abb[10] ~ 0.020,
                          month %in% month.abb[11:12] ~ 0.045))

# testing for one month of births (first 3 years of life)
rep <- 3 # number of years

births <- as.data.frame(matrix(NA, 12*rep, 5))
colnames(births) <- c("time", "month", "rate", "susceptible", "infected")

births <- births %>% mutate(month = rep(month.abb, rep),
                          time = 1:nrow(births),
                          # rate = 0.05,
                          # rate = ifelse(month %in% c(month.abb[11:12], month.abb[1:3]), 0.10, 0.05),
                          rate = case_when(month == month.abb[1] ~ 0.015,
                                           month %in% month.abb[2:3] ~ 0.005,
                                           month %in% month.abb[4:8] ~ 0.000,
                                           month == month.abb[9] ~ 0.010,
                                           month == month.abb[10] ~ 0.020,
                                           month %in% month.abb[11:12] ~ 0.045))

births[1, "susceptible"] <- 61942

for(row in 1:nrow(births)){
  births[row, "infected"] <- births[row, "susceptible"] * births[row, "rate"]
  births[row + 1, "susceptible"] <- births[row, "susceptible"] - births[row, "infected"]
}

births <- births %>% 
  mutate(cum_sum = cumsum(infected))

births %>% 
  ggplot() +
  geom_line(aes(x = time, y = cum_sum)) +
  scale_x_continuous(breaks = seq(1, nrow(births), 6), labels = c(rep(c("January", "July"), rep), "January")) +
  theme_bw() +
  labs(x = "",
       y = "Cumulative Infections")
  
# data <- read_excel("./data/births_pregnancies.xlsx", sheet = "All") %>%
#   mutate(year_month = paste(year, month, sep = "_"),
#          date = as.Date(as.yearmon(`year_month`, "%Y_%b"))) %>%
#   select(year, month, date, births) %>% 
#   mutate(sum_births = cumsum(replace_na(births, 0)),
#          over2yr = replace_na(lag(sum_births, 24), 0),
#          under2yr = sum_births - over2yr) %>% 
#   mutate(births.1 = lead(births, 1),
#          births.2 = lead(births, 2),
#          births.3 = lead(births, 3),
#          births.4 = lead(births, 4),
#          births.5 = lead(births, 5),
#          births.6 = lead(births, 6),
#          births.7 = lead(births, 7),
#          births.8 = lead(births, 8),
#          births.9 = lead(births, 9),
#          preg_1sttri = rowSums(across(`births.7`:`births.9`), na.rm = TRUE),
#          preg_2ndtri = rowSums(across(`births.4`:`births.6`), na.rm = TRUE),
#          preg_3rdtri = rowSums(across(`births.1`:`births.3`), na.rm = TRUE)) %>% 
#   select(-c("births.1":"births.9")) %>% 
#   mutate(sum_preg = rowSums(across(`preg_1sttri`:`preg_3rdtri`), na.rm = TRUE),
#          prop_1sttri = preg_1sttri/sum_preg,
#          prop_2ndtri = preg_2ndtri/sum_preg,
#          prop_3rdtri = preg_3rdtri/sum_preg)

# plot births and pregnancies
data %>% 
  ggplot() +
  geom_line(aes(x = date, y = under2yr), colour = "black") +
  scale_x_date(date_breaks = "1 year",
               labels = date_format("%Y %b"),
               limits = as.Date(c('2011-01-01','2023-01-01'))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1 )) +
  labs(x = "Date",
       y = "Count")

