# load libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)
library(viridisLite)
library(plotly)

# load data and calculate over 2yrs, under 2yrs, pregnancy trimester counts and proportions
data <- read_excel("./data/births_pregnancies.xlsx", sheet = "All") %>%
  mutate(year_month = paste(year, month, sep = "_"),
         date = as.Date(as.yearmon(`year_month`, "%Y_%b")),) %>%
  select(year, month, date, births) 
# %>% 
#   mutate(rate = case_when(month == month.abb[1] ~ 0.015,
#                           month %in% month.abb[2:3] ~ 0.005,
#                           month %in% month.abb[4:8] ~ 0.000,
#                           month == month.abb[9] ~ 0.010,
#                           month == month.abb[10] ~ 0.020,
#                           month %in% month.abb[11:12] ~ 0.045))

# testing model for one month of births (first 3 years of life) using totals
rep <- 3 # number of years
rate_scale <- 5 # scaling rate of infection to achieve 90-95% in first 3yrs of life

births <- as.data.frame(matrix(NA, 12*rep, 5))
colnames(births) <- c("time", "month", "rate", "susceptible", "infected") # use for totals only

births <- births %>% mutate(month = rep(month.abb, rep),
                          time = 1:nrow(births),
                          # rate = 0.05,
                          # rate = ifelse(month %in% c(month.abb[11:12], month.abb[1:3]), 0.10, 0.05),
                          rate = (case_when(month == month.abb[1] ~ 0.015,
                                           month %in% month.abb[2:3] ~ 0.005,
                                           month %in% month.abb[4:8] ~ 0.000,
                                           month == month.abb[9] ~ 0.010,
                                           month == month.abb[10] ~ 0.020,
                                           month %in% month.abb[11:12] ~ 0.045))*rate_scale)

# births[1, "susceptible"] <- 61942 # january
births[1, "susceptible"] <- 61920 # july

for(row in 1:nrow(births)){
  births[row, "infected"] <- births[row, "susceptible"] * births[row, "rate"]
  births[row + 1, "susceptible"] <- births[row, "susceptible"] - births[row, "infected"]
}

# plot monthly infections
births %>% 
  ggplot() +
  geom_line(aes(x = time, y = infected)) +
  scale_x_continuous(breaks = seq(1, nrow(births), 6), labels = c(rep(c("January", "July"), rep), "January")) +
  theme_bw() +
  labs(x = "",
       y = "Count")

# testing model for one month of births (first 3 years of life) using totals
rep <- 3 # number of years
rate_scale <- 5 # scaling rate of infection to achieve 90-95% in first 3yrs of life

births <- as.data.frame(matrix(NA, 12*rep, 4))
colnames(births) <- c("time", "month", "rate", "susceptible") # use when splitting into immunity levels

births <- births %>% mutate(month = rep(month.abb, rep), # january
                            # month = rep(c(month.abb[7:12], month.abb[1:6]), rep), # july
                            time = 1:nrow(births),
                            # rate = 0.05,
                            # rate = ifelse(month %in% c(month.abb[11:12], month.abb[1:3]), 0.10, 0.05),
                            rate_base = (case_when(month == month.abb[1] ~ 0.015,
                                              month %in% month.abb[2:3] ~ 0.005,
                                              month %in% month.abb[4:8] ~ 0.000,
                                              month == month.abb[9] ~ 0.010,
                                              month == month.abb[10] ~ 0.020,
                                              month %in% month.abb[11:12] ~ 0.045))*rate_scale)

births[1, "susceptible"] <- 61942 # january
# births[1, "susceptible"] <- 61920 # july

# join proportion of women to births
births <- births %>% 
  left_join(women.prop %>% select(-count)) %>% 
  # set rates based on immunity level
  mutate(rate_inf = case_when(level == 1 ~ rate_base * 0,
                          level == 2 ~ rate_base * 0.5,
                          level == 3 ~ rate_base * 0.5,
                          level == 4 ~ rate_base * 1),
         rate_dis = case_when(level == 1 ~ rate_base * 0,
                              level == 2 ~ rate_base * 0,
                              level == 3 ~ rate_base * 0.5,
                              level == 4 ~ rate_base * 1)) %>% 
  mutate(rate = rate_base,
         proportion = ifelse(month == "Jan", proportion, NA), # change based on january vs july
         susceptible_sub = susceptible * proportion,
         infected = susceptible_sub * rate) %>% 
  select(-c(susceptible, proportion)) %>% 
  group_by(level) %>% 
  nest()

for(lev in 1:4){
  subdata <- births[[2]][[lev]]
  
  for(row in 1:nrow(subdata)){
    subdata[row, "infected"] <- subdata[row, "susceptible_sub"] * subdata[row, "rate"]
    subdata[row + 1, "susceptible_sub"] <- subdata[row, "susceptible_sub"] - subdata[row, "infected"]
  }
  
  births[[2]][[lev]] <- subdata
}

births <- births %>% unnest() %>% filter(!is.na(month))

# plot monthly infections
births %>% 
  ggplot() +
  geom_line(aes(x = time, y = infected, colour = level)) +
  scale_x_continuous(breaks = seq(1, 36, 6), labels = c(rep(c("January", "July"), rep))) +
  scale_colour_viridis_d(option = "H") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "",
       y = "Count",
       colour = "Immunity Level")

births %>% 
  plot_ly() %>% 
  add_trace(x = ~time,
            y = ~infected,
            type = "scatter",
            mode = "lines",
            text = ~month,
            hovertemplate = paste('<b>Month</b>: %{text}',
                                  '<br><b>Proportion Infected</b>: %{y}',
                                  '<extra></extra>')) %>%

# reshaping for cumulative sums
births_cum <- births %>% 
  mutate(cum_sum = cumsum(infected),
         prop = (cum_sum/61942)*100)

# plot of cumulative outputs
births_cum %>% 
  ggplot() +
  geom_line(aes(x = time, y = prop)) +
  scale_x_continuous(breaks = seq(1, nrow(births), 6), labels = c(rep(c("January", "July"), rep), "January")) +
  theme_bw() +
  labs(x = "",
       y = "Proportion Infected (%)")

births_cum %>% 
  plot_ly() %>% 
  add_trace(x = ~time,
            y = ~prop,
            type = "scatter",
            mode = "lines",
            text = ~month,
            hovertemplate = paste('<b>Month</b>: %{text}',
                                  '<br><b>Proportion Infected</b>: %{y}',
                                  '<extra></extra>')) %>% 
  layout(xaxis = list(title = "Time",
                      tickmode = "array",
                      ticktext = list("January", "July", "January", "July", "January", "July", "January"),
                      tickvals = list(1, 7, 13, 19, 25, 31, 37)),
         yaxis = list(title = "Proportion Infected (%)"))

# plotting rate of infection
births %>% 
  plot_ly() %>% 
  add_trace(x = ~time,
            y = ~rate,
            type = "scatter",
            mode = "lines",
            text = ~month,
            hovertemplate = paste('<b>Month</b>: %{text}',
                                  '<br><b>Rate</b>: %{y}',
                                  '<extra></extra>')) %>% 
  layout(xaxis = list(title = "Time",
                      tickmode = "array",
                      ticktext = list("January", "July", "January", "July", "January", "July", "January"),
                      tickvals = list(1, 7, 13, 19, 25, 31, 37)),
         yaxis = list(title = "Rate of Infection"))


births %>% 
  plot_ly() %>% 
  add_trace(x = ~time, y = ~rate_base, type = "scatter", mode = "lines", linetype = ~level, color = "Base") %>%
  add_trace(x = ~time, y = ~rate_inf, type = "scatter", mode = "lines", linetype= ~level, color = "Infection") %>%
  add_trace(x = ~time, y = ~rate_dis, type = "scatter", mode = "lines", linetype = ~level, color = "Disease") %>%
  layout(xaxis = list(title = "Time",
                      tickmode = "array",
                      ticktext = list("January", "July", "January", "July", "January", "July", "January"),
                      tickvals = list(1, 7, 13, 19, 25, 31, 37)),
         yaxis = list(title = "Rate"))
  
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

