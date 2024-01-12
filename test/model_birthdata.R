setwd("~/Desktop/PhD")

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
data <- read_excel("births_pregnancies.xlsx", sheet = "All") %>%
  mutate(year_month = paste(year, month, sep = "_"),
         date = as.Date(as.yearmon(`year_month`, "%Y_%b"))) %>%
  select(year, month, date, births) %>% 
  mutate(sum_births = cumsum(replace_na(births, 0)),
         over2yr = replace_na(lag(sum_births, 24), 0),
         under2yr = sum_births - over2yr) %>% 
  mutate(births.1 = lead(births, 1),
         births.2 = lead(births, 2),
         births.3 = lead(births, 3),
         births.4 = lead(births, 4),
         births.5 = lead(births, 5),
         births.6 = lead(births, 6),
         births.7 = lead(births, 7),
         births.8 = lead(births, 8),
         births.9 = lead(births, 9),
         preg_1sttri = rowSums(across(`births.7`:`births.9`), na.rm = TRUE),
         preg_2ndtri = rowSums(across(`births.4`:`births.6`), na.rm = TRUE),
         preg_3rdtri = rowSums(across(`births.1`:`births.3`), na.rm = TRUE)) %>% 
  select(-c("births.1":"births.9")) %>% 
  mutate(sum_preg = rowSums(across(`preg_1sttri`:`preg_3rdtri`), na.rm = TRUE),
         prop_1sttri = preg_1sttri/sum_preg,
         prop_2ndtri = preg_2ndtri/sum_preg,
         prop_3rdtri = preg_3rdtri/sum_preg)

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

# ODE model
# library(deSolve)
# library(reshape2)
# 
# sir_ode <- function(times, init, parms){
#   with(as.list(c(parms,init)), {
#     # ODEs
#     dS <- -beta*S*I
#     dI <- beta*S*I-gamma*I
#     dR <- gamma*I
#     list(c(dS,dI,dR))
#   })
# }
# parms <- c(beta=0.1,gamma=0.16)
# init <- c(S=61942,I=1,R=0) 
# times <- seq(0,200,length.out=2001)
# sir_out <- lsoda(init,times,sir_ode,parms)
# sir_out_long <- melt(as.data.frame(sir_out),"time")

# infection model in women, calculate infection status in 1,000,000 women based on monthly infection rate
women <- as.data.frame(matrix(NA, 192, 28))
colnames(women) <- c("time", "month", "susceptible", "rate", str_c(rep("I", 24), 1:24))

women <- women %>% mutate(month = rep(month.abb, 16),
                          time = 1:192,
                          # rate = 0.05,
                          rate = ifelse(month %in% c(month.abb[11:12], month.abb[1:3]), 0.10, 0.05))

women[1, "susceptible"] <- 1000000
women[1, "I1"] <- women[1, "susceptible"] * women[1, "rate"]

for (row in 1:23){
  women[row + 1, "susceptible"] <- women[row, "susceptible"] - women[row, "I1"]
  women[row + 1, "I1"] <- women[row + 1, "susceptible"] * women[row + 1, "rate"]
  women[row + 1, "I2"] <- women[row, "I1"]
  if(!is.na(women[row, "I2"])){
    women[row + 1, "I3"] <- women[row, "I2"]
  } else {women[row + 1, "I3"] <- NA}  
  
  if(!is.na(women[row, "I3"])){
    women[row + 1, "I4"] <- women[row, "I3"]
  } else {women[row + 1, "I4"] <- NA}
  
  if(!is.na(women[row, "I4"])){
    women[row + 1, "I5"] <- women[row, "I4"]
  } else {women[row + 1, "I5"] <- NA}
  
  if(!is.na(women[row, "I5"])){
    women[row + 1, "I6"] <- women[row, "I5"]
  } else {women[row + 1, "I6"] <- NA}
  
  if(!is.na(women[row, "I6"])){
    women[row + 1, "I7"] <- women[row, "I6"]
  } else {women[row + 1, "I7"] <- NA}
  
  if(!is.na(women[row, "I7"])){
    women[row + 1, "I8"] <- women[row, "I7"]
  } else {women[row + 1, "I8"] <- NA}
  
  if(!is.na(women[row, "I8"])){
    women[row + 1, "I9"] <- women[row, "I8"]
  } else {women[row + 1, "I9"] <- NA}
  
  if(!is.na(women[row, "I9"])){
    women[row + 1, "I10"] <- women[row, "I9"]
  } else {women[row + 1, "I10"] <- NA}
  
  if(!is.na(women[row, "I10"])){
    women[row + 1, "I11"] <- women[row, "I10"]
  } else {women[row + 1, "I11"] <- NA}
  
  if(!is.na(women[row, "I11"])){
    women[row + 1, "I12"] <- women[row, "I11"]
  } else {women[row + 1, "I12"] <- NA}
  
  if(!is.na(women[row, "I12"])){
    women[row + 1, "I13"] <- women[row, "I12"]
  } else {women[row + 1, "I13"] <- NA}
  
  if(!is.na(women[row, "I13"])){
    women[row + 1, "I14"] <- women[row, "I13"]
  } else {women[row + 1, "I14"] <- NA}
  
  if(!is.na(women[row, "I14"])){
    women[row + 1, "I15"] <- women[row, "I14"]
  } else {women[row + 1, "I15"] <- NA}
  
  if(!is.na(women[row, "I15"])){
    women[row + 1, "I16"] <- women[row, "I15"]
  } else {women[row + 1, "I16"] <- NA}
  
  if(!is.na(women[row, "I16"])){
    women[row + 1, "I17"] <- women[row, "I16"]
  } else {women[row + 1, "I17"] <- NA}
  
  if(!is.na(women[row, "I17"])){
    women[row + 1, "I18"] <- women[row, "I17"]
  } else {women[row + 1, "I18"] <- NA}
  
  if(!is.na(women[row, "I18"])){
    women[row + 1, "I19"] <- women[row, "I18"]
  } else {women[row + 1, "I19"] <- NA}
  
  if(!is.na(women[row, "I19"])){
    women[row + 1, "I20"] <- women[row, "I19"]
  } else {women[row + 1, "I20"] <- NA}
  
  if(!is.na(women[row, "I20"])){
    women[row + 1, "I21"] <- women[row, "I20"]
  } else {women[row + 1, "I21"] <- NA}
  
  if(!is.na(women[row, "I21"])){
    women[row + 1, "I22"] <- women[row, "I21"]
  } else {women[row + 1, "I22"] <- NA}
  
  if(!is.na(women[row, "I22"])){
    women[row + 1, "I23"] <- women[row, "I22"]
  } else {women[row + 1, "I23"] <- NA}
  
  if(!is.na(women[row, "I23"])){
    women[row + 1, "I24"] <- women[row, "I23"]
  } else {women[row + 1, "I24"] <- NA}
  
}

for (row in 24:nrow(women)) {
  women[row + 1, "susceptible"] <- women[row, "susceptible"] - women[row, "I1"] + women[row, "I24"]
  women[row + 1, "I1"] <- women[row + 1, "susceptible"] * women[row + 1, "rate"]
  women[row + 1, "I2"] <- women[row, "I1"]
  if(!is.na(women[row, "I2"])){
    women[row + 1, "I3"] <- women[row, "I2"]
  } else {women[row + 1, "I3"] <- NA}  
  
  if(!is.na(women[row, "I3"])){
    women[row + 1, "I4"] <- women[row, "I3"]
  } else {women[row + 1, "I4"] <- NA}
  
  if(!is.na(women[row, "I4"])){
    women[row + 1, "I5"] <- women[row, "I4"]
  } else {women[row + 1, "I5"] <- NA}
  
  if(!is.na(women[row, "I5"])){
    women[row + 1, "I6"] <- women[row, "I5"]
  } else {women[row + 1, "I6"] <- NA}
  
  if(!is.na(women[row, "I6"])){
    women[row + 1, "I7"] <- women[row, "I6"]
  } else {women[row + 1, "I7"] <- NA}
  
  if(!is.na(women[row, "I7"])){
    women[row + 1, "I8"] <- women[row, "I7"]
  } else {women[row + 1, "I8"] <- NA}
  
  if(!is.na(women[row, "I8"])){
    women[row + 1, "I9"] <- women[row, "I8"]
  } else {women[row + 1, "I9"] <- NA}
  
  if(!is.na(women[row, "I9"])){
    women[row + 1, "I10"] <- women[row, "I9"]
  } else {women[row + 1, "I10"] <- NA}
  
  if(!is.na(women[row, "I10"])){
    women[row + 1, "I11"] <- women[row, "I10"]
  } else {women[row + 1, "I11"] <- NA}
  
  if(!is.na(women[row, "I11"])){
    women[row + 1, "I12"] <- women[row, "I11"]
  } else {women[row + 1, "I12"] <- NA}
  
  if(!is.na(women[row, "I12"])){
    women[row + 1, "I13"] <- women[row, "I12"]
  } else {women[row + 1, "I13"] <- NA}
  
  if(!is.na(women[row, "I13"])){
    women[row + 1, "I14"] <- women[row, "I13"]
  } else {women[row + 1, "I14"] <- NA}
  
  if(!is.na(women[row, "I14"])){
    women[row + 1, "I15"] <- women[row, "I14"]
  } else {women[row + 1, "I15"] <- NA}
  
  if(!is.na(women[row, "I15"])){
    women[row + 1, "I16"] <- women[row, "I15"]
  } else {women[row + 1, "I16"] <- NA}
  
  if(!is.na(women[row, "I16"])){
    women[row + 1, "I17"] <- women[row, "I16"]
  } else {women[row + 1, "I17"] <- NA}
  
  if(!is.na(women[row, "I17"])){
    women[row + 1, "I18"] <- women[row, "I17"]
  } else {women[row + 1, "I18"] <- NA}
  
  if(!is.na(women[row, "I18"])){
    women[row + 1, "I19"] <- women[row, "I18"]
  } else {women[row + 1, "I19"] <- NA}
  
  if(!is.na(women[row, "I19"])){
    women[row + 1, "I20"] <- women[row, "I19"]
  } else {women[row + 1, "I20"] <- NA}
  
  if(!is.na(women[row, "I20"])){
    women[row + 1, "I21"] <- women[row, "I20"]
  } else {women[row + 1, "I21"] <- NA}
  
  if(!is.na(women[row, "I21"])){
    women[row + 1, "I22"] <- women[row, "I21"]
  } else {women[row + 1, "I22"] <- NA}
  
  if(!is.na(women[row, "I22"])){
    women[row + 1, "I23"] <- women[row, "I22"]
  } else {women[row + 1, "I23"] <- NA}
  
  if(!is.na(women[row, "I23"])){
    women[row + 1, "I24"] <- women[row, "I23"]
  } else {women[row + 1, "I24"] <- NA}
  
}

women.long <- women %>% 
  mutate(sum = rowSums(across(c("susceptible", "I1":"I24")), na.rm = TRUE)) %>% 
  pivot_longer(c("I1":"I24", "susceptible"), names_to = "infection", values_to = "count") %>% 
  mutate(infection = factor(infection, levels = c("susceptible", str_c(rep("I", 24), 1:24))),
         proportion = count/sum)

# plot infection status in women
women.long %>% 
  ggplot() +
  geom_bar(aes(x = time, y = proportion, fill = infection), position = "fill", stat = "identity") +
  scale_fill_manual(values = c("grey", viridis(24))) +
  scale_x_continuous(breaks = seq(1, 193, 6), labels = c(rep(c("January", "July"), 16), "January")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Time",
       y = "Proportion",
       fill = "Infection Status")

# women.long %>%
#   plot_ly() %>%
#   add_trace(x = ~time,
#             y = ~proportion,
#             type = "bar",
#             color = ~infection) %>%
#   layout(barmode = "stack",
#          legend = list(traceorder = "normal"))

# disrupt infection rate
women.disrupt <- women
women.disrupt[75:87, "rate"] <- 0
women.disrupt[, c("susceptible", str_c(rep("I", 24), 1:24))] <- NA

women.disrupt[1, "susceptible"] <- 1000000
women.disrupt[1, "I1"] <- women.disrupt[1, "susceptible"] * women.disrupt[1, "rate"]

for (row in 1:23){
  women.disrupt[row + 1, "susceptible"] <- women.disrupt[row, "susceptible"] - women.disrupt[row, "I1"]
  women.disrupt[row + 1, "I1"] <- women.disrupt[row + 1, "susceptible"] * women.disrupt[row + 1, "rate"]
  women.disrupt[row + 1, "I2"] <- women.disrupt[row, "I1"]
  if(!is.na(women.disrupt[row, "I2"])){
    women.disrupt[row + 1, "I3"] <- women.disrupt[row, "I2"]
  } else {women.disrupt[row + 1, "I3"] <- NA}  
  
  if(!is.na(women.disrupt[row, "I3"])){
    women.disrupt[row + 1, "I4"] <- women.disrupt[row, "I3"]
  } else {women.disrupt[row + 1, "I4"] <- NA}
  
  if(!is.na(women.disrupt[row, "I4"])){
    women.disrupt[row + 1, "I5"] <- women.disrupt[row, "I4"]
  } else {women.disrupt[row + 1, "I5"] <- NA}
  
  if(!is.na(women.disrupt[row, "I5"])){
    women.disrupt[row + 1, "I6"] <- women.disrupt[row, "I5"]
  } else {women.disrupt[row + 1, "I6"] <- NA}
  
  if(!is.na(women.disrupt[row, "I6"])){
    women.disrupt[row + 1, "I7"] <- women.disrupt[row, "I6"]
  } else {women.disrupt[row + 1, "I7"] <- NA}
  
  if(!is.na(women.disrupt[row, "I7"])){
    women.disrupt[row + 1, "I8"] <- women.disrupt[row, "I7"]
  } else {women.disrupt[row + 1, "I8"] <- NA}
  
  if(!is.na(women.disrupt[row, "I8"])){
    women.disrupt[row + 1, "I9"] <- women.disrupt[row, "I8"]
  } else {women.disrupt[row + 1, "I9"] <- NA}
  
  if(!is.na(women.disrupt[row, "I9"])){
    women.disrupt[row + 1, "I10"] <- women.disrupt[row, "I9"]
  } else {women.disrupt[row + 1, "I10"] <- NA}
  
  if(!is.na(women.disrupt[row, "I10"])){
    women.disrupt[row + 1, "I11"] <- women.disrupt[row, "I10"]
  } else {women.disrupt[row + 1, "I11"] <- NA}
  
  if(!is.na(women.disrupt[row, "I11"])){
    women.disrupt[row + 1, "I12"] <- women.disrupt[row, "I11"]
  } else {women.disrupt[row + 1, "I12"] <- NA}
  
  if(!is.na(women.disrupt[row, "I12"])){
    women.disrupt[row + 1, "I13"] <- women.disrupt[row, "I12"]
  } else {women.disrupt[row + 1, "I13"] <- NA}
  
  if(!is.na(women.disrupt[row, "I13"])){
    women.disrupt[row + 1, "I14"] <- women.disrupt[row, "I13"]
  } else {women.disrupt[row + 1, "I14"] <- NA}
  
  if(!is.na(women.disrupt[row, "I14"])){
    women.disrupt[row + 1, "I15"] <- women.disrupt[row, "I14"]
  } else {women.disrupt[row + 1, "I15"] <- NA}
  
  if(!is.na(women.disrupt[row, "I15"])){
    women.disrupt[row + 1, "I16"] <- women.disrupt[row, "I15"]
  } else {women.disrupt[row + 1, "I16"] <- NA}
  
  if(!is.na(women.disrupt[row, "I16"])){
    women.disrupt[row + 1, "I17"] <- women.disrupt[row, "I16"]
  } else {women.disrupt[row + 1, "I17"] <- NA}
  
  if(!is.na(women.disrupt[row, "I17"])){
    women.disrupt[row + 1, "I18"] <- women.disrupt[row, "I17"]
  } else {women.disrupt[row + 1, "I18"] <- NA}
  
  if(!is.na(women.disrupt[row, "I18"])){
    women.disrupt[row + 1, "I19"] <- women.disrupt[row, "I18"]
  } else {women.disrupt[row + 1, "I19"] <- NA}
  
  if(!is.na(women.disrupt[row, "I19"])){
    women.disrupt[row + 1, "I20"] <- women.disrupt[row, "I19"]
  } else {women.disrupt[row + 1, "I20"] <- NA}
  
  if(!is.na(women.disrupt[row, "I20"])){
    women.disrupt[row + 1, "I21"] <- women.disrupt[row, "I20"]
  } else {women.disrupt[row + 1, "I21"] <- NA}
  
  if(!is.na(women.disrupt[row, "I21"])){
    women.disrupt[row + 1, "I22"] <- women.disrupt[row, "I21"]
  } else {women.disrupt[row + 1, "I22"] <- NA}
  
  if(!is.na(women.disrupt[row, "I22"])){
    women.disrupt[row + 1, "I23"] <- women.disrupt[row, "I22"]
  } else {women.disrupt[row + 1, "I23"] <- NA}
  
  if(!is.na(women.disrupt[row, "I23"])){
    women.disrupt[row + 1, "I24"] <- women.disrupt[row, "I23"]
  } else {women.disrupt[row + 1, "I24"] <- NA}
  
}

for (row in 24:nrow(women.disrupt)) {
  women.disrupt[row + 1, "susceptible"] <- women.disrupt[row, "susceptible"] - women.disrupt[row, "I1"] + women.disrupt[row, "I24"]
  women.disrupt[row + 1, "I1"] <- women.disrupt[row + 1, "susceptible"] * women.disrupt[row + 1, "rate"]
  women.disrupt[row + 1, "I2"] <- women.disrupt[row, "I1"]
  if(!is.na(women.disrupt[row, "I2"])){
    women.disrupt[row + 1, "I3"] <- women.disrupt[row, "I2"]
  } else {women.disrupt[row + 1, "I3"] <- NA}  
  
  if(!is.na(women.disrupt[row, "I3"])){
    women.disrupt[row + 1, "I4"] <- women.disrupt[row, "I3"]
  } else {women.disrupt[row + 1, "I4"] <- NA}
  
  if(!is.na(women.disrupt[row, "I4"])){
    women.disrupt[row + 1, "I5"] <- women.disrupt[row, "I4"]
  } else {women.disrupt[row + 1, "I5"] <- NA}
  
  if(!is.na(women.disrupt[row, "I5"])){
    women.disrupt[row + 1, "I6"] <- women.disrupt[row, "I5"]
  } else {women.disrupt[row + 1, "I6"] <- NA}
  
  if(!is.na(women.disrupt[row, "I6"])){
    women.disrupt[row + 1, "I7"] <- women.disrupt[row, "I6"]
  } else {women.disrupt[row + 1, "I7"] <- NA}
  
  if(!is.na(women.disrupt[row, "I7"])){
    women.disrupt[row + 1, "I8"] <- women.disrupt[row, "I7"]
  } else {women.disrupt[row + 1, "I8"] <- NA}
  
  if(!is.na(women.disrupt[row, "I8"])){
    women.disrupt[row + 1, "I9"] <- women.disrupt[row, "I8"]
  } else {women.disrupt[row + 1, "I9"] <- NA}
  
  if(!is.na(women.disrupt[row, "I9"])){
    women.disrupt[row + 1, "I10"] <- women.disrupt[row, "I9"]
  } else {women.disrupt[row + 1, "I10"] <- NA}
  
  if(!is.na(women.disrupt[row, "I10"])){
    women.disrupt[row + 1, "I11"] <- women.disrupt[row, "I10"]
  } else {women.disrupt[row + 1, "I11"] <- NA}
  
  if(!is.na(women.disrupt[row, "I11"])){
    women.disrupt[row + 1, "I12"] <- women.disrupt[row, "I11"]
  } else {women.disrupt[row + 1, "I12"] <- NA}
  
  if(!is.na(women.disrupt[row, "I12"])){
    women.disrupt[row + 1, "I13"] <- women.disrupt[row, "I12"]
  } else {women.disrupt[row + 1, "I13"] <- NA}
  
  if(!is.na(women.disrupt[row, "I13"])){
    women.disrupt[row + 1, "I14"] <- women.disrupt[row, "I13"]
  } else {women.disrupt[row + 1, "I14"] <- NA}
  
  if(!is.na(women.disrupt[row, "I14"])){
    women.disrupt[row + 1, "I15"] <- women.disrupt[row, "I14"]
  } else {women.disrupt[row + 1, "I15"] <- NA}
  
  if(!is.na(women.disrupt[row, "I15"])){
    women.disrupt[row + 1, "I16"] <- women.disrupt[row, "I15"]
  } else {women.disrupt[row + 1, "I16"] <- NA}
  
  if(!is.na(women.disrupt[row, "I16"])){
    women.disrupt[row + 1, "I17"] <- women.disrupt[row, "I16"]
  } else {women.disrupt[row + 1, "I17"] <- NA}
  
  if(!is.na(women.disrupt[row, "I17"])){
    women.disrupt[row + 1, "I18"] <- women.disrupt[row, "I17"]
  } else {women.disrupt[row + 1, "I18"] <- NA}
  
  if(!is.na(women.disrupt[row, "I18"])){
    women.disrupt[row + 1, "I19"] <- women.disrupt[row, "I18"]
  } else {women.disrupt[row + 1, "I19"] <- NA}
  
  if(!is.na(women.disrupt[row, "I19"])){
    women.disrupt[row + 1, "I20"] <- women.disrupt[row, "I19"]
  } else {women.disrupt[row + 1, "I20"] <- NA}
  
  if(!is.na(women.disrupt[row, "I20"])){
    women.disrupt[row + 1, "I21"] <- women.disrupt[row, "I20"]
  } else {women.disrupt[row + 1, "I21"] <- NA}
  
  if(!is.na(women.disrupt[row, "I21"])){
    women.disrupt[row + 1, "I22"] <- women.disrupt[row, "I21"]
  } else {women.disrupt[row + 1, "I22"] <- NA}
  
  if(!is.na(women.disrupt[row, "I22"])){
    women.disrupt[row + 1, "I23"] <- women.disrupt[row, "I22"]
  } else {women.disrupt[row + 1, "I23"] <- NA}
  
  if(!is.na(women.disrupt[row, "I23"])){
    women.disrupt[row + 1, "I24"] <- women.disrupt[row, "I23"]
  } else {women.disrupt[row + 1, "I24"] <- NA}
  
}

women.disrupt.long <- women.disrupt %>% 
  mutate(sum = rowSums(across(c("susceptible", "I1":"I24")), na.rm = TRUE)) %>% 
  pivot_longer(c("I1":"I24", "susceptible"), names_to = "infection", values_to = "count") %>% 
  mutate(infection = factor(infection, levels = c("susceptible", str_c(rep("I", 24), 1:24))),
         proportion = count/sum)

# plot infection status in women
women.disrupt.long %>% 
  ggplot() +
  geom_bar(aes(x = time, y = proportion, fill = infection), position = "fill", stat = "identity") +
  geom_vline(aes(xintercept = 75), color = "red") +
  geom_vline(aes(xintercept = 87), color = "red") +
  scale_fill_manual(values = c("grey", viridis(24))) +
  scale_x_continuous(breaks = seq(1, 193, 6), labels = c(rep(c("January", "July"), 16), "January")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Time",
       y = "Proportion",
       fill = "Infection Status")

# determine average infectious status proportions from 7th year onwards
women.prop <- women.long %>% 
  filter(!time %in% c(1:72)) %>% 
  group_by(month, infection) %>% 
  summarise(across(c("count", "proportion"), mean, na.rm = TRUE))

women.prop %>% 
  filter(!is.na(month)) %>% 
  mutate(month = factor(month, levels = month.abb)) %>% 
  ggplot() + 
  geom_bar(aes(x = month, y = proportion, fill = infection), position = "fill", stat = "identity") +
  scale_fill_manual(values = c("grey", viridis(24))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Month",
       y = "Proportion",
       fill = "Infection Status")

# assign susceptibility to babies born
# grouping infection status into 3
women.prop <- women.prop %>% 
  filter(!is.na(month)) %>% 
  mutate(group = ifelse(infection %in% c("susceptible", str_c(rep("I", 8), 1:8)), 1,
                        ifelse(infection %in% str_c(rep("I", 8), 9:16), 2, 3))) %>% 
  group_by(month, group) %>% 
  summarise(proportion = sum(proportion))

infection <- data %>%
  select(c("year":"births")) %>% 
  left_join(women.prop) %>% 
  mutate(susceptibility = floor(births*proportion),
         inf_normal = ifelse(month %in% c("Nov", "Dec", "Jan", "Feb", "Mar"), 0.3, 0.05),
         inf_pandemic = ifelse(year == 2020 & month %in% c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") | year == 2021 & month %in% c("Jan", "Feb", "Mar"), 0, inf_normal)) %>% 
  mutate(inf_normal = ifelse(group == 1, inf_normal*0.5, inf_normal),
         inf_pandemic = ifelse(group == 1, inf_pandemic*0.5, inf_pandemic))
