setwd("~/Desktop/PhD")

# load libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)

# load data
data <- read_excel("births_pregnancies.xlsx", sheet = "All") %>%
  mutate(year_month = paste(year, month, sep = "_"))

# infection <- data %>%
#   select(year, month, year_month, time, births) %>% 
#   mutate(pregnancies_3rdtri = lead(rollapply(births, 3, sum, fill = NA, align = "left"), 1),
#          percent_normal = ifelse(month %in% c("Nov", "Dec", "Jan", "Feb", "Mar"), 0.3, 0.05),
#          percent_pandemic = ifelse(year == 2020 & month %in% c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") | year == 2021 & month %in% c("Jan", "Feb", "Mar"), 0, percent_normal),
#          susceptible_pregnancies = NA,
#          infected_pregnancies = NA)
# 
# infection[12, "susceptible_pregnancies"] <- infection[12, "pregnancies_3rdtri"]
# infection[12, "infected_pregnancies"] <- infection[12, "susceptible_pregnancies"] * infection[12, "percent_pandemic"]
# 
# for (n in 13:nrow(infection)) {
#   infection[n, "susceptible_pregnancies"] <- infection[n, "pregnancies_3rdtri"] - infection[n - 1, "infected_pregnancies"] 
#   infection[n, "infected_pregnancies"] <- infection[n, "susceptible_pregnancies"] * infection[n, "percent_pandemic"]
# }

infection <- data %>%
  select(year, month, year_month, time, births) %>% 
  mutate(percent_normal = ifelse(month %in% c("Nov", "Dec", "Jan", "Feb", "Mar"), 0.3, 0.05),
         percent_pandemic = ifelse(year == 2020 & month %in% c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") | year == 2021 & month %in% c("Jan", "Feb", "Mar"), 0, percent_normal),
         sus_preg_3rdtri_1 = lead(births, 3),
         inf_preg_3rdtri_1 = floor(sus_preg_3rdtri_1 * percent_pandemic),
         sus_preg_3rdtri_2 = lag(sus_preg_3rdtri_1 - inf_preg_3rdtri_1, 1),
         inf_preg_3rdtri_2 = floor(sus_preg_3rdtri_2 * percent_pandemic),
         sus_preg_3rdtri_3 = lag(sus_preg_3rdtri_2 - inf_preg_3rdtri_2, 1),
         inf_preg_3rdtri_3 = floor(sus_preg_3rdtri_3 * percent_pandemic),
         inf_preg = rowSums(across(c("inf_preg_3rdtri_1", "inf_preg_3rdtri_2", "inf_preg_3rdtri_3")), na.rm = TRUE),
         sum_births = cumsum(replace_na(births, 0)),
         over2yr = replace_na(lag(sum_births, 24), 0),
         under2yr = sum_births - over2yr,
         protected_kids_1 = lag(inf_preg_3rdtri_1, 3),
         protected_kids_2_1 = lag(inf_preg_3rdtri_2, 2),
         protected_kids_2_2 = lag(inf_preg_3rdtri_2, 3),
         protected_kids_3_1 = lag(inf_preg_3rdtri_3, 1),
         protected_kids_3_2 = lag(inf_preg_3rdtri_3, 2),
         protected_kids_3_3 = lag(inf_preg_3rdtri_3, 3),
         protected_kids = rowSums(across(protected_kids_1:protected_kids_3_3), na.rm = TRUE),
         susceptible_kids_under2yr = under2yr - protected_kids,
         proportion_susceptible_kids_under2yr = susceptible_kids_under2yr/under2yr)

england <- data %>%
  select(year, month, year_month, time, births) %>% 
  mutate(percent_normal = ifelse(month %in% c("Nov", "Dec", "Jan", "Feb", "Mar"), 0.3, 0.05),
         percent_pandemic = case_when(year == 2020 & month %in% c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") | year == 2021 & month %in% c("Jan", "Feb", "Mar") ~ 0,
                                      # year == 2021 & month == "Apr" ~ 0.025,
                                      # year == 2021 & month == "May" ~ 0.05,
                                      # year == 2021 & month == "Jun" ~ 0.10,
                                      # year == 2021 & month == "Jul" ~ 0.20,
                                      # year == 2021 & month == "Aug" ~ 0.30,
                                      # year == 2021 & month == "Sep" ~ 0.30,
                                      # year == 2021 & month == "Oct" ~ 0.30,
                                      # year == 2021 & month == "Nov" ~ 0.20,
                                      # year == 2021 & month == "Dec" ~ 0.10,
                                      # year == 2022 & month == "Jan" ~ 0.05,
                                      # year == 2022 & month == "Feb" ~ 0.05,
                                      # year == 2022 & month == "Mar" ~ 0.05,
                                      year == 2021 & month == "Apr" ~ 0.03,
                                      year == 2021 & month == "May" ~ 0.03,
                                      year == 2021 & month == "Jun" ~ 0.03,
                                      year == 2021 & month == "Jul" ~ 0.05,
                                      year == 2021 & month == "Aug" ~ 0.05,
                                      year == 2021 & month == "Sep" ~ 0.05,
                                      year == 2021 & month == "Oct" ~ 0.03,
                                      year == 2021 & month == "Nov" ~ 0.03,
                                      year == 2021 & month == "Dec" ~ 0,
                                      year == 2022 & month == "Jan" ~ 0,
                                      year == 2022 & month == "Feb" ~ 0,
                                      year == 2022 & month == "Mar" ~ 0,
                                      year == 2022 & month == "Apr" ~ 0,
                                      year == 2022 & month == "May" ~ 0,
                                      year == 2022 & month == "Jun" ~ 0,
                                      year == 2022 & month == "Jul" ~ 0,
                                      year == 2022 & month == "Aug" ~ 0,
                                      year == 2022 & month == "Sep" ~ 0.03,
                                      year == 2022 & month == "Oct" ~ 0.05,
                                      year == 2022 & month == "Nov" ~ 0.10,
                                      year == 2022 & month == "Dec" ~ 0.05),
         percent_pandemic = round(ifelse(is.na(percent_pandemic), percent_normal, percent_pandemic), 2),
         percent_pandemic_kids = ifelse(year == 2021 & month %in% c("Aug", "Sep", "Oct", "Nov", "Dec") | year == 2022, 0.50, percent_pandemic),
         # ifelse(year == 2020 & month %in% c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") | year == 2021 & month %in% c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul"), 0, percent_normal),
         sus_preg_3rdtri_1 = lead(births, 3),
         inf_preg_3rdtri_1 = floor(sus_preg_3rdtri_1 * percent_pandemic),
         sus_preg_3rdtri_2 = lag(sus_preg_3rdtri_1 - inf_preg_3rdtri_1, 1),
         inf_preg_3rdtri_2 = floor(sus_preg_3rdtri_2 * percent_pandemic),
         sus_preg_3rdtri_3 = lag(sus_preg_3rdtri_2 - inf_preg_3rdtri_2, 1),
         inf_preg_3rdtri_3 = floor(sus_preg_3rdtri_3 * percent_pandemic),
         inf_preg = rowSums(across(c("inf_preg_3rdtri_1", "inf_preg_3rdtri_2", "inf_preg_3rdtri_3")), na.rm = TRUE),
         sum_births = cumsum(replace_na(births, 0)),
         # over2yr = replace_na(lag(sum_births, 24), 0),
         # under2yr = sum_births - over2yr,
         over1yr = replace_na(lag(sum_births, 12), 0),
         under1yr = sum_births - over1yr) %>% 
  select(-c(sus_preg_3rdtri_1, sus_preg_3rdtri_2, sus_preg_3rdtri_3)) %>% 
  mutate(protected_kids_1_3 = lag(inf_preg_3rdtri_1, 3),
         protected_kids_1_4 = lag(inf_preg_3rdtri_1, 4),
         protected_kids_1_5 = lag(inf_preg_3rdtri_1, 5),
         protected_kids_1_6 = lag(inf_preg_3rdtri_1, 6),
         protected_kids_2_2 = lag(inf_preg_3rdtri_2, 2),
         protected_kids_2_3 = lag(inf_preg_3rdtri_2, 3),
         protected_kids_2_4 = lag(inf_preg_3rdtri_2, 4),
         protected_kids_2_5 = lag(inf_preg_3rdtri_2, 5),
         protected_kids_2_6 = lag(inf_preg_3rdtri_2, 6),
         protected_kids_3_1 = lag(inf_preg_3rdtri_3, 1),
         protected_kids_3_2 = lag(inf_preg_3rdtri_3, 2),
         protected_kids_3_3 = lag(inf_preg_3rdtri_3, 3),
         protected_kids_3_4 = lag(inf_preg_3rdtri_3, 4),
         protected_kids_3_5 = lag(inf_preg_3rdtri_3, 5),
         protected_kids_3_6 = lag(inf_preg_3rdtri_3, 6),
         protected_kids = rowSums(across(protected_kids_1_3:protected_kids_3_6), na.rm = TRUE),
         # susceptible_kids_under2yr = under2yr - protected_kids,
         # proportion_susceptible_kids_under2yr = susceptible_kids_under2yr/under2yr,
         susceptible_kids_under1yr = under1yr - protected_kids,
         proportion_susceptible_kids_under1yr = susceptible_kids_under1yr/under1yr) %>% 
  select(-c(inf_preg_3rdtri_1:inf_preg_3rdtri_3, protected_kids_1_3:protected_kids_3_6)) %>%
  mutate(# infected_kids_under2yr = susceptible_kids_under2yr * percent_pandemic_kids,
         infected_kids_under1yr = susceptible_kids_under1yr * percent_pandemic_kids)

# england[13, "susceptible_kids_under1yr"] <- england[13, "under1yr"] - england[13, "protected_kids"]
# england[13, "infected_kids_under1yr"] <- floor(england[13, "susceptible_kids_under1yr"] * england[13, "percent_pandemic_kids"])
# 
# for (n in 14:nrow(england)) {
#   england[n, "susceptible_kids_under1yr"] <- england[n, "susceptible_kids_under1yr"] - england[n - 1, "infected_kids_under1yr"]
#   england[n, "infected_kids_under1yr"] <- england[n, "susceptible_kids_under1yr"] * england[n, "percent_pandemic_kids"]
# }

# figures
# create x-axis labels
label <- data %>% select(year_month) %>% slice(seq(1, 156, 12)) %>% pull()

england %>% 
ggplot() +
  geom_line(aes(x = time, y = births), colour = "black") +
  # geom_line(aes(x = time, y = protected_kids), colour = "green") +
  # geom_line(aes(x = time, y = susceptible_kids_under1yr), colour = "blue") +
  # geom_line(aes(x = time, y = infected_kids_under1yr), colour = "red") +
  scale_x_continuous(breaks = seq(1, 156, 12), labels = label) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1 )) +
  labs(x = "Time",
       y = "Count")

england %>%
  filter(!year == 2023) %>% 
  pivot_longer(c(inf_preg, protected_kids, susceptible_kids_under1yr, infected_kids_under1yr), names_to = "category") %>% 
  ggplot() +
  geom_line(aes(x = time, y = value, group = category, colour = category)) +
  scale_x_continuous(breaks = seq(1, 156, 12), labels = label) +
  scale_color_manual(name = "Category", breaks = c("inf_preg", "protected_kids", "susceptible_kids_under1yr", "infected_kids_under1yr"), labels = c("inf_preg" = "Infected Pregnant Mothers", "protected_kids" = "Protected Children", "susceptible_kids_under1yr" = "Susceptible Children <1yr", "infected_kids_under1yr" = "Infected Children <1yr"), values = c("black", "green", "blue", "red")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1 )) +
  labs(x = "Time",
       y = "Count")

#### SCOTLAND BIRTH DATA

scotland <- read_excel("monthly-births-october-23.xlsx", sheet = "r") %>% 
  group_by(Year, Month) %>% 
  summarise(births = sum(`Births registered`)) %>% 
  ungroup() %>% 
  filter(Year >= 2011) %>% 
  dplyr::rename(year = Year,
         month = Month) %>% 
  mutate(month = factor(month, levels = month.name, labels = month.abb),
         year_month = paste(year, month, sep = "_"),
         date = as.Date(as.yearmon(`year_month`, "%Y_%B"))) %>% 
  mutate(percent_normal = ifelse(month %in% c("Nov", "Dec", "Jan", "Feb", "Mar"), 0.3, 0.05),
         percent_pandemic = case_when(year == 2020 & month %in% c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") | year == 2021 & month %in% c("Jan", "Feb", "Mar") ~ 0,
                                      # year == 2021 & month == "Apr" ~ 0.025,
                                      # year == 2021 & month == "May" ~ 0.05,
                                      # year == 2021 & month == "Jun" ~ 0.10,
                                      # year == 2021 & month == "Jul" ~ 0.20,
                                      # year == 2021 & month == "Aug" ~ 0.30,
                                      # year == 2021 & month == "Sep" ~ 0.30,
                                      # year == 2021 & month == "Oct" ~ 0.30,
                                      # year == 2021 & month == "Nov" ~ 0.20,
                                      # year == 2021 & month == "Dec" ~ 0.10,
                                      # year == 2022 & month == "Jan" ~ 0.05,
                                      # year == 2022 & month == "Feb" ~ 0.05,
                                      # year == 2022 & month == "Mar" ~ 0.05,
                                      year == 2021 & month == "Apr" ~ 0.03,
                                      year == 2021 & month == "May" ~ 0.03,
                                      year == 2021 & month == "Jun" ~ 0.03,
                                      year == 2021 & month == "Jul" ~ 0.05,
                                      year == 2021 & month == "Aug" ~ 0.05,
                                      year == 2021 & month == "Sep" ~ 0.05,
                                      year == 2021 & month == "Oct" ~ 0.03,
                                      year == 2021 & month == "Nov" ~ 0.03,
                                      year == 2021 & month == "Dec" ~ 0,
                                      year == 2022 & month == "Jan" ~ 0,
                                      year == 2022 & month == "Feb" ~ 0,
                                      year == 2022 & month == "Mar" ~ 0,
                                      year == 2022 & month == "Apr" ~ 0,
                                      year == 2022 & month == "May" ~ 0,
                                      year == 2022 & month == "Jun" ~ 0,
                                      year == 2022 & month == "Jul" ~ 0,
                                      year == 2022 & month == "Aug" ~ 0,
                                      year == 2022 & month == "Sep" ~ 0.03,
                                      year == 2022 & month == "Oct" ~ 0.05,
                                      year == 2022 & month == "Nov" ~ 0.10,
                                      year == 2022 & month == "Dec" ~ 0.05),
         percent_pandemic = round(ifelse(is.na(percent_pandemic), percent_normal, percent_pandemic), 2),
         percent_pandemic_kids = ifelse(year == 2021 & month %in% c("Aug", "Sep", "Oct", "Nov", "Dec") | year == 2022, 0.50, percent_pandemic),
         # ifelse(year == 2020 & month %in% c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") | year == 2021 & month %in% c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul"), 0, percent_normal),
         sus_preg_3rdtri_1 = lead(births, 3),
         inf_preg_3rdtri_1 = floor(sus_preg_3rdtri_1 * percent_pandemic),
         sus_preg_3rdtri_2 = lag(sus_preg_3rdtri_1 - inf_preg_3rdtri_1, 1),
         inf_preg_3rdtri_2 = floor(sus_preg_3rdtri_2 * percent_pandemic),
         sus_preg_3rdtri_3 = lag(sus_preg_3rdtri_2 - inf_preg_3rdtri_2, 1),
         inf_preg_3rdtri_3 = floor(sus_preg_3rdtri_3 * percent_pandemic),
         inf_preg = rowSums(across(c("inf_preg_3rdtri_1", "inf_preg_3rdtri_2", "inf_preg_3rdtri_3")), na.rm = TRUE),
         sum_births = cumsum(replace_na(births, 0)),
         # over2yr = replace_na(lag(sum_births, 24), 0),
         # under2yr = sum_births - over2yr,
         over1yr = replace_na(lag(sum_births, 12), 0),
         under1yr = sum_births - over1yr) %>% 
  select(-c(sus_preg_3rdtri_1, sus_preg_3rdtri_2, sus_preg_3rdtri_3)) %>%
  mutate(protected_kids_1_3 = lag(inf_preg_3rdtri_1, 3),
         protected_kids_1_4 = lag(inf_preg_3rdtri_1, 4),
         protected_kids_1_5 = lag(inf_preg_3rdtri_1, 5),
         protected_kids_1_6 = lag(inf_preg_3rdtri_1, 6),
         protected_kids_2_2 = lag(inf_preg_3rdtri_2, 2),
         protected_kids_2_3 = lag(inf_preg_3rdtri_2, 3),
         protected_kids_2_4 = lag(inf_preg_3rdtri_2, 4),
         protected_kids_2_5 = lag(inf_preg_3rdtri_2, 5),
         protected_kids_2_6 = lag(inf_preg_3rdtri_2, 6),
         protected_kids_3_1 = lag(inf_preg_3rdtri_3, 1),
         protected_kids_3_2 = lag(inf_preg_3rdtri_3, 2),
         protected_kids_3_3 = lag(inf_preg_3rdtri_3, 3),
         protected_kids_3_4 = lag(inf_preg_3rdtri_3, 4),
         protected_kids_3_5 = lag(inf_preg_3rdtri_3, 5),
         protected_kids_3_6 = lag(inf_preg_3rdtri_3, 6),
         protected_kids = rowSums(across(protected_kids_1_3:protected_kids_3_6), na.rm = TRUE),
         # susceptible_kids_under2yr = under2yr - protected_kids,
         # proportion_susceptible_kids_under2yr = susceptible_kids_under2yr/under2yr,
         susceptible_kids_under1yr = under1yr - protected_kids,
         proportion_susceptible_kids_under1yr = susceptible_kids_under1yr/under1yr) %>% 
  select(-c(inf_preg_3rdtri_1:inf_preg_3rdtri_3, protected_kids_1_3:protected_kids_3_6)) %>%
  mutate(# infected_kids_under2yr = susceptible_kids_under2yr * percent_pandemic_kids,
    infected_kids_under1yr = susceptible_kids_under1yr * percent_pandemic_kids)

scotland %>% 
  ggplot() +
  geom_line(aes(x = date, y = births), colour = "black") +
  # geom_line(aes(x = time, y = protected_kids), colour = "green") +
  # geom_line(aes(x = time, y = susceptible_kids_under1yr), colour = "blue") +
  # geom_line(aes(x = time, y = infected_kids_under1yr), colour = "red") +
  scale_x_date(date_breaks = "1 year") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1 )) +
  labs(x = "Time",
       y = "Count")

scotland %>% 
  filter(!year == 2023) %>% 
  pivot_longer(c(inf_preg, protected_kids, susceptible_kids_under1yr, infected_kids_under1yr), names_to = "category") %>% 
  ggplot() +
  geom_line(aes(x = date, y = value, group = category, colour = category)) +
  scale_x_date(date_breaks = "1 year") +
  scale_color_manual(name = "Category", breaks = c("inf_preg", "protected_kids", "susceptible_kids_under1yr", "infected_kids_under1yr"), labels = c("inf_preg" = "Infected Pregnant Mothers", "protected_kids" = "Protected Children", "susceptible_kids_under1yr" = "Susceptible Children <1yr", "infected_kids_under1yr" = "Infected Children <1yr"), values = c("black", "green", "blue", "red")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1 )) +
  labs(x = "Time",
       y = "Count")
