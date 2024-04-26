# load libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)
library(viridisLite)
library(plotly)

# -------------------------------------------------------------------------

# load data and calculate over 2yrs, under 2yrs, pregnancy trimester counts and proportions
birth_data <- read_excel("./data/births_pregnancies.xlsx", sheet = "All") %>%
  mutate(year_month = paste(year, month, sep = "_"),
         date = as.Date(as.yearmon(`year_month`, "%Y_%b")),) %>%
  select(year, month, date, births) %>% 
  filter(!is.na(births)) %>% 
  slice(1:(12*10)) %>% 
  mutate(time = 1:nrow(.))

# -------------------------------------------------------------------------

# infection and disease model for monthly births
model_births <- function(month_chosen = c(), disruption_year = c()){
  rep <- 3 # number of years
  rate_scale <- 5 # scaling rate of infection to achieve 90-95% in first 3yrs of life
  
  output <- as.data.frame(matrix(NA, 12*rep, 4))
  colnames(output) <- c("time", "month", "rate", "susceptible")
  
  # month sequence based on month selection
  month_seq <- case_when(month_chosen == "Jan" ~ rep(month.abb, rep),
                         month_chosen == "Feb" ~ rep(c(month.abb[2:12], month.abb[1]), rep),
                         month_chosen == "Mar" ~ rep(c(month.abb[3:12], month.abb[1:2]), rep),
                         month_chosen == "Apr" ~ rep(c(month.abb[4:12], month.abb[1:3]), rep),
                         month_chosen == "May" ~ rep(c(month.abb[5:12], month.abb[1:4]), rep),
                         month_chosen == "Jun" ~ rep(c(month.abb[6:12], month.abb[1:5]), rep),
                         month_chosen == "Jul" ~ rep(c(month.abb[7:12], month.abb[1:6]), rep),
                         month_chosen == "Aug" ~ rep(c(month.abb[8:12], month.abb[1:7]), rep),
                         month_chosen == "Sep" ~ rep(c(month.abb[9:12], month.abb[1:8]), rep),
                         month_chosen == "Oct" ~ rep(c(month.abb[10:12], month.abb[1:9]), rep),
                         month_chosen == "Nov" ~ rep(c(month.abb[11:12], month.abb[1:10]), rep),
                         month_chosen == "Dec" ~ rep(c(month.abb[12], month.abb[1:11]), rep))
  
  output <- output %>% mutate(month = month_seq,
                              time = 1:nrow(output),
                              rate = (case_when(month == month.abb[1] ~ 0.015,
                                                month %in% month.abb[2:3] ~ 0.005,
                                                month %in% month.abb[4:8] ~ 0.000,
                                                month == month.abb[9] ~ 0.010,
                                                month == month.abb[10] ~ 0.020,
                                                month %in% month.abb[11:12] ~ 0.045))*rate_scale)
  
  output[1, "susceptible"] <- case_when(month_chosen == "Jan" ~ 61942,
                                        month_chosen == "Feb" ~ 59783,
                                        month_chosen == "Mar" ~ 61043,
                                        month_chosen == "Apr" ~ 58088,
                                        month_chosen == "May" ~ 62736,
                                        month_chosen == "Jun" ~ 59664,
                                        month_chosen == "Jul" ~ 61920,
                                        month_chosen == "Aug" ~ 61995,
                                        month_chosen == "Sep" ~ 62949,
                                        month_chosen == "Oct" ~ 63387,
                                        month_chosen == "Nov" ~ 59593,
                                        month_chosen == "Dec" ~ 59574)
  
  # read women.prop dataset
  women.prop <- readRDS("./output/data/women_prop.rds")
  
  # join women's immunity level proportions to output
  output <- output %>% 
    left_join(women.prop %>% select(-count) %>% filter(disruption == disruption_year)) %>% 
    # set probability of infection/disease based on immunity level
    mutate(prob_inf = case_when(level == 1 ~ 0,
                                level == 2 ~ 0.5,
                                level == 3 ~ 0.5,
                                level == 4 ~ 1),
           prob_dis = case_when(level == 1 ~ 0,
                                level == 2 ~ 0,
                                level == 3 ~ 0.5,
                                level == 4 ~ 1)) %>% 
    # introduce waning immunity to probability of infection
    mutate(waning = case_when(time <= 6 ~ 1,
                              time > 6 & time <= 12 ~ 0.5,
                              time > 12 & time <= 24 ~ 0.2,
                              time > 24 ~ 0),
           prob_inf = 1 - ((1 - prob_inf) * waning)) %>%
    # introduce aging to probability of disease
    mutate(aging = case_when(time <= 6 ~ 1,
                             time > 6 & time <= 12 ~ 0.5,
                             time > 12 & time <= 24 ~ 0.2,
                             time > 24 ~ 0),
           prob_dis = prob_dis * aging) %>%
    # determine number of susceptible based on immunity level proportions  
    mutate(proportion = ifelse(month == month_chosen, proportion, NA),
           susceptible_sub = susceptible * proportion) %>% 
    select(-c(susceptible, proportion)) %>% 
    rename(susceptible = susceptible_sub) %>% 
    # determine initial number of infected and disease
    mutate(infected = susceptible * rate * prob_inf,
           disease = infected * prob_dis) %>% 
    group_by(level) %>% 
    nest()        
  
  for(lev in 1:4){
    subdata <- output[[2]][[lev]]
    
    for(row in 1:nrow(subdata)){
      subdata[row, "infected"] <- subdata[row, "susceptible"] * subdata[row, "rate"] * subdata[row, "prob_inf"]
      subdata[row, "disease"] <- subdata[row, "infected"] * subdata[row, "prob_dis"]
      subdata[row + 1, "susceptible"] <- subdata[row, "susceptible"] - subdata[row, "infected"]
    }
    
    output[[2]][[lev]] <- subdata
  }
  
  output <- output %>% unnest() %>% filter(!is.na(month))
  
  # calculating and adding rows for totals
  output <- bind_rows(output,
                      output %>% 
                        group_by(time, month) %>% 
                        summarise(susceptible = sum(susceptible),
                                  infected = sum(infected),
                                  disease = sum(disease)) %>% 
                        ungroup() %>% 
                        mutate(level = "total",
                               rate = NA,
                               prob_inf = NA,
                               prob_dis = NA)) %>% 
    mutate(level = factor(level, levels = c("1", "2", "3", "4", "total"), labels = c("1 (high)", "2", "3", "4 (low)", "total")))
  
  # save model output
  saveRDS(output, file = paste0("./output/data/monthly/births_", month_chosen, ".rds"))
}

# -------------------------------------------------------------------------

# model one year of monthly births
for(m in month.abb){
  model_births(month_chosen = m, disruption_year = 0)
}

# -------------------------------------------------------------------------

# determine age at infection and disease

for(m in 1:12){
  assign(paste0("births_", month.abb[m]), readRDS(file = paste0("./output/data/monthly/births_", month.abb[m], ".rds")) %>% mutate(month_born = month.abb[m]))
}

# month of age
births_age_month <- bind_rows(births_Jan, births_Feb, births_Mar, births_Apr, births_May, births_Jun, births_Jul, births_Aug, births_Sep, births_Oct, births_Nov, births_Dec) %>% 
  mutate(month_born = factor(month_born, levels = month.abb),
         population = case_when(month_born == "Jan" ~ 61942,
                                month_born == "Feb" ~ 59783,
                                month_born == "Mar" ~ 61043,
                                month_born == "Apr" ~ 58088,
                                month_born == "May" ~ 62736,
                                month_born == "Jun" ~ 59664,
                                month_born == "Jul" ~ 61920,
                                month_born == "Aug" ~ 61995,
                                month_born == "Sep" ~ 62949,
                                month_born == "Oct" ~ 63387,
                                month_born == "Nov" ~ 59593,
                                month_born == "Dec" ~ 59574))

saveRDS(births_age_month, file = "./output/data/births_age_month.rds")

# age groups
births_age <- bind_rows(births_Jan, births_Feb, births_Mar, births_Apr, births_May, births_Jun, births_Jul, births_Aug, births_Sep, births_Oct, births_Nov, births_Dec) %>% 
  mutate(month_born = factor(month_born, levels = month.abb),
         `<6` = ifelse(time <= 6, 1, 0),
         `6-12` = ifelse(time >= 6 & time <= 12, 1, 0),
         `12-24` = ifelse(time >= 12 & time <= 24, 1, 0),
         `24-36` = ifelse(time >= 24 & time <= 36, 1, 0)) %>%
  select(-time) %>% 
  pivot_longer(cols = c("<6", "6-12", "12-24", "24-36"), names_to = "age", values_to = "value") %>%
  mutate(age = factor(age, levels = c("<6", "6-12", "12-24", "24-36"))) %>% 
  filter(value == 1) %>% 
  select(-value) %>% 
  group_by(level, month_born, age) %>% 
  summarise(infected = sum(infected),
            disease = sum(disease)) %>% 
  pivot_longer(cols = c("infected", "disease"), names_to = "type", values_to = "count")

saveRDS(births_age, file = "./output/data/births_age.rds")

# -------------------------------------------------------------------------

# combine monthly modeled data into 1 yearly dataset

for(m in 1:12){
  data <- readRDS(file = paste0("./output/data/monthly/births_", month.abb[m], ".rds")) %>% 
    mutate(month_born = month.abb[m],
           time = m:(35+m)) %>% 
    ungroup()
  
  assign(paste0("births_", month.abb[m]), data)
}

births_year <- bind_rows(births_Jan, births_Feb, births_Mar, births_Apr, births_May, births_Jun, births_Jul, births_Aug, births_Sep, births_Oct, births_Nov, births_Dec) %>% 
  mutate(month_born = factor(month_born, levels = month.abb),
         population = case_when(month_born == "Jan" ~ 61942,
                                month_born == "Feb" ~ 59783,
                                month_born == "Mar" ~ 61043,
                                month_born == "Apr" ~ 58088,
                                month_born == "May" ~ 62736,
                                month_born == "Jun" ~ 59664,
                                month_born == "Jul" ~ 61920,
                                month_born == "Aug" ~ 61995,
                                month_born == "Sep" ~ 62949,
                                month_born == "Oct" ~ 63387,
                                month_born == "Nov" ~ 59593,
                                month_born == "Dec" ~ 59574))

saveRDS(births_year, file = "./output/data/births_year.rds")

# yearly dataset with distribution of infection/disease per season by month of birth

births_season <- births_year %>% 
  filter(level == "total") %>% 
  filter(time >=7 & time <=42) %>% 
  mutate(season = case_when(time >= 7 & time <= 18 ~ 1,
                            time >= 19 & time <= 30 ~ 2,
                            time >= 31 & time <= 42 ~ 3)) %>% 
  group_by(season, month_born) %>% 
  summarise(infected = sum(infected),
            disease = sum(disease))

saveRDS(births_season, file = "./output/data/births_season.rds")

births_season %>% 
  plot_ly() %>%
  add_trace(x = ~season,
            y = ~infected,
            split = ~month_born,
            color = ~month_born,
            type = "bar",
            legendgroup = ~month_born,
            showlegend = TRUE) %>% 
  layout(yaxis = list(title = "Infection Count",
                      range = list(0, 30000),
                      tickmode = "linear",
                      tick0 = 0,
                      dtick = 5000,
                      tickformat = "digits"),
         xaxis = list(title = "Season (Jul-Jun)"))

# -------------------------------------------------------------------------

# need to repeat lines 137-215 for each disruption category by specifying disruption_year argument in model

# combining different different immunity proportions
births_year_disrupt <- bind_rows(readRDS(file = "./output/data/disruption/births_year_0.rds") %>% mutate(disruption = 0),
                                 readRDS(file = "./output/data/disruption/births_year_1.rds") %>% mutate(disruption = 1),
                                 readRDS(file = "./output/data/disruption/births_year_2.rds") %>% mutate(disruption = 2),
                                 readRDS(file = "./output/data/disruption/births_year_3.rds") %>% mutate(disruption = 3))
saveRDS(births_year_disrupt, file = "./output/data/births_year_disrupt.rds")

births_age_disrupt <- bind_rows(readRDS(file = "./output/data/disruption/births_age_0.rds") %>% mutate(disruption = 0),
                                readRDS(file = "./output/data/disruption/births_age_1.rds") %>% mutate(disruption = 1),
                                readRDS(file = "./output/data/disruption/births_age_2.rds") %>% mutate(disruption = 2),
                                readRDS(file = "./output/data/disruption/births_age_3.rds") %>% mutate(disruption = 3))
saveRDS(births_age_disrupt, file = "./output/data/births_age_disrupt.rds")

births_age_month_disrupt <- bind_rows(readRDS(file = "./output/data/disruption/births_age_month_0.rds") %>% mutate(disruption = 0),
                                      readRDS(file = "./output/data/disruption/births_age_month_1.rds") %>% mutate(disruption = 1),
                                      readRDS(file = "./output/data/disruption/births_age_month_2.rds") %>% mutate(disruption = 2),
                                      readRDS(file = "./output/data/disruption/births_age_month_3.rds") %>% mutate(disruption = 3))
saveRDS(births_age_month_disrupt, file = "./output/data/births_age_month_disrupt.rds")
