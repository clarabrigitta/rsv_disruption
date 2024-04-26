library(dplyr)
library(tidyr)
library(stringr)


# -------------------------------------------------------------------------
# data preparation

# load birth data
birth_data <- read_excel("./data/births_pregnancies.xlsx", sheet = "All") %>%
  mutate(year_month = paste(year, month, sep = "_"),
         date = as.Date(as.yearmon(`year_month`, "%Y_%b")),) %>%
  select(year, month, date, births) %>% 
  filter(!is.na(births)) %>% 
  slice(1:(12*10)) %>% 
  mutate(time = 1:nrow(.))

# create reference table for rates to join
rate_reference <- readRDS("./output/data/women/women_long_disrupt.rds") %>% 
  select(time, month, rate) %>% 
  distinct() %>% 
  filter(time <= 12*75, time > 12*60) %>% 
  mutate(month = factor(month, levels = month.abb),
         time = 1:nrow(.)) %>% 
  cross_join(data.frame(level = 1:4)) %>% 
  mutate(level = factor(level, levels = 1:4))

# create 10 years of infection history/immunity split
immunity_split <- readRDS("./output/data/women/women_long_disrupt.rds") %>% 
  filter(time <= 12*70, time > 12*60) %>% 
  mutate(month = factor(month, levels = month.abb)) %>% 
  filter(infection != "susceptible_naive") %>% 
  mutate(level = as.factor(case_when(infection %in% str_c(rep("I", 6), 1:6) ~ 1,
                                     infection %in% str_c(rep("I", 9), 7:15) ~ 2,
                                     infection %in% str_c(rep("I", 9), 16:24) ~ 3,
                                     infection == "susceptible_reinf" ~ 4))) %>% 
  group_by(time, rate, month, level) %>% 
  summarise(across(c("count", "proportion"), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(time = rep(1:(12*10), each = 4)) %>% 
  # combining with birth data
  select(-count) %>% 
  left_join(birth_data %>% select(month, births, time)) %>% 
  mutate(susceptible = proportion * births)


# -------------------------------------------------------------------------
# model continuous monthly births, each for 3 months

# nesting data set to model
model_continuous_births <- immunity_split %>% 
  nest(.by = time)

# model
for(n in 1:nrow(model_continuous_births)){
  data <- model_continuous_births[[2]][[n]] %>% 
    mutate(time = n) %>% 
    right_join(rate_reference) %>% 
    filter(time %in% c(n:(n+12*3-1))) %>% 
    # distinguish between calendar months and months since birth
    rename(time_calendar = time) %>% 
    mutate(time_birth = rep(1:36, each = 4)) %>% 
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
    mutate(waning = case_when(time_birth <= 6 ~ 1,
                              time_birth > 6 & time_birth <= 12 ~ 0.5,
                              time_birth > 12 & time_birth <= 24 ~ 0.2,
                              time_birth > 24 ~ 0),
           prob_inf = 1 - ((1 - prob_inf) * waning)) %>%
    # introduce aging to probability of disease
    mutate(aging = case_when(time_birth <= 6 ~ 1,
                             time_birth > 6 & time_birth <= 12 ~ 0.5,
                             time_birth > 12 & time_birth <= 24 ~ 0.2,
                             time_birth > 24 ~ 0),
           prob_dis = prob_dis * aging) %>% 
    # determine initial number of infected and disease
    mutate(infected = susceptible * rate * prob_inf,
           disease = infected * prob_dis) %>% 
    group_by(level) %>% 
    nest()
  
  for(lev in 1:4){
    subdata <- data[[2]][[lev]]
    
    for(row in 1:nrow(subdata)){
      subdata[row, "infected"] <- subdata[row, "susceptible"] * subdata[row, "rate"] * subdata[row, "prob_inf"]
      subdata[row, "disease"] <- subdata[row, "infected"] * subdata[row, "prob_dis"]
      subdata[row + 1, "susceptible"] <- subdata[row, "susceptible"] - subdata[row, "infected"]
    }
    
    data[[2]][[lev]] <- subdata
  }
  
  data <- data %>% 
    unnest() %>%
    filter(!is.na(month)) %>%
    mutate(`<6` = ifelse(time_birth < 6, 1, 0),
           `6-12` = ifelse(time_birth >= 6 & time_birth < 12, 1, 0),
           `12-24` = ifelse(time_birth >= 12 & time_birth < 24, 1, 0),
           `24-36` = ifelse(time_birth >= 24 & time_birth <= 36, 1, 0))
  
  model_continuous_births[[2]][[n]] <- data
}

output <- model_continuous_births %>% unnest()

saveRDS(output, file = "./output/data/continuous_model.rds")