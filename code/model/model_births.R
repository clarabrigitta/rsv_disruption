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
data <- read_excel("./data/births_pregnancies.xlsx", sheet = "All") %>%
  mutate(year_month = paste(year, month, sep = "_"),
         date = as.Date(as.yearmon(`year_month`, "%Y_%b")),) %>%
  select(year, month, date, births) 

# -------------------------------------------------------------------------

# testing model for one month of births (first 3 years of life) using totals
rep <- 3 # number of years
rate_scale <- 5 # scaling rate of infection to achieve 90-95% in first 3yrs of life

births <- as.data.frame(matrix(NA, 12*rep, 4))
colnames(births) <- c("time", "month", "rate", "susceptible")

births <- births %>% mutate(month = rep(month.abb, rep), # january, july: rep(c(month.abb[7:12], month.abb[1:6]), rep)
                            time = 1:nrow(births),
                            rate = (case_when(month == month.abb[1] ~ 0.015,
                                              month %in% month.abb[2:3] ~ 0.005,
                                              month %in% month.abb[4:8] ~ 0.000,
                                              month == month.abb[9] ~ 0.010,
                                              month == month.abb[10] ~ 0.020,
                                              month %in% month.abb[11:12] ~ 0.045))*rate_scale)

births[1, "susceptible"] <- 61942 # january, july: 61920

# join women's immunity level proportions to births
births <- births %>% 
  left_join(women.prop %>% select(-count)) %>% 
  # set probability of infection/disease based on immunity level
  mutate(prob_inf = case_when(level == 1 ~ 0,
                              level == 2 ~ 0.5,
                              level == 3 ~ 0.5,
                              level == 4 ~ 1),
         prob_dis = case_when(level == 1 ~ 0,
                              level == 2 ~ 0,
                              level == 3 ~ 0.5,
                              level == 4 ~ 1)) %>% 
  # calculate risk of infection/disease based on immunity level
  mutate(risk_inf = rate * prob_inf,
         risk_dis = rate * prob_inf * prob_dis,
         # determine number of susceptible based on immunity level proportions
         proportion = ifelse(month == "Jan", proportion, NA), # change based on month of interest (e.g., Jan vs Jul)
         susceptible_sub = susceptible * proportion) %>% 
  select(-c(susceptible, proportion)) %>% 
  rename(susceptible = susceptible_sub) %>% 
  # determine initial number of infected and disease
  mutate(infected = susceptible * rate * prob_inf,
         disease = infected * prob_dis) %>% 
  group_by(level) %>% 
  nest()        

for(lev in 1:4){
  subdata <- births[[2]][[lev]]
  
  for(row in 1:nrow(subdata)){
    subdata[row, "infected"] <- subdata[row, "susceptible"] * subdata[row, "rate"] * subdata[row, "prob_inf"]
    subdata[row, "disease"] <- subdata[row, "infected"] * subdata[row, "prob_dis"]
    subdata[row + 1, "susceptible"] <- subdata[row, "susceptible"] - subdata[row, "infected"]
  }
  
  births[[2]][[lev]] <- subdata
}

births <- births %>% unnest() %>% filter(!is.na(month))

# calculating and adding rows for totals
births <- bind_rows(births,
                    births %>% 
                      group_by(time, month) %>% 
                      summarise(susceptible = sum(susceptible),
                                infected = sum(infected),
                                disease = sum(disease)) %>% 
                      ungroup() %>% 
                      mutate(level = "total",
                             rate = NA,
                             prob_inf = NA,
                             prob_dis = NA,
                             risk_inf = NA,
                             risk_dis = NA))

# save model output
saveRDS(births, file = "./output/data/births.rds")

# -------------------------------------------------------------------------
# introduce waning
waning <- as.data.frame(matrix(NA, 12*3, 3))
colnames(waning) <- c("time", "month", "waning")
waning <- waning %>% 
  mutate(time = 1:36,
         month = rep(month.abb, 3),
         waning = case_when(time <= 6 ~ 100,
                            time > 6 & time <= 12 ~ 80,
                            time > 12 & time <= 24 ~ 40,
                            time > 24 ~ 0))

# plot waning curve
waning %>% 
  ggplot() +
  geom_line(aes(x = time, y = waning)) +
  scale_y_continuous(breaks = seq(0, 100, 20)) +
  scale_x_continuous(breaks = c(1, 6, 12, 24, 36)) +
  theme_bw() +
  labs(x = "Months",
       y = "Percent Reduction (%)")

test <- births %>%
  mutate(waning = case_when(time <= 6 ~ 1,
                            time > 6 & time <= 12 ~ 0.8,
                            time > 12 & time <= 24 ~ 0.4,
                            time > 24 ~ 0))

# -------------------------------------------------------------------------

# unused code
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