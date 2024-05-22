# load libraries
library(readxl)
library(plotly)
library(dplyr)
library(tidyr)
library(stringr)

# load data sets of interest
mobility <- read.csv(file = "./data/changes-visitors-covid.csv") %>%
  filter(Code == "GBR") %>% 
  mutate(Day = as.Date(Day, format = "%Y-%m-%d")) %>% 
  mutate(across(`retail_and_recreation`:`workplaces`, ~ rollmean(.x, k = 30, fill = NA, align = "right"))) %>% 
  pivot_longer(cols = `retail_and_recreation`:`workplaces`, names_to = "category", values_to = "value") %>% 
  group_by(yearmon = as.yearmon(Day)) %>% 
  arrange() %>% 
  filter(day(Day) == 1) %>% 
  ungroup() %>% 
  mutate(time = rep(783:(783+31), each = 6)) %>% 
  # mutate(value = abs(value/100))
  # mutate(value = value/100)
  mutate(value = 1-abs(value/100)) %>% 
  filter(category == "workplaces")

restrictions <- read_excel("./data/uk_restrictions.xlsx") %>% 
  select(-source) %>% 
  mutate(start = as.Date(start, format = "%Y-%m-%d"))

# plot mobility data
plot_ly(mobility, 
        x = ~Day, 
        y = ~value, 
        split = ~category,
        group = ~category,
        colour = ~category,
        type = 'scatter', 
        mode = 'lines') %>%
  layout(xaxis = list(title = 'Date',
                      range = c(as.numeric(as.POSIXct("2018-01-01", format="%Y-%m-%d"))*1000,
                                as.numeric(as.POSIXct("2024-12-31", format="%Y-%m-%d"))*1000),
                      type = "date"),
         yaxis = list(title = 'Percent Change'))