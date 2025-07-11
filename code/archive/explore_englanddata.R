# load libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(viridisLite)
library(plotly)

# load data
july23 <- read_excel("./data/Weekly_Influenza_and_COVID19_report_data_summer_w27_report.xlsx", sheet = "Supple_15__Datamart_-_RSV_Age_%") %>% 
  slice(-(1:5))
february24 <- read_excel("./data/Weekly_Influenza_and_COVID19_report_data_w5.xlsx", sheet = "Figure_49__SARIWatch-RSV-agegrp") %>% 
  slice(-(1:5))

# rename columns
colnames(july23) <- july23[1, ]
july23 <- july23[-1, ]

colnames(february24) <- february24[1, ]
february24 <- february24[-1, ]

# plot
july23 %>% 
  pivot_longer(`0 to 4 years`:`65+ years`) %>% 
  mutate(value = as.numeric(value)) %>% 
  ggplot() +
  geom_line(aes(x = factor(`Week no`, levels = (c(27:52, 1:26))), y = value, group = name, colour = name)) +
  scale_colour_viridis_d(option = "D") +
  theme_bw() +
  labs( x = "Week",
        y = "Positivity (%)",
        colour = "Age Group")

february24 %>% 
  pivot_longer(`0 to 4 years`:`85+ years`) %>% 
  mutate(value = as.numeric(value)) %>% 
  ggplot() +
  geom_line(aes(x = factor(`Week number`, levels = (c(27:52, 1:26))), y = value, group = name, colour = name)) +
  scale_colour_viridis_d(option = "D") +
  theme_bw() +
  labs( x = "Week",
        y = "Rate",
        colour = "Age Group")
