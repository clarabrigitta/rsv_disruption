# load libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridisLite)
library(plotly)
library(lubridate)

# load data

# weekly rate of laboratory confirmed cases by age and pathogen
rate <- read.csv("./data/respiratory_age_20240515.csv") %>%
  mutate(date = as.Date(as.character(WeekBeginning), "%Y%m%d"))

# weekly count of laboratory confirmed cases by age and pathogen
count <- read.csv("./data/respiratory_scot_20240515.csv") %>%
  mutate(date = as.Date(as.character(WeekBeginning), "%Y%m%d"))

# data on lock down restrictions
restrict <- read_excel("./data/uk_restrictions.xlsx") %>% 
  mutate(start = as.Date(start))

# scottish population data by age
population <- read_excel("./data/mid-year-pop-est-22-data.xlsx", sheet = "Table 1", skip = 3) %>% 
  filter(`Area name` == "Scotland",
         Sex == "Persons") %>% 
  select(1:10) %>% 
  select(-`All ages`) %>% 
  pivot_longer(cols = `0`:`4`, names_to = "age_year", values_to = "population") %>% 
  mutate(age = ifelse(age_year == 0, "<1", "1-4")) %>% 
  group_by(age) %>% 
  summarise(population = sum(population))

# scottish birth data by month registered
births <- read_excel("./data/births-time-series-22-bt.3.xlsx", skip = 3) %>% 
  head(-7) %>% 
  select(1:14) %>% 
  select(-Total) %>% 
  rename(year = Year) %>% 
  pivot_longer(cols = `Jan`:`Dec`, names_to = "month", values_to = "births") %>% 
  filter(year >= 2012, year <= 2022) %>% 
  mutate(month = rep(month.abb, 10),
         year_month = paste(year, month, sep = "_"),
         date = as.Date(as.yearmon(`year_month`, "%Y_%b"))) %>%
  select(year, month, date, births) %>% 
  mutate(time = 1:nrow(.))

# extrapolate scottish births
# scottish birth data by month registered
births <- read_excel("./data/births-time-series-22-bt.3.xlsx", skip = 3) %>% 
  rename(Jun = June, Jul = July) %>% 
  head(-7) %>% 
  select(1:14) %>% 
  select(-Total) %>% 
  rename(year = Year) %>% 
  pivot_longer(cols = `Jan`:`Dec`, names_to = "month", values_to = "births") %>% 
  mutate(yearmon = as.yearmon(paste(year, month, sep = "_"), "%Y_%b"),
         date = as.Date(yearmon)) %>%
  filter(year >= 2012) %>% 
  mutate(time = 1:nrow(.))

lm(births~time, data = births)
  
ggplot(data.frame(x = c(0, 900)), aes(x = x)) +
  stat_function(fun = function(x){-8.131*x + 4903.856}) +
  theme_bw() +
  labs(x = "Months", y = "Births")

birth_data <- read_excel("./data/births-time-series-22-bt.3.xlsx", skip = 3) %>% 
  rename(Jun = June, Jul = July) %>% 
  head(-7) %>% 
  select(1:14) %>% 
  select(-Total) %>% 
  rename(year = Year) %>% 
  pivot_longer(cols = `Jan`:`Dec`, names_to = "month", values_to = "births") %>% 
  filter(year >= 2012) %>% 
  mutate(year = as.numeric(year),
         yearmon = as.yearmon(paste(year, month, sep = "_"), "%Y_%b"),
         date = as.Date(yearmon)) %>% 
  right_join(rate_reference %>% select(-c(rate, level)) %>% distinct()) %>%
  # extrapolate births beyong 2022 (linear regression)
  mutate(births = ifelse(is.na(births), -5.499*time + 8095.307, births))

# percentage of rate per age group
test <- left_join(filter(rate, Pathogen == "Respiratory syncytial virus"), rate %>% 
  filter(Pathogen == "Respiratory syncytial virus") %>% 
  group_by(date) %>% 
  summarise(totalrate = sum(RatePer100000))) %>%
  mutate(perc = round((RatePer100000/totalrate)*100, 1))

# weekly number of laboratory-confirmed cases by pathogen and flu type in Scotland
count %>% 
  filter(Pathogen == "Respiratory syncytial virus",
         date >= "2017-01-12") %>%
  ggplot() +
  geom_line(aes(x = date, y = NumberCasesPerWeek, colour = Pathogen)) +
  geom_vline(xintercept = as.numeric(seq(from = as.Date("2016-12-01"), 
                                         to = as.Date("2024-12-01"), 
                                         by = "1 year")), linetype = "dashed", color = "black") +
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
  scale_colour_viridis_d(option = "D") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  labs(x = "Time",
       y = "Number of Cases (per week)")

plot_ly() %>% 
  add_trace(data = count %>% filter(Pathogen == "Respiratory syncytial virus"),
            x = ~date,
            y = ~NumberCasesPerWeek,
            type = 'scatter',
            mode = 'lines',
            color = ~Pathogen) %>% 
  layout(xaxis = list(title = 'Date', dtick = "M1", tickangle = -45), 
         yaxis = list(title = 'Number of Cases (per week)', showgrid = T))

# weekly rate per 100,000 by pathogen, flu type and age group
rate %>% 
  filter(Pathogen == "Respiratory syncytial virus",
         date >= "2017-01-12") %>%
  ggplot() +
  geom_line(aes(x = date, y = RatePer100000, colour = AgeGroup)) +
  geom_vline(xintercept = as.numeric(seq(from = as.Date("2016-12-01"), 
                                         to = as.Date("2024-12-01"), 
                                         by = "1 year")), linetype = "dashed", color = "black") +
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
  scale_colour_viridis_d(option = "H") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  labs(x = "Time",
       y = "Rate (per 100,000)",
       colour = "Age Group")

plot_ly() %>% 
  add_trace(data = filter(rate, Pathogen == "Respiratory syncytial virus"),
            x = ~date,
            y = ~RatePer100000,
            type = 'scatter',
            mode = 'lines',
            color = ~AgeGroup) %>% 
  layout(xaxis = list(title = 'Date', dtick = "M1", tickangle = -45), 
         yaxis = list(title = 'Rate (per 100,000)', showgrid = T))

test %>% 
  ggplot() +
  geom_bar(aes(fill = AgeGroup, y = perc, x = date), position = "fill", stat = "identity") +
  scale_fill_viridis_d(option = "D") +
  theme_bw() +
  labs(x = "Year",
       y = "Percentage (%)")

# adding vertical lines for restrictions
# by pathogen
fig <- plot_ly() %>% 
  add_trace(data = count,
            x = ~date,
            y = ~NumberCasesPerWeek,
            type = 'scatter',
            mode = 'lines',
            color = ~Pathogen) %>% 
  layout(xaxis = list(title = 'Date', dtick = "M1", tickangle = -45), 
         yaxis = list(title = 'Number of Cases (per week)', showgrid = T)) 

# by age group
fig <- plot_ly() %>% 
  add_trace(data = filter(rate,Pathogen == "Adenovirus"),
            x = ~date,
            y = ~RatePer100000,
            type = 'scatter',
            mode = 'lines',
            color = ~AgeGroup) %>% 
  layout(xaxis = list(title = 'Date', dtick = "M1", tickangle = -45), 
         yaxis = list(title = 'Rate (per 100,000)', showgrid = T))

# fig %>% add_segments(data = restrict %>% slice(1),
#                      x = ~start, 
#                      xend = ~start, 
#                      y = 0, 
#                      yend = 3000,
#                      showlegend = FALSE,
#                      color = ~target,
#                      customdata = ~target,
#                      hoverinfo = "text",
#                      text = ~comments,
#                      hovertemplate = paste(
#                        "<b>%{x|%d/%m/%Y}</b><br>",
#                        "%{customdata}: %{text}",
#                        "<extra></extra>")
#                      )

for(row in 1:nrow(restrict)){
  fig <- fig %>% add_segments(data = restrict %>% slice(row),
                              x = ~start, 
                              xend = ~start, 
                              y = 0, 
                              yend = 50,
                              legendgroup = ~target,
                              # showlegend = FALSE,
                              linetype = ~target, 
                              line = list(color = "black",
                                          width = 0.5),
                              opacity = 0.7,
                              customdata = ~target,
                              hoverinfo = "text",
                              text = ~comments,
                              name = ~comments,
                              hovertemplate = paste(
                                "<b>%{x|%d/%m/%Y}</b><br>",
                                "%{customdata}: %{text}",
                                "<extra></extra>") 
  ) %>% 
    layout(legend = list(groupclick = "toggleitem"))
}
fig

unique(rate$Pathogen)


# -------------------------------------------------------------------------

# smoothing 15-44 RSV rate to use as baseline

rate_scot <- read.csv("./data/respiratory_age_20240515.csv") %>%
  mutate(date = as.Date(as.character(WeekBeginning), "%Y%m%d")) %>% 
  filter(Pathogen == "Respiratory syncytial virus",
         AgeGroup == "15-44 years") %>%
  mutate(yearmon = as.yearmon(date),
         year = year(date),
         month = month(date)) %>% 
  # filter(year >= 2017, year < 2023) %>%
  group_by(yearmon, year, month) %>%
  summarise(RatePer100000 = mean(RatePer100000)) %>%
  ungroup() %>% 
  mutate(rate_scot = (RatePer100000/1000)*300,
         time = 745:(745+nrow(.)-1)) %>% 
  slice(-(1:4))

saveRDS(rate_scot, file = "./output/data/prefitting/rate_scot.rds")

plot_ly() %>% 
  add_trace(data = rate_scot,
            x = ~yearmon,
            y = ~rate_scot,
            type = 'scatter',
            mode = 'lines') %>% 
  layout(xaxis = list(title = 'Date', tickangle = -45), 
         yaxis = list(title = 'Rate', showgrid = T))
