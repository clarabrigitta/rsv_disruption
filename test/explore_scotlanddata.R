setwd("~/Desktop/PhD")

# load libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(viridisLite)
library(plotly)

# load data
rate <- read.csv("./data/respiratory_age_20231220.csv") %>%
  mutate(date = as.Date(as.character(WeekBeginning), "%Y%m%d"))
count <- read.csv("./data/respiratory_scot_20231220.csv") %>%
  mutate(date = as.Date(as.character(WeekBeginning), "%Y%m%d"))
restrict <- read_excel("uk_restrictions.xlsx") %>% 
  mutate(start = as.Date(start))

# percentage of rate per age group
test <- left_join(filter(rate, Pathogen == "Respiratory syncytial virus"), rate %>% 
  filter(Pathogen == "Respiratory syncytial virus") %>% 
  group_by(date) %>% 
  summarise(totalrate = sum(RatePer100000))) %>%
  mutate(perc = round((RatePer100000/totalrate)*100, 1))

# weekly number of laboratory-confirmed cases by pathogen and flu type in Scotland
count %>% 
  # filter(Pathogen == "Respiratory syncytial virus") %>%
  ggplot() +
  geom_line(aes(x = date, y = NumberCasesPerWeek, colour = Pathogen)) +
  scale_colour_viridis_d(option = "D") +
  theme_bw() +
  labs(x = "Year",
       y = "Number of Cases (per week)")

plot_ly() %>% 
  add_trace(data = count,
            x = ~date,
            y = ~NumberCasesPerWeek,
            type = 'scatter',
            mode = 'lines',
            color = ~Pathogen) %>% 
  layout(xaxis = list(title = 'Date', dtick = "M1", tickangle = -45), 
         yaxis = list(title = 'Number of Cases (per week)', showgrid = T))

# weekly rate per 100,000 by pathogen, flu type and age group
rate %>% 
  filter(Pathogen == "Respiratory syncytial virus") %>%
  ggplot() +
  geom_line(aes(x = date, y = RatePer100000, colour = AgeGroup)) +
  scale_colour_viridis_d(option = "D") +
  theme_bw() +
  labs(x = "Year",
       y = "Rate (per 100,000)")

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
