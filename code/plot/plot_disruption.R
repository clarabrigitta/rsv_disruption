# load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(knitr)
library(viridisLite)

# load data
women_prop <- readRDS("/Users/lsh2301561/Desktop/rsv_disruption/output/data/women/women_prop.rds")

births_year <- readRDS(file = "/Users/lsh2301561/Desktop/rsv_disruption/output/data/births_year.rds")
births_age <- readRDS(file = "/Users/lsh2301561/Desktop/rsv_disruption/output/data/births_age.rds")
births_age_month <- readRDS(file = "/Users/lsh2301561/Desktop/rsv_disruption/output/data/births_age_month.rds")

births_year_disrupt <- readRDS(file = "/Users/lsh2301561/Desktop/rsv_disruption/output/data/births_year_disrupt.rds")
births_age_disrupt <- readRDS(file = "/Users/lsh2301561/Desktop/rsv_disruption/output/data/births_age_disrupt.rds")
births_age_month_disrupt <- readRDS(file = "/Users/lsh2301561/Desktop/rsv_disruption/output/data/births_age_month_disrupt.rds")

# -------------------------------------------------------------------------

# IMMUNITY SPLIT

# plot monthly immunity split by disruption level
# key question: how does the temporal trend of immunity change over the course of a year before and after disruption?

women_prop %>%
  mutate(disruption = factor(disruption, levels = 0:3, labels = c("No disruption", "Beginning of disruption", "1 year after disruption", "2 years after disruption")),
         level = factor(level, levels = c("1", "2", "3", "4", "total"), labels = c("1 (high)", "2", "3", "4 (low)", "total"))) %>% 
  ggplot() +
  geom_bar(aes(x = month, y = proportion, fill = level), position = "fill", stat = "identity") +
  scale_fill_manual(values = viridis(4)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Month",
       y = "Proportion",
       fill = "Immunity Level") + 
  facet_wrap(~disruption, nrow = 2, ncol = 2)

# plot annual immunity split by disruption level
# key question: is there more immunity in babies in one year before disruption, year of disruption, 1 year after disruption or 2 years after disruption?

women_prop %>% 
  group_by(level, disruption) %>%
  summarise(across(c("count"), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(proportion = count/12000000,
         disruption = factor(disruption, levels = 0:3, labels = c("No disruption", "Beginning of disruption", "1 year after disruption", "2 years after disruption")),
         level = factor(level, levels = c("1", "2", "3", "4", "total"), labels = c("1 (high)", "2", "3", "4 (low)", "total"))) %>% 
  ggplot() +
  geom_bar(aes(x = disruption, y = proportion, fill = level), position = "fill", stat = "identity") +
  scale_fill_manual(values = viridis(4)) +
  theme_bw() +
  labs(x = "Disruption Level",
       y = "Proportion",
       fill = "Immunity Level")


# -------------------------------------------------------------------------

# INFECTION
# no longer considering month of birth

# plot monthly first infection by disruption level
# key question: how does the temporal trend of first infection change with disruption level?

births_year_disrupt %>% 
  group_by(disruption, time, month, level) %>% 
  summarise(across(c("susceptible", "infected", "disease", "population"), sum, na.rm = TRUE)) %>% 
  filter(level == "total") %>% 
  mutate(level = as.character(level),
         disruption = factor(disruption, levels = 0:3, labels = c("No disruption", "Beginning of disruption", "1 year after disruption", "2 years after disruption"))) %>% 
  plot_ly() %>% 
  add_trace(
    x = ~time,
    y = ~infected,
    split = ~disruption,
    color = ~disruption,
    colors = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"),
    type = "scatter",
    mode = "line",
    text = ~month
  ) %>% 
  layout(xaxis = list(title = "Calendar month",
                      range = list(1, 48),
                      tickmode = "array",
                      tickvals = ~time,
                      ticktext = ~month,
                      tickangle = -45),
         yaxis = list(title = "Count",
                      range = list(0, 120000),
                      tickmode = "linear",
                      tick0 = 0,
                      dtick = 10000, 
                      tickformat = "digits"))

# plot monthly first infection by disruption level and immunity level (sub-analysis of above)
# key question: how does the temporal trend of first infection change with disruption level by immunity level?

input <- births_year_disrupt %>% 
  group_by(disruption, time, month, level) %>% 
  summarise(across(c("susceptible", "infected", "disease", "population"), sum, na.rm = TRUE)) %>% 
  filter(level != "total") %>% 
  mutate(level = as.character(level),
         disruption = factor(disruption, levels = 0:3, labels = c("No disruption", "Beginning of disruption", "1 year after disruption", "2 years after disruption")))

monthly_infection_immunity <- function(data){
  fig <- plot_ly() %>% 
    add_trace(data = data,
              x = ~time,
              y = ~infected,
              split = ~level,
              color = ~level, 
              colors = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"),
              legendgroup = ~level,
              type = "scatter",
              mode = "line",
              text = ~month,
              hovertemplate = paste('<b>Month</b>: %{text}',
                                    '<br><b>Count</b>: %{y}',
                                    '<extra></extra>'),
              showlegend = TRUE) %>% 
    layout(xaxis = list(title = "Calendar month",
                        range = list(1, 48),
                        tickmode = "array",
                        tickvals = seq(1, 48, 3),
                        ticktext = rep(c(month.abb[1], month.abb[4], month.abb[7], month.abb[10]), 4),
                        tickangle = -45),
           yaxis = list(title = "Count",
                        range = list(0, 100000),
                        tickmode = "linear",
                        tick0 = 0,
                        dtick = 10000, 
                        tickformat = "digits"),
           legend = list(title = list(text = "Immunity Level")),
           annotations = list(x = 0.5, y = 1, text = unique(data$disruption), showarrow = F, xref='paper', yref='paper', yanchor = "bottom", xanchor = "center", align = "center"))
}


fig <- subplot(
  monthly_infection_immunity(input %>% filter(disruption == "No disruption")), 
  monthly_infection_immunity(input %>% filter(disruption == "Beginning of disruption")), 
  monthly_infection_immunity(input %>% filter(disruption == "1 year after disruption")), 
  monthly_infection_immunity(input %>% filter(disruption == "2 years after disruption")),
  nrows = 2,
  shareX = FALSE,
  shareY = TRUE,
  margin = c(0.02, 0.02, 0.05, 0.06)
)

for(n in 5:16){
  fig[["x"]][["data"]][[n]][["showlegend"]] <-  FALSE
}

fig

# plot annual first infection by disruption level
# key question: is there more or less infection in babies in one year before disruption, year of disruption, 1 year after disruption or 2 years after disruption?

births_year_disrupt %>% 
  group_by(disruption, level) %>% 
  summarise(across(c("susceptible", "infected", "disease", "population"), sum, na.rm = TRUE)) %>% 
  filter(level == "total") %>% 
  mutate(level = as.character(level),
         disruption = factor(disruption, levels = 0:3, labels = c("No disruption", "Beginning of disruption", "1 year after disruption", "2 years after disruption"))) %>% 
  plot_ly() %>% 
  add_trace(x = ~disruption,
            y = ~infected,
            color = ~disruption,
            colors = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"),
            type = "bar",
            showlegend = FALSE) %>%
  layout(yaxis = list(title = "Count",
                      range = list(0, 660000),
                      tickmode = "linear",
                      tick0 = 0,
                      dtick = 50000, 
                      tickformat = "digits"))

# plot annual first infection by disruption level and immunity level (sub-analysis of above)
# key question: is there more or less infection in babies of certain immunity levels in one year before disruption, year of disruption, 1 year after disruption or 2 years after disruption?

births_year_disrupt %>% 
  group_by(disruption, level) %>% 
  summarise(across(c("susceptible", "infected", "disease", "population"), sum, na.rm = TRUE)) %>% 
  filter(level != "total") %>% 
  mutate(level = as.character(level),
         disruption = factor(disruption, levels = 0:3, labels = c("No disruption", "Beginning of disruption", "1 year after disruption", "2 years after disruption"))) %>% 
  plot_ly() %>% 
  add_trace(x = ~disruption,
            y = ~infected,
            split = ~level,
            color = ~level,
            colors = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"),
            type = "bar",
            showlegend = TRUE) %>%
  layout(xaxis = list(title = "Disruption Level"),
         yaxis = list(title = "Count",
                      range = list(0, 660000),
                      tickmode = "linear",
                      tick0 = 0,
                      dtick = 50000, 
                      tickformat = "digits"),
         legend = list(title = list(text = "Immunity Level")),
         barmode = "group")

# plot age distribution of first infection by disruption level
# key question: does the age distribution of infections change before and after disruption?

births_age_disrupt %>% 
  group_by(type, disruption, level, age) %>% 
  summarise(count = sum(count)) %>% 
  mutate(level = as.character(level),
         disruption = factor(disruption, levels = 0:3, labels = c("No disruption", "Beginning of disruption", "1 year after disruption", "2 years after disruption"))) %>% 
  filter(level == "total", type == "infected") %>% 
  plot_ly() %>% 
  add_trace(x = ~age,
            y = ~count,
            split = ~disruption,
            color = ~disruption,
            colors = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"),
            type = "bar",
            showlegend = TRUE) %>%
  layout(xaxis = list(title = "Age Group"),
         yaxis = list(title = "Count",
                      range = list(0, 240000),
                      tickmode = "linear",
                      tick0 = 0,
                      dtick = 20000,
                      tickformat = "digits"),
         legend = list(title = list(text = "Disruption Level")),
         barmode = "group")

# plot age distribution of first infection by disruption level and immunity level (sub-analysis of above)
# key question: are there immunity-level-related patterns that contribute to the age distribution of infections before and after disruption?

input <- births_age_disrupt %>% 
  group_by(type, disruption, level, age) %>% 
  summarise(count = sum(count)) %>% 
  mutate(level = as.character(level),
         disruption = factor(disruption, levels = 0:3, labels = c("No disruption", "Beginning of disruption", "1 year after disruption", "2 years after disruption"))) %>% 
  filter(level != "total", type == "infected")

age_infection_immunity <- function(data){
  fig <- plot_ly() %>% 
    add_trace(data = data,
              x = ~age,
              y = ~count,
              split = ~level,
              color = ~level, 
              colors = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"),
              legendgroup = ~level,
              type = "bar",
              showlegend = TRUE) %>% 
    layout(xaxis = list(title = "Age Group"),
           yaxis = list(title = "Count",
                        range = list(0, 160000),
                        tickmode = "linear",
                        tick0 = 0,
                        dtick = 20000, 
                        tickformat = "digits"),
           legend = list(title = list(text = "Immunity Level")),
           annotations = list(x = 0.5, y = 1, text = unique(data$disruption), showarrow = F, xref='paper', yref='paper', yanchor = "bottom", xanchor = "center", align = "center"),
           barmode = "group")
}


fig <- subplot(
  age_infection_immunity(input %>% filter(disruption == "No disruption")), 
  age_infection_immunity(input %>% filter(disruption == "Beginning of disruption")), 
  age_infection_immunity(input %>% filter(disruption == "1 year after disruption")), 
  age_infection_immunity(input %>% filter(disruption == "2 years after disruption")),
  nrows = 2,
  shareX = FALSE,
  shareY = TRUE,
  margin = c(0.02, 0.02, 0.05, 0.06)
)

for(n in 5:16){
  fig[["x"]][["data"]][[n]][["showlegend"]] <-  FALSE
}

fig

# -------------------------------------------------------------------------

# DISEASE
# no longer considering month of birth

# plot monthly first occurrence of disease by disruption level
# key question: how does the temporal trend of disease change with disruption level?

births_year_disrupt %>% 
  group_by(disruption, time, month, level) %>% 
  summarise(across(c("susceptible", "infected", "disease", "population"), sum, na.rm = TRUE)) %>% 
  filter(level == "total") %>% 
  mutate(level = as.character(level),
         disruption = factor(disruption, levels = 0:3, labels = c("No disruption", "Beginning of disruption", "1 year after disruption", "2 years after disruption"))) %>% 
  plot_ly() %>% 
  add_trace(
    x = ~time,
    y = ~disease,
    split = ~disruption,
    color = ~disruption,
    colors = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"),
    type = "scatter",
    mode = "line",
    text = ~month
  ) %>% 
  layout(xaxis = list(title = "Calendar month",
                      range = list(1, 48),
                      tickmode = "array",
                      tickvals = ~time,
                      ticktext = ~month,
                      tickangle = -45),
         yaxis = list(title = "Count",
                      range = list(0, 80000),
                      tickmode = "linear",
                      tick0 = 0,
                      dtick = 10000, 
                      tickformat = "digits"))

# plot monthly first occurrence of disease by disruption level and immunity level (sub-analysis of above)
# key question: how does the temporal trend of disease change with disruption level by immunity level?

input <- births_year_disrupt %>% 
  group_by(disruption, time, month, level) %>% 
  summarise(across(c("susceptible", "infected", "disease", "population"), sum, na.rm = TRUE)) %>% 
  filter(level != "total") %>% 
  mutate(level = as.character(level),
         disruption = factor(disruption, levels = 0:3, labels = c("No disruption", "Beginning of disruption", "1 year after disruption", "2 years after disruption")))

monthly_disease_immunity <- function(data){
  fig <- plot_ly() %>% 
    add_trace(data = data,
              x = ~time,
              y = ~disease,
              split = ~level,
              color = ~level, 
              colors = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"),
              legendgroup = ~level,
              type = "scatter",
              mode = "line",
              text = ~month,
              hovertemplate = paste('<b>Month</b>: %{text}',
                                    '<br><b>Count</b>: %{y}',
                                    '<extra></extra>'),
              showlegend = TRUE) %>% 
    layout(xaxis = list(title = "Calendar month",
                        range = list(1, 48),
                        tickmode = "array",
                        tickvals = seq(1, 48, 3),
                        ticktext = rep(c(month.abb[1], month.abb[4], month.abb[7], month.abb[10]), 4),
                        tickangle = -45),
           yaxis = list(title = "Count",
                        range = list(0, 80000),
                        tickmode = "linear",
                        tick0 = 0,
                        dtick = 10000, 
                        tickformat = "digits"),
           legend = list(title = list(text = "Immunity Level")),
           annotations = list(x = 0.5, y = 1, text = unique(data$disruption), showarrow = F, xref='paper', yref='paper', yanchor = "bottom", xanchor = "center", align = "center"))
}


fig <- subplot(
  monthly_disease_immunity(input %>% filter(disruption == "No disruption")), 
  monthly_disease_immunity(input %>% filter(disruption == "Beginning of disruption")), 
  monthly_disease_immunity(input %>% filter(disruption == "1 year after disruption")), 
  monthly_disease_immunity(input %>% filter(disruption == "2 years after disruption")),
  nrows = 2,
  shareX = FALSE,
  shareY = TRUE,
  margin = c(0.02, 0.02, 0.05, 0.06)
)

for(n in 5:16){
  fig[["x"]][["data"]][[n]][["showlegend"]] <-  FALSE
}

fig

# plot annual first occurrence of disease by disruption level
# key question: is there more or less disease in babies in one year before disruption, year of disruption, 1 year after disruption or 2 years after disruption?

births_year_disrupt %>% 
  group_by(disruption, level) %>% 
  summarise(across(c("susceptible", "infected", "disease", "population"), sum, na.rm = TRUE)) %>% 
  filter(level == "total") %>% 
  mutate(level = as.character(level),
         disruption = factor(disruption, levels = 0:3, labels = c("No disruption", "Beginning of disruption", "1 year after disruption", "2 years after disruption"))) %>% 
  plot_ly() %>% 
  add_trace(x = ~disruption,
            y = ~disease,
            color = ~disruption,
            colors = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"),
            type = "bar",
            showlegend = FALSE) %>%
  layout(yaxis = list(title = "Count",
                      range = list(0, 300000),
                      tickmode = "linear",
                      tick0 = 0,
                      dtick = 50000, 
                      tickformat = "digits"))

# plot annual first occurrence of disease by disruption level and immunity level (sub-analysis of above)
# key question: is there more or less disease in babies of certain immunity levels in one year before disruption, year of disruption, 1 year after disruption or 2 years after disruption?

births_year_disrupt %>% 
  group_by(disruption, level) %>% 
  summarise(across(c("susceptible", "infected", "disease", "population"), sum, na.rm = TRUE)) %>% 
  filter(level != "total") %>% 
  mutate(level = as.character(level),
         disruption = factor(disruption, levels = 0:3, labels = c("No disruption", "Beginning of disruption", "1 year after disruption", "2 years after disruption"))) %>% 
  plot_ly() %>% 
  add_trace(x = ~disruption,
            y = ~disease,
            split = ~level,
            color = ~level,
            colors = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"),
            type = "bar",
            showlegend = TRUE) %>%
  layout(xaxis = list(title = "Disruption Level"),
         yaxis = list(title = "Count",
                      range = list(0, 250000),
                      tickmode = "linear",
                      tick0 = 0,
                      dtick = 50000, 
                      tickformat = "digits"),
         legend = list(title = list(text = "Immunity Level")),
         barmode = "group")

# plot age distribution of first occurrence of disease by disruption level
# key question: does the age distribution of disease change before and after disruption?

births_age_disrupt %>% 
  group_by(type, disruption, level, age) %>% 
  summarise(count = sum(count)) %>% 
  mutate(level = as.character(level),
         disruption = factor(disruption, levels = 0:3, labels = c("No disruption", "Beginning of disruption", "1 year after disruption", "2 years after disruption"))) %>% 
  filter(level == "total", type == "disease") %>% 
  plot_ly() %>% 
  add_trace(x = ~age,
            y = ~count,
            split = ~disruption,
            color = ~disruption,
            colors = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"),
            type = "bar",
            showlegend = TRUE) %>%
  layout(xaxis = list(title = "Age Group"),
         yaxis = list(title = "Count",
                      range = list(0, 180000),
                      tickmode = "linear",
                      tick0 = 0,
                      dtick = 20000,
                      tickformat = "digits"),
         legend = list(title = list(text = "Disruption Level")),
         barmode = "group")

# plot age distribution of first occurrence of disease by disruption level and immunity level (sub-analysis of above)
# key question: are there immunity-level-related patterns that contribute to the age distribution of disease before and after disruption?

input <- births_age_disrupt %>% 
  group_by(type, disruption, level, age) %>% 
  summarise(count = sum(count)) %>% 
  mutate(level = as.character(level),
         disruption = factor(disruption, levels = 0:3, labels = c("No disruption", "Beginning of disruption", "1 year after disruption", "2 years after disruption"))) %>% 
  filter(level != "total", type == "disease")

age_infection_immunity <- function(data){
  fig <- plot_ly() %>% 
    add_trace(data = data,
              x = ~age,
              y = ~count,
              split = ~level,
              color = ~level, 
              colors = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"),
              legendgroup = ~level,
              type = "bar",
              showlegend = TRUE) %>% 
    layout(xaxis = list(title = "Age Group"),
           yaxis = list(title = "Count",
                        range = list(0, 160000),
                        tickmode = "linear",
                        tick0 = 0,
                        dtick = 20000, 
                        tickformat = "digits"),
           legend = list(title = list(text = "Immunity Level")),
           annotations = list(x = 0.5, y = 1, text = unique(data$disruption), showarrow = F, xref='paper', yref='paper', yanchor = "bottom", xanchor = "center", align = "center"),
           barmode = "group")
}


fig <- subplot(
  age_infection_immunity(input %>% filter(disruption == "No disruption")), 
  age_infection_immunity(input %>% filter(disruption == "Beginning of disruption")), 
  age_infection_immunity(input %>% filter(disruption == "1 year after disruption")), 
  age_infection_immunity(input %>% filter(disruption == "2 years after disruption")),
  nrows = 2,
  shareX = FALSE,
  shareY = TRUE,
  margin = c(0.02, 0.02, 0.05, 0.06)
)

for(n in 5:16){
  fig[["x"]][["data"]][[n]][["showlegend"]] <-  FALSE
}

fig
