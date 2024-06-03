## plot outputs of births model
## load libraries
library(dplyr)
library(ggplot2)
library(plotly)
library(viridisLite)
library(stringr)

# plot rate of exposure
plot_rate <- function(data){
  fig <- plot_ly() %>%
    add_trace(data = data %>% filter(!is.na(rate)),
              x = ~time,
              y = ~rate,
              type = "scatter",
              mode = "lines",
              text = ~month,
              hovertemplate = paste('<b>Month</b>: %{text}',
                                    '<br><b>Rate</b>: %{y}',
                                    '<extra></extra>')) %>%
    layout(xaxis = list(title = "Time",
                        tickmode = "array",
                        ticktext = list("January", "July", "January", "July", "January", "July", "January"),
                        tickvals = list(1, 7, 13, 19, 25, 31, 37)),
           yaxis = list(title = "Rate of exposure"))
  
  return(fig)
}

# plot probability of infection/disease

plot_probability <- function(data, type = c("infection", "disease")){
  
  if(type == "infection"){
    fig <- plot_ly() %>%
      add_trace(data = data %>% filter(!is.na(rate)),
                x = ~time,
                y = ~prob_inf,
                split = ~level,
                type = "scatter",
                mode = "lines",
                text = ~month,
                hovertemplate = paste('<b>Month</b>: %{text}',
                                      '<br><b>Probability</b>: %{y}',
                                      '<extra></extra>')) %>%
      layout(xaxis = list(title = "Time since birth",
                          tickmode = "array",
                          ticktext = list("January", "July", "January", "July", "January", "July", "January"),
                          tickvals = list(1, 7, 13, 19, 25, 31, 37)),
             yaxis = list(title = "Probability of infection"),
             legend = list(title = list(text = "Immunity Level")))
  } else {
    fig <- plot_ly() %>%
      add_trace(data = data %>% filter(!is.na(rate)),
                x = ~time,
                y = ~prob_dis,
                split = ~level,
                type = "scatter",
                mode = "lines",
                text = ~month,
                hovertemplate = paste('<b>Month</b>: %{text}',
                                      '<br><b>Probability</b>: %{y}',
                                      '<extra></extra>')) %>%
      layout(xaxis = list(title = "Time since birth",
                          tickmode = "array",
                          ticktext = list("January", "July", "January", "July", "January", "July", "January"),
                          tickvals = list(1, 7, 13, 19, 25, 31, 37)),
             yaxis = list(title = "Probability of disease"),
             legend = list(title = list(text = "Immunity Level")))
  }
  
  return(fig)
}

# plot monthly count of infections

plot_infections <- function(data){
  
  fig <- plot_ly() %>% 
    add_trace(data = data,
              x = ~time,
              y = ~infected,
              split = ~level,
              color = ~level, 
              colors = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd"),
              legendgroup = ~level,
              type = "scatter",
              mode = "lines",
              text = ~month,
              hovertemplate = paste('<b>Month</b>: %{text}',
                                    '<br><b>Count</b>: %{y}',
                                    '<extra></extra>'),
              showlegend = TRUE) %>% 
    layout(xaxis = list(title = "Time since birth",
                        range = list(1, 48),
                        tickmode = "array",
                        tickvals = seq(1, 48, 3),
                        ticktext = rep(c(month.abb[1], month.abb[4], month.abb[7], month.abb[10]), 4),
                        tickangle = -45),
           yaxis = list(title = "Count",
                        range = list(0, 12000),
                        tickmode = "linear",
                        tick0 = 0,
                        dtick = 2000, 
                        tickformat = "digits"),
           legend = list(title = list(text = "Immunity Level")),
           annotations = list(x = 0.5, y = 1, text = unique(data$month_born), showarrow = F, xref='paper', yref='paper', yanchor = "bottom", xanchor = "center", align = "center"))
  
  return(fig)
}

# plot monthly count of disease

plot_disease <- function(data){
  
  fig <- plot_ly() %>% 
    add_trace(data = data,
              x = ~time,
              y = ~disease,
              split = ~level,
              color = ~level,
              colors = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd"),
              legendgroup = ~level,
              type = "scatter",
              mode = "lines",
              text = ~month,
              hovertemplate = paste('<b>Month</b>: %{text}',
                                    '<br><b>Count</b>: %{y}',
                                    '<extra></extra>'),
              showlegend = TRUE) %>% 
    layout(xaxis = list(title = "Time since birth",
                        range = list(1, 48),
                        tickmode = "array",
                        tickvals = seq(1, 48, 3),
                        ticktext = rep(c(month.abb[1], month.abb[4], month.abb[7], month.abb[10]), 4),
                        tickangle = -45),
           yaxis = list(title = "Count",
                        range = list(0, 10000),
                        tickmode = "linear",
                        tick0 = 0,
                        dtick = 2000, 
                        tickformat = "digits"),
           legend = list(title = list(text = "Immunity Level")),
           annotations = list(x = 0.5, y = 1, text = unique(data$month_born), showarrow = F, xref='paper', yref='paper', yanchor = "bottom", xanchor = "center", align = "center"))
  
  return(fig)
}

# plot of cumulative proportion of babies infected by calendar month

plot_cumpropinf <- function(data){
  fig <- data %>%
    filter(level == "total") %>%
    group_by(month_born) %>%
    mutate(cum_sum = cumsum(infected),
           prop = (cum_sum/population)*100) %>%
    plot_ly() %>%
    add_trace(x = ~time,
              y = ~prop,
              split = ~month_born,
              color = ~month_born,
              legendgroup = ~month_born,
              type = "scatter",
              mode = "lines",
              text = ~month,
              hovertemplate = paste('<b>Month</b>: %{text}',
                                    '<br><b>Proportion Infected</b>: %{y}',
                                    '<extra></extra>'),
              showlegend = TRUE) %>%
    layout(xaxis = list(title = "Calendar months",
                        tickangle = -45,
                        tickmode = "array",
                        ticktext = rep(c("Jan", "Mar", "Jul", "Oct"), 4),
                        tickvals = seq(1, 48, 3)),
           yaxis = list(title = "Proportion Infected (%)",
                        range = list(0, 100),
                        tickmode = "linear",
                        tick0 = 0,
                        dtick = 20))

  return(fig)
}

# plot of cumulative proportion of babies with disease by calendar month

plot_cumpropdis <- function(data){
  fig <- data %>%
    filter(level == "total") %>%
    group_by(month_born) %>%
    mutate(cum_sum = cumsum(disease),
           prop = (cum_sum/population)*100) %>%
    plot_ly() %>%
    add_trace(x = ~time,
              y = ~prop,
              split = ~month_born,
              color = ~month_born,
              legendgroup = ~month_born,
              type = "scatter",
              mode = "lines",
              text = ~month,
              hovertemplate = paste('<b>Month</b>: %{text}',
                                    '<br><b>Proportion with disease</b>: %{y}',
                                    '<extra></extra>'),
              showlegend = TRUE) %>%
    layout(xaxis = list(title = "Calendar months",
                        tickangle = -45,
                        tickmode = "array",
                        ticktext = rep(c("Jan", "Mar", "Jul", "Oct"), 4),
                        tickvals = seq(1, 48, 3)),
           yaxis = list(title = "Proportion with disease (%)",
                        range = list(0, 50),
                        tickmode = "linear",
                        tick0 = 0,
                        dtick = 5))

  return(fig)
}

# plot age at infection and disease

plot_age <- function(data, types = c("infected", "disease")){
  fig <- data %>% 
    filter(type == types) %>%
    plot_ly() %>%
    add_trace(x = ~age,
              y = ~count,
              split = ~level,
              color = ~level,
              colors = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd"),
              type = "bar",
              legendgroup = ~level,
              showlegend = TRUE) %>% 
    layout(yaxis = list(title = "Count",
                        range = list(0, ifelse(types == "infected", 31000, 30000)),
                        tickmode = "linear",
                        tick0 = 0,
                        dtick = 5000,
                        tickformat = "digits"),
           xaxis = list(title = "Age Group"),
           annotations = list(x = 0.5, y = 1, text = unique(data$month_born), showarrow = F, xref='paper', yref='paper', yanchor = "bottom", xanchor = "center", align = "center"))
  
  return(fig)
}

# plot cumulative proportion of infection/disease by month of age

plot_cumprop <- function(data, type = c("infection", "disease")){
  
  if(type == "infection"){
    
    fig <- data %>%
      filter(level == "total") %>%
      group_by(month_born) %>%
      mutate(cum_sum = cumsum(infected),
             prop = (cum_sum/population)*100) %>%
      plot_ly() %>%
      add_trace(x = ~time,
                y = ~prop,
                split = ~month_born,
                color = ~month_born,
                legendgroup = ~month_born,
                type = "scatter",
                mode = "lines",
                hovertemplate = paste('<b>Age (months)</b>: %{x}',
                                      '<br><b>Proportion Infected</b>: %{y}',
                                      '<extra></extra>'),
                showlegend = TRUE) %>%
      layout(xaxis = list(title = "Month of age",
                          range = list(0, 36),
                          tickmode = "linear",
                          tick0 = 0,
                          dtick = 3),
             yaxis = list(title = "Proportion Infected (%)",
                          range = list(0, 100),
                          tickmode = "linear",
                          tick0 = 0,
                          dtick = 20))
    
    return(fig)
  } else {
    
    fig <- data %>%
      filter(level == "total") %>%
      group_by(month_born) %>%
      mutate(cum_sum = cumsum(disease),
             prop = (cum_sum/population)*100) %>%
      plot_ly() %>%
      add_trace(x = ~time,
                y = ~prop,
                split = ~month_born,
                color = ~month_born,
                legendgroup = ~month_born,
                type = "scatter",
                mode = "lines",
                hovertemplate = paste('<b>Age (months)</b>: %{x}',
                                      '<br><b>Proportion with disease</b>: %{y}',
                                      '<extra></extra>'),
                showlegend = TRUE) %>%
      layout(xaxis = list(title = "Month of age",
                          range = list(0, 36),
                          tickmode = "linear",
                          tick0 = 0,
                          dtick = 3),
             yaxis = list(title = "Proportion with disease (%)",
                          range = list(0, 50),
                          tickmode = "linear",
                          tick0 = 0,
                          dtick = 5))
    
    return(fig)
    
  }
}
