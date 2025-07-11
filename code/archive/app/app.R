# Load packages

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(viridisLite)
library(plotly)

# Define UI

ui <- fluidPage(
  
  sidebarLayout(
    
    # Inputs: Select variables to plot
    sidebarPanel(
      
      # Select duration of immunity
      sliderInput(inputId = "immunity", 
                  label = h4("Duration of Immunity"),
                  min = 1, max = 60, value = 24),
      # Select scale for rate of infection
      sliderInput(inputId = "scale", 
                  label = h4("Scaling Factor for Rate of Infection"),
                  min = 0, max = 10, value = 5),
      checkboxInput(inputId = "disruption",
                    label = "Disruption",
                    value = TRUE),
      width = 3
    ),
    
    # Output: Show scatterplot
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Mothers",
                 plotlyOutput(outputId = "women_rate"),
                 plotOutput(outputId = "epidemic_curve"),
                 plotOutput(outputId = "infection_history")),
        tabPanel("Babies",
                 plotlyOutput(outputId = "babies_rate"),
                 plotlyOutput(outputId = "babies_proportion")),
        tabPanel("Mothers - Extra",
                 plotOutput(outputId = "cum_annual"),
                 plotOutput(outputId = "cum_monthly"),
                 plotOutput(outputId = "prop_heatmap"))
      ),
      width = 9
    )
  )
)

# Define server

server <- function(input, output, session) {
  output$women_rate <- renderPlotly({
    # history of infection model in women, calculate infection status in 1,000,000 women based on monthly infection rate
    n_interest <- input$immunity # number of months of interest for history of infection
    rep <- 75 # number of years
    rate_scale <- input$scale # scaling rate of infection for exploration
    n_burn <- 65 # burn-in period
    
    women <- as.data.frame(matrix(NA, 12*rep, 5+n_interest))
    colnames(women) <- c("time", "month", "susceptible_naive", "susceptible_reinf", "rate", str_c(rep("I", n_interest), 1:n_interest))
    
    women <- women %>% mutate(month = rep(month.abb, rep),
                              time = 1:nrow(women),
                              # rate = 0.05,
                              # rate = ifelse(month %in% c(month.abb[11:12], month.abb[1:3]), 0.10, 0.05),
                              rate = (case_when(month == month.abb[1] ~ 0.015,
                                                month %in% month.abb[2:3] ~ 0.005,
                                                month %in% month.abb[4:8] ~ 0.000,
                                                month == month.abb[9] ~ 0.010,
                                                month == month.abb[10] ~ 0.020,
                                                month %in% month.abb[11:12] ~ 0.045))*rate_scale)
    
    if(input$disruption == TRUE){
      women[(12*n_burn+3):(12*(n_burn+1)+3), "rate"] <- 0 # setting disruption after 25 year burn-in,  0 between March
    }
    
    women %>% 
      filter(time > 12*60) %>%
      plot_ly() %>% 
      add_trace(x = ~time,
                y = ~rate,
                type = "scatter",
                mode = "lines",
                text = ~month,
                hovertemplate = paste('<b>Month</b>: %{text}',
                                      '<br><b>Rate</b>: %{y}',
                                      '<extra></extra>')) %>% 
      layout(xaxis = list(title = "Time",
                          tickmode = "array",
                          ticktext = c(rep(c("January", "July"), rep), "January"),
                          tickvals = seq(1, nrow(women), 6),
                          tickangle = -45),
             yaxis = list(title = "Rate of Infection"))
  })
  
  output$epidemic_curve <- renderPlot({
    # history of infection model in women, calculate infection status in 1,000,000 women based on monthly infection rate
    n_interest <- input$immunity # number of months of interest for history of infection
    rep <- 75 # number of years
    rate_scale <- input$scale # scaling rate of infection for exploration
    n_burn <- 65 # burn-in period
    
    women <- as.data.frame(matrix(NA, 12*rep, 5+n_interest))
    colnames(women) <- c("time", "month", "susceptible_naive", "susceptible_reinf", "rate", str_c(rep("I", n_interest), 1:n_interest))
    
    women <- women %>% mutate(month = rep(month.abb, rep),
                              time = 1:nrow(women),
                              # rate = 0.05,
                              # rate = ifelse(month %in% c(month.abb[11:12], month.abb[1:3]), 0.10, 0.05),
                              rate = (case_when(month == month.abb[1] ~ 0.015,
                                                month %in% month.abb[2:3] ~ 0.005,
                                                month %in% month.abb[4:8] ~ 0.000,
                                                month == month.abb[9] ~ 0.010,
                                                month == month.abb[10] ~ 0.020,
                                                month %in% month.abb[11:12] ~ 0.045))*rate_scale)
    
    if(input$disruption == TRUE){
      women[(12*n_burn+3):(12*(n_burn+1)+3), "rate"] <- 0 # setting disruption after 25 year burn-in, 0 between March
    }
    
    # initial state
    women[1, "susceptible_naive"] <- 1000000
    # women[1, "I1"] <- 200000
    # women[1, "I2"] <- 100000
    # women[1, "I24"] <- 1000000 # problem when susceptible naive is 0 at beginning
    
    
    for (row in 1:nrow(women)) {
      women[row + 1, "I1"] <-   ifelse(is.na(women[row, "susceptible_reinf"]), women[row, "susceptible_naive"] * women[row, "rate"], (women[row, "susceptible_naive"] * women[row, "rate"]) + (women[row, "susceptible_reinf"] * women[row, "rate"]))
      women[row + 1, "susceptible_naive"] <- women[row, "susceptible_naive"] - (women[row, "susceptible_naive"] * women[row, "rate"])
      women[row + 1, "susceptible_reinf"] <- ifelse(is.na(women[row, "susceptible_reinf"]), women[row, paste0("I", n_interest)], women[row, "susceptible_reinf"] - (women[row, "susceptible_reinf"] * women[row, "rate"]) + women[row, paste0("I", n_interest)])
      
      for (month in 2:n_interest){
        if(!is.na(women[row, paste0("I", month-1)])){
          women[row + 1, paste0("I", month)] <- women[row, paste0("I", month-1)]
        } else {women[row + 1, paste0("I", month)] <- NA}
      }
    }
    
    # plot monthly infections
    women %>% 
      filter(time > 12*60) %>%
      mutate(I = lead(I1, 1)) %>% 
      ggplot() +
      geom_line(aes(x = time, y = I)) +
      # geom_line(aes(x = time, y = rate*500000), linetype = "dashed", colour = "blue") +
      scale_x_continuous(breaks = seq(1, nrow(women), 6), labels = c(rep(c("January", "July"), rep), "January")) +
      scale_y_continuous(name = "Infection Count",
                         # sec.axis = sec_axis(~./500000, name = "Rate of Infection")
                         ) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            # axis.title.y.right = element_text(color = "blue"),
            # axis.text.y.right = element_text(color = "blue"),
            # axis.ticks.y.right = element_line(color = "blue"),
            # axis.line.y.right = element_line(color = "blue")
            ) +
      labs(x = "Time")
  })
  
  output$infection_history <- renderPlot({
    # history of infection model in women, calculate infection status in 1,000,000 women based on monthly infection rate
    n_interest <- input$immunity # number of months of interest for history of infection
    rep <- 75 # number of years
    rate_scale <- input$scale # scaling rate of infection for exploration
    n_burn <- 65 # burn-in period
    
    women <- as.data.frame(matrix(NA, 12*rep, 5+n_interest))
    colnames(women) <- c("time", "month", "susceptible_naive", "susceptible_reinf", "rate", str_c(rep("I", n_interest), 1:n_interest))
    
    women <- women %>% mutate(month = rep(month.abb, rep),
                              time = 1:nrow(women),
                              # rate = 0.05,
                              # rate = ifelse(month %in% c(month.abb[11:12], month.abb[1:3]), 0.10, 0.05),
                              rate = (case_when(month == month.abb[1] ~ 0.015,
                                                month %in% month.abb[2:3] ~ 0.005,
                                                month %in% month.abb[4:8] ~ 0.000,
                                                month == month.abb[9] ~ 0.010,
                                                month == month.abb[10] ~ 0.020,
                                                month %in% month.abb[11:12] ~ 0.045))*rate_scale)
    
    if(input$disruption == TRUE){
      women[(12*n_burn+3):(12*(n_burn+1)+3), "rate"] <- 0 # setting disruption after 25 year burn-in, 0 between March
    }
    
    # initial state
    women[1, "susceptible_naive"] <- 1000000
    # women[1, "I1"] <- 200000
    # women[1, "I2"] <- 100000
    # women[1, "I24"] <- 1000000 # problem when susceptible naive is 0 at beginning
    
    
    for (row in 1:nrow(women)) {
      women[row + 1, "I1"] <-   ifelse(is.na(women[row, "susceptible_reinf"]), women[row, "susceptible_naive"] * women[row, "rate"], (women[row, "susceptible_naive"] * women[row, "rate"]) + (women[row, "susceptible_reinf"] * women[row, "rate"]))
      women[row + 1, "susceptible_naive"] <- women[row, "susceptible_naive"] - (women[row, "susceptible_naive"] * women[row, "rate"])
      women[row + 1, "susceptible_reinf"] <- ifelse(is.na(women[row, "susceptible_reinf"]), women[row, paste0("I", n_interest)], women[row, "susceptible_reinf"] - (women[row, "susceptible_reinf"] * women[row, "rate"]) + women[row, paste0("I", n_interest)])
      
      for (month in 2:n_interest){
        if(!is.na(women[row, paste0("I", month-1)])){
          women[row + 1, paste0("I", month)] <- women[row, paste0("I", month-1)]
        } else {women[row + 1, paste0("I", month)] <- NA}
      }
    }
    
    # reshaping data for plotting
    women.long <- women %>% 
      filter(!is.na(month)) %>% 
      mutate(sum = rowSums(across(c("susceptible_naive", "susceptible_reinf", "I1":paste0("I", n_interest))), na.rm = TRUE)) %>% 
      pivot_longer(c("I1":paste0("I", n_interest), "susceptible_naive", "susceptible_reinf"), names_to = "infection", values_to = "count") %>% 
      mutate(infection = factor(infection, levels = c("susceptible_naive", "susceptible_reinf", rev(str_c(rep("I", n_interest), 1:n_interest)))),
             proportion = count/sum)
    
    # plot infection status in women
    women.long %>% 
      filter(time > 12*60) %>%
      ggplot() +
      geom_bar(aes(x = time, y = proportion, fill = infection), position = "fill", stat = "identity") +
      # geom_line(aes(x = time, y = rate*10), linetype = "solid", colour = "red") +
      scale_fill_manual(values = c("lightgrey", "darkgrey", viridis(n_interest))) +
      scale_x_continuous(breaks = seq(1, nrow(women), 6), labels = c(rep(c("January", "July"), rep), "January")) +
      # scale_y_continuous(sec.axis = sec_axis(~./10, name = "Rate of Infection")) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom",
            legend.direction = "horizontal"
            # axis.title.y.right = element_text(color = "red"),
            # axis.text.y.right = element_text(color = "red"),
            # axis.ticks.y.right = element_line(color = "red"),
            # axis.line.y.right = element_line(color = "red")
      ) +
      guides(fill = guide_legend(nrow = ifelse(input$immunity > 40, 3, 2))) +
      labs(x = "Time",
           y = "Proportion",
           fill = "Infection Status")
  })
  
  output$babies_rate <- renderPlotly({
    rep <- 3 # number of years
    rate_scale <- input$scale
    
    births <- as.data.frame(matrix(NA, 12*rep, 5))
    colnames(births) <- c("time", "month", "rate", "susceptible", "infected")
    
    births <- births %>% mutate(month = rep(month.abb, rep),
                                time = 1:nrow(births),
                                # rate = 0.05,
                                # rate = ifelse(month %in% c(month.abb[11:12], month.abb[1:3]), 0.10, 0.05),
                                rate = (case_when(month == month.abb[1] ~ 0.015,
                                                  month %in% month.abb[2:3] ~ 0.005,
                                                  month %in% month.abb[4:8] ~ 0.000,
                                                  month == month.abb[9] ~ 0.010,
                                                  month == month.abb[10] ~ 0.020,
                                                  month %in% month.abb[11:12] ~ 0.045))*rate_scale)
    
    births[1, "susceptible"] <- 61942
    
    for(row in 1:nrow(births)){
      births[row, "infected"] <- births[row, "susceptible"] * births[row, "rate"]
      births[row + 1, "susceptible"] <- births[row, "susceptible"] - births[row, "infected"]
    }
    
    births <- births %>% 
      mutate(cum_sum = cumsum(infected),
             prop = (cum_sum/61942)*100)
    
    births %>% 
      plot_ly() %>% 
      add_trace(x = ~time,
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
             yaxis = list(title = "Rate of Infection"))
  })
  
  output$babies_proportion <- renderPlotly({
    # testing for one month of births (first 3 years of life)
    rep <- 3 # number of years
    rate_scale <- input$scale # scaling rate of infection for exploration
    
    births <- as.data.frame(matrix(NA, 12*rep, 5))
    colnames(births) <- c("time", "month", "rate", "susceptible", "infected")
    
    births <- births %>% mutate(month = rep(month.abb, rep),
                                time = 1:nrow(births),
                                # rate = 0.05,
                                # rate = ifelse(month %in% c(month.abb[11:12], month.abb[1:3]), 0.10, 0.05),
                                rate = (case_when(month == month.abb[1] ~ 0.015,
                                                  month %in% month.abb[2:3] ~ 0.005,
                                                  month %in% month.abb[4:8] ~ 0.000,
                                                  month == month.abb[9] ~ 0.010,
                                                  month == month.abb[10] ~ 0.020,
                                                  month %in% month.abb[11:12] ~ 0.045))*rate_scale)
    
    births[1, "susceptible"] <- 61942
    
    for(row in 1:nrow(births)){
      births[row, "infected"] <- births[row, "susceptible"] * births[row, "rate"]
      births[row + 1, "susceptible"] <- births[row, "susceptible"] - births[row, "infected"]
    }
    
    births <- births %>% 
      mutate(cum_sum = cumsum(infected),
             prop = (cum_sum/61942)*100)
    
    births %>% 
      plot_ly() %>% 
      add_trace(x = ~time,
                y = ~prop,
                type = "scatter",
                mode = "lines",
                text = ~month,
                hovertemplate = paste('<b>Month</b>: %{text}',
                                      '<br><b>Proportion Infected</b>: %{y}',
                                      '<extra></extra>')) %>% 
      layout(xaxis = list(title = "Time",
                          tickmode = "array",
                          ticktext = list("January", "July", "January", "July", "January", "July", "January"),
                          tickvals = list(1, 7, 13, 19, 25, 31, 37)),
             yaxis = list(title = "Proportion Infected (%)"))
  })
  
  output$cum_annual <- renderPlot({
    # history of infection model in women, calculate infection status in 1,000,000 women based on monthly infection rate
    n_interest <- input$immunity # number of months of interest for history of infection
    rep <- 75 # number of years
    rate_scale <- input$scale # scaling rate of infection for exploration
    n_burn <- 65 # burn-in period
    
    women <- as.data.frame(matrix(NA, 12*rep, 5+n_interest))
    colnames(women) <- c("time", "month", "susceptible_naive", "susceptible_reinf", "rate", str_c(rep("I", n_interest), 1:n_interest))
    
    women <- women %>% mutate(month = rep(month.abb, rep),
                              time = 1:nrow(women),
                              # rate = 0.05,
                              # rate = ifelse(month %in% c(month.abb[11:12], month.abb[1:3]), 0.10, 0.05),
                              rate = (case_when(month == month.abb[1] ~ 0.015,
                                                month %in% month.abb[2:3] ~ 0.005,
                                                month %in% month.abb[4:8] ~ 0.000,
                                                month == month.abb[9] ~ 0.010,
                                                month == month.abb[10] ~ 0.020,
                                                month %in% month.abb[11:12] ~ 0.045))*rate_scale)
    
    if(input$disruption == TRUE){
      women[(12*n_burn+3):(12*(n_burn+1)+3), "rate"] <- 0 # setting disruption after 25 year burn-in, 0 between March
    }
    
    # initial state
    women[1, "susceptible_naive"] <- 1000000
    # women[1, "I1"] <- 200000
    # women[1, "I2"] <- 100000
    # women[1, "I24"] <- 1000000 # problem when susceptible naive is 0 at beginning
    
    
    for (row in 1:nrow(women)) {
      women[row + 1, "I1"] <-   ifelse(is.na(women[row, "susceptible_reinf"]), women[row, "susceptible_naive"] * women[row, "rate"], (women[row, "susceptible_naive"] * women[row, "rate"]) + (women[row, "susceptible_reinf"] * women[row, "rate"]))
      women[row + 1, "susceptible_naive"] <- women[row, "susceptible_naive"] - (women[row, "susceptible_naive"] * women[row, "rate"])
      women[row + 1, "susceptible_reinf"] <- ifelse(is.na(women[row, "susceptible_reinf"]), women[row, paste0("I", n_interest)], women[row, "susceptible_reinf"] - (women[row, "susceptible_reinf"] * women[row, "rate"]) + women[row, paste0("I", n_interest)])
      
      for (month in 2:n_interest){
        if(!is.na(women[row, paste0("I", month-1)])){
          women[row + 1, paste0("I", month)] <- women[row, paste0("I", month-1)]
        } else {women[row + 1, paste0("I", month)] <- NA}
      }
    }
    
    # reshaping data for plotting
    women.annual.v1 <- women %>%
      filter(!is.na(month)) %>% 
      mutate(year = rep(1:rep, each = 12),
             I0 = lead(I1)) %>% 
      group_by(year) %>% 
      summarise(infected = sum(I0)) %>% 
      ungroup()
    
    # plot infection status in women
    women.annual.v1 %>% 
      filter(year > 60) %>%
      ggplot(aes(x = year, y = infected)) +
      scale_x_continuous(breaks = seq(1, nrow(women), 6), labels = c(rep(c("January", "July"), rep), "January")) +
      geom_bar(stat = "identity") +
      theme_bw() +
      labs(x = "Year",
           y = "Annual Incidence")
  })
  
  output$cum_monthly <- renderPlot({
    # history of infection model in women, calculate infection status in 1,000,000 women based on monthly infection rate
    n_interest <- input$immunity # number of months of interest for history of infection
    rep <- 75 # number of years
    rate_scale <- input$scale # scaling rate of infection for exploration
    n_burn <- 65 # burn-in period
    
    women <- as.data.frame(matrix(NA, 12*rep, 5+n_interest))
    colnames(women) <- c("time", "month", "susceptible_naive", "susceptible_reinf", "rate", str_c(rep("I", n_interest), 1:n_interest))
    
    women <- women %>% mutate(month = rep(month.abb, rep),
                              time = 1:nrow(women),
                              # rate = 0.05,
                              # rate = ifelse(month %in% c(month.abb[11:12], month.abb[1:3]), 0.10, 0.05),
                              rate = (case_when(month == month.abb[1] ~ 0.015,
                                                month %in% month.abb[2:3] ~ 0.005,
                                                month %in% month.abb[4:8] ~ 0.000,
                                                month == month.abb[9] ~ 0.010,
                                                month == month.abb[10] ~ 0.020,
                                                month %in% month.abb[11:12] ~ 0.045))*rate_scale)
    
    if(input$disruption == TRUE){
      women[(12*n_burn+3):(12*(n_burn+1)+3), "rate"] <- 0 # setting disruption after 25 year burn-in, 0 between March
    }
    
    # initial state
    women[1, "susceptible_naive"] <- 1000000
    # women[1, "I1"] <- 200000
    # women[1, "I2"] <- 100000
    # women[1, "I24"] <- 1000000 # problem when susceptible naive is 0 at beginning
    
    
    for (row in 1:nrow(women)) {
      women[row + 1, "I1"] <-   ifelse(is.na(women[row, "susceptible_reinf"]), women[row, "susceptible_naive"] * women[row, "rate"], (women[row, "susceptible_naive"] * women[row, "rate"]) + (women[row, "susceptible_reinf"] * women[row, "rate"]))
      women[row + 1, "susceptible_naive"] <- women[row, "susceptible_naive"] - (women[row, "susceptible_naive"] * women[row, "rate"])
      women[row + 1, "susceptible_reinf"] <- ifelse(is.na(women[row, "susceptible_reinf"]), women[row, paste0("I", n_interest)], women[row, "susceptible_reinf"] - (women[row, "susceptible_reinf"] * women[row, "rate"]) + women[row, paste0("I", n_interest)])
      
      for (month in 2:n_interest){
        if(!is.na(women[row, paste0("I", month-1)])){
          women[row + 1, paste0("I", month)] <- women[row, paste0("I", month-1)]
        } else {women[row + 1, paste0("I", month)] <- NA}
      }
    }
    
    # reshaping data for plotting
    women.annual.v2 <- women %>%
      filter(!is.na(month)) %>%
      tail(-6) %>% # changing years to be jul-jul instead of jan-jan
      head(-6) %>% 
      mutate(year = rep(1:(rep-1), each = 12), # rep decreased by 1 for jul-jul year
             I0 = lead(I1)) %>% 
      group_by(year) %>% 
      mutate(cumsum = cumsum(I0))
    
    # plot infection status in women
    women.annual.v2 %>% 
      mutate(year = as.character(year)) %>% 
      filter(time > 12*60) %>%
      ggplot(aes(x = time, y = cumsum, fill = year)) +
      scale_x_continuous(breaks = seq(1, nrow(women), 6), labels = c(rep(c("January", "July"), rep), "January")) +
      geom_bar(stat = "identity") +
      scale_fill_viridis_d(option = "D") +
      theme_bw() +
      theme(legend.position = "none") +
      labs(x = "Time",
           y = "Cumulative Incidence (Annually)")
  })
  
  output$prop_heatmap <- renderPlot({
    # history of infection model in women, calculate infection status in 1,000,000 women based on monthly infection rate
    n_interest <- input$immunity # number of months of interest for history of infection
    rep <- 75 # number of years
    rate_scale <- input$scale # scaling rate of infection for exploration
    n_burn <- 65 # burn-in period
    
    women <- as.data.frame(matrix(NA, 12*rep, 5+n_interest))
    colnames(women) <- c("time", "month", "susceptible_naive", "susceptible_reinf", "rate", str_c(rep("I", n_interest), 1:n_interest))
    
    women <- women %>% mutate(month = rep(month.abb, rep),
                              time = 1:nrow(women),
                              # rate = 0.05,
                              # rate = ifelse(month %in% c(month.abb[11:12], month.abb[1:3]), 0.10, 0.05),
                              rate = (case_when(month == month.abb[1] ~ 0.015,
                                                month %in% month.abb[2:3] ~ 0.005,
                                                month %in% month.abb[4:8] ~ 0.000,
                                                month == month.abb[9] ~ 0.010,
                                                month == month.abb[10] ~ 0.020,
                                                month %in% month.abb[11:12] ~ 0.045))*rate_scale)
    
    if(input$disruption == TRUE){
      women[(12*n_burn+3):(12*(n_burn+1)+3), "rate"] <- 0 # setting disruption after 25 year burn-in, 0 between March
    }
    
    # initial state
    women[1, "susceptible_naive"] <- 1000000
    # women[1, "I1"] <- 200000
    # women[1, "I2"] <- 100000
    # women[1, "I24"] <- 1000000 # problem when susceptible naive is 0 at beginning
    
    
    for (row in 1:nrow(women)) {
      women[row + 1, "I1"] <-   ifelse(is.na(women[row, "susceptible_reinf"]), women[row, "susceptible_naive"] * women[row, "rate"], (women[row, "susceptible_naive"] * women[row, "rate"]) + (women[row, "susceptible_reinf"] * women[row, "rate"]))
      women[row + 1, "susceptible_naive"] <- women[row, "susceptible_naive"] - (women[row, "susceptible_naive"] * women[row, "rate"])
      women[row + 1, "susceptible_reinf"] <- ifelse(is.na(women[row, "susceptible_reinf"]), women[row, paste0("I", n_interest)], women[row, "susceptible_reinf"] - (women[row, "susceptible_reinf"] * women[row, "rate"]) + women[row, paste0("I", n_interest)])
      
      for (month in 2:n_interest){
        if(!is.na(women[row, paste0("I", month-1)])){
          women[row + 1, paste0("I", month)] <- women[row, paste0("I", month-1)]
        } else {women[row + 1, paste0("I", month)] <- NA}
      }
    }
    
    # reshaping data for plotting
    women.long <- women %>% 
      filter(!is.na(month)) %>% 
      mutate(sum = rowSums(across(c("susceptible_naive", "susceptible_reinf", "I1":paste0("I", n_interest))), na.rm = TRUE)) %>% 
      pivot_longer(c("I1":paste0("I", n_interest), "susceptible_naive", "susceptible_reinf"), names_to = "infection", values_to = "count") %>% 
      mutate(infection = factor(infection, levels = c("susceptible_naive", "susceptible_reinf", rev(str_c(rep("I", n_interest), 1:n_interest)))),
             proportion = count/sum)
    
    # plot infection status in women
    women.long %>% 
      filter(time > 12*60) %>%
      filter(!infection %in% c("susceptible_naive", "susceptible_reinf")) %>%
      ggplot() +
      geom_tile(aes(x = time, y = infection, fill = proportion)) +
      scale_x_continuous(breaks = seq(1, nrow(women), 6), labels = c(rep(c("January", "July"), rep), "January")) +
      scale_y_discrete(limits = rev) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "right") +
      labs(x = "Time",
           y = "Time Since Infection",
           fill = "Proportion (%)")
  })
}

# Create a Shiny app object

shinyApp(ui = ui, server = server)