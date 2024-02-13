# Load packages

library(shiny)
library(ggplot2)

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
                  min = 0, max = 5, value = 1)
    ),
    
    # Output: Show scatterplot
    mainPanel(
      plotOutput(outputId = "epidemic_curve"),
      plotOutput(outputId = "infection_history")
    )
  )
)

# Define server

server <- function(input, output, session) {
  output$epidemic_curve <- renderPlot({
    # history of infection model in women, calculate infection status in 1,000,000 women based on monthly infection rate
    n_interest <- input$immunity # number of months of interest for history of infection
    rep <- 75 # number of years
    rate_scale <- input$scale # scaling rate of infection for exploration
    
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
      geom_line(aes(x = time, y = rate*500000), linetype = "dashed", colour = "blue") +
      scale_x_continuous(breaks = seq(1, nrow(women), 6), labels = c(rep(c("January", "July"), rep), "January")) +
      scale_y_continuous(name = "Infection Count",
                         sec.axis = sec_axis(~./500000, name = "Rate of Infection")) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title.y.right = element_text(color = "blue"),
            axis.text.y.right = element_text(color = "blue"),
            axis.ticks.y.right = element_line(color = "blue"),
            axis.line.y.right = element_line(color = "blue")) +
      labs(x = "Time")
  })
  
  output$infection_history <- renderPlot({
    # history of infection model in women, calculate infection status in 1,000,000 women based on monthly infection rate
    n_interest <- input$immunity # number of months of interest for history of infection
    rep <- 75 # number of years
    rate_scale <- input$scale # scaling rate of infection for exploration
    
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
}

# Create a Shiny app object

shinyApp(ui = ui, server = server)