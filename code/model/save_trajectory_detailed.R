library(dplyr)
library(tidyr)
library(stringr)
library(zoo)
library(readxl)
library(purrr)

# -------------------------------------------------------------------------

traj_function <- function(lambda, theta1, theta2, omega1, omega2, alpha1, alpha2, stored_data, delta, n_interest){
  
  # matrix key:
  # 1 = time
  # 2 = rate
  # 3 - 28 = infection history status
  # 29 = births
  women <- stored_data[[1]]
  
  # adding base case import rate to exposure rate
  women[, 2] <- women[, 2] + delta
  
  # subject rate to disruption factor lambda
  women[303:313, 2] <-  women[303:313, 2] * lambda # period corresponding to March 2020-2021
  
  # initial state
  women[1, 3] <- 1000000
  
  ## model women
  for (row in 3:nrow(women)) {
    # map_dbl(3:nrow(women),
    #     function(x) {})
    
    women[row - 1, 5] <- women[row - 2, 3] * women[row - 2, 2] + women[row - 2, 4] * women[row - 2, 2]
    women[row - 1, 3] <- women[row - 2, 3] - women[row - 2, 3] * women[row - 2, 2]
    women[row - 1, 4] <- women[row - 2, 4] - women[row - 2, 4] * women[row - 2, 2] + women[row - 2, 4+n_interest]
    women[row, c(6:(4+n_interest), 4)] <- women[row - 1, 5:(4+n_interest)]
    
  }
  
  # create link between mothers and babies (infection history/immunity split)
  women <- women[181:360, ] # selecting for 2010 onwards to model babies (15years 1995-2010 to model mothers as "burn-in")
  women[, 1] <- 1:180 # re-labeling time (modelling 15yrs 2010-2024)
  
  # calculate proportion of women in each infection history status per month
  women[, 3:(4+n_interest)] <- women[, 3:(4+n_interest)]/1000000
  babies <- women[, -3] # remove susceptible_naive column since negligible in later time steps
  
  # calculate number of babies born with an immunity profile based on births and proportion
  babies[, 3:(3+n_interest)] <- babies[, 3:(3+n_interest)] * babies[, 4+n_interest]
  
  ## model babies
  # matrix key:
  # 1 = time_calendar
  # 2 = rate
  # 3 - 27 = immunity levels
  # 28 = births
  # 29 = birth_month
  # 30 = time_birth
  # 31 = waning
  # 32 = aging
  # 33 = infected
  # 34 = disease
  
  # adding base case import rate to exposure rate
  stored_data[[3]] <- stored_data[[3]] + delta
  
  # apply lambda to rate vector
  stored_data[[3]][123:133] <- stored_data[[3]][123:133] * lambda # period corresponding to March 2020-2021
  
  data <- map(1:nrow(babies),
              function(x){
                
                subdata <- stored_data[[2]]
                subdata[1, 1:(5+n_interest)] <- babies[x, ]
                subdata[, 29] <- babies[x, 29]
                subdata[, 1] <- x:(x+12*4-1)
                subdata[, 2] <- stored_data[[3]][x:(x+12*4-1)]
                subdata <- cbind(subdata,
                                 waning = 1/(1 + exp(omega1 * (subdata[, 6+n_interest]-omega2))),
                                 aging = 1/(1 + exp(alpha1 * (subdata[, 6+n_interest]-alpha2))),
                                 infected = 0,
                                 disease = 0)
                start_inf <- 1/(1 + exp(-theta1 * (stored_data[[4]]-theta2))) # starting probability of infection given maternal immunity
                
                for(month in 2:48){
                  subdata[month, 3:(3+n_interest)] <- subdata[month - 1, 3:(3+n_interest)] # susceptible babies to next time step
                  subdata[month - 1, 3:(3+n_interest)] <- subdata[month - 1, 3:(3+n_interest)] * subdata[month - 1, 2] * (1 - ((1 - start_inf) * subdata[month - 1, n_interest+7])) # calculate number of babies infected in each immunity level
                  subdata[month - 1, n_interest+9] <- sum(subdata[month - 1, 3:(3+n_interest)]) # total number of infections at that time step
                  subdata[month, 3:(3+n_interest)] <- subdata[month, 3:(3+n_interest)] - subdata[month - 1, 3:(3+n_interest)] # deduct infected from susceptible
                  subdata[month - 1, 3:(3+n_interest)] <- subdata[month - 1, 3:(3+n_interest)] * subdata[month - 1, n_interest+8] # calculate number of babies that develop disease in each immunity level
                  subdata[month - 1, n_interest+10] <- sum(subdata[month - 1, 3:(3+n_interest)]) # total number of babies that develop disease at that time step
                }
                
                return(subdata)
                
              })
  
  data <- do.call(rbind, data)[, c(1, 2, 29:34)] # unlist map output
  data <- data[data[, "time_calendar"] >= 82 & data[, "time_calendar"] <= 178, ] # selecting times to match Scottish rate data (Scottish data spans oct 2016 - dec 2024, but cutting off oct 2024 because of birth data)
  
  return(data)
  
}
