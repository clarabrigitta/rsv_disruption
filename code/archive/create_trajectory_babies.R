create_trajectory_babies <- function(lambda, stored_data, delta, n_interest){
  
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

  return(babies)
  
}