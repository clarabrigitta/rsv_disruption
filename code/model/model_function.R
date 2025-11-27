library(dplyr)
library(tidyr)
library(stringr)
library(zoo)
library(readxl)
library(purrr)

# -------------------------------------------------------------------------

model_function <- function(lambda, theta1, theta2, omega1, omega2, alpha1, alpha2, stored_data, delta, n_interest){

  # matrix key:
  # 1 = time
  # 2 = rate
  # 3 - 28 = infection history status
  # 29 = births
  women <- stored_data[[1]]
  # cat("The current women matrix rate: ",  women[303:315, 2], "\n")
  # adding base case import rate to exposure rate
  women[, 2] <- women[, 2] + delta
  # cat("The current women matrix rate after applying delta: ",  women[303:315, 2], "\n")

  # subject rate to disruption factor lambda
  # cat("The lambda: ",  lambda, "\n")
  if (length(lambda) == 1) {
    women[303:315, 2] <-  women[303:315, 2] * lambda # period corresponding to March 2020 - March 2021 = 315 (January 2021  = 313)
  } else {
    women[303:(303 + length(lambda) - 1) , 2] <-  women[303:(303 + length(lambda) - 1), 2] * lambda # adjusts depending on length of lambda from March 20202
  }
  # cat("The current women matrix rate after applying lambda: ",  women[303:(303 + length(lambda)), 2], "\n" )

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
  
  
  if (length(lambda) == 1) {
    stored_data[[3]][123:135] <- stored_data[[3]][123:135] * lambda
  } else {
    stored_data[[3]][123:(123 + length(lambda) - 1)] <- stored_data[[3]][123:(123 + length(lambda) - 1)] * lambda
  }
  
  data <- map(1:nrow(babies),
              function(x){
                
                subdata <- stored_data[[2]]
                subdata[1, 1:(5+n_interest)] <- babies[x, ]
                subdata[, 1] <- x:(x+12*4)
                subdata[, 2] <- stored_data[[3]][x:(x+12*4)]
                subdata <- cbind(subdata,
                                 waning = 1/(1 + exp(omega1 * (subdata[, 6+n_interest]-omega2))),
                                 aging = 1/(1 + exp(alpha1 * (subdata[, 6+n_interest]-alpha2))),
                                 infected = 0,
                                 disease = 0)
                start_inf <- 1/(1 + exp(-theta1 * (stored_data[[4]]-theta2))) # starting probability of infection given maternal immunity
                
                for(month in 2:49){
                  subdata[month, 3:(3+n_interest)] <- subdata[month - 1, 3:(3+n_interest)] # susceptible babies to next time step
                  subdata[month - 1, 3:(3+n_interest)] <- subdata[month - 1, 3:(3+n_interest)] * subdata[month - 1, 2] * (1 - ((1 - start_inf) * subdata[month - 1, n_interest+7])) # calculate number of babies infected in each immunity level
                  subdata[month - 1, n_interest+9] <- sum(subdata[month - 1, 3:(3+n_interest)]) # total number of infections at that time step
                  subdata[month, 3:(3+n_interest)] <- subdata[month, 3:(3+n_interest)] - subdata[month - 1, 3:(3+n_interest)] # deduct infected from susceptible
                  subdata[month - 1, 3:(3+n_interest)] <- subdata[month - 1, 3:(3+n_interest)] * subdata[month - 1, n_interest+8] # calculate number of babies that develop disease in each immunity level
                  subdata[month - 1, n_interest+10] <- sum(subdata[month - 1, 3:(3+n_interest)]) # total number of babies that develop disease at that time step
                }
                
                subdata <- subdata[-nrow(subdata), ]
                
                return(subdata)
                
              })

  data <- do.call(rbind, data)[, c(1, 6+n_interest, 10+n_interest)] # unlist map output
  data <- cbind(data, age = 0)
  data[data[, 2] > 11, 4] <- 1 # 0 = <1, 1 = 1-4
  data <- tapply( data[, 3], list(data[, 1],  data[, 4]), sum) # calculate counts of disease per age group per month
  data <- cbind(data[, 1:2],
                time = 1:227)
  data <- rbind(data[82:178, 2:3], data[82:178, c(1, 3)]) # selecting times to match Scottish rate data (Scottish data spans oct 2016 - dec 2024, but cutting off oct 2024 because of birth data)

  return(data)

}

# -------------------------------------------------------------------------
# Rcpp implementation for improved performance
# -------------------------------------------------------------------------

library(Rcpp)

sourceCpp(code = '
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix model_women_cpp(NumericMatrix women, NumericVector lambda, 
                               double delta, int n_interest) {
  // CRITICAL: Clone the input matrix to avoid modifying the original
  // Without this, repeated calls during MCMC will accumulate changes!
  NumericMatrix women_copy = clone(women);
  
  int nrows = women_copy.nrow();
  int ncols = women_copy.ncol();
  
  // Add delta to rates
  for(int i = 0; i < nrows; i++) {
    women_copy(i, 1) += delta;
  }
  
  // Apply lambda to disruption period
  int start_idx = 302; // 303 - 1 for 0-based indexing
  int end_idx;
  
  // Match R behavior: if lambda has length 1, apply to 13 months (303:315 in R = 302:314 in C++)
  if(lambda.size() == 1) {
    end_idx = start_idx + 13;
  } else {
    end_idx = start_idx + lambda.size();
  }
  
  if(end_idx > nrows) end_idx = nrows;
  
  for(int i = start_idx; i < end_idx; i++) {
    int lambda_idx = i - start_idx;
    // For scalar lambda, use the same value for all months
    if(lambda.size() == 1) {
      women_copy(i, 1) *= lambda[0];
    } else if(lambda_idx < lambda.size()) {
      women_copy(i, 1) *= lambda[lambda_idx];
    }
  }
  
  // Initial state
  women_copy(0, 2) = 1000000.0;
  
  // Model women dynamics
  // R columns (1-indexed): 1=time, 2=rate, 3=susceptible_naive, 4=susceptible_reinf, 5=I1, 6=I2, ..., 4+n_interest=I(n_interest)
  // C++ columns (0-indexed): 0=time, 1=rate, 2=susceptible_naive, 3=susceptible_reinf, 4=I1, 5=I2, ..., 3+n_interest=I(n_interest)
  for(int row = 2; row < nrows; row++) {
    // R: women[row - 1, 5] <- women[row - 2, 3] * women[row - 2, 2] + women[row - 2, 4] * women[row - 2, 2]
    // I1 gets newly infected from both susceptible groups
    women_copy(row - 1, 4) = women_copy(row - 2, 2) * women_copy(row - 2, 1) + women_copy(row - 2, 3) * women_copy(row - 2, 1);
    
    // R: women[row - 1, 3] <- women[row - 2, 3] - women[row - 2, 3] * women[row - 2, 2]
    // susceptible_naive loses people to infection
    women_copy(row - 1, 2) = women_copy(row - 2, 2) - women_copy(row - 2, 2) * women_copy(row - 2, 1);
    
    // R: women[row - 1, 4] <- women[row - 2, 4] - women[row - 2, 4] * women[row - 2, 2] + women[row - 2, 4+n_interest]
    // susceptible_reinf loses to infection but gains from I(n_interest)
    women_copy(row - 1, 3) = women_copy(row - 2, 3) - women_copy(row - 2, 3) * women_copy(row - 2, 1) + women_copy(row - 2, 3 + n_interest);
    
    // R: women[row, c(6:(4+n_interest), 4)] <- women[row - 1, 5:(4+n_interest)]
    // This assigns: columns 6 to 4+n_interest get values from 5 to 4+n_interest-1, and column 4 gets value from 4+n_interest
    // In C++: columns 5 to 3+n_interest get values from 4 to 3+n_interest-1, and column 3 gets value from 3+n_interest
    // Shift infection history: I2 <- I1, I3 <- I2, ..., I(n_interest) <- I(n_interest-1)
    for(int col = 5; col <= 3 + n_interest; col++) {
      women_copy(row, col) = women_copy(row - 1, col - 1);
    }
    // susceptible_reinf <- I(n_interest)
    women_copy(row, 3) = women_copy(row - 1, 3 + n_interest);
  }
  
  return women_copy;
}

// [[Rcpp::export]]
NumericMatrix model_baby_cohort_cpp(NumericVector baby_init, NumericMatrix template_matrix,
                                     NumericVector rates, NumericVector start_inf,
                                     double omega1, double omega2, 
                                     double alpha1, double alpha2,
                                     int n_interest, int time_offset) {
  
  int nrows_template = template_matrix.nrow();
  int ncols_template = template_matrix.ncol();
  
  // Create subdata matrix with extra columns for waning, aging, infected, disease
  int ncols = ncols_template + 4;
  NumericMatrix subdata(nrows_template, ncols);
  
  // Initialize by copying the template (this gets us the time_birth column)
  for(int i = 0; i < nrows_template; i++) {
    for(int j = 0; j < ncols_template; j++) {
      subdata(i, j) = template_matrix(i, j);
    }
    // Initialize extra columns to 0
    for(int j = ncols_template; j < ncols; j++) {
      subdata(i, j) = 0.0;
    }
  }
  
  // Copy baby_init to first row (this includes time, rate, and other columns from babies)
  // In R: subdata[1, 1:(5+n_interest)] <- babies[x, ]
  // babies has 30 columns, so we copy all of them
  for(int i = 0; i < baby_init.size() && i < ncols_template; i++) {
    subdata(0, i) = baby_init[i];
  }
  
  // Now set time and rates for ALL rows (this overwrites columns 0 and 1)
  // In R: subdata[, 1] <- x:(x+12*4) and subdata[, 2] <- stored_data[[3]][x:(x+12*4)]
  for(int i = 0; i < nrows_template; i++) {
    subdata(i, 0) = time_offset + i;
    int rate_idx = time_offset + i - 1;
    if(rate_idx >= 0 && rate_idx < rates.size()) {
      subdata(i, 1) = rates[rate_idx];
    }
  }
  
  // Calculate waning and aging columns (these are added after the original columns)
  int waning_col = ncols_template;     // First extra column
  int aging_col = ncols_template + 1;  // Second extra column
  int infected_col = ncols_template + 2; // Third extra column
  int disease_col = ncols_template + 3;  // Fourth extra column
  
  for(int i = 0; i < nrows_template; i++) {
    double time_birth = subdata(i, 5 + n_interest);
    subdata(i, waning_col) = 1.0 / (1.0 + exp(omega1 * (time_birth - omega2)));
    subdata(i, aging_col) = 1.0 / (1.0 + exp(alpha1 * (time_birth - alpha2)));
    subdata(i, infected_col) = 0.0;
    subdata(i, disease_col) = 0.0;
  }
  
  // Model dynamics through time
  // R loop: for(month in 2:49), accessing rows month and month-1 (1-indexed)
  // C++: month represents Rs month value, but we use month-1 and month-2 as indices (0-indexed)
  int max_month = (49 < nrows_template) ? 49 : nrows_template;
  
  for(int month = 2; month <= max_month; month++) {
    int current_idx = month - 1;  // C++ index for R row month
    int prev_idx = month - 2;      // C++ index for R row month-1
    
    // Copy susceptible to next time step
    // R: subdata[month, 3:(3+n_interest)] <- subdata[month - 1, 3:(3+n_interest)]
    for(int col = 2; col <= 2 + n_interest; col++) {
      subdata(current_idx, col) = subdata(prev_idx, col);
    }
    
    double rate = subdata(prev_idx, 1);
    double waning = subdata(prev_idx, waning_col);
    
    // Calculate infections
    // R: subdata[month - 1, 3:(3+n_interest)] <- subdata[month - 1, 3:(3+n_interest)] * subdata[month - 1, 2] * (1 - ((1 - inf_prob) * subdata[month - 1, n_interest+7]))
    double total_infected = 0.0;
    for(int col = 2; col <= 2 + n_interest; col++) {
      int immunity_level = col - 2;
      double inf_prob = (immunity_level < start_inf.size()) ? start_inf[immunity_level] : start_inf[start_inf.size() - 1];
      
      // Calculate infection with bounds checking
      double susceptible = subdata(prev_idx, col);
      double inf_factor = 1.0 - ((1.0 - inf_prob) * waning);
      double infected = susceptible * rate * inf_factor;
      
      // Safety check: infected cannot exceed susceptible
      if(infected > susceptible) {
        infected = susceptible;
      }
      // Safety check: prevent overflow
      if(!std::isfinite(infected) || infected > 1e100) {
        infected = susceptible; // Cap at susceptible count
      }
      
      subdata(prev_idx, col) = infected;
      total_infected += infected;
    }
    subdata(prev_idx, infected_col) = total_infected;
    
    // Deduct infected from susceptible
    // R: subdata[month, 3:(3+n_interest)] <- subdata[month, 3:(3+n_interest)] - subdata[month - 1, 3:(3+n_interest)]
    for(int col = 2; col <= 2 + n_interest; col++) {
      subdata(current_idx, col) -= subdata(prev_idx, col);
      // Ensure non-negative (numerical precision can cause small negatives)
      if(subdata(current_idx, col) < 0.0) {
        subdata(current_idx, col) = 0.0;
      }
    }
    
    // Calculate disease
    // R: subdata[month - 1, 3:(3+n_interest)] <- subdata[month - 1, 3:(3+n_interest)] * subdata[month - 1, n_interest+8]
    double aging = subdata(prev_idx, aging_col);
    double total_disease = 0.0;
    for(int col = 2; col <= 2 + n_interest; col++) {
      double disease = subdata(prev_idx, col) * aging;
      
      // Safety check: prevent overflow
      if(!std::isfinite(disease) || disease > 1e100) {
        disease = subdata(prev_idx, col); // Cap at infected count
      }
      
      subdata(prev_idx, col) = disease;
      total_disease += disease;
    }
    
    // Final safety check on total
    if(!std::isfinite(total_disease) || total_disease > 1e100) {
      total_disease = 0.0;
    }
    
    subdata(prev_idx, disease_col) = total_disease;
  }
  
  return subdata;
}
')

model_function_rcpp <- function(lambda, theta1, theta2, omega1, omega2, alpha1, alpha2, stored_data, delta, n_interest){
  
  # Input validation
  if(any(!is.finite(c(lambda, theta1, theta2, omega1, omega2, alpha1, alpha2, delta)))) {
    warning("Non-finite parameters detected in model_function_rcpp")
    return(matrix(NA, nrow=194, ncol=2))
  }
  
  # Model women using Rcpp
  women <- stored_data[[1]]
  women <- model_women_cpp(women, lambda, delta, n_interest)
  
  # Select 2010 onwards
  women <- women[181:360, ]
  women[, 1] <- 1:180
  
  # Calculate proportions
  women[, 3:(4+n_interest)] <- women[, 3:(4+n_interest)] / 1000000
  babies <- women[, -3]
  
  # Calculate babies with immunity profiles
  babies[, 3:(3+n_interest)] <- babies[, 3:(3+n_interest)] * babies[, 4+n_interest]
  
  # Prepare rate vector with delta and lambda
  rates <- stored_data[[3]] + delta
  # Apply disruption to the appropriate range based on lambda length
  if(length(lambda) == 1) {
    rates[123:135] <- rates[123:135] * lambda
  } else {
    rates[123:(123 + length(lambda) - 1)] <- rates[123:(123 + length(lambda) - 1)] * lambda
  }
  
  # Calculate starting infection probabilities
  start_inf <- 1/(1 + exp(-theta1 * (stored_data[[4]] - theta2)))
  
  # Process each baby cohort using Rcpp
  result_list <- vector("list", nrow(babies))
  
  for(x in seq_len(nrow(babies))) {
    baby_init <- babies[x, 1:(5+n_interest)]
    
    # Check for non-finite values in baby_init
    if(any(!is.finite(baby_init))) {
      warning(sprintf("Non-finite baby_init values in cohort %d", x))
      result_list[[x]] <- matrix(0, nrow=48, ncol=3)
      next
    }
    
    subdata <- model_baby_cohort_cpp(
      baby_init = baby_init,
      template_matrix = stored_data[[2]],
      rates = rates,
      start_inf = start_inf,
      omega1 = omega1,
      omega2 = omega2,
      alpha1 = alpha1,
      alpha2 = alpha2,
      n_interest = n_interest,
      time_offset = x
    )
    
    # Check for non-finite or extreme values in subdata
    if(any(!is.finite(subdata))) {
      warning(sprintf("Non-finite values detected in cohort %d subdata", x))
      result_list[[x]] <- matrix(0, nrow=48, ncol=3)
      next
    }
    
    # Remove last row (to match R version: subdata <- subdata[-nrow(subdata), ])
    # Then extract relevant columns: time=1, time_birth=6+n_interest, disease=ncols_template+4
    ncols_template <- ncol(stored_data[[2]])
    subdata <- subdata[-nrow(subdata), ]
    result_list[[x]] <- subdata[, c(1, 6+n_interest, ncols_template + 4)]
  }
  
  # Combine results
  data <- do.call(rbind, result_list)
  
  # Debug: check what we got
  # cat("Combined data dimensions:", dim(data), "\n")
  # cat("Sample of data (first 5 rows):\n")
  # print(head(data, 5))
  
  # Add age column: 0 for <1 year (<=11 months), 1 for 1-4 years (>11 months)
  data <- cbind(data, age = 0)
  data[data[, 2] > 11, 4] <- 1
  
  # Debug: check age distribution
  # cat("Age distribution:", table(data[, 4]), "\n")
  
  # Aggregate by time and age - tapply returns a matrix with rows=time, cols=age
  agg_data <- tapply(data[, 3], list(data[, 1], data[, 4]), sum)
  
  # Check dimensions
  # cat("Aggregated data dimensions:", dim(agg_data), "\n")
  
  if(is.null(dim(agg_data))) {
    stop("Aggregation resulted in a vector instead of matrix")
  }
  
  if(ncol(agg_data) < 2) {
    # If we only have one age group, create a second column of zeros
    warning("Only one age group found, adding empty second column")
    if(colnames(agg_data)[1] == "0") {
      agg_data <- cbind(agg_data, "1" = 0)
    } else {
      agg_data <- cbind("0" = 0, agg_data)
    }
  }
  
  # Combine with time index
  data <- cbind(agg_data[, 1:2], time = seq_len(nrow(agg_data)))
  
  # Select relevant time periods
  if(nrow(data) >= 178) {
    data <- rbind(data[82:178, 2:3], data[82:178, c(1, 3)])
  } else {
    warning("Not enough rows in data to subset [82:178], got ", nrow(data), " rows")
  }
  
  # Final validation - silently handle issues rather than warning during MCMC
  # Warnings can cause performance problems during intensive sampling
  # if(any(!is.finite(data))) {
  #   warning("Non-finite values in final output data")
  # }
  # if(any(data < 0, na.rm=TRUE)) {
  #   warning("Negative values in final output data")
  #   warning("Negative count:", sum(data < 0, na.rm=TRUE))
  #   warning("Negative locations:")
  #   # print(which(data < 0, arr.ind=TRUE))
  # }
  # if(any(abs(data) > 1e50, na.rm=TRUE)) {
  #   warning("Extremely large values (>1e50) in final output data")
  # }
  
  return(data)
}


model_function_old <- function(lambda, theta1, theta2, omega1, omega2, alpha1, alpha2, stored_data, delta, n_interest){

  # matrix key:
  # 1 = time
  # 2 = rate
  # 3 - 28 = infection history status
  # 29 = births
  women <- stored_data[[1]]
  
  # adding base case import rate to exposure rate
  women[, 2] <- women[, 2] + delta
  
  # subject rate to disruption factor lambda
  women[303:315, 2] <-  women[303:315, 2] * lambda # period corresponding to March 2020 - March 2021 = 315 (January 2021  = 313)
  
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
  stored_data[[3]][123:135] <- stored_data[[3]][123:135] * lambda # period corresponding to March 2020 - March 2021 = 135 (January 2021 = 133)
  
  data <- map(1:nrow(babies),
              function(x){
                
                subdata <- stored_data[[2]]
                subdata[1, 1:(5+n_interest)] <- babies[x, ]
                subdata[, 1] <- x:(x+12*4)
                subdata[, 2] <- stored_data[[3]][x:(x+12*4)]
                subdata <- cbind(subdata,
                                 waning = 1/(1 + exp(omega1 * (subdata[, 6+n_interest]-omega2))),
                                 aging = 1/(1 + exp(alpha1 * (subdata[, 6+n_interest]-alpha2))),
                                 infected = 0,
                                 disease = 0)
                start_inf <- 1/(1 + exp(-theta1 * (stored_data[[4]]-theta2))) # starting probability of infection given maternal immunity
                
                for(month in 2:49){
                  subdata[month, 3:(3+n_interest)] <- subdata[month - 1, 3:(3+n_interest)] # susceptible babies to next time step
                  subdata[month - 1, 3:(3+n_interest)] <- subdata[month - 1, 3:(3+n_interest)] * subdata[month - 1, 2] * (1 - ((1 - start_inf) * subdata[month - 1, n_interest+7])) # calculate number of babies infected in each immunity level
                  subdata[month - 1, n_interest+9] <- sum(subdata[month - 1, 3:(3+n_interest)]) # total number of infections at that time step
                  subdata[month, 3:(3+n_interest)] <- subdata[month, 3:(3+n_interest)] - subdata[month - 1, 3:(3+n_interest)] # deduct infected from susceptible
                  subdata[month - 1, 3:(3+n_interest)] <- subdata[month - 1, 3:(3+n_interest)] * subdata[month - 1, n_interest+8] # calculate number of babies that develop disease in each immunity level
                  subdata[month - 1, n_interest+10] <- sum(subdata[month - 1, 3:(3+n_interest)]) # total number of babies that develop disease at that time step
                }
                
                subdata <- subdata[-nrow(subdata), ]
                
                return(subdata)
                
              })

  data <- do.call(rbind, data)[, c(1, 6+n_interest, 10+n_interest)] # unlist map output
  data <- cbind(data, age = 0)
  data[data[, 2] > 11, 4] <- 1 # 0 = <1, 1 = 1-4
  data <- tapply( data[, 3], list(data[, 1],  data[, 4]), sum) # calculate counts of disease per age group per month
  data <- cbind(data[, 1:2],
                time = 1:227)
  data <- rbind(data[82:178, 2:3], data[82:178, c(1, 3)]) # selecting times to match Scottish rate data (Scottish data spans oct 2016 - dec 2024, but cutting off oct 2024 because of birth data)

  return(data)

}

