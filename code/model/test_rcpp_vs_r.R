library(dplyr)
library(tidyr)
library(stringr)
library(zoo)
library(readxl)
library(purrr)
library(Rcpp)

# Source the model functions
source("code/model/model_function.R")

# Load the stored data
source("code/model/create_data.R")

# Create stored_data
stored_data <- create_data(n_interest = 25, rep = 30, factor = 1)

cat("Stored data structure:\n")
cat("stored_data[[1]] (women_mat) dimensions:", dim(stored_data[[1]]), "\n")
cat("stored_data[[2]] (empty matrix) dimensions:", dim(stored_data[[2]]), "\n")
cat("stored_data[[3]] (rate_vector) length:", length(stored_data[[3]]), "\n")
cat("stored_data[[4]] (level) length:", length(stored_data[[4]]), "\n")
cat("\n")

# Verify column structure of empty matrix
cat("Empty matrix column structure (first few cols):\n")
cat(colnames(stored_data[[2]]), "\n\n")

# Set up test parameters
lambda <- c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
theta1 <- 0.5
theta2 <- 10
omega1 <- 0.5
omega2 <- 6
alpha1 <- 0.5
alpha2 <- 12
delta <- 0.001
n_interest <- 25

cat("=== Testing R vs Rcpp Implementation ===\n\n")

# -------------------------------------------------------------------------
# Test 1: Women modeling
# -------------------------------------------------------------------------
cat("TEST 1: Women Modeling\n")
cat("----------------------\n")

women_r <- stored_data[[1]]
women_cpp <- stored_data[[1]]

# R version - women processing
women_r[, 2] <- women_r[, 2] + delta
if (length(lambda) == 1) {
  women_r[303:315, 2] <- women_r[303:315, 2] * lambda
} else {
  women_r[303:(303 + length(lambda) - 1), 2] <- women_r[303:(303 + length(lambda) - 1), 2] * lambda
}
women_r[1, 3] <- 1000000

for (row in 3:nrow(women_r)) {
  women_r[row - 1, 5] <- women_r[row - 2, 3] * women_r[row - 2, 2] + women_r[row - 2, 4] * women_r[row - 2, 2]
  women_r[row - 1, 3] <- women_r[row - 2, 3] - women_r[row - 2, 3] * women_r[row - 2, 2]
  women_r[row - 1, 4] <- women_r[row - 2, 4] - women_r[row - 2, 4] * women_r[row - 2, 2] + women_r[row - 2, 4+n_interest]
  women_r[row, c(6:(4+n_interest), 4)] <- women_r[row - 1, 5:(4+n_interest)]
}

# Rcpp version
lambda_vec <- if(length(lambda) == 1) rep(lambda, 13) else lambda
women_cpp <- model_women_cpp(women_cpp, lambda_vec, delta, n_interest)

# Compare
cat("Women matrices equal?", all.equal(women_r, women_cpp), "\n")
if (!isTRUE(all.equal(women_r, women_cpp))) {
  cat("Max difference:", max(abs(women_r - women_cpp), na.rm = TRUE), "\n")
  cat("Number of differing elements:", sum(abs(women_r - women_cpp) > 1e-10, na.rm = TRUE), "\n")
}

# -------------------------------------------------------------------------
# Test 2: Prepare babies data
# -------------------------------------------------------------------------
cat("\nTEST 2: Babies Data Preparation\n")
cat("--------------------------------\n")

women_r <- women_r[181:360, ]
women_r[, 1] <- 1:180
women_r[, 3:(4+n_interest)] <- women_r[, 3:(4+n_interest)]/1000000
babies_r <- women_r[, -3]
babies_r[, 3:(3+n_interest)] <- babies_r[, 3:(3+n_interest)] * babies_r[, 4+n_interest]

women_cpp <- women_cpp[181:360, ]
women_cpp[, 1] <- 1:180
women_cpp[, 3:(4+n_interest)] <- women_cpp[, 3:(4+n_interest)]/1000000
babies_cpp <- women_cpp[, -3]
babies_cpp[, 3:(3+n_interest)] <- babies_cpp[, 3:(3+n_interest)] * babies_cpp[, 4+n_interest]

cat("Babies matrices equal?", all.equal(babies_r, babies_cpp), "\n")

# -------------------------------------------------------------------------
# Test 3: Rate vector preparation
# -------------------------------------------------------------------------
cat("\nTEST 3: Rate Vector Preparation\n")
cat("--------------------------------\n")

rates_r <- stored_data[[3]] + delta
rates_r[123:135] <- rates_r[123:135] * lambda

rates_cpp <- stored_data[[3]] + delta
rates_cpp[123:135] <- rates_cpp[123:135] * lambda_vec[1:13]

cat("Rate vectors equal?", all.equal(rates_r, rates_cpp), "\n")

# -------------------------------------------------------------------------
# Test 4: Starting infection probabilities
# -------------------------------------------------------------------------
cat("\nTEST 4: Starting Infection Probabilities\n")
cat("-----------------------------------------\n")

start_inf <- 1/(1 + exp(-theta1 * (stored_data[[4]] - theta2)))
cat("Start_inf length:", length(start_inf), "\n")
cat("Start_inf range:", range(start_inf), "\n")
cat("Any NA/NaN in start_inf?", any(is.na(start_inf) | is.nan(start_inf)), "\n")

# -------------------------------------------------------------------------
# Test 5: Single baby cohort comparison
# -------------------------------------------------------------------------
cat("\nTEST 5: Single Baby Cohort (first cohort)\n")
cat("------------------------------------------\n")

x <- 1

# Check dimensions
cat("babies_r dimensions:", dim(babies_r), "\n")
cat("babies_r[x,] length:", length(babies_r[x, ]), "\n")
cat("stored_data[[2]] dimensions:", dim(stored_data[[2]]), "\n")
cat("Range 1:(5+n_interest) =", 1:(5+n_interest), "\n")
cat("babies_r[x, ] values:", babies_r[x, ], "\n")
cat("babies_r[x, 1:(5+n_interest)] attempt - this is:", length(1:(5+n_interest)), "elements\n\n")

# R version
subdata_r <- stored_data[[2]]
cat("Before assignment, subdata_r[1, 1:10]:", subdata_r[1, 1:10], "\n")
subdata_r[1, 1:(5+n_interest)] <- babies_r[x, ]
cat("After babies assignment, subdata_r[1, 1:10]:", subdata_r[1, 1:10], "\n")
subdata_r[, 1] <- x:(x+12*4)
subdata_r[, 2] <- rates_r[x:(x+12*4)]
subdata_r <- cbind(subdata_r,
                   waning = 1/(1 + exp(omega1 * (subdata_r[, 6+n_interest]-omega2))),
                   aging = 1/(1 + exp(alpha1 * (subdata_r[, 6+n_interest]-alpha2))),
                   infected = 0,
                   disease = 0)

cat("R subdata dimensions:", dim(subdata_r), "\n")
cat("R subdata columns: time, rate, immunity(", n_interest, "), births, birth_month, time_birth, waning, aging, infected, disease\n")
cat("R waning column index:", ncol(subdata_r) - 3, "\n")
cat("R aging column index:", ncol(subdata_r) - 2, "\n")

# Cpp version
baby_init <- babies_cpp[x, 1:(5+n_interest)]

cat("baby_init from babies_cpp:", baby_init, "\n")
cat("baby_init length:", length(baby_init), "\n")
cat("Are babies_r and babies_cpp equal for row", x, "?", all.equal(babies_r[x,], babies_cpp[x,]), "\n\n")

subdata_cpp <- model_baby_cohort_cpp(
  baby_init = baby_init,
  template_matrix = stored_data[[2]],
  rates = rates_cpp,
  start_inf = start_inf,
  omega1 = omega1,
  omega2 = omega2,
  alpha1 = alpha1,
  alpha2 = alpha2,
  n_interest = n_interest,
  time_offset = x
)

nrows_template <- nrow(stored_data[[2]])
ncols_template <- ncol(stored_data[[2]])

cat("Cpp subdata dimensions:", dim(subdata_cpp), "\n")
cat("Cpp ncols_template:", ncols_template, "\n")
cat("Cpp waning column index:", ncols_template + 1, "\n")
cat("Cpp aging column index:", ncols_template + 2, "\n")
cat("Cpp time_birth column (30) first 5 values:", subdata_cpp[1:5, 31], "\n")
cat("Cpp subdata row 1, columns 28-35:", subdata_cpp[1, 28:35], "\n")

# Check initial conditions
cat("\nInitial row comparison:\n")
cat("R first row (first 10 cols):", subdata_r[1, 1:min(10, ncol(subdata_r))], "\n")
cat("Cpp first row (first 10 cols):", subdata_cpp[1, 1:min(10, ncol(subdata_cpp))], "\n")
cat("R first row (all 35 cols):\n")
print(subdata_r[1, ])
cat("Cpp first row (all 35 cols):\n")
print(subdata_cpp[1, ])

# Check waning/aging values
cat("\nWaning/Aging comparison (first 5 rows):\n")
cat("R waning:", subdata_r[1:5, ncol(subdata_r)-3], "\n")
cat("Cpp waning:", subdata_cpp[1:5, ncols_template + 1], "\n")
cat("R aging:", subdata_r[1:5, ncol(subdata_r)-2], "\n")
cat("Cpp aging:", subdata_cpp[1:5, ncols_template + 2], "\n")

# Run R version dynamics
for(month in 2:49){
  subdata_r[month, 3:(3+n_interest)] <- subdata_r[month - 1, 3:(3+n_interest)]
  subdata_r[month - 1, 3:(3+n_interest)] <- subdata_r[month - 1, 3:(3+n_interest)] * subdata_r[month - 1, 2] * (1 - ((1 - start_inf) * subdata_r[month - 1, n_interest+7]))
  subdata_r[month - 1, n_interest+9] <- sum(subdata_r[month - 1, 3:(3+n_interest)])
  subdata_r[month, 3:(3+n_interest)] <- subdata_r[month, 3:(3+n_interest)] - subdata_r[month - 1, 3:(3+n_interest)]
  subdata_r[month - 1, 3:(3+n_interest)] <- subdata_r[month - 1, 3:(3+n_interest)] * subdata_r[month - 1, n_interest+8]
  subdata_r[month - 1, n_interest+10] <- sum(subdata_r[month - 1, 3:(3+n_interest)])
}

subdata_r <- subdata_r[-nrow(subdata_r), ]

# Extract relevant columns from both
cat("\nExtracted columns comparison:\n")
cat("R: extracting columns c(1, 6+n_interest, 10+n_interest) =", c(1, 6+n_interest, 10+n_interest), "\n")
cat("Cpp: extracting columns c(1, 6+n_interest, ncols_template+4) =", c(1, 6+n_interest, ncols_template + 4), "\n")

result_r <- subdata_r[1:48, c(1, 6+n_interest, 10+n_interest)]
result_cpp <- subdata_cpp[1:48, c(1, 6+n_interest, ncols_template + 4)]

cat("\nR result dimensions:", dim(result_r), "\n")
cat("Cpp result dimensions:", dim(result_cpp), "\n")
cat("R result sample (first 5 rows):\n")
print(head(result_r, 5))
cat("Cpp result sample (first 5 rows):\n")
print(head(result_cpp, 5))

cat("\nResults equal?", all.equal(result_r, result_cpp), "\n")
if (!isTRUE(all.equal(result_r, result_cpp))) {
  cat("Max difference:", max(abs(result_r - result_cpp), na.rm = TRUE), "\n")
  diff_indices <- which(abs(result_r - result_cpp) > 1e-10, arr.ind = TRUE)
  cat("Number of differing elements:", nrow(diff_indices), "\n")
  if (nrow(diff_indices) > 0) {
    cat("First few differing elements:\n")
    print(head(diff_indices, 10))
    for (i in 1:min(5, nrow(diff_indices))) {
      idx <- diff_indices[i, ]
      cat(sprintf("  Row %d, Col %d: R=%.6f, Cpp=%.6f\n", 
                  idx[1], idx[2], result_r[idx[1], idx[2]], result_cpp[idx[1], idx[2]]))
    }
  }
}

# Check for NaN/Inf values
cat("\nChecking for NaN/Inf:\n")
cat("R result: any NaN?", any(is.nan(result_r)), "any Inf?", any(is.infinite(result_r)), "\n")
cat("Cpp result: any NaN?", any(is.nan(result_cpp)), "any Inf?", any(is.infinite(result_cpp)), "\n")

# -------------------------------------------------------------------------
# Test 6: Full function comparison
# -------------------------------------------------------------------------
cat("\n\nTEST 6: Full Function Comparison\n")
cat("=================================\n")

result_full_r <- model_function(lambda, theta1, theta2, omega1, omega2, alpha1, alpha2, stored_data, delta, n_interest)
result_full_cpp <- model_function_rcpp(lambda, theta1, theta2, omega1, omega2, alpha1, alpha2, stored_data, delta, n_interest)

cat("Full R result dimensions:", dim(result_full_r), "\n")
cat("Full Cpp result dimensions:", dim(result_full_cpp), "\n")

cat("\nR result sample (first 10 rows):\n")
print(head(result_full_r, 10))

cat("\nCpp result sample (first 10 rows):\n")
print(head(result_full_cpp, 10))

cat("\nFull results equal?", all.equal(result_full_r, result_full_cpp), "\n")

cat("\nChecking for NaN/Inf in full results:\n")
cat("R: any NaN?", any(is.nan(result_full_r)), "any Inf?", any(is.infinite(result_full_r)), "\n")
cat("Cpp: any NaN?", any(is.nan(result_full_cpp)), "any Inf?", any(is.infinite(result_full_cpp)), "\n")

if (any(is.nan(result_full_r)) || any(is.infinite(result_full_r))) {
  cat("\nR NaN/Inf locations:\n")
  print(which(is.nan(result_full_r) | is.infinite(result_full_r), arr.ind = TRUE))
}

if (any(is.nan(result_full_cpp)) || any(is.infinite(result_full_cpp))) {
  cat("\nCpp NaN/Inf locations:\n")
  print(which(is.nan(result_full_cpp) | is.infinite(result_full_cpp), arr.ind = TRUE))
}

cat("\n=== Testing Complete ===\n")
