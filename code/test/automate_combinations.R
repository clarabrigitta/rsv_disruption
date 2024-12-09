library(BayesianTools)
library(posterior)
library(bayesplot)
library(ggplot2)
library(gridExtra)
library(parallel)

source("~/Desktop/rsv_disruption/code/model/model_function.R")

# -------------------------------------------------------------------------

# weekly rate of laboratory confirmed cases by age and pathogen aggregated to monthly
scotland_rate <- read.csv("./data/respiratory_age_20240515.csv") %>%
  mutate(date = as.Date(as.character(WeekBeginning), "%Y%m%d")) %>% 
  filter(Pathogen == "Respiratory syncytial virus",
         AgeGroup %in% c("<1 years", "1-4 years")) %>% 
  rename(age = AgeGroup,
         rate = RatePer100000) %>%
  mutate(yearmon = as.yearmon(date)) %>% 
  group_by(age, yearmon) %>% 
  summarise(rate = sum(rate)) %>% 
  ungroup() %>% 
  # to calculate counts
  mutate(population = ifelse(age == "<1 years", 47186, 200551),
         count = rate / 100000 * population)

# -------------------------------------------------------------------------

# set duration of maternal immunity
duration = 24

# create fixed datasets
save_data <- create_data(n_interest = duration, rep = 30, n_burn = 20)

# pick which parameters to fit
full_parameters <- c("detection", "disruption", "inf_imm1", "inf_imm2", "waning1", "waning2", "aging1", "aging2")
parameters <- c("inf_imm1", "inf_imm2", "waning1", "waning2", "aging1", "aging2")
diff <- setdiff(full_parameters, parameters)

# assign values for parameters not fitted
params_values <- list("detection" = 0.07, "disruption" = -4.3, "inf_imm1" = 0.2, "inf_imm2" = 20, "waning1" = 1.5, "waning2" = 7, "aging1" = 1.5, "aging2" = 26)
fixed_params <- params_values[diff]

# assign range for parameters fitted
lb_values <- list("detection" = 0, "disruption" = -10, "inf_imm1" = 0.2, "inf_imm2" = 0, "waning1" = 0.2, "waning2" = 0, "aging1" = 0.2, "aging2" = 0)
fitted_lb <- unlist(lb_values[parameters])
ub_values <- list("detection" = 1, "disruption" = 0, "inf_imm1" = 1.5, "inf_imm2" = 20, "waning1" = 1.5, "waning2" = 30, "aging1" = 1.5, "aging2" = 30)
fitted_ub <- unlist(ub_values[parameters])

# plug into fitting code
# poisson likelihood function
likelihood_all <- function(params){
  
  params <- as.list(params)
  names(params) <- parameters
  combined_params <- c(params, fixed_params)

  likelihood <- dpois(round(scotland_rate$count, digits = 0),
                      as.numeric(round(model_function(
                        lambda = exp(combined_params$disruption), theta1 = combined_params$inf_imm1, 
                        theta2 = combined_params$inf_imm2, omega1 = combined_params$waning1, 
                        omega2 = combined_params$waning2, alpha1 = combined_params$aging1, 
                        alpha2 = combined_params$aging2, 
                        stored_data = save_data, 
                        delta = 0.015, 
                        n_interest = duration)[, 1], digits = 0)) * combined_params$detection,
                      log = T)
  
  return(sum(likelihood))  
}

setUp_all <- createBayesianSetup(likelihood_all,
                                 lower = fitted_lb, 
                                 upper = fitted_ub, 
                                 names = parameters)

settings = list(iterations = 50, nrChains = 1, message = TRUE)

Sys.time()
results <- mclapply(1:4,
                    function(x) {
                      runMCMC(bayesianSetup = setUp_all, sampler = "DEzs", settings = settings)
                    },
                    mc.cores = 4)
Sys.time()

out_all <- createMcmcSamplerList(results)
