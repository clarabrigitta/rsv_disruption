library(BayesianTools)
library(posterior)
library(bayesplot)
library(ggplot2)
library(gridExtra)
library(parallel)
library(here)

here()

source(here("code", "model", "create_data.R"))
source(here("code", "model", "model_function.R"))
source(here("code", "model", "create_combinations.R"))
source(here("code", "model", "save_trajectory.R"))
source(here("code", "plot", "plot_trajectories.R"))
source(here("code", "plot", "plot_hdi.R"))
source(here("code", "plot", "plot_traceplot.R"))
source(here("code", "plot", "plot_likelihood.R"))
source(here("code", "plot", "plot_shapes.R"))

n <- Sys.getenv("SLURM_ARRAY_TASK_ID")
n <- as.integer(n)

# -------------------------------------------------------------------------

# weekly rate of laboratory confirmed cases by age and pathogen aggregated to monthly
scotland_rate <- read.csv(here("data", "respiratory_age_20240515.csv")) %>%
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
duration = 24 # changeable as extra feature later on

# create fixed datasets
save_data <- create_data(n_interest = duration, rep = 30, n_burn = 20)

# create combinations to run
combinations <- create_combinations()

print(paste("start iteration number", n, "time:", Sys.time()))
# poisson likelihood function
likelihood <- function(params){
  
  params <- as.list(params)
  names(params) <- combinations[[n]]$name[combinations[[n]]$ind]
  fixed_params <- as.list(combinations[[n]]$fixed[!combinations[[n]]$ind])
  names(fixed_params) <- combinations[[n]]$name[!combinations[[n]]$ind]
  combined_params <- c(params, fixed_params)
  
  likelihood <- dpois(round(scotland_rate$count, digits = 0),
                      as.numeric(round(model_function(
                        lambda = exp(combined_params$disruption),
                        theta1 = combined_params$inf_imm1, theta2 = combined_params$inf_imm2,
                        omega1 = combined_params$waning1, omega2 = combined_params$waning2, 
                        alpha1 = combined_params$aging1, alpha2 = combined_params$aging2, 
                        stored_data = save_data, 
                        delta = 0.015, 
                        n_interest = duration)[, 1], digits = 0)) * combined_params$detection,
                      log = T)
  
  return(sum(likelihood))  
}

# fitting setup
setup <- createBayesianSetup(likelihood,
                             lower = combinations[[n]]$lb[combinations[[n]]$ind], 
                             upper = combinations[[n]]$ub[combinations[[n]]$ind], 
                             names = combinations[[n]]$name[combinations[[n]]$ind])

# settings = list(iterations = 10, nrChains = 1, message = TRUE)
settings = list(iterations = 100000, nrChains = 1, message = TRUE, burnin = 50000) # don't thin for now

# fit model and save output
results <- mclapply(1:4,
                    function(x) {
                      runMCMC(bayesianSetup = setup, sampler = "DEzs", settings = settings)
                    },
                    mc.cores = 4)

out <- createMcmcSamplerList(results)

saveRDS(out, file = here("output", "data", "parameters", "combinations", "test", paste0("out", n, ".rds")))

# compute trajectories
# traj <- save_trajectory(out)

# plot outputs
# plot_traceplot(out)
# plot_trajectories(traj)
# plot_hdi(traj)
# plot_shapes(out)

print(paste("end iteration number", n, "time:", Sys.time()))