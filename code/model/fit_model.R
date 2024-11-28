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

# poisson likelihood function
likelihood_all <- function(param){
  detection <- param[1]
  disruption <- param[2]
  inf_imm1 <- param[3]
  inf_imm2 <- param[4]
  waning1 <- param[5]
  waning2 <- param[6]
  aging1 <- param[7]
  aging2 <- param[8]
  # caseimport <- param[9]

  likelihood <- dpois(round(scotland_rate$count, digits = 0),
                      as.numeric(round(model_function(lambda = exp(disruption), theta1 = inf_imm1, theta2 = inf_imm2, omega1 = waning1, omega2 = waning2, alpha1 = aging1, alpha2 = aging2, stored_data = save_data, delta = 0.015)[, 1], digits = 0)) * detection,
                      log = T)
  
  return(sum(likelihood))  
}

setUp_all <- createBayesianSetup(likelihood_all, lower = c(0, -10,
                                                           0.2, 0,
                                                           0.2, 0,
                                                           0.2, 0), upper = c(1, 0,
                                                                              1.5, 20,
                                                                              1.5, 30,
                                                                              1.5, 30))

settings = list(iterations = 50000, nrChains = 1, message = TRUE, burnin = 25000)

Sys.time()
results <- mclapply(1:4,
                    function(x) {
                      runMCMC(bayesianSetup = setUp_all, sampler = "DEzs", settings = settings)
                    },
                    mc.cores = 4)
Sys.time()

out_all <- createMcmcSamplerList(results)
saveRDS(out_all, file = paste0("./output/data/parameters/out_", "...", ".rds"))

posterior <- getSample(out_all)

summary(out_all)
plot(out_all, which = c(1:2), start = 10)
plot(out_all, which = c(3:4), start = 10)
plot(out_all, which = c(5:6), start = 10)
plot(out_all, which = c(7:8), start = 10)
correlationPlot(out_all)
traceplot(out_all)

# -------------------------------------------------------------------------

# saving trajectories

posterior <- getSample(out_all)
Sys.time()
traj <- mclapply(1:nrow(posterior),
                 function(n){
                   model_function(lambda = exp(posterior[n, 2]), theta1 = posterior[n, 3], theta2 = posterior[n, 4], omega1 = posterior[n, 5], omega2 = posterior[n, 6], alpha1 = posterior[n, 7], alpha2 = posterior[n, 8], stored_data = save_data, delta = 0.015)[, 1] * posterior[n, 1]
                 },
                 mc.cores = 4)
Sys.time()
traj_all <- traj
saveRDS(traj_all, file = paste0("./output/data/trajectories/traj_", "...", ".rds"))
