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

# detection rate likelihood (to make up for difference between model and Scotland data)
likelihood_detection <- function(param){
  detection <- param[1]
  likelihood <- dbinom(round(scotland_rate$count, digits = 0),
                       size = as.numeric(round(model_function(lambda = 0, theta = 1/25, omega = -1/48, alpha = -1/48, stored_data = save_data)[, 1], digits = 0)),
                       prob = detection,
                       log = T)
  
  return(sum(likelihood))  
}

setUp_detection <- createBayesianSetup(likelihood_detection, lower = c(0), upper = c(1))

settings = list(iterations = 10000, nrChains = 1, message = TRUE)

Sys.time()
results <- mclapply(1:4,
                     function(x) {
                       runMCMC(bayesianSetup = setUp_detection, sampler = "Metropolis", settings = settings)
                       },
                     mc.cores = 12)
Sys.time()

out_detection <- createMcmcSamplerList(results)

posterior <- getSample(out_detection)

summary(out_detection)
plot(out_detection) 

# troubleshooting plots
# library(coda)
# mcmc_trimmed <- window(out_detection[[1]][["chain"]][[1]], start = 50 +1)
# traceplot(mcmc_trimmed[, c("par 1")])

# -------------------------------------------------------------------------

# adding parameters one at a time
likelihood_two <- function(param){
  detection <- param[1]
  disruption <- param[2]
  likelihood <- dbinom(round(scotland_rate$count, digits = 0),
                       size = as.numeric(round(model_function(lambda = exp(disruption), theta = 1/25, omega = -1/48, alpha = -1/48, stored_data = save_data)[, 1], digits = 0)),
                       prob = detection,
                       log = T)
  
  return(sum(likelihood))  
}

setUp_2 <- createBayesianSetup(likelihood_two, lower = c(0, -10), upper = c(1, 0))

settings = list(iterations = 10000, nrChains = 1, message = TRUE)

Sys.time()
results <- mclapply(1:4,
                    function(x) {
                      runMCMC(bayesianSetup = setUp_2, sampler = "DEzs", settings = settings)
                    },
                    mc.cores = 4)
Sys.time()

out_twoDEzs <- createMcmcSamplerList(results)

posterior <- getSample(out_two)

summary(out_two)
plot(out_two) 

# three parameters
likelihood_three <- function(param){
  detection <- param[1]
  disruption <- param[2]
  inf_imm <- param[3]
  likelihood <- dbinom(round(scotland_rate$count, digits = 0),
                       size = as.numeric(round(model_function(lambda = exp(disruption), theta = inf_imm, omega = -1/48, alpha = -1/48, stored_data = save_data)[, 1], digits = 0)),
                       prob = detection,
                       log = T)
  
  return(sum(likelihood))  
}

setUp_3 <- createBayesianSetup(likelihood_three, lower = c(0, -10, 0), upper = c(1, 0, 1))

settings = list(iterations = 10000, nrChains = 1, message = TRUE)

Sys.time()
results <- mclapply(1:4,
                    function(x) {
                      runMCMC(bayesianSetup = setUp_3, sampler = "DEzs", settings = settings)
                    },
                    mc.cores = 4)
Sys.time()

out_threeDEzs <- createMcmcSamplerList(results)

posterior <- getSample(out_three)

summary(out_three)
plot(out_three) 

# four parameters
likelihood_four <- function(param){
  detection <- param[1]
  disruption <- param[2]
  inf_imm <- param[3]
  waning <- param[4]
  
  likelihood <- dbinom(round(scotland_rate$count, digits = 0),
                       size = as.numeric(round(model_function(lambda = exp(disruption), theta = inf_imm, omega = waning, alpha = -1/48, stored_data = save_data)[, 1], digits = 0)),
                       prob = detection,
                       log = T)
  
  return(sum(likelihood))  
}

setUp_4 <- createBayesianSetup(likelihood_four, lower = c(0, -10, 0, -1), upper = c(1, 0, 1, 0))

settings = list(iterations = 10000, nrChains = 1, message = TRUE)

Sys.time()
results <- mclapply(1:4,
                    function(x) {
                      runMCMC(bayesianSetup = setUp_4, sampler = "DEzs", settings = settings)
                    },
                    mc.cores = 4)
Sys.time()

out_fourDEzs <- createMcmcSamplerList(results)

posterior <- getSample(out_four)

summary(out_four)
plot(out_four)

# -------------------------------------------------------------------------

# single likelihood function
likelihood_all <- function(param){
  detection <- param[1]
  disruption <- param[2]
  inf_imm <- param[3]
  waning <- param[4]
  aging <- param[5]
  
  likelihood <- dbinom(round(scotland_rate$count, digits = 0),
                       size = as.numeric(round(model_function(lambda = exp(disruption), theta = inf_imm, omega = waning, alpha = aging, stored_data = save_data)[, 1], digits = 0)),
                       prob = detection,
                       log = T)
  
  return(sum(likelihood))  
}

setUp_all <- createBayesianSetup(likelihood_all, lower = c(0, -10, 0, -1, -1), upper = c(1, 0, 1, 0, 0))

settings = list(iterations = 10000, nrChains = 1, message = TRUE)

Sys.time()
results <- mclapply(1:4,
                    function(x) {
                      runMCMC(bayesianSetup = setUp_all, sampler = "DEzs", settings = settings)
                    },
                    mc.cores = 4)
Sys.time()

out_allDEzs <- createMcmcSamplerList(results)

posterior <- getSample(out_all)

summary(out_all)
plot(out_all, which = c(1:4))

# -------------------------------------------------------------------------
# saving trajectories

posterior <- getSample(out_allDEzs)
Sys.time()
traj <- mclapply(1:nrow(posterior),
                 function(n){
                   model_function(lambda = exp(posterior[n, 2]), theta = posterior[n, 3], omega = posterior[n, 4], alpha = posterior[n, 5], stored_data = save_data)[, 1] * posterior[n, 1]
                 },
                 mc.cores = 4)
Sys.time()

traj_allDEzs <- traj