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
  ungroup()

View(cbind(scotland_rate, model = model_function(lambda = 0, theta = 1/25, omega = -1/48, alpha = -1/48, stored_data = save_data, uni = 0.2)[, 1]))

# plot scotland vs model rate
both <- cbind(scotland_rate, model = model_function(lambda = 0.037, theta = 0.041, omega = 0.037, alpha = -1/48, stored_data = save_data, uni = 0.081)[, 1])

ggplot(both) +
  geom_line(aes(x = yearmon, y = rate, colour = age), linetype = 1) +
  geom_line(aes(x = yearmon, y = model, colour = age), linetype = 2) +
  theme_classic()

# plot mcmc output
plot(out_uni)
plot(out1)
plot(out2)

# -------------------------------------------------------------------------
library(BayesianTools)
library(posterior)
library(bayesplot)

source("~/Desktop/rsv_disruption/code/model/model_function.R")

# adding likelihood for universal scaling factor for rate of exposure (to make up for difference between model and Scotland data)
likelihood_uni <- function(param){
  scale <- param[1]
  stdev <- param[2]
  likelihood <- dnorm(scotland_rate$rate,
                      mean = model_function(lambda = 0, theta = 1/25, omega = -1/48, alpha = -1/48, stored_data = save_data, uni = scale)[, 1],
                      sd = stdev,
                      log = T)
  
  return(sum(likelihood))  
}

setUp_uni <- createBayesianSetup(likelihood_uni, lower = c(0, 0), upper = c(1, 1000))

settings = list(iterations = 1000, nrChains = 4, message = TRUE)
out_uni <- runMCMC(bayesianSetup = setUp_uni, sampler = "Metropolis", settings = settings)

posterior <- getSample(out_uni)

summary(out_uni)
plot(out_uni) 

# scaling factor for rate during disruption

# likelihood function
likelihood1 <- function(param){
  disrupt <- param[1]
  stdev <- param[2]
  likelihood <- dnorm(scotland_rate$rate,
                      mean = model_function(lambda = disrupt, theta = 1/25, omega = -1/48, alpha = -1/48, stored_data = save_data, uni = 0.08)[, 1],
                      sd = stdev,
                      log = T)
  
  return(sum(likelihood))  
}

# keeping track of time
# time <- Sys.time()
# likelihood1(c(0.0, 1.0))
# Sys.time() - time
# bench::mark(
#   likelihood1(c(0.0, 1.0))
# )

# setting up Bayesian model
setUp1 <- createBayesianSetup(likelihood1, lower = c(0, 0), upper = c(1, 1000))

# run for 10,000 steps
settings = list(iterations = 1000, nrChains = 4, message = TRUE)
out1 <- runMCMC(bayesianSetup = setUp1, sampler = "Metropolis", settings = settings)

posterior <- getSample(out1)

summary(out1)
plot(out1) # plot internally calls tracePlot(out)


# -------------------------------------------------------------------------

# probability of infection given immunity level

# likelihood function
likelihood2 <- function(param){
  inf_imm <- param[1]
  stdev <- param[2]
  likelihood = dnorm(scotland_rate$rate,
                     mean = model_function(lambda = 0, theta = inf_imm, omega = -1/48, alpha = -1/48, stored_data = save_data, uni = 0.08)[, 1],
                     sd = stdev, 
                     log = T)
  
  return(sum(likelihood))  
}

# setting up Bayesian model
setUp2 <- createBayesianSetup(likelihood2, lower = c(-1, 0), upper = c(1, 200))

# run for 100,000 steps
settings = list(iterations = 1000, nrChains = 4, message = FALSE)
out2 <- runMCMC(bayesianSetup = setUp2, sampler = "Metropolis", settings = settings)

posterior <- getSample(out2)

summary(out2)
plot(out2) # plot internally calls tracePlot(out)


# -------------------------------------------------------------------------

# waning immunity given time since birth/age

# likelihood function
likelihood3 <- function(param){
  waning_imm <- param[1]
  stdev <- param[2]
  likelihood = dnorm(scotland_rate$rate,
                     mean = model_function(lambda = 0, theta = 1/25, omega = waning_imm, alpha = -1/48, stored_data = save_data, uni = 0.08)[, 1],
                     sd = stdev, 
                     log = T)
  
  return(sum(likelihood))  
}

# setting up Bayesian model
setUp3 <- createBayesianSetup(likelihood3, lower = c(-1, 0), upper = c(1, 200))

# run for 100,000 steps
settings = list(iterations = 1000, nrChains = 4, message = TRUE)
out3 <- runMCMC(bayesianSetup = setUp1, sampler = "Metropolis", settings = settings)

posterior <- getSample(out3)

summary(out3)
plot(out3) # plot internally calls tracePlot(out)


# -------------------------------------------------------------------------


# aging given time since birth/age

# likelihood function
likelihood4 <- function(param){
  age <- param[1]
  stdev <- param[2]
  likelihood = dnorm(scotland_rate$rate,
                     mean = model_function(lambda = 0, theta = 1/25, omega = -1/48, alpha = age, stored_data = save_data, uni = 0.08)[, 1],
                     sd = stdev, 
                     log = T)
  
  return(sum(likelihood))  
}

# setting up Bayesian model
setUp4 <- createBayesianSetup(likelihood4, lower = c(-1, 0), upper = c(1, 200))

# run for 100,000 steps
settings = list(iterations = 1000, nrChains = 4, message = FALSE)
out4 <- runMCMC(bayesianSetup = setUp1, sampler = "Metropolis", settings = settings)

posterior <- getSample(out4)

summary(out4)
plot(out4) # plot internally calls tracePlot(out)
