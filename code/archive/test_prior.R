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

# -------------------------------------------------------------------------


# prior predictive distribution

# example
n_sim <- 10000
theta <-  runif(n_sim, 4, 12)
sigma = 1.5
y = rnorm(n_sim, theta, sigma)
hist(y, xlab = "Sleep hours")

# exploring different distributions
x <- seq(-0.2, 0.2, length=1000)
plot(x, dnorm(x, mean = 0.04, sd = 0.05), type="l", lwd=1)


# -------------------------------------------------------------------------
library(BayesianTools)
library(posterior)
library(bayesplot)

# scaling factor for rate during disruption

# likelihood function
likelihood1 <- function(param){
  disrupt <- param[1]
  stdev <- param[2]
  likelihood <- dnorm(scotland_rate$rate,
                      mean = model_function(lambda = disrupt, theta = 1/25, omega = -1/48, alpha = -1/48, stored_data = save_data)[, 1],
                      sd = stdev,
                      log = T)
  
  return(sum(likelihood1))  
}

# keeping track of time
# time <- Sys.time()
# likelihood1(c(0.0, 1.0))
# Sys.time() - time
# bench::mark(
#   likelihood1(c(0.0, 1.0))
# )

# setting up Bayesian model
setUp1 <- createBayesianSetup(likelihood1, lower = c(0, 0), upper = c(1, 1))

# run for 10,000 steps
settings = list(iterations = 10000, nrChains = 4, message = TRUE)
print(Sys.time())
out1 <- runMCMC(bayesianSetup = setUp1, sampler = "Metropolis", settings = settings)
print(Sys.time())
posterior <- getSample(out1)

summary(out1)
plot(out1) # plot internally calls tracePlot(out)


# -------------------------------------------------------------------------


# probability of infection
prob <- as.data.frame(matrix(NA, 25, 3))
colnames(prob) <- c("month", "prob_inf", "prob_dis")
prob <- prob %>%
  mutate(time = 1:25,
         prob_inf = case_when(time <=6 ~ 0,
                              time >6 & time <=15 ~ 0.5,
                              time >15 & time <=24  ~ 0.5,
                              time >24 ~ 1),
         prob_dis = case_when(time <=6 ~ 0,
                              time >6 & time <=15  ~ 0,
                              time >15 & time <=24 ~ 0.5,
                              time >24 ~ 1))

ggplot(data.frame(x = c(0, 25)), aes(x = x)) + 
  stat_function(fun = function(x){1/(1+exp(-x + exp(2.5)))}, aes(colour = "sigmoidal")) +
  geom_line(data = prob, aes(x = time, y = prob_inf, colour = "step-wise")) +
  stat_function(fun = function(x){0.04*x}, aes(colour = "linear")) +
  stat_function(fun = function(x){0.0441*exp(0.1248*x)}, aes(colour = "exponential")) +
  scale_colour_manual("Shapes", values = c("blue", "red",  "green", "black")) +
  scale_x_continuous(breaks = seq(0, 25, 5)) +
  theme_bw() +
  labs(x = "Immunity level", y = "Probability of infection")

# probability of infection given immunity level

# likelihood function
likelihood2 <- function(param){
  inf_imm <- param[1]
  stdev <- param[2]
  likelihood = dnorm(scotland_rate$rate,
                     mean = test %>% 
                       # model_function(lambda = 0, theta = inf_imm, omega = -1/48, alpha = -1/48) %>%
                       filter(yearmon %in% (scotland_rate %>% select(yearmon) %>% distinct() %>% pull()),
                              type == "disease") %>%
                       select(rate) %>% 
                       pull() %>% 
                       mean(),
                     sd = stdev, 
                     log = T)
  
  return(sum(likelihood))  
}

# setting up Bayesian model
setUp2 <- createBayesianSetup(likelihood2, lower = c(0, 0), upper = c(1, 1))

# run for 100,000 steps
settings = list(iterations = 10000, nrChains = 4, message = FALSE)
out2 <- runMCMC(bayesianSetup = setUp1, sampler = "Metropolis", settings = settings)

posterior <- getSample(out2)

summary(out2)
plot(out2) # plot internally calls tracePlot(out)


# -------------------------------------------------------------------------


# waning
waning <- as.data.frame(matrix(NA, 12*4, 3))
colnames(waning) <- c("time", "month", "waning")
waning <- waning %>%
  mutate(time = 1:48,
         month = rep(month.abb, 4),
         waning = case_when(time <= 6 ~ 0,
                           time > 6 & time <= 12 ~ 0.5,
                           time > 12 & time <= 24 ~ 0.8,
                           time > 24 ~ 1))

waning_linear <- function(x){1-(-1/48*x+1)}
waning_exponential <- function(x){1-(1*0.9^x)}

ggplot(data.frame(x = c(0, 48)), aes(x = x)) + 
  geom_line(data = waning, aes(x = time, y = waning, colour = "step-wise")) +
  stat_function(fun = waning_linear, aes(colour = "linear")) +
  stat_function(fun = waning_exponential, aes(colour = "exponential")) +
  theme_bw() +
  scale_colour_manual("Shapes", values = c("blue", "red", "black")) +
  scale_x_continuous(breaks = seq(0, 48, 3)) +
  labs(x = "Months since birth", y = "% increase on probability of infection", title = "Waning immunity over time")


# waning immunity given time since birth/age

# likelihood function
likelihood3 <- function(param){
  waning_imm <- param[1]
  stdev <- param[2]
  likelihood = dnorm(scotland_rate$rate,
                     mean = test %>% 
                       # model_function(lambda = 0, theta = 1/25, omega = waning_imm, alpha = -1/48) %>%
                       filter(yearmon %in% (scotland_rate %>% select(yearmon) %>% distinct() %>% pull()),
                              type == "disease") %>%
                       select(rate) %>% 
                       pull() %>% 
                       mean(),
                     sd = stdev, 
                     log = T)
  
  return(sum(likelihood))  
}

# setting up Bayesian model
setUp3 <- createBayesianSetup(likelihood3, lower = c(-1, 0), upper = c(1, 1))

# run for 100,000 steps
settings = list(iterations = 10000, nrChains = 4, message = FALSE)
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
                     mean = test %>% 
                       # model_function(lambda = 0, theta = 1/25, omega = -1/48, alpha = age) %>%
                       filter(yearmon %in% (scotland_rate %>% select(yearmon) %>% distinct() %>% pull()),
                              type == "disease") %>%
                       select(rate) %>% 
                       pull() %>% 
                       mean(),
                     sd = stdev, 
                     log = T)
  
  return(sum(likelihood))  
}

# setting up Bayesian model
setUp4 <- createBayesianSetup(likelihood4, lower = c(-1, 0), upper = c(1, 1))

# run for 100,000 steps
settings = list(iterations = 10000, nrChains = 4, message = FALSE)
out4 <- runMCMC(bayesianSetup = setUp1, sampler = "Metropolis", settings = settings)

posterior <- getSample(out4)

summary(out4)
plot(out4) # plot internally calls tracePlot(out)
