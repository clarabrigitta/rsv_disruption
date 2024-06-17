# plot prior predictive distribution

# example
n_sim <- 10000
theta <-  runif(n_sim, 4, 12)
sigma = 1.5
y = rnorm(n_sim, theta, sigma)
hist(y, xlab = "Sleep hours")

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
  stat_function(fun = function(x){1/25*x}, aes(colour = "linear")) +
  stat_function(fun = function(x){0.0441*exp(0.1248*x)}, aes(colour = "exponential")) +
  scale_colour_manual("Shapes", values = c("blue", "red",  "green", "black")) +
  scale_x_continuous(breaks = seq(0, 25, 5)) +
  theme_bw() +
  labs(x = "Immunity level", y = "Probability of infection")

# prior probability density
n_sim <- 10000
theta <-  runif(n_sim, 0.02, 0.04)
sigma = 0.01
y = rnorm(n_sim, theta, sigma)
hist(y, xlab = "Sleep hours")

x <- seq(-0.2, 0.2, length=1000)
plot(x, dnorm(x, mean = 0.04, sd = 0.05), type="l", lwd=1)

# likelihood function
likelihood1 <- function(param){
  prob <- param
  likelihood = dnorm(data$prob_inf, mean = prob, sd = 0.5, log = T)

  return(sum(likelihood))  
}

# setting up Bayesian model
setUp1 <- createBayesianSetup(likelihood1, lower = c(0), upper = c(1))

# run for 100,000 steps
settings = list(iterations = 10000, message = FALSE)
out1 <- runMCMC(bayesianSetup = setUp1, sampler = "Metropolis", settings = settings)

posterior <- getSample(out1)

summary(out1)
plot(out1) # plot internally calls tracePlot(out)
