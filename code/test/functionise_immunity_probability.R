# functionise immunity split

function(x){-1*x+26}

ggplot(data.frame(x = c(1, 25)), aes(x = x)) + 
  stat_function(fun = function(x){1*x}) +
  scale_x_continuous(breaks = seq(0, 25, 5)) +
  theme_bw() +
  labs(x = "Months since maternal infection", y = "Immunity level")

# functionise starting probabilities
# range input for x = (0, 25)

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
  
function(x){1/25*x} # linear
function(x){0.0441*exp(0.1248*x)} # exponential

ggplot(data.frame(x = c(0, 25)), aes(x = x)) + 
  geom_line(data = prob, aes(x = time, y = prob_inf, colour = "step-wise")) +
  stat_function(fun = function(x){1/25*x}, aes(colour = "linear")) +
  stat_function(fun = function(x){0.0441*exp(0.1248*x)}, aes(colour = "exponential")) +
  scale_colour_manual("Shapes", values = c("blue", "red", "black")) +
  scale_x_continuous(breaks = seq(0, 25, 5)) +
  theme_bw() +
  labs(x = "Immunity level", y = "Probability of infection")

# probability of disease
function(x){0.0526*x - 0.31585} # linear
function(x){0.0194*exp(0.1577*x)} # exponential

ggplot(data.frame(x = c(0, 25)), aes(x = x)) + 
  geom_line(data = prob, aes(x = time, y = prob_dis, colour = "step-wise")) +
  stat_function(fun = function(x){0.0526*x - 0.3158}, aes(colour = "linear")) +
  stat_function(fun = function(x){0.0194*exp(0.1577*x)}, aes(colour = "exponential")) +
  scale_colour_manual("Shapes", values = c("blue", "red", "black")) +
  scale_x_continuous(breaks = seq(0, 25, 5)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
  theme_bw() +
  labs(x = "Immunity level", y = "Probability of disease")



  
