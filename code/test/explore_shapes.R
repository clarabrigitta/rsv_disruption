# load libraries
library(dplyr)
library(ggplot2)

# plot waning immunity
curve(1-x/36, from = 0, to = 36, xlab = "Months since birth", ylab = "Waning immunity", main = "Waning immunity over time")
curve(1*0.9^x, from = 0, to = 36, xlab = "Months since birth", ylab = "Waning immunity", main = "Waning immunity over time")

waning <- as.data.frame(matrix(NA, 12*3, 3))
colnames(waning) <- c("time", "month", "waning")
waning <- waning %>%
  mutate(time = 1:36,
         month = rep(month.abb, 3),
         waning = case_when(time <= 6 ~ 1,
                            time > 6 & time <= 12 ~ 0.5,
                            time > 12 & time <= 24 ~ 0.2,
                            time > 24 ~ 0))

waning_linear <- function(x){1-x/36}
waning_exponential <- function(x){1*0.9^x}

ggplot(data.frame(x = c(0, 36)), aes(x = x)) + 
  geom_line(data = waning, aes(x = time, y = waning, colour = "step-wise")) +
  stat_function(fun = waning_linear, aes(colour = "linear")) +
  stat_function(fun = waning_exponential, aes(colour = "exponential")) +
  theme_bw() +
  scale_colour_manual("Shapes", values = c("blue", "red", "black")) +
  scale_x_continuous(breaks = seq(0, 36, 3)) +
  labs(x = "Months since birth", y = "% decrease on probability of disease", title = "Aging over time")

# plot increasing effect of aging
curve(1-(1-x/36), from = 0, to = 36, xlab = "Months since birth", ylab = "Increasing age", main = "Increasing age over time")
curve(1-(1*0.9^x), from = 0, to = 36, xlab = "Months since birth", ylab = "Increasing age", main = "Increasing age over time")

aging <- as.data.frame(matrix(NA, 12*3, 3))
colnames(aging) <- c("time", "month", "aging")
aging <- aging %>%
  mutate(time = 1:36,
         month = rep(month.abb, 3),
         aging = case_when(time <= 6 ~ 0,
                           time > 6 & time <= 12 ~ 0.5,
                           time > 12 & time <= 24 ~ 0.8,
                           time > 24 ~ 1))

aging_linear <- function(x){-1/36*x + 1}
aging_exponential <- function(x){1-(1*0.9^x)}

ggplot(data.frame(x = c(0, 36)), aes(x = x)) + 
  geom_line(data = aging, aes(x = time, y = aging, colour = "step-wise")) +
  stat_function(fun = aging_linear, aes(colour = "linear")) +
  stat_function(fun = aging_exponential, aes(colour = "exponential")) +
  theme_bw() +
  scale_colour_manual("Shapes", values = c("blue", "red", "black")) +
  scale_x_continuous(breaks = seq(0, 36, 3)) +
  labs(x = "Months since birth", y = "% increase on probability of infection", title = "Waning immunity over time")
