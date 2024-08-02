ggplot(data.frame(x = c(0, 48)), aes(x = x)) + 
  stat_function(fun = function(x){1 -1/48*x}) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 48, 4)) +
  labs(title = "Omega (guess)", x = "Months since birth", y = "Waning") # rate of waning immunity

ggplot(data.frame(x = c(0, 48)), aes(x = x)) + 
  stat_function(fun = function(x){1/48*x}) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 48, 4)) +
  labs(title = "Omega (guess)", x = "Months since birth", y = "") # increase in probability of infection

prob_inf <- theta * c(1:25)

waning <- 1 - 1/24 * c(1:48)
waning[waning < 0] <- 0
waning <- 1/48 * c(1:48)

1 - ((1 - prob_inf) * waning) 
# 1 - probability of infection is probability of not getting infected (equal to probability of immunity)
# multiply by waning (which is function of time since birth) and indicates how "fast" immunity is degrading, how "whole" is probability of immunity after a certain time after birth
# we want probability of infection, so we do 1 minus at the beginning
# everything in bracket is working with probability of immunity, so we need to subtract it from 1 to get probability of infection

min(prob_inf * (1 + waning), 1)
