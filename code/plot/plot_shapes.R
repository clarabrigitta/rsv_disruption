# prob_inf = 1
# (1 - ((1 - prob_inf) * 1/(1 + exp(1.5 * (x - 6.9)))))
# ggplot(data.frame(x = c(0, 48)), aes(x = x)) +
#   stat_function(fun = function(x){(1 - ((1 - prob_inf) * 1/(1 + exp(1.5 * (x - 6.9)))))}) +
#   scale_x_continuous(breaks = seq(0, 48, 4)) +
#   theme_bw() +
#   labs(x = "Time", y = "Probability of infection")

library(ggplot2)

plot_shapes <- function(out){
  
  posterior <- getSample(out)
  fixed <- matrix(combinations[[n]]$fixed[!combinations[[n]]$ind],
                  nrow = nrow(posterior), 
                  ncol = sum(!combinations[[n]]$ind),
                  byrow = TRUE,
                  dimnames = list(NULL, combinations[[n]]$name[!combinations[[n]]$ind]))
  posterior <- cbind(posterior, fixed)
  
  values <- combinations[[n]]$fixed[!combinations[[n]]$ind]
  names(values) <- combinations[[n]]$name[!combinations[[n]]$ind]
  values <- c(MAP(out)$parametersMAP, values)
  
  # maternal immunity plot
  maternal <- ggplot(data.frame(x = c(0, 25)), aes(x = x))
  
  for(r in 1:nrow(posterior)){
    maternal <- maternal +
      local({
        row <- r  # Capture the current value of r
        geom_function(fun = function(x) {
          1 / (1 + exp(-posterior[row, "inf_imm1"] * (x - posterior[row, "inf_imm2"])))
        }, alpha = 0.05)
      })
  }
  
  maternal <- maternal +
    geom_function(fun = function(x){1/(1+exp(-values["inf_imm1"]*(x-values["inf_imm2"])))},
                  colour = "red") +
    theme_bw() +
    labs(x = "Months since maternal infection", y = "Probability of infection at birth")
  
  # waning plot
  waning <- ggplot(data.frame(x = c(0, 48)), aes(x = x))
  
  for(r in 1:nrow(posterior)){
    waning <- waning +
      local({
        row <- r  # Capture the current value of r
        geom_function(fun = function(x) {
          1 / (1 + exp(posterior[row, "waning1"] * (x - posterior[row, "waning2"])))
        }, alpha = 0.05)
      })
  }
  
  waning <- waning +
    geom_function(fun = function(x){1/(1+exp(values["waning1"]*(x-values["waning2"])))},
                  colour = "red") +
    theme_bw() +
    labs(x = "Months since birth", y = "Waning immunity")
  
  # aging plot
  aging <- ggplot(data.frame(x = c(0, 48)), aes(x = x))
  
  for(r in 1:nrow(posterior)){
    aging <- aging +
      local({
        row <- r  # Capture the current value of r
        geom_function(fun = function(x) {
          1 / (1 + exp(posterior[row, "aging1"] * (x - posterior[row, "aging2"])))
        }, alpha = 0.05)
      })
  }
  
  aging <- aging +
    geom_function(fun = function(x){1/(1+exp(values["aging1"]*(x-values["aging2"])))},
                  colour = "red") +
    theme_bw() +
    labs(x = "Months since birth", y = "Probability of disease")
  
  fig <- grid.arrange(maternal, waning, aging, ncol = 3, nrow = 1)
  
  ggsave(filename = here("output", "figures", "shapes", "test", paste0(n, ".png")), plot = fig, width = 12, height = 4, dpi = 300)
  
}