library(BayesianTools)

plot_likelihood <- function(out){
  
  png(here("output", "figures", "likelihood", paste0(n, ".png")), width = 800, height = 400)
  plot(out, start = 10, parametersOnly = F, whichParameters = "Llikelihood")
  dev.off()
  
}