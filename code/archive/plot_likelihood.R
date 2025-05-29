library(BayesianTools)

plot_likelihood <- function(out){
  
  dir.create(here("output", "figures", "likelihood", format(Sys.Date(), "%d%m%Y")))
  png(here("output", "figures", "likelihood", format(Sys.Date(), "%d%m%Y"), paste0(n, ".png")), width = 800, height = 400)
  plot(out, start = 10, parametersOnly = F, whichParameters = "Llikelihood")
  dev.off()
  
}