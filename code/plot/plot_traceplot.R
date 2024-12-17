library(BayesianTools)

plot_traceplot <- function(out){
  
  pdf(here("output", "figures", "traceplots", "test", paste0(n, ".pdf")), width = 8, height = 10)
  plot(out, start = 10)
  dev.off()
  
}
