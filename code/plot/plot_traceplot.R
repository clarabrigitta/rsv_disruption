library(BayesianTools)

plot_traceplot <- function(out){
  
  dir.create(here("output", "figures", "traceplots", format(Sys.Date(), "%d%m%Y")))
  pdf(here("output", "figures", "traceplots", format(Sys.Date(), "%d%m%Y"), paste0(n, ".pdf")), width = 8, height = 10)
  plot(out, start = 10)
  dev.off()
  
}
