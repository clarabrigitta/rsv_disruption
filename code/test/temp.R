# load edited shapes script (thinned)
plot_shapes <- function(out){
  
  posterior <- getSample(out, thin = 1000)
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
  
  dir.create(here("output", "figures", "shapes", format(Sys.Date(), "%d%m%Y")))
  ggsave(filename = here("output", "figures", "shapes", format(Sys.Date(), "%d%m%Y"), paste0(n, ".png")), plot = fig, width = 12, height = 4, dpi = 300)
  
}

# code to discard runs with likelihood below certain value
# posterior <- getSample(out17, thin = 1000, parametersOnly = F)
# plot(out17, start = 10, thin = 1000, parametersOnly = F, whichParameters = "Llikelihood")
# subset <- posterior[posterior[, "Llikelihood"] > -8140, ]
# plot(subset[, "Llikelihood"], type = "l", 
#      main = "Filtered Llikelihood Plot", 
#      xlab = "Iteration", 
#      ylab = "Llikelihood")
# plot(density(subset[, "Llikelihood"]), 
#      main = "Density Plot of Llikelihood", 
#      xlab = "Llikelihood", 
#      ylab = "Density",
#      col = "blue",
#      lwd = 2)
# 
# posterior <- getSample(out17, thin = 1000, parametersOnly = F, coda = TRUE)
# subset <- lapply(posterior, function(chain) {
#   chain_df <- as.data.frame(chain)
#   filtered_df <- chain_df[chain_df$Llikelihood > -8140, ]
#   as.mcmc(filtered_df, start = 1, end = nrow(filtered_df), thin = 1)
# })
# subset <- Filter(function(chain) niter(chain) > 0, subset)
# 
# start_times <- sapply(subset, start)
# end_times <- sapply(subset, end)
# thin_values <- sapply(subset, thin)
# 
# print(start_times)
# print(end_times)
# print(thin_values)
# 
# posterior <- mcmc.list(subset)

for(n in 17:18){
  
  out <- readRDS(here("output", "data", "parameters", "19022025", paste0("out", n, ".rds")))
  
  posterior <- getSample(out, thin = 1000)
  fixed <- matrix(combinations[[n]]$fixed[!combinations[[n]]$ind],
                  nrow = nrow(posterior), 
                  ncol = sum(!combinations[[n]]$ind),
                  byrow = TRUE,
                  dimnames = list(NULL, combinations[[n]]$name[!combinations[[n]]$ind]))
  posterior <- cbind(posterior, fixed)
  
  traj <- mclapply(1:nrow(posterior),
                   function(r){
                     output <- traj_function(lambda = exp(posterior[r, "disruption"]), 
                                             theta1 = posterior[r, "inf_imm1"], theta2 = posterior[r, "inf_imm2"], 
                                             omega1 = posterior[r, "waning1"], omega2 = posterior[r, "waning2"], 
                                             alpha1 = posterior[r, "aging1"], alpha2 = posterior[r, "aging2"], 
                                             stored_data = save_data, 
                                             delta = 0.0075,  
                                             n_interest = duration)
                     output[, 8] <- output[, 8] * posterior[r, "detection"]
                     return(output)
                   },
                   mc.cores = 4)
  
  dir.create(here("output", "data", "trajectories", format(Sys.Date(), "%d%m%Y")))
  saveRDS(traj, file = here("output", "data", "trajectories", format(Sys.Date(), "%d%m%Y"), paste0("traj", n, ".rds"))) # needs to go in separate function
  
  plot_traceplot(out)
  plot_likelihood(out)

  plot_trajectories(traj)
  plot_hdi(traj)
  plot_shapes(out)
}
