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

for(n in 19:23){
  
  out <- readRDS(here("output", "data", "parameters", "19022025", paste0("out", n, ".rds")))
  
  posterior <- getSample(out, thin = 1000)
  fixed <- matrix(combinations[[n]]$fixed[!combinations[[n]]$ind],
                  nrow = nrow(posterior), 
                  ncol = sum(!combinations[[n]]$ind),
                  byrow = TRUE,
                  dimnames = list(NULL, combinations[[n]]$name[!combinations[[n]]$ind]))
  posterior <- cbind(posterior, fixed)
  
  # set duration of maternal immunity
  duration = combinations[[n]]$duration
  
  # create fixed datasets
  save_data <- create_data(n_interest = duration, rep = 30, factor = combinations[[n]]$factor)
  
  traj <- mclapply(1:nrow(posterior),
                   function(r){
                     output <- traj_function(lambda = exp(posterior[r, "disruption"]),
                                             theta1 = posterior[r, "inf_imm1"], theta2 = posterior[r, "inf_imm2"],
                                             omega1 = posterior[r, "waning1"], omega2 = posterior[r, "waning2"],
                                             alpha1 = posterior[r, "aging1"], alpha2 = posterior[r, "aging2"],
                                             stored_data = save_data,
                                             delta = 0.0075,
                                             n_interest = duration)
                     output[, c(3:27, 34)] <- output[, c(3:27, 34)] * posterior[r, "detection"]
                     return(output)
                   },
                   mc.cores = 4) # for trajectories including more detail (e.g., birth_month, etc.)
  
  traj <- mclapply(1:nrow(posterior),
                   function(r){
                     model_function(lambda = exp(posterior[r, "disruption"]), 
                                    theta1 = posterior[r, "inf_imm1"], theta2 = posterior[r, "inf_imm2"], 
                                    omega1 = posterior[r, "waning1"], omega2 = posterior[r, "waning2"], 
                                    alpha1 = posterior[r, "aging1"], alpha2 = posterior[r, "aging2"], 
                                    stored_data = save_data, 
                                    delta = 0.0075,  
                                    n_interest = duration)[, 1] * posterior[r, "detection"]
                   },
                   mc.cores = 4) # for trajectories like model output
  
  women <- women_function(lambda = exp(-4.3),
                          stored_data = save_data, 
                          delta = 0.0075,  
                          n_interest = duration) # for maternal infection history
  
  dir.create(here("output", "data", "trajectories", format(Sys.Date(), "%d%m%Y")))
  saveRDS(traj, file = here("output", "data", "trajectories", format(Sys.Date(), "%d%m%Y"), paste0("traj", n, ".rds"))) # needs to go in separate function
  
  plot_traceplot(out)
  plot_likelihood(out)

  plot_trajectories(traj)
  plot_hdi(traj)
  plot_shapes(out)
}
