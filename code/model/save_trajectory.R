library(dplyr)
library(tidyr)
library(parallel)


save_trajectory <- function(out){
  
  posterior <- getSample(out, thin = 100)
  posterior <- posterior[1:2000, ]
  fixed <- matrix(combinations[[n]]$fixed[!combinations[[n]]$ind],
                  nrow = nrow(posterior), 
                  ncol = sum(!combinations[[n]]$ind),
                  byrow = TRUE,
                  dimnames = list(NULL, combinations[[n]]$name[!combinations[[n]]$ind]))
  posterior <- cbind(posterior, fixed)
  
  traj <- mclapply(1:nrow(posterior),
                   function(r){
                     model_function(lambda = exp(unlist(posterior[r, "disruption"])), 
                                    theta1 = as.numeric(posterior[r, "inf_imm1"]), theta2 = as.numeric(posterior[r, "inf_imm2"]), 
                                    omega1 = as.numeric(posterior[r, "waning1"]), omega2 = as.numeric(posterior[r, "waning2"]), 
                                    alpha1 = as.numeric(posterior[r, "aging1"]), alpha2 = as.numeric(posterior[r, "aging2"]), 
                                    stored_data = save_data, 
                                    delta = 0.0075,  
                                    n_interest = duration)[, 1] * as.numeric(posterior[r, "detection"])
                   },
                   mc.cores = 4)
  
  dir.create(here("output", "data", "trajectories", format(Sys.Date(), "%d%m%Y")))
  saveRDS(traj, file = here("output", "data", "trajectories", format(Sys.Date(), "%d%m%Y"), paste0("traj_david_", n, ".rds"))) # needs to go in separate function
  
  return(traj)
  
}