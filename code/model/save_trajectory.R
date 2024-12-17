library(dplyr)
library(tidyr)
library(parallel)

save_trajectory <- function(out){
  
  posterior <- getSample(out)
  fixed <- matrix(combinations[[n]]$fixed[!combinations[[n]]$ind],
                  nrow = nrow(posterior), 
                  ncol = sum(!combinations[[n]]$ind),
                  byrow = TRUE,
                  dimnames = list(NULL, combinations[[n]]$name[!combinations[[n]]$ind]))
  posterior <- cbind(posterior, fixed)
  
  traj <- mclapply(1:nrow(posterior),
                   function(r){
                     model_function(lambda = exp(posterior[r, "disruption"]), 
                                    theta1 = posterior[r, "inf_imm1"], theta2 = posterior[r, "inf_imm2"], 
                                    omega1 = posterior[r, "waning1"], omega2 = posterior[r, "waning2"], 
                                    alpha1 = posterior[r, "aging1"], alpha2 = posterior[r, "aging2"], 
                                    stored_data = save_data, 
                                    delta = 0.015,  
                                    n_interest = duration)[, 1] * posterior[r, "detection"]
                   },
                   mc.cores = 4)
  
  
  saveRDS(traj, file = here("output", "data", "trajectories", "test", paste0("traj", n, ".rds"))) # needs to go in separate function
  
  return(traj)
  
}