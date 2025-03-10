# prob_inf = 1
# (1 - ((1 - prob_inf) * 1/(1 + exp(1.5 * (x - 6.9)))))
# ggplot(data.frame(x = c(0, 48)), aes(x = x)) +
#   stat_function(fun = function(x){(1 - ((1 - prob_inf) * 1/(1 + exp(1.5 * (x - 6.9)))))}) +
#   scale_x_continuous(breaks = seq(0, 48, 4)) +
#   theme_bw() +
#   labs(x = "Time", y = "Probability of infection")

library(ggplot2)

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
  
  # alternative method (plot means and 95% after calculating function values)
  
  x_vals <- seq(0, 25, length.out = 500)
  
  maternal_data <- matrix(nrow = nrow(posterior), ncol = 500)
  
  for(r in 1:nrow(posterior)){
    maternal_data[r ,] <- 1 - (1 / (1 + exp(-posterior[r, "inf_imm1"] * (x_vals - posterior[r, "inf_imm2"]))))
  }
  
  maternal_data <- maternal_data %>% hdi() %>% rbind(mean = colMedians(maternal_data)) %>% t() %>% cbind(x_vals)
  
  maternal <- ggplot() +  
    geom_line(data = maternal_data, aes(x = x_vals, y = mean), colour = "red", size = 1) +
    geom_ribbon(data = maternal_data, aes(x = x_vals, ymax = upper, ymin = lower), alpha = 0.4, linetype = 0) +
    labs(x = "Months since maternal infection", y = "Proportion of immunity at birth") +
    theme_bw() +
    xlim(0, 25)
  
  x_vals <- seq(0, 48, length.out = 500)
  
  waning_data <- matrix(nrow = nrow(posterior), ncol = 500)
  
  for(r in 1:nrow(posterior)){
    waning_data[r ,] <- 1 / (1 + exp(posterior[r, "waning1"] * (x_vals - posterior[r, "waning2"])))
  }
  
  waning_data <- waning_data %>% hdi() %>% rbind(mean = colMedians(waning_data)) %>% t() %>% cbind(x_vals)

  
  waning <- ggplot() +  
    geom_line(data = waning_data, aes(x = x_vals, y = mean), colour = "red", size = 1) +
    geom_ribbon(data = waning_data, aes(x = x_vals, ymax = upper, ymin = lower), alpha = 0.4, linetype = 0) +
    labs(x = "Age (months)", y = "Waning immunity") +
    theme_bw() 
  
  x_vals <- seq(0, 48, length.out = 500)
  
  aging_data <- matrix(nrow = nrow(posterior), ncol = 500)
  
  for(r in 1:nrow(posterior)){
    aging_data[r ,] <- 1 / (1 + exp(posterior[r, "aging1"] * (x_vals - posterior[r, "aging2"])))
  }
  
  aging_data <- aging_data %>% hdi() %>% rbind(mean = colMedians(aging_data)) %>% t() %>% cbind(x_vals)
  
  aging <- ggplot() +  
    geom_line(data = aging_data, aes(x = x_vals, y = mean), colour = "red", size = 1) +
    geom_ribbon(data = aging_data, aes(x = x_vals, ymax = upper, ymin = lower), alpha = 0.4, linetype = 0) +
    labs(x = "Age (months)", y = "Probability of disease") +
    theme_bw() 
  
  # # alternative method (plot trajectory of mean and 95% CI parameter values)
  # parameters <- posterior %>% hdi() %>% rbind(mean = colMeans(posterior))
  # 
  # x_vals <- seq(0, 48, length.out = 500)
  # 
  # maternal_min <- 1 - (1 / (1 + exp(-parameters["lower", "inf_imm1"] * (x_vals - parameters["lower", "inf_imm2"]))))
  # maternal_max <- 1 - (1 / (1 + exp(-parameters["upper", "inf_imm1"] * (x_vals - parameters["upper", "inf_imm2"]))))
  # waning_min <- 1 / (1 + exp(parameters["lower", "waning1"] * (x_vals - parameters["lower", "waning2"])))
  # waning_max <- 1 / (1 + exp(parameters["upper", "waning1"] * (x_vals - parameters["upper", "waning2"])))
  # aging_min <- 1 / (1 + exp(parameters["lower", "aging1"] * (x_vals - parameters["lower", "aging2"])))
  # aging_max <- 1 / (1 + exp(parameters["upper", "aging1"] * (x_vals - parameters["upper", "aging2"])))
  # 
  # ribbon_data <- data.frame(
  #   x = x_vals,
  #   maternal_min = maternal_min,
  #   maternal_max = maternal_max,
  #   waning_min = waning_min,
  #   waning_max = waning_max,
  #   aging_min = aging_min,
  #   aging_max = aging_max
  # )
  # 
  # maternal <- ggplot(data.frame(x = c(0, 25)), aes(x = x)) +  
  #   geom_vline(xintercept = c(4, 6), linetype = "dashed", color = "black", linewidth = 0.5) +
  #   geom_rect(aes(xmin = 0, xmax = 4, ymin = -Inf, ymax = Inf), fill = "#0B0405FF", alpha = 0.1) +
  #   geom_rect(aes(xmin = 4, xmax = 6, ymin = -Inf, ymax = Inf), fill = "#357BA2FF", alpha = 0.1) +
  #   geom_rect(aes(xmin = 6, xmax = 25, ymin = -Inf, ymax = Inf), fill = "#78D6AEFF", alpha = 0.1) +
  #   geom_ribbon(data = ribbon_data, aes(x = x, ymax = maternal_max, ymin = maternal_min), alpha = 0.4, linetype = 0) +
  #   geom_function(fun = function(x){1-(1/(1+exp(-parameters["mean", "inf_imm1"]*(x-parameters["mean","inf_imm2"]))))},
  #                 colour = "red", size = 1) +
  #   labs(x = "Months since maternal infection", y = "Proportion of immunity at birth") +
  #   theme_bw() +
  #   xlim(0, 25)
  # 
  # waning <- ggplot(data.frame(x = c(0, 48)), aes(x = x)) +
  #   geom_ribbon(data = ribbon_data, aes(x = x, ymax = waning_max, ymin = waning_min), alpha = 0.3, linetype = 0) +
  #   geom_function(fun = function(x){1 / (1 + exp(parameters["mean", "waning1"] * (x - parameters["mean", "waning2"])))},
  #                 colour = "red", size = 1) +
  #   labs(x = "Age (months)", y = "Waning immunity") +
  #   theme_bw()
  # 
  # aging <- ggplot(data.frame(x = c(0, 48)), aes(x = x)) +
  #   geom_ribbon(data = ribbon_data, aes(x = x, ymax = aging_max, ymin = aging_min), alpha = 0.3, linetype = 0) +
  #   geom_function(fun = function(x){1 / (1 + exp(parameters["mean", "aging1"] * (x - parameters["mean", "aging2"])))},
  #                 colour = "red", size = 1) +
  #   labs(x = "Age (months)", y = "Probability of disease") +
  #   theme_bw()
  
  # probability of infection heatmap
  x_mother <- seq(0, 25, length.out = 500)
  x_age <- seq(0, 48, length.out = 500)
  
  heatmap_data <- expand.grid(x_age = x_age, x_mother = x_mother)
  
  heatmap_data <- heatmap_data %>%
    mutate(prob_inf = 1 - ((1 - (1 / (1 + exp(-parameters["mean", "inf_imm1"] * 
                                                (x_mother - parameters["mean", "inf_imm2"]))))) * 
                             (1 / (1 + exp(parameters["mean", "waning1"] * 
                                             (x_age - parameters["mean", "waning2"])))))
    )
  
  heatmap <- ggplot(heatmap_data) +
    geom_tile(aes(x = x_age, y = x_mother, fill = prob_inf)) +
    scale_fill_paletteer_c("ggthemes::Green-Gold") +
    labs(x = "Age (months)",
         y = "Months since maternal infection",
         fill = "Probability of infection") +
    scale_y_continuous(limits = c(0, 8), breaks = seq(0, 10, 2)) + 
    theme_minimal()
  
  # probability of disease heatmap
  x_mother <- seq(0, 25, length.out = 500)
  x_age <- seq(0, 48, length.out = 500)
  
  heatmap_data <- expand.grid(x_age = x_age, x_mother = x_mother)
  
  heatmap_data <- heatmap_data %>%
    mutate(prob_dis = (1 - ((1 - (1 / (1 + exp(-parameters["mean", "inf_imm1"] * 
                                                (x_mother - parameters["mean", "inf_imm2"]))))) * 
                             (1 / (1 + exp(parameters["mean", "waning1"] * 
                                             (x_age - parameters["mean", "waning2"])))))) * (1 / (1 + exp(parameters["mean", "aging1"] * 
                                                                                                            (x_age - parameters["mean", "aging2"]))))
    )

  heatmap <- ggplot(heatmap_data) +
    geom_tile(aes(x = x_age, y = x_mother, fill = prob_dis)) +
    scale_fill_paletteer_c("ggthemes::Red-Gold") +
    labs(x = "Age (months)",
         y = "Months since maternal infection",
         fill = "Probability of disease") +
    scale_y_continuous(limits = c(0, 8), breaks = seq(0, 10, 2)) + 
    theme_minimal()
  
  # 1 - probability of infection is probability of not getting infected (equal to probability of immunity)
  # multiply by waning (which is function of time since birth) and indicates how "fast" immunity is degrading, how "whole" is probability of immunity after a certain time after birth
  # we want probability of infection, so we do 1 minus at the beginning
  # everything in bracket is working with probability of immunity, so we need to subtract it from 1 to get probability of infection
  
  # # maternal immunity plot
  # maternal <- ggplot(data.frame(x = c(0, 25)), aes(x = x))
  # 
  # for(r in 1:nrow(posterior)){
  #   maternal <- maternal +
  #     local({
  #       row <- r  # Capture the current value of r
  #       geom_function(fun = function(x) {
  #         1 / (1 + exp(-posterior[row, "inf_imm1"] * (x - posterior[row, "inf_imm2"])))
  #       }, alpha = 0.05)
  #     })
  # }
  # 
  # maternal <- maternal +
  #   geom_function(fun = function(x){1/(1+exp(-values["inf_imm1"]*(x-values["inf_imm2"])))},
  #                 colour = "red") +
  #   theme_bw() +
  #   labs(x = "Months since maternal infection", y = "Probability of infection at birth")
  # 
  # # waning plot
  # waning <- ggplot(data.frame(x = c(0, 48)), aes(x = x))
  # 
  # for(r in 1:nrow(posterior)){
  #   waning <- waning +
  #     local({
  #       row <- r  # Capture the current value of r
  #       geom_function(fun = function(x) {
  #         1 / (1 + exp(posterior[row, "waning1"] * (x - posterior[row, "waning2"])))
  #       }, alpha = 0.05)
  #     })
  # }
  # 
  # waning <- waning +
  #   geom_function(fun = function(x){1/(1+exp(values["waning1"]*(x-values["waning2"])))},
  #                 colour = "red") +
  #   theme_bw() +
  #   labs(x = "Months since birth", y = "Waning immunity")
  # 
  # # aging plot
  # aging <- ggplot(data.frame(x = c(0, 48)), aes(x = x))
  # 
  # for(r in 1:nrow(posterior)){
  #   aging <- aging +
  #     local({
  #       row <- r  # Capture the current value of r
  #       geom_function(fun = function(x) {
  #         1 / (1 + exp(posterior[row, "aging1"] * (x - posterior[row, "aging2"])))
  #       }, alpha = 0.05)
  #     })
  # }
  #
  # aging <- aging +
  #   geom_function(fun = function(x){1/(1+exp(values["aging1"]*(x-values["aging2"])))},
  #                 colour = "red") +
  #   theme_bw() +
  #   labs(x = "Months since birth", y = "Probability of disease")
  
  fig <- grid.arrange(maternal, waning, aging, ncol = 3, nrow = 1)
  
  dir.create(here("output", "figures", "shapes", format(Sys.Date(), "%d%m%Y")))
  ggsave(filename = here("output", "figures", "shapes", format(Sys.Date(), "%d%m%Y"), paste0(n, ".png")), plot = fig, width = 12, height = 4, dpi = 300)
  
}