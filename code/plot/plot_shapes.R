library(paletteer)
library(ggplot2)
library(patchwork)
library(matrixStats)

plot_shapes <- function(out){
  
  posterior <- getSample(out, thin = 100)
  posterior <- posterior[1:2000, ]
  fixed <- matrix(combinations[[n]]$fixed[!combinations[[n]]$ind],
                  nrow = nrow(posterior), 
                  ncol = sum(!combinations[[n]]$ind),
                  byrow = TRUE,
                  dimnames = list(NULL, combinations[[n]]$name[!combinations[[n]]$ind]))
  posterior <- cbind(posterior, fixed)
  
  # plot means and 95% after calculating function values

  x_vals <- seq(0, 25, length.out = 500)

  maternal_data <- matrix(nrow = nrow(posterior), ncol = 500)

  for(r in 1:nrow(posterior)){
    maternal_data[r ,] <- 1 - (1 / (1 + exp(-posterior[r, "inf_imm1"] * (x_vals - posterior[r, "inf_imm2"]))))
  }

  maternal_data <- maternal_data %>% hdi() %>% rbind(mean = colMeans(maternal_data)) %>% t() %>% cbind(x_vals)

  maternal <- ggplot() +
    geom_line(data = maternal_data, aes(x = x_vals, y = mean), colour = "red", size = 1.5) +
    geom_ribbon(data = maternal_data, aes(x = x_vals, ymax = upper, ymin = lower), alpha = 0.4, linetype = 0) +
    labs(x = "Months since maternal infection", y = "Proportion of immunity at birth") +
    theme_bw() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14)) +
    xlim(0, 25)

  x_vals <- seq(0, 48, length.out = 500)

  waning_data <- matrix(nrow = nrow(posterior), ncol = 500)

  for(r in 1:nrow(posterior)){
    waning_data[r ,] <- 1 / (1 + exp(posterior[r, "waning1"] * (x_vals - posterior[r, "waning2"])))
  }

  waning_data <- waning_data %>% hdi() %>% rbind(mean = colMeans(waning_data)) %>% t() %>% cbind(x_vals)

  waning <- ggplot() +
    geom_line(data = waning_data, aes(x = x_vals, y = mean), colour = "red", size = 1.5) +
    geom_ribbon(data = waning_data, aes(x = x_vals, ymax = upper, ymin = lower), alpha = 0.4, linetype = 0) +
    labs(x = "Age (months)", y = "Waning immunity") +
    theme_bw() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14)) 

  x_vals <- seq(0, 48, length.out = 500)

  aging_data <- matrix(nrow = nrow(posterior), ncol = 500)

  for(r in 1:nrow(posterior)){
    aging_data[r ,] <- 1 / (1 + exp(posterior[r, "aging1"] * (x_vals - posterior[r, "aging2"])))
  }

  aging_data <- aging_data %>% hdi() %>% rbind(mean = colMeans(aging_data)) %>% t() %>% cbind(x_vals)

  aging <- ggplot() +
    geom_line(data = aging_data, aes(x = x_vals, y = mean), colour = "red", size = 1.5) +
    geom_ribbon(data = aging_data, aes(x = x_vals, ymax = upper, ymin = lower), alpha = 0.4, linetype = 0) +
    labs(x = "Age (months)", y = "Probability of disease") +
    theme_bw() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14)) + 
    coord_cartesian(ylim = c(0, NA))
  
  parameters <- posterior %>% hdi() %>% rbind(mean = colMeans(posterior))
  
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
  
  inf_heatmap <- ggplot(heatmap_data) +
    geom_tile(aes(x = x_age, y = x_mother, fill = prob_inf)) +
    scale_fill_paletteer_c("ggthemes::Green-Gold") +
    labs(x = "Age (months)",
         y = "Months since maternal infection",
         fill = "Probability of infection") +
    scale_y_continuous(limits = c(0, 8), breaks = seq(0, 10, 2)) + 
    theme_bw() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 12)) 
  
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
  
  dis_heatmap <- ggplot(heatmap_data) +
    geom_tile(aes(x = x_age, y = x_mother, fill = prob_dis)) +
    scale_fill_paletteer_c("ggthemes::Red-Gold") +
    labs(x = "Age (months)",
         y = "Months since maternal infection",
         fill = "Probability of disease") +
    scale_y_continuous(limits = c(0, 8), breaks = seq(0, 10, 2)) + 
    theme_bw() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 12)) 
  
  fig <- (maternal + waning + aging) / (inf_heatmap + dis_heatmap) + plot_annotation(tag_levels = "A") + theme(plot.tag = element_text(size = 14))
  
  dir.create(here("output", "figures", "shapes", format(Sys.Date(), "%d%m%Y")))
  ggsave(filename = here("output", "figures", "shapes", format(Sys.Date(), "%d%m%Y"), paste0(n, ".png")), plot = fig, width = 13, height = 8, dpi = 300)
  
}