library(paletteer)
library(ggplot2)
library(patchwork)
library(matrixStats)

plot_shapes_maternal <- function(out){
  
  posterior <- getSample(out, thin = 100)
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
  
  maternal <- ggplot(data.frame(x = c(0, 25)), aes(x = x)) +
    geom_vline(xintercept = c(3, 6), linetype = "dashed", color = "black", linewidth = 0.5) +
    geom_rect(aes(xmin = 0, xmax = 3, ymin = -Inf, ymax = Inf), fill = "#0B0405FF", alpha = 0.1) +
    geom_rect(aes(xmin = 3, xmax = 6, ymin = -Inf, ymax = Inf), fill = "#357BA2FF", alpha = 0.1) +
    geom_rect(aes(xmin = 6, xmax = 25, ymin = -Inf, ymax = Inf), fill = "#78D6AEFF", alpha = 0.1) +
    geom_line(data = maternal_data, aes(x = x_vals, y = mean), colour = "red", size = 1) +
    geom_ribbon(data = maternal_data, aes(x = x_vals, ymax = upper, ymin = lower), alpha = 0.4, linetype = 0) +
    annotate("text", x = 1.5, y = 0.8, label = "High\nImmunity", size = 4, fontface = "bold") +
    annotate("text", x = 5.5, y = 0.8, label = "Low\nImmunity", size = 4, fontface = "bold") +
    annotate("text", x = 15, y = 0.8, label = "No\nImmunity", size = 4, fontface = "bold") +
    labs(x = "Months since last infection in mothers", y = "Proportion of immunity at birth in babies") +
    theme_bw() +
    xlim(0, 25)
  
  return(maternal)
  
}