library(patchwork)

# plot rates for entire study period and one year
rate <- ((dates %>% 
            distinct() %>% 
            filter(yearmon >= "Jul 2010", yearmon <= "Jul 2011") %>% 
            mutate(rate = rate + 0.0075) %>% 
            ggplot() +
            geom_line(aes(x = time_calendar, y = rate)) +
            scale_x_continuous(breaks = seq(7, 19, 1), labels = c(month.abb[7:12], month.abb[1:7])) +
            theme_bw() +
            labs(y = "Rate of RSV Exposure (\u03bb)",
                 x = "Month of Year") +
            theme(axis.text=element_text(size=12),
                  axis.title=element_text(size=14))) / (dates %>% 
                                                          distinct() %>% 
                                                          filter(yearmon >= "Oct 2016", yearmon <= "Oct 2024") %>% 
                                                          mutate(rate = rate + 0.0075) %>% 
                                                          mutate(rate = ifelse(yearmon >= "Mar 2020" & yearmon <= "Mar 2021", rate * exp(-4), rate)) %>% 
                                                          ggplot() +
                                                          geom_line(aes(x = yearmon, y = rate)) +
                                                          theme_bw() +
                                                          labs(y = "Rate of RSV Exposure (\u03bb)",
                                                               x = "Month") +
                                                          theme(axis.text=element_text(size=12),
                                                                axis.title=element_text(size=14))) + plot_annotation(tag_levels = "A"))

# plot births for study period
births <- (birth_data %>% 
  filter(yearmon >= "Jan 2010") %>% 
  ggplot() +
  geom_line(aes(x = yearmon, y = births)) +
  theme_bw() +
  scale_x_yearmon(breaks = seq(from = as.yearmon("Jan 2010"), 
                               to = as.yearmon("Oct 2024"), 
                               by = 1)) +
  labs(x = "Time (months)",
       y = "Number of births") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        axis.text.x = element_text(angle = 45, hjust = 1))) / (birth_data %>% 
  filter(yearmon >= "Jan 2010") %>% 
  ggplot() +
  geom_line(aes(x = yearmon, y = births)) +
  theme_bw() +
  scale_x_yearmon(breaks = seq(from = as.yearmon("Jan 2010"), 
                               to = as.yearmon("Feb 2025"), 
                               by = 1)) +
  labs(x = "Time (months)",
       y = "Number of births") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        axis.text.x = element_text(angle = 45, hjust = 1))) +  plot_annotation(tag_levels = "A") + theme(plot.tag = element_text(size = 14))

# extract posteriors for sensitivity analysis on duration of immunity
n = 19
out <- readRDS(here("output", "data", "parameters", "15032025*", paste0("out", n, ".rds")))

posterior <- getSample(out, thin = 100)
posterior <- posterior[1:2000, ]
fixed <- matrix(combinations[[n]]$fixed[!combinations[[n]]$ind],
                nrow = nrow(posterior), 
                ncol = sum(!combinations[[n]]$ind),
                byrow = TRUE,
                dimnames = list(NULL, combinations[[n]]$name[!combinations[[n]]$ind]))
posterior19 <- cbind(posterior, fixed)

n = 20
out <- readRDS(here("output", "data", "parameters", "15032025*", paste0("out", n, ".rds")))

posterior <- getSample(out, thin = 100)
posterior <- posterior[1:2000, ]
fixed <- matrix(combinations[[n]]$fixed[!combinations[[n]]$ind],
                nrow = nrow(posterior), 
                ncol = sum(!combinations[[n]]$ind),
                byrow = TRUE,
                dimnames = list(NULL, combinations[[n]]$name[!combinations[[n]]$ind]))
posterior20 <- cbind(posterior, fixed)

out <- readRDS(here("output", "data", "parameters", "15032025*", paste0("out", n, ".rds")))

n = 21
posterior <- getSample(out, thin = 100)
posterior <- posterior[1:2000, ]
fixed <- matrix(combinations[[n]]$fixed[!combinations[[n]]$ind],
                nrow = nrow(posterior), 
                ncol = sum(!combinations[[n]]$ind),
                byrow = TRUE,
                dimnames = list(NULL, combinations[[n]]$name[!combinations[[n]]$ind]))
posterior21 <- cbind(posterior, fixed)

n = 17
out <- readRDS(here("output", "data", "parameters", "15032025*", paste0("out", n, ".rds")))

posterior <- getSample(out, thin = 100)
posterior <- posterior[1:2000, ]
fixed <- matrix(combinations[[n]]$fixed[!combinations[[n]]$indq
],
nrow = nrow(posterior), 
ncol = sum(!combinations[[n]]$ind),
byrow = TRUE,
dimnames = list(NULL, combinations[[n]]$name[!combinations[[n]]$ind]))
posterior <- cbind(posterior, fixed)

# plot sensitivity analysis for duration of immunity
sensitivity_duration <- posterior %>% 
  as.data.frame() %>% 
  pivot_longer(1:6, names_to = "parameters") %>% 
  mutate(duration = "24 months") %>% 
  bind_rows(posterior19 %>% 
              as.data.frame() %>% 
              pivot_longer(1:6, names_to = "parameters") %>% 
              mutate(duration = "12 months")) %>% 
  bind_rows(posterior20 %>% 
              as.data.frame() %>% 
              pivot_longer(1:6, names_to = "parameters") %>% 
              mutate(duration = "36 months")) %>% 
  bind_rows(posterior21 %>% 
              as.data.frame() %>% 
              pivot_longer(1:6, names_to = "parameters") %>% 
              mutate(duration = "60 months")) %>% 
  mutate(parameters = factor(parameters, levels = c("inf_imm1", "inf_imm2", "waning1", "waning2", "aging1", "aging2"))) %>% 
  ggplot() +
  geom_boxplot(aes(x = parameters, y = value, fill = duration), outliers = FALSE, size = 0.3) +
  scale_x_discrete(labels = c(aging1 = expression(alpha[1]),
                              aging2 = expression(alpha[2]),
                              inf_imm1 = expression(theta[1]),
                              inf_imm2 = expression(theta[2]),
                              waning1 = expression(omega[1]),
                              waning2 = expression(omega[2]))) +
  scale_fill_viridis_d() +
  labs(x = "Parameters Estimated",
       y = "Value",
       fill = "Immunity Duration\nin Mothers") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

# plot infection history in mothers with burn-in
burnin <- women %>% 
  as.data.frame() %>% 
  pivot_longer(c("I1":paste0("I", n_interest), "susceptible_naive", "susceptible_reinf"), names_to = "infection", values_to = "count") %>% 
  mutate(infection = factor(infection, levels = c("susceptible_naive", "susceptible_reinf", rev(str_c(rep("I", n_interest), 1:n_interest))))) %>% 
  left_join(dates, by = join_by(time)) %>% 
  filter(time < 359) %>% 
  ggplot() +
  geom_bar(aes(x = date, y = count, fill = infection), position = "stack", stat = "identity") +
  scale_fill_manual(values = c("lightgrey", "darkgrey", viridis(n_interest))) +
  scale_x_date(date_breaks = "1 year", date_labels = "%b %Y") +
  coord_cartesian(xlim = c(as.Date("1996-01-01"), as.Date("2024-01-01"))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text=element_text(size=12),
        legend.title=element_text(size=14)) +
  labs(x = "Months",
       y = "Count",
       fill = "Infection Status")

# plot sensitivity analysis for disruption scenario
sensitivity_contacts <- posterior17 %>% 
  as.data.frame() %>% 
  pivot_longer(1:6, names_to = "parameters") %>% 
  mutate(contact = "12-month reduction") %>% 
  select(-c(detection, disruption)) %>% 
  bind_rows(posterior57 %>% 
              as.data.frame() %>% 
              mutate(disruption = exp(disruption)) %>% 
              pivot_longer(1:7, names_to = "parameters") %>% 
              mutate(contact = "alternative scenario") %>% 
              select(-c(detection))) %>% 
  mutate(parameters = factor(parameters, levels = c("inf_imm1", "inf_imm2", "waning1", "waning2", "aging1", "aging2", "disruption"))) %>% 
  ggplot() +
  geom_boxplot(aes(x = parameters, y = value, fill = contact), outliers = FALSE, size = 0.3) +
  scale_x_discrete(labels = c(aging1 = expression(alpha[1]),
                              aging2 = expression(alpha[2]),
                              inf_imm1 = expression(theta[1]),
                              inf_imm2 = expression(theta[2]),
                              waning1 = expression(omega[1]),
                              waning2 = expression(omega[2]),
                              disruption = expression(exp(delta)))) +
  scale_fill_viridis_d() +
  labs(x = "Parameters Estimated",
       y = "Value",
       fill = "Lockdown Lifting\nScenario") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

# plot disruption scenarios over time
disruption <- as.data.frame(matrix(nrow = 17, ncol = 2000))
for(r in 1:nrow(as.data.frame(posterior57))){
  disruption[, r] <- c(rep(0.01, 2), seq(0.01, exp(as.data.frame(posterior57)[r , "disruption"]), length.out = 3), rep(exp(as.data.frame(posterior57)[r , "disruption"]), 1), seq(exp(as.data.frame(posterior57)[r , "disruption"]), 0.01, length.out = 5), rep(0.01, 2), seq(0.01, 1, length.out = 4))
}

disruption_processed <- disruption %>%
  t() %>% 
  hdi() %>% 
  rbind(mean = rowMeans(disruption)) %>% 
  t() %>% 
  as.data.frame() %>% 
  mutate(month = seq(as.Date("2020-03-01"), as.Date("2021-07-01"), by = "month"),
         ori = c(rep(exp(-4.3), 13), rep(1, 4))) %>% 
  ggplot() +
  geom_line(aes(x = month, y = ori, colour = "12-month reduction", fill = "12-month reduction")) +
  geom_line(aes(x = month, y = mean, colour = "alternative scenario", fill = "alternative scenario")) +
  geom_ribbon(aes(x = month, ymax= upper, ymin = lower, colour = "alternative scenario", fill = "alternative scenario"), alpha = 0.5) +
  scale_x_date(date_breaks = "3 month") +
  scale_colour_manual(
    name = "Lockdown Lifting\nScenario",
    values = c("12-month reduction"   = "#440154FF", "alternative scenario" = "#FDE725FF")) +
  scale_fill_manual(
    name = "Lockdown Lifting\nScenario",
    values = c("12-month reduction"   = "#440154FF","alternative scenario" = "#FDE725FF")) +
  labs(x = "Month",
       y = "Value") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

dir.create(here("output", "figures", "supplements", format(Sys.Date(), "%d%m%Y")))
ggsave(filename = here("output", "figures", "supplements", format(Sys.Date(), "%d%m%Y"), "rate.png"), plot = rate, width = 10, height = 7, dpi = 300)
ggsave(filename = here("output", "figures", "supplements", format(Sys.Date(), "%d%m%Y"), "births.png"), plot = births, width = 8, height = 6, dpi = 300)
ggsave(filename = here("output", "figures", "supplements", format(Sys.Date(), "%d%m%Y"), "sensitivity_contacts.png"), plot = sensitivity_contacts, width = 10, height = 6, dpi = 300)
ggsave(filename = here("output", "figures", "supplements", format(Sys.Date(), "%d%m%Y"), "burnin.png"), plot = burnin, width = 10, height = 6, dpi = 300)
ggsave(filename = here("output", "figures", "supplements", format(Sys.Date(), "%d%m%Y"), "sensitivity_duration.png"), plot = sensitivity_duration, width = 10, height = 6, dpi = 300)
ggsave(filename = here("output", "figures", "supplements", format(Sys.Date(), "%d%m%Y"), "disruption_scenarios.png"), plot = disruption_processed, width = 10, height = 6, dpi = 300)

# plots: sensitivity analysis for detection rate
sensitivity_detection <- posterior17 %>% 
  as.data.frame() %>% 
  pivot_longer(1:6, names_to = "parameters") %>% 
  mutate(detection = "0.07") %>% 
  bind_rows(posterior42 %>% 
              as.data.frame() %>% 
              pivot_longer(1:6, names_to = "parameters") %>% 
              mutate(detection = "0.05")) %>% 
  bind_rows(posterior43 %>% 
              as.data.frame() %>% 
              pivot_longer(1:6, names_to = "parameters") %>% 
              mutate(detection = "0.09")) %>% 
  mutate(parameters = factor(parameters, levels = c("inf_imm1", "inf_imm2", "waning1", "waning2", "aging1", "aging2"))) %>% 
  ggplot() +
  geom_boxplot(aes(x = parameters, y = value, fill = detection), outliers = FALSE, size = 0.3) +
  scale_x_discrete(labels = c(aging1 = expression(alpha[1]),
                              aging2 = expression(alpha[2]),
                              inf_imm1 = expression(theta[1]),
                              inf_imm2 = expression(theta[2]),
                              waning1 = expression(omega[1]),
                              waning2 = expression(omega[2]))) +
  scale_fill_viridis_d() +
  labs(x = "Parameters Estimated",
       y = "Value",
       fill = "Detection Rate") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

data <- do.call(rbind, traj17) %>% 
  as.data.frame() %>% 
  hdi() %>% 
  t() %>% 
  bind_cols(scotland_rate[, c(1, 2, 3)]) %>% 
  cbind(mean = colMeans(do.call(rbind, traj17))) %>% 
  mutate(detection = 0.07) %>% 
  rbind(do.call(rbind, traj42) %>% 
          as.data.frame() %>% 
          hdi() %>% 
          t() %>% 
          bind_cols(scotland_rate[, c(1, 2, 3)]) %>% 
          cbind(mean = colMeans(do.call(rbind, traj42))) %>% 
          mutate(detection = 0.05)) %>% 
  rbind(do.call(rbind, traj43) %>% 
          as.data.frame() %>% 
          hdi() %>% 
          t() %>% 
          bind_cols(scotland_rate[, c(1, 2, 3)]) %>% 
          cbind(mean = colMeans(do.call(rbind, traj43))) %>% 
          mutate(detection = 0.09)) %>% 
  mutate(detection = factor(detection, levels = c(0.05, 0.07, 0.09)))

fig <- ggplot(data) +
  geom_line(aes(x = yearmon, y = mean, color = detection), linetype = 1) +
  geom_ribbon(aes(x = yearmon, ymax = upper, ymin = lower, fill = detection), alpha = 0.3, linetype = 0) +
  geom_point(aes(x = yearmon, y = count, shape = "Data"), size = 2, colour = "black") +
  scale_colour_viridis_d(name = "Detection Rate") +  
  scale_fill_viridis_d(name = "Detection Rate") +  
  scale_shape_manual(name = "", values = c("Data" = 16)) +
  labs(x = "Time (months)", y = "Number of RSV cases") + 
  theme_classic() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        strip.text = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        legend.position = "bottom") + 
  facet_wrap(~age)

ggsave(filename = here("output", "figures", "supplements", format(Sys.Date(), "%d%m%Y"), "traj_detection.png"), plot = fig, width = 10, height = 7, dpi = 300)

maternal <- as.data.frame(maternal17) %>% 
  mutate(detection = 0.07) %>% 
  rbind(as.data.frame(maternal42) %>% 
          mutate(detection = 0.05)) %>% 
  rbind(as.data.frame(maternal43) %>% 
          mutate(detection = 0.09)) %>% 
  as.data.frame() %>% 
  mutate(detection = factor(detection, levels = c(0.05, 0.07, 0.09)))

m <- ggplot(maternal) +
  geom_line(aes(x = x_vals, y = mean, colour = detection), size = 1.5) +
  geom_ribbon(aes(x = x_vals, ymax = upper, ymin = lower, colour = detection, fill = detection), alpha = 0.4, linetype = 0) +
  labs(x = "Months since maternal infection", y = "Proportion of immunity at birth") +
  scale_colour_viridis_d(name = "Detection Rate") +  
  scale_fill_viridis_d(name = "Detection Rate") + 
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text = element_text(size = 12),
        legend.position = "none") +
  xlim(0, 25)

waning <- as.data.frame(waning17) %>% 
  mutate(detection = 0.07) %>% 
  rbind(as.data.frame(waning42) %>% 
          mutate(detection = 0.05)) %>% 
  rbind(as.data.frame(waning43) %>% 
          mutate(detection = 0.09)) %>% 
  as.data.frame() %>% 
  mutate(detection = factor(detection, levels = c(0.05, 0.07, 0.09)))

w <- ggplot(waning) +
  geom_line(aes(x = x_vals, y = mean, colour = detection), size = 1.5) +
  geom_ribbon(aes(x = x_vals, ymax = upper, ymin = lower, colour = detection, fill = detection), alpha = 0.4, linetype = 0) +
  labs(x = "Months since waning infection", y = "Proportion of immunity at birth") +
  scale_colour_viridis_d(name = "Detection Rate") +  
  scale_fill_viridis_d(name = "Detection Rate") + 
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text = element_text(size = 12),
        legend.position = "bottom") +
  xlim(0, 48)

aging <- as.data.frame(aging17) %>% 
  mutate(detection = 0.07) %>% 
  rbind(as.data.frame(aging42) %>% 
          mutate(detection = 0.05)) %>% 
  rbind(as.data.frame(aging43) %>% 
          mutate(detection = 0.09)) %>% 
  as.data.frame() %>% 
  mutate(detection = factor(detection, levels = c(0.05, 0.07, 0.09)))

a <- ggplot(aging) +
  geom_line(aes(x = x_vals, y = mean, colour = detection), size = 1.5) +
  geom_ribbon(aes(x = x_vals, ymax = upper, ymin = lower, colour = detection, fill = detection), alpha = 0.4, linetype = 0) +
  labs(x = "Months since aging infection", y = "Proportion of immunity at birth") +
  scale_colour_viridis_d(name = "Detection Rate") +  
  scale_fill_viridis_d(name = "Detection Rate") + 
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text = element_text(size = 12),
        legend.position = "none") +
  xlim(0, 48)

fig <- (m + w + a) + plot_annotation(tag_levels = "A") + theme(plot.tag = element_text(size = 14))
ggsave(filename = here("output", "figures", "supplements", format(Sys.Date(), "%d%m%Y"), "shapes_detection.png"), plot = fig, width = 10, height = 6, dpi = 300)