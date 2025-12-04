library(patchwork)

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
fixed <- matrix(combinations[[n]]$fixed[!combinations[[n]]$ind],
                nrow = nrow(posterior), 
                ncol = sum(!combinations[[n]]$ind),
                byrow = TRUE,
                dimnames = list(NULL, combinations[[n]]$name[!combinations[[n]]$ind]))
posterior <- cbind(posterior, fixed)

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

sensitivity_contacts <- posterior17 %>% 
  as.data.frame() %>% 
  pivot_longer(1:6, names_to = "parameters") %>% 
  mutate(contact = "12-month reduction") %>% 
  select(-c(detection, disruption)) %>% 
  # bind_rows(posterior52 %>% 
  #             as.data.frame() %>% 
  #             pivot_longer(1:6, names_to = "parameters") %>% 
  #             mutate(contact = "fixed reduction and gradual increase") %>% 
  #             select(-c(detection, disruption)) %>% 
  #             mutate(value = as.numeric(value))) %>% 
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

disruption_scenarios <- data.frame(time = 1:25,
           month = seq(as.Date("2020-03-01"), as.Date("2022-03-01"), by = "month"),
           ori = c(rep(exp(-4.3), 13), rep(1, 25 - 13)),
           alt = c(rep(0.01, 2), seq(0.01, 0.1, length.out = 3), rep(0.1, 1), seq(0.1, 0.01, length.out = 5), rep(0.01, 2), seq(0.01, 1, length.out = 4), rep(NA, 25-17)),
           alt_old = exp(c(rep(-5, 2), seq(-5, red_exp, length.out = 3), rep(red_exp, 2), seq(red_exp, -5, length.out = 4), rep(-5, 3), seq(-5, 0, length.out = 11))),
           scale_scot1 = exp(c(rep(-5, 2), seq(-5, red_exp, length.out = 3), rep(red_exp, 2), seq(red_exp, -5, length.out = 4), rep(-5, 3), seq(-5, 0, length.out = 11))),
           scale_scot2 = exp(c(rep(-5, 2), seq(-5, -2.302585, length.out = 3), rep(-2.302585, 2), seq(-2.302585, -5, length.out = 4), rep(-5, 3), seq(-5, 0, length.out = 11))),
           scale_scot3 = c(rep(0.001, 2), seq(0.001, red_lin, length.out = 3), rep(red_lin, 2), seq(red_lin, 0.001, length.out = 4), rep(0.001, 3), seq(0.001, 1, length.out = 11)),
           scale_scot4 = c(rep(0.001, 2), seq(0.001, 0.1, length.out = 3), rep(0.1, 2), seq(0.1, 0.001, length.out = 4), rep(0.001, 3), seq(0.001, 1, length.out = 11))) %>% 
  pivot_longer(cols = 3:8) %>% 
  # mutate(name = factor(name, levels = c("ori", "alt"))) %>% 
  ggplot() +
  geom_line(aes(x = month, y = value, colour = name)) +
  scale_x_date(date_breaks = "6 month") +
  # scale_colour_viridis_d(labels = c("ori" = "12-month reduction", "alt" = "fitted reduction and gradual increase")) +
  labs(x = "Month",
       y = "Value",
       colour = "Lockdown Lifting\nScenario") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

dir.create(here("output", "figures", "supplements", format(Sys.Date(), "%d%m%Y")))
ggsave(filename = here("output", "figures", "supplements", format(Sys.Date(), "%d%m%Y"), "rate.png"), plot = rate, width = 10, height = 7, dpi = 300)
ggsave(filename = here("output", "figures", "supplements", format(Sys.Date(), "%d%m%Y"), "births.png"), plot = births, width = 8, height = 6, dpi = 300)
ggsave(filename = here("output", "figures", "supplements", format(Sys.Date(), "%d%m%Y"), "sensitivity_contacts.png"), plot = sensitivity_contacts, width = 10, height = 6, dpi = 300)
ggsave(filename = here("output", "figures", "supplements", format(Sys.Date(), "%d%m%Y"), "burnin.png"), plot = burnin, width = 10, height = 6, dpi = 300)
ggsave(filename = here("output", "figures", "supplements", format(Sys.Date(), "%d%m%Y"), "disruption_scenarios.png"), plot = disruption_scenarios, width = 10, height = 6, dpi = 300)

