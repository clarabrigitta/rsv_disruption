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

births <- birth_data %>% 
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
        axis.text.x = element_text(angle = 45, hjust = 1))

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

dir.create(here("output", "figures", "supplements", format(Sys.Date(), "%d%m%Y")))
ggsave(filename = here("output", "figures", "supplements", format(Sys.Date(), "%d%m%Y"), "rate.png"), plot = rate, width = 10, height = 7, dpi = 300)
ggsave(filename = here("output", "figures", "supplements", format(Sys.Date(), "%d%m%Y"), "births.png"), plot = births, width = 10, height = 6, dpi = 300)
ggsave(filename = here("output", "figures", "supplements", format(Sys.Date(), "%d%m%Y"), "sensitivity_duration.png"), plot = sensitivity_duration, width = 10, height = 6, dpi = 300)