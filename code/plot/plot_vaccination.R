library(ggplot2)
library(cowplot)

# -------------------------------------------------------------------------
# plot vaccination scenario (split)

count_split <- ggplot() +
  geom_line(data = split,
            aes(x = time_birth, y = mean, colour = season, linetype = season)) +
  geom_ribbon(data = split, aes(x = time_birth, ymax = upper, ymin = lower, fill = season), alpha = 0.2, linetype = 0) +
  scale_x_continuous(breaks = seq(0, 48, 6)) +
  scale_linetype_manual(values = c("2017-18" = "solid", "2018-19" = "solid", "2019-20" = "solid", "2020-21"= "dashed", "2021-22" = "solid", "2022-23" = "solid", "2023-24" = "solid", "2024-25" = "solid", "2025-26" = "solid", "2026-27" = "solid", "2027-28" = "solid")) + 
  scale_colour_manual(values = c("2017-18" = "grey", "2018-19" = "grey", "2019-20" = "grey", "2020-21"= "grey", "2021-22" = "#FABA39FF", "2022-23" = "#E4460AFF", "2023-24" = "#7A0403FF", "2024-25" = "#18DEC1FF", "2025-26" = "#3AA2FCFF", "2026-27" = "#455ED2FF", "2027-28" = "#30123BFF")) + 
  scale_fill_manual(values = c("2017-18" = "grey", "2018-19" = "grey", "2019-20" = "grey", "2020-21"= "grey", "2021-22" = "#FABA39FF", "2022-23" = "#E4460AFF", "2023-24" = "#7A0403FF", "2024-25" = "#18DEC1FF", "2025-26" = "#3AA2FCFF", "2026-27" = "#455ED2FF", "2027-28" = "#30123BFF")) + 
  theme_bw() +
  labs(x = "Age (months)", y = "Number of RSV Disease Cases", colour = "Season", linetype = "Season", fill = "Season") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        strip.text = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  facet_wrap(~level, labeller = as_labeller(c("unvaccinated" = "Babies born to unvaccinated mothers", "vaccinated" = "Babies born to vaccinated mothers")))

# -------------------------------------------------------------------------
# plot vaccination scenario (total)

count_total <- total %>% 
  ggplot() +
  geom_line(aes(x = time_birth, y = mean, colour = season, linetype = season)) +
  geom_ribbon(aes(x = time_birth, ymax = upper, ymin = lower, fill = season), alpha = 0.2, linetype = 0) +
  scale_x_continuous(breaks = seq(0, 48, 6)) +
  scale_linetype_manual(values = c("2017-18" = "solid", "2018-19" = "solid", "2019-20" = "solid", "2020-21"= "dashed", "2021-22" = "solid", "2022-23" = "solid", "2023-24" = "solid", "2024-25" = "solid", "2025-26" = "solid", "2026-27" = "solid", "2027-28" = "solid")) + 
  scale_colour_manual(values = c("2017-18" = "grey", "2018-19" = "grey", "2019-20" = "grey", "2020-21"= "grey", "2021-22" = "#FABA39FF", "2022-23" = "#E4460AFF", "2023-24" = "#7A0403FF", "2024-25" = "#18DEC1FF", "2025-26" = "#3AA2FCFF", "2026-27" = "#455ED2FF", "2027-28" = "#30123BFF")) + 
  scale_fill_manual(values = c("2017-18" = "grey", "2018-19" = "grey", "2019-20" = "grey", "2020-21"= "grey", "2021-22" = "#FABA39FF", "2022-23" = "#E4460AFF", "2023-24" = "#7A0403FF", "2024-25" = "#18DEC1FF", "2025-26" = "#3AA2FCFF", "2026-27" = "#455ED2FF", "2027-28" = "#30123BFF")) + 
  theme_bw() +
  labs(x = "Age (months)", y = "Number of RSV Disease Cases", colour = "Season", linetype = "Season", fill = "Season") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        strip.text = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

# -------------------------------------------------------------------------
# plot cases averted total and split by age group

count_averted <- cases_averted %>%
  filter(yearmon >= "Jul 2024") %>% 
  ggplot() +
  geom_line(aes(x = yearmon, y = mean)) +
  geom_ribbon(aes(x = yearmon, ymax = upper, ymin = lower), alpha = 0.2, linetype = 0) +  
  geom_vline(xintercept = as.numeric(as.yearmon(c("Sep 2024"))), linetype = "dashed", color = "red", linewidth = 0.5) +
  scale_x_yearmon(n = 12, format = "%b %Y") +
  theme_bw() +
  labs(x = "Time (months)", y = "Number of RSV Disease\nCases Averted") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        strip.text = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1))

# -------------------------------------------------------------------------
# final plot

fig <- (count_total  | count_averted) / count_split +  plot_annotation(tag_levels = "A") + theme(plot.tag = element_text(size = 14))
ggsave(filename = here("output", "figures", "vaccination.png"), plot = fig, width = 13, height = 9, dpi = 300)
