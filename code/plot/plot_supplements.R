fig <- ((dates %>% 
           select(-level) %>% 
           distinct() %>% 
           filter(yearmon >= "Jul 2010", yearmon <= "Jul 2011") %>% 
           mutate(rate = rate + 0.0075) %>% 
           ggplot() +
           geom_line(aes(x = time, y = rate)) +
           scale_x_continuous(breaks = seq(7, 19, 1), labels = c(month.abb[7:12], month.abb[1:7])) +
           theme_bw() +
           labs(y = "Rate of RSV Exposure (\u03bb)",
                x = "Month of Year") +
           theme(axis.text=element_text(size=12),
                 axis.title=element_text(size=14))) / (dates %>% 
           select(-level) %>% 
           distinct() %>% 
           filter(yearmon >= "Jul 2019", yearmon <= "Jul 2021") %>% 
             mutate(rate = rate + 0.0075) %>% 
             mutate(rate = ifelse(yearmon >= "Mar 2020" & yearmon <= "Mar 2021", rate * exp(-4), rate)) %>% 
           ggplot() +
           geom_line(aes(x = yearmon, y = rate)) +
           theme_bw() +
           labs(y = "Rate of RSV Exposure (\u03bb)",
                x = "Month") +
           theme(axis.text=element_text(size=12),
                 axis.title=element_text(size=14))) + plot_annotation(tag_levels = "A"))

birth_data %>% 
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


