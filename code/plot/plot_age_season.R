plot_age_season <- function(traj_birth_month, birth_data){

  birth_data <- birth_data %>% 
    filter(year >= 2010) %>% 
    mutate(time = 1:178) %>% 
    filter(time %in% c((82-48):178))
  
  birth_age <- map(1:nrow(birth_data),
                   function(x){
                     data <- birth_data[x, ] %>% 
                       uncount(weights = 48) %>% 
                       mutate(time_calendar = as.numeric(birth_data[x, "time"]):(as.numeric(birth_data[x, "time"])+47),
                              time_birth = 1:48)
                     return(data)
                   }) %>% 
    do.call(rbind, .) %>% 
    left_join(dates[, c("time_calendar", "season")]) %>% 
    filter(!is.na(season)) %>% 
    group_by(season, time_birth) %>% 
    summarise(births = sum(births)) 

  data <- lapply(traj_birth_month, function(x) {
    as.data.frame(x) %>%
      select(-c("waning", "aging", "infected", "disease", "births", "rate")) %>% 
      pivot_longer(cols = c(`susceptible_reinf`:`I24`), names_to = "last_exp") %>% 
      left_join(dates[, c("time_calendar", "season")]) %>% 
      filter(!is.na(season)) %>% 
      group_by(season, time_birth) %>% 
      summarise(disease = sum(value))
  })
  
  data <- rbindlist(data)
  
  data <- data %>% 
    group_by(season, time_birth) %>% 
    summarise(mean = mean(disease),
              lower = hdi(disease)[[1]],
              upper = hdi(disease)[[2]]) %>% 
    left_join(birth_age) %>% 
    mutate(attack_rate = (mean/births)*100000,
           lower_rate = (lower/births)*100000,
           upper_rate = (upper/births)*100000) %>% 
    filter(!season == "2024-25")
  
  fig_2122 <- ggplot() +
    geom_ribbon(data = data %>% mutate(season = factor(season, levels = c("2017-18", "2018-19", "2019-20", "2020-21", "2022-23", "2023-24", "2021-22"))), aes(x = time_birth, ymax = upper_rate, ymin = lower_rate, fill = season), alpha = 0.4, linetype = 0) +
    geom_line(data = data %>% mutate(season = factor(season, levels = c("2017-18", "2018-19", "2019-20", "2020-21", "2022-23", "2023-24", "2021-22"))), aes(x = time_birth, y = attack_rate, group = season, colour = season, linetype = season), size = 1) +
    scale_x_continuous(breaks = seq(0, 48, by = 6)) +
    scale_colour_manual(values = c("2017-18" = "grey", "2018-19" = "grey", "2019-20" = "grey", "2020-21"= "grey", "2021-22" = "#FABA39FF", "2022-23" = "grey", "2023-24" = "grey")) + 
    scale_fill_manual(values = c("2017-18" = "grey", "2018-19" = "grey", "2019-20" = "grey", "2020-21"= "grey", "2021-22" = "#FABA39FF", "2022-23" = "grey", "2023-24" = "grey")) + 
    scale_linetype_manual(values = c("2017-18" = "solid", "2018-19" = "solid", "2019-20" = "solid", "2020-21"= "dashed", "2021-22" = "solid", "2022-23" = "solid", "2023-24" = "solid")) + 
    theme_classic() +
    labs(x = "Age (Months)", y = "Annual RSV Disease Rate (per 100,000)", colour = "Season") +
    theme(legend.position = "none",
          axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          strip.text = element_text(size = 14, face = "bold"),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 12))
  
  fig_2223 <- ggplot() +
    geom_ribbon(data = data %>% mutate(season = factor(season, levels = c("2017-18", "2018-19", "2019-20", "2020-21", "2022-23", "2023-24", "2021-22"))), aes(x = time_birth, ymax = upper_rate, ymin = lower_rate, fill = season), alpha = 0.4, linetype = 0) +
    geom_line(data = data %>% mutate(season = factor(season, levels = c("2017-18", "2018-19", "2019-20", "2020-21", "2021-22", "2023-24", "2022-23"))), aes(x = time_birth, y = attack_rate, group = season, colour = season, linetype = season), size = 1) +
    scale_x_continuous(breaks = seq(0, 48, by = 6)) +
    scale_colour_manual(values = c("2017-18" = "grey", "2018-19" = "grey", "2019-20" = "grey", "2020-21"= "grey", "2021-22" = "grey", "2022-23" = "#E4460AFF", "2023-24" = "grey")) + 
    scale_fill_manual(values = c("2017-18" = "grey", "2018-19" = "grey", "2019-20" = "grey", "2020-21"= "grey", "2021-22" = "grey", "2022-23" = "#E4460AFF", "2023-24" = "grey")) + 
    scale_linetype_manual(values = c("2017-18" = "solid", "2018-19" = "solid", "2019-20" = "solid", "2020-21"= "dashed", "2021-22" = "solid", "2022-23" = "solid", "2023-24" = "solid")) + 
    theme_classic() +
    labs(x = "Age (Months)", y = "Annual RSV Disease Rate (per 100,000)", colour = "Season") +
    theme(legend.position = "none",
          axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          strip.text = element_text(size = 14, face = "bold"),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 12))
  
  fig_2324 <- ggplot() +
    geom_ribbon(data = data %>% mutate(season = factor(season, levels = c("2017-18", "2018-19", "2019-20", "2020-21", "2022-23", "2023-24", "2021-22"))), aes(x = time_birth, ymax = upper_rate, ymin = lower_rate, fill = season), alpha = 0.4, linetype = 0) +
    geom_line(data = data, aes(x = time_birth, y = attack_rate, group = season, colour = season, linetype = season), size = 1) +
    scale_x_continuous(breaks = seq(0, 48, by = 6)) +
    scale_colour_manual(values = c("2017-18" = "grey", "2018-19" = "grey", "2019-20" = "grey", "2020-21"= "grey", "2021-22" = "grey", "2022-23" = "grey", "2023-24" = "#7A0403FF")) + 
    scale_fill_manual(values = c("2017-18" = "grey", "2018-19" = "grey", "2019-20" = "grey", "2020-21"= "grey", "2021-22" = "grey", "2022-23" = "grey", "2023-24" = "#7A0403FF")) + 
    scale_linetype_manual(values = c("2017-18" = "solid", "2018-19" = "solid", "2019-20" = "solid", "2020-21"= "dashed", "2021-22" = "solid", "2022-23" = "solid", "2023-24" = "solid")) + 
    theme_classic() +
    labs(x = "Age (Months)", y = "Annual RSV Disease Rate (per 100,000)", colour = "Season") +
    theme(legend.position = "none",
          axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          strip.text = element_text(size = 14, face = "bold"),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 12))
  
  p_legend <- ggplot() +
    geom_line(data = data, aes(x = time_birth, y = attack_rate, group = season, colour = season, linetype = season), size = 1) +
    geom_ribbon(data = data, aes(x = time_birth, ymax = upper_rate, ymin = lower_rate, fill = season), alpha = 0.4, linetype = 0) +
    scale_colour_manual(values = c("2017-18" = "grey", "2018-19" = "grey", "2019-20" = "grey", "2020-21"= "grey", "2021-22" = "#FABA39FF", "2022-23" = "#E4460AFF", "2023-24" = "#7A0403FF")) + 
    scale_fill_manual(values = c("2017-18" = "grey", "2018-19" = "grey", "2019-20" = "grey", "2020-21"= "grey", "2021-22" = "#FABA39FF", "2022-23" = "#E4460AFF", "2023-24" = "#7A0403FF")) + 
    scale_linetype_manual(values = c("2017-18" = "solid", "2018-19" = "solid", "2019-20" = "solid", "2020-21"= "dashed", "2021-22" = "solid", "2022-23" = "solid", "2023-24" = "solid")) + 
    theme_classic() +
    labs(x = "Age (Months)", y = "Annual RSV Disease Rate (per 100,000)", colour = "Season", fill = "Season", linetype = "Season") +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          strip.text = element_text(size = 14, face = "bold"),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 12)) 
  
  legend_extract <- suppressWarnings(cowplot::get_legend(p_legend))

  fig <- (fig_2122 | fig_2223 | fig_2324 | legend_extract) + plot_layout(guides = "collect", widths = c(1, 1, 1, 0.2), axis_titles = "collect") + plot_annotation(tag_levels = "A") + theme(plot.tag = element_text(size = 14))
  
  dir.create(here("output", "figures", "age", format(Sys.Date(), "%d%m%Y")))
  ggsave(filename = here("output", "figures", "age", format(Sys.Date(), "%d%m%Y"), paste0(n, "season", ".png")), plot = fig, width = 12, height = 5, dpi = 300)
  
  return(fig)
}