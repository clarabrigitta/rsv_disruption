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
    group_by(time_calendar, time_birth, birth_month) %>% 
    summarise(births = sum(births)) %>% 
    mutate(birth_month = as.numeric(birth_month))
  
  data <- do.call(rbind, traj_birth_month) %>% 
    as.data.frame() %>% 
    group_by(time_calendar, birth_month, time_birth) %>% 
    mutate(disease_mean = mean(disease)) %>% 
    select(-c(waning, aging, infected, disease, births)) %>% 
    distinct() %>% 
    ungroup() %>% 
    left_join(dates %>% 
                select(-level) %>% 
                unique() %>% 
                rename(time_calendar = time),
              by = join_by(time_calendar)) %>% 
    mutate(season = cut(as.numeric(yearmon), 
                        breaks = as.yearmon(c("Jul 2017", "Jul 2018", "Jul 2019", "Jul 2020", "Jul 2021", "Jul 2022", "Jul 2023", "Jul 2024")), 
                        labels = c("2017-18", "2018-19", "2019-20", "2020-21", "2021-22", "2022-23", "2023-24"), 
                        right = FALSE)) %>% 
    filter(!is.na(season)) %>% 
    left_join(birth_age, by = join_by(time_calendar, birth_month, time_birth)) %>% 
    group_by(time_birth, season) %>% 
    summarise(disease = sum(disease_mean),
              births = sum(births)) %>% 
    ungroup() %>% 
    mutate(attack_rate = (disease/births)*100000)
  
  fig_2122 <- ggplot() +
    geom_line(data = data %>% mutate(season = factor(season, levels = c("2017-18", "2018-19", "2019-20", "2020-21", "2022-23", "2023-24", "2021-22"))), aes(x = time_birth, y = attack_rate, group = season, colour = season, linetype = season), size = 1) +
    scale_x_continuous(breaks = seq(0, 48, by = 6)) +
    scale_colour_manual(values = c("2017-18" = "grey", "2018-19" = "grey", "2019-20" = "grey", "2020-21"= "grey", "2021-22" = "#FABA39FF", "2022-23" = "grey", "2023-24" = "grey")) + 
    scale_linetype_manual(values = c("2017-18" = "solid", "2018-19" = "solid", "2019-20" = "solid", "2020-21"= "dashed", "2021-22" = "solid", "2022-23" = "solid", "2023-24" = "solid")) + 
    theme_classic() +
    labs(x = "Age (Months)", y = "Annual RSV Disease Rate (per 100,000)", colour = "Season") +
    theme(legend.position = "none")
  
  fig_2223 <- ggplot() +
    geom_line(data = data %>% mutate(season = factor(season, levels = c("2017-18", "2018-19", "2019-20", "2020-21", "2021-22", "2023-24", "2022-23"))), aes(x = time_birth, y = attack_rate, group = season, colour = season, linetype = season), size = 1) +
    scale_x_continuous(breaks = seq(0, 48, by = 6)) +
    scale_colour_manual(values = c("2017-18" = "grey", "2018-19" = "grey", "2019-20" = "grey", "2020-21"= "grey", "2021-22" = "grey", "2022-23" = "#E4460AFF", "2023-24" = "grey")) + 
    scale_linetype_manual(values = c("2017-18" = "solid", "2018-19" = "solid", "2019-20" = "solid", "2020-21"= "dashed", "2021-22" = "solid", "2022-23" = "solid", "2023-24" = "solid")) + 
    theme_classic() +
    labs(x = "Age (Months)", y = "Annual RSV Disease Rate (per 100,000)", colour = "Season") +
    theme(legend.position = "none")
  
  fig_2324 <- ggplot() +
    geom_line(data = data, aes(x = time_birth, y = attack_rate, group = season, colour = season, linetype = season), size = 1) +
    scale_x_continuous(breaks = seq(0, 48, by = 6)) +
    scale_colour_manual(values = c("2017-18" = "grey", "2018-19" = "grey", "2019-20" = "grey", "2020-21"= "grey", "2021-22" = "grey", "2022-23" = "grey", "2023-24" = "#7A0403FF")) + 
    scale_linetype_manual(values = c("2017-18" = "solid", "2018-19" = "solid", "2019-20" = "solid", "2020-21"= "dashed", "2021-22" = "solid", "2022-23" = "solid", "2023-24" = "solid")) + 
    theme_classic() +
    labs(x = "Age (Months)", y = "Annual RSV Disease Rate (per 100,000)", colour = "Season") +
    theme(legend.position = "none")
  
  p_legend <- ggplot() +
    geom_line(data = data, aes(x = time_birth, y = attack_rate, group = season, colour = season, linetype = season), size = 1) +
    scale_colour_manual(values = c("2017-18" = "grey", "2018-19" = "grey", "2019-20" = "grey", "2020-21"= "grey", "2021-22" = "#FABA39FF", "2022-23" = "#E4460AFF", "2023-24" = "#7A0403FF")) + 
    scale_linetype_manual(values = c("2017-18" = "solid", "2018-19" = "solid", "2019-20" = "solid", "2020-21"= "dashed", "2021-22" = "solid", "2022-23" = "solid", "2023-24" = "solid")) + 
    theme_classic() +
    labs(x = "Age (Months)", y = "Annual RSV Disease Rate (per 100,000)", colour = "Season", linetype = "Season") 
  
  legend <- get_legend(p_legend)

  fig <- (fig_2122 | fig_2223 | fig_2324 | legend) + plot_layout(guides = "collect", widths = c(1, 1, 1, 0.2), axis_titles = "collect") + plot_annotation(tag_levels = "A") 
  
  dir.create(here("output", "figures", "age", format(Sys.Date(), "%d%m%Y")))
  ggsave(filename = here("output", "figures", "age", format(Sys.Date(), "%d%m%Y"), paste0(n, "season", ".png")), plot = fig, width = 10, height = 4, dpi = 300)
  
  return(fig)
}