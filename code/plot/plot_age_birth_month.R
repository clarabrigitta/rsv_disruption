plot_age_birth_month <- function(traj_birth_month, birth_data){
  
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
    filter(time_calendar %in% c(82:178)) %>% 
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
                        labels = c("2017_18", "2018_19", "2019_20", "2020_21", "2021_22", "2022_23", "2023_24"), 
                        right = FALSE)) %>% 
    filter(!is.na(season)) %>% 
    left_join(birth_age, by = join_by(time_calendar, birth_month, time_birth)) %>% 
    # filter(time_birth <= 12) %>% 
    group_by(time_birth, birth_month, season) %>% 
    summarise(disease = sum(disease_mean),
              births = sum(births)) %>% 
    ungroup() %>% 
    mutate(risk = disease/births,
           birth_month = factor(birth_month, levels = 1:12, labels = month.abb)) %>%
    group_by(season, time_birth) %>%
    mutate(july_risk = risk[birth_month == "Jul"],
           risk_ratio = risk / july_risk) %>%
    ungroup()
  
  fig <- ggplot() +
    geom_bar(data = data %>% filter(season %in% c("2017_18", "2021_22", "2022_23", "2023_24")), aes(x = birth_month, y = risk_ratio, fill = season), stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("2017_18" = "grey", "2021_22" = "#FABA39FF", "2022_23" = "#E4460AFF", "2023_24" = "#7A0403FF")) + 
    theme_classic() +
    labs(x = "Birth Month", y = "Risk Ratio", fill = "Season")
  
  # ggplot() +
  #   geom_tile(data = data %>% filter(season %in% c("2017_18", "2021_22", "2022_23", "2023_24")), aes(x = birth_month, y = time_birth, fill = risk_ratio)) +
  #   scale_fill_viridis_c(option = "magma", direction = -1) +
  #   labs(x = "Birth Month",
  #        y = "Age (months)",
  #        fill = "Risk Ratio") +
  #   theme_classic() +
  #   facet_wrap(~season, labeller = labeller(season = c("2017_18" = "2017-18", "2021_22" = "2021-22", "2022_23" = "2022-23"))) +
  #   theme(strip.text = element_text(face = "bold", size = 12))
  
  # fig <- ggplot() +
  #   geom_line(data = data %>% filter(season %in% c("2017_18", "2021_22", "2022_23")) %>% filter(birth_month %in% c("Jan", "May", "Sep")), aes(x = time_birth, y = attack_rate, group = birth_month, colour = birth_month)) +
  #   scale_colour_viridis_d(option = "plasma") +
  #   theme_classic() +
  #   labs(x = "Age (Months)", y = "Annual RSV Disease Rate (per 100,000)", colour = "Birth Month") +
  #   facet_wrap(~season, labeller = labeller(season = c("2017_18" = "2017-18", "2021_22" = "2021-22", "2022_23" = "2022-23"))) +
  #   theme(strip.text = element_text(face = "bold", size = 12))
  
  return(fig)
}