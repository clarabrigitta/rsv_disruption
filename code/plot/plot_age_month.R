plot_age_month <- function(traj_birth_month, birth_data) {
  
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
                        labels = c("2017_18", "2018_19", "2019_20", "2020_21", "2021_22", "2022_23", "2023_24"), 
                        right = FALSE)) %>% 
    filter(!is.na(season)) %>% 
    left_join(birth_age, by = join_by(time_calendar, birth_month, time_birth)) %>% 
    group_by(time_birth, season) %>% 
    summarise(disease = sum(disease_mean),
              births = sum(births)) %>% 
    ungroup() %>% 
    mutate(attack_rate = (disease/births)*100000) %>% 
    select(-c(disease, births)) %>% 
    pivot_wider(names_from = season, values_from = attack_rate) %>% 
    select(-c("2018_19", "2019_20", "2020_21")) %>% 
    mutate(across(`2021_22`:`2023_24`, 
                  .fns = ~ ((.x - `2017_18`) / `2017_18`) * 100,
                  .names = "{.col}")) %>% 
    select(-`2017_18`) %>% 
    pivot_longer(`2021_22`:`2023_24`, names_to = "season", values_to = "rel_change")

  fig <- ggplot() +
    geom_tile(data = data, aes(x = season, y = time_birth, fill = rel_change)) +
    scale_y_continuous(breaks = seq(0, 48, by = 6)) +
    scale_x_discrete(labels = c("2021_22" = "2021-22", "2022_23" = "2022-23", "2023_24" = "2023-24")) +
    scale_fill_viridis_c(option = "magma", direction = -1) +
    labs(x = "Season",
         y = "Age (months)",
         fill = "Relative Change in\nAnnual Disease Rate\nCompared to the 2017-18\nSeason (%)") +
    theme_classic()
  
  dir.create(here("output", "figures", "age", format(Sys.Date(), "%d%m%Y")))
  ggsave(filename = here("output", "figures", "age", format(Sys.Date(), "%d%m%Y"), paste0(n, "month", ".png")), plot = fig, width = 7, height = 4, dpi = 300)
  
  return(fig)
}