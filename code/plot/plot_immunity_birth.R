plot_immunity_birth <- function(traj_babies){
  
  data <- traj_women %>% 
    as.data.frame() %>% 
    left_join(dates %>% select(-c(rate, level)) %>% distinct(), by = join_by(time)) %>% 
    mutate(season = cut(as.numeric(yearmon), 
                        breaks = as.yearmon(c("Jul 2017", "Jul 2018", "Jul 2019", "Jul 2020", "Jul 2021", "Jul 2022", "Jul 2023", "Jul 2024")), 
                        labels = c("2017_18", "2018_19", "2019_20", "2020_21", "2021_22", "2022_23", "2023_24"), 
                        right = FALSE)) %>% 
    filter(!is.na(season)) %>% 
    select(-susceptible_naive) %>% 
    pivot_longer(cols = `susceptible_reinf`:`I24`, names_to = "last_exp") %>% 
    mutate(last_exp = recode(last_exp, "susceptible_reinf" = ">24", "I24" = "24", "I23" = "23", "I22" = "22", "I21" = "21", "I20" = "20", "I19" = "19", "I18" = "18",
                             "I17" = "17", "I16" = "16", "I15" = "15", "I14" = "14", "I13" = "13", "I12" = "12", "I11" = "11", "I10" = "10", "I9" = "9", "I8" = "8",
                             "I7" = "7", "I6" = "6", "I5" = "5", "I4" = "4", "I3" = "3", "I2" = "2", "I1" = "1")) %>% 
    mutate(level = case_when(last_exp %in% c(1:3) ~ "1-3 months",
                             last_exp %in% c(4:6) ~ "4-6 months",
                             last_exp %in% c(7:24, ">24") ~ ">6 months")) %>% 
    group_by(season, level) %>% 
    summarise(value = sum(value)) %>% 
    mutate(level = factor(level, levels = c("1-3 months", "4-6 months", ">6 months")))
  
  fig <- ggplot() +
    geom_bar(data = data %>% filter(season %in% c("2017_18", "2020_21", "2021_22", "2022_23", "2023_24")), aes(x = season, y = value, fill = level), stat = "identity", position = "fill") +
    scale_fill_manual(values = c("#0B0405FF", "#357BA2FF", "#78D6AEFF")) + 
    scale_x_discrete(labels = c("2017_18" = "2017-18", "2020_21" = "2020-21", "2021_22" = "2021-22", "2022_23" = "2022-23", "2023_24" = "2023-24")) +
    theme_classic() +
    labs(x = "Season", y = "Proportion of mothers", fill = "Months since last\ninfection in mothers")
  
  return(fig)
}