plot_immunity_rate <- function(traj_babies, traj_birth_month){
  
  birth_data <- traj_babies %>% 
    as.data.frame() %>% 
    filter(time %in% c((82-48):178))
  
  birth_immunity <- map(1:nrow(birth_data),
                        function(x){
                          data <- birth_data[x, ] %>% 
                            uncount(weights = 48) %>% 
                            mutate(time_calendar = as.numeric(birth_data[x, "time"]):(as.numeric(birth_data[x, "time"])+47),
                                   time_birth = 1:48)
                          return(data)
                        }) %>% 
    do.call(rbind, .) %>% 
    group_by(time_calendar, time_birth, birth_month) %>% 
    summarise(across(c(`susceptible_reinf`:`I24`, `births`), sum, .names = "{.col}")) %>% 
    mutate(birth_month = as.numeric(birth_month)) %>% 
    select(-births) %>% 
    pivot_longer(cols = c(`susceptible_reinf`:`I24`), names_to = "last_exp") %>% 
    mutate(last_exp = recode(last_exp, "susceptible_reinf" = ">24", "I24" = "24", "I23" = "23", "I22" = "22", "I21" = "21", "I20" = "20", "I19" = "19", "I18" = "18",
                             "I17" = "17", "I16" = "16", "I15" = "15", "I14" = "14", "I13" = "13", "I12" = "12", "I11" = "11", "I10" = "10", "I9" = "9", "I8" = "8",
                             "I7" = "7", "I6" = "6", "I5" = "5", "I4" = "4", "I3" = "3", "I2" = "2", "I1" = "1")) %>% 
    mutate(last_exp = factor(last_exp, levels = c(1:24, ">24"))) %>%
    mutate(level = case_when(last_exp %in% c(1:4) ~ "high",
                             last_exp %in% c(5:7) ~ "low",
                             last_exp %in% c(7:24, ">24") ~ "none")) %>% 
    group_by(time_calendar, level) %>% 
    summarise(births = sum(value))
  
  data <- do.call(rbind, traj_birth_month) %>% 
    as.data.frame() %>% 
    select(-c(waning, aging, infected)) %>% 
    group_by(time_calendar, birth_month, time_birth) %>% 
    summarise(across(c(`susceptible_reinf`:`I24`, `disease`), mean, .names = "{.col}")) %>% 
    ungroup() %>% 
    left_join(dates %>% select(-c(rate, level)) %>% distinct() %>% rename(time_calendar = time), by = join_by(time_calendar)) %>% 
    mutate(season = cut(as.numeric(yearmon), 
                        breaks = as.yearmon(c("Jul 2017", "Jul 2018", "Jul 2019", "Jul 2020", "Jul 2021", "Jul 2022", "Jul 2023", "Jul 2024")), 
                        labels = c("2017_18", "2018_19", "2019_20", "2020_21", "2021_22", "2022_23", "2023_24"), 
                        right = FALSE)) %>% 
    filter(!is.na(season)) %>% 
    pivot_longer(cols = `susceptible_reinf`:`I24`, names_to = "last_exp") %>% 
    mutate(last_exp = recode(last_exp, "susceptible_reinf" = ">24", "I24" = "24", "I23" = "23", "I22" = "22", "I21" = "21", "I20" = "20", "I19" = "19", "I18" = "18",
                             "I17" = "17", "I16" = "16", "I15" = "15", "I14" = "14", "I13" = "13", "I12" = "12", "I11" = "11", "I10" = "10", "I9" = "9", "I8" = "8",
                             "I7" = "7", "I6" = "6", "I5" = "5", "I4" = "4", "I3" = "3", "I2" = "2", "I1" = "1")) %>% 
    mutate(last_exp = factor(last_exp, levels = c(1:24, ">24"))) %>% 
    mutate(level = case_when(last_exp %in% c(1:3) ~ "high",
                             last_exp %in% c(4:6) ~ "low",
                             last_exp %in% c(7:24, ">24") ~ "none")) %>% 
    group_by(time_calendar, yearmon, season, level) %>% 
    summarise(disease = sum(value)) %>% 
    left_join(birth_immunity) %>% 
    ungroup() %>% 
    group_by(season, level) %>% 
    summarise(disease = sum(disease),
              births = sum(births)) %>% 
    mutate(attack_rate = (disease/births)*100000)
  
  fig <- ggplot() +
    geom_bar(data = data %>% filter(season %in% c("2017_18", "2020_21", "2021_22", "2022_23", "2023_24")), aes(x = season, y = attack_rate, fill = level), stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("#0B0405FF", "#357BA2FF", "#78D6AEFF"), labels = c("High", "Low", "None")) + 
    scale_x_discrete(labels = c("2017_18" = "2017-18", "2020_21" = "2020-21", "2021_22" = "2021-22", "2022_23" = "2022-23", "2023_24" = "2023-24")) +
    theme_classic() +
    labs(x = "Season", y = "Annual RSV Disease Rate (per 100,000)", fill = "Immunity level")
  
  return(fig)
}