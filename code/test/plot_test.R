# age-specific epidemic (disease) curves by birth month
traj <- do.call(rbind, traj18) %>% 
  as.data.frame() %>% 
  group_by(time_calendar, birth_month, time_birth) %>% 
  mutate(disease_mean = mean(disease)) %>% 
  select(-c(waning, aging, infected, disease)) %>% 
  distinct() %>% 
  ungroup() %>% 
  mutate(age = ifelse(time_birth < 12, "<1 year", "1-4 years")) %>% 
  group_by(time_calendar, birth_month, age) %>% 
  summarise(disease = sum(disease_mean)) %>% 
  ungroup() %>% 
  left_join(dates %>% 
              select(-level) %>% 
              unique() %>% 
              rename(time_calendar = time)) %>% 
  mutate(birth_month = as.factor(birth_month))


fig <- ggplot(traj) +
  geom_line(aes(x = yearmon, y = disease, colour = birth_month), alpha = 0.7, size = 1) +
  theme_classic() +
  scale_colour_manual(values = colorRampPalette(brewer.pal(11, "RdBu"))(12)) +
  labs(x = "Time (Months)", y = "Disease Count", colour = "Birth Month") +
  facet_wrap(~age)
fig


fig <- traj %>% 
  group_by(age) %>% 
  do(p = plot_ly(., x = ~yearmon, y = ~disease, type = 'scatter', mode = 'lines', color = ~birth_month, colors = "Set3")) %>%
  subplot(nrows = 1, shareX = TRUE, shareY = TRUE)
fig
# differences in proportion not really seen in 1-4 year olds but more in <1 year olds after pandemic period. 
# infections highest in december babies/babies born later in the year in 1-4s and infections highest in babies born earlier in the year in <1s
# post-pandemic there was a shift in the proportion of infections born in different months.

# -------------------------------------------------------------------------

# age-specific epidemic (disease) curves by age group
traj <- do.call(rbind, traj18) %>% 
  as.data.frame() %>% 
  group_by(time_calendar, birth_month, time_birth) %>% 
  mutate(disease_mean = mean(disease)) %>% 
  select(-c(waning, aging, infected, disease)) %>% 
  distinct() %>% 
  ungroup() %>% 
  mutate(age = case_when(time_birth <= 3 ~ "0-3",
                         time_birth > 3 & time_birth <= 6 ~ "4-6",
                         time_birth > 6 & time_birth <= 9 ~ "7-9",
                         time_birth > 9 & time_birth <= 12 ~ "10-12",
                         time_birth > 12 & time_birth <= 15 ~ "13-15",
                         time_birth > 15 & time_birth <= 18 ~ "16-18",
                         time_birth > 18 & time_birth <= 21 ~ "19-21",
                         time_birth > 21 & time_birth <= 24 ~ "22-24",
                         time_birth > 24 & time_birth <= 27 ~ "25-27",
                         time_birth > 27 & time_birth <= 30 ~ "28-30",
                         time_birth > 30 & time_birth <= 33 ~ "31-33",
                         time_birth > 33 & time_birth <= 36 ~ "34-36",
                         time_birth > 36 & time_birth <= 39 ~ "37-39",
                         time_birth > 39 & time_birth <= 42 ~ "40-42",
                         time_birth > 42 & time_birth <= 45 ~ "43-45",
                         time_birth > 45 & time_birth <= 48 ~ "46-48")
  ) %>%
  mutate(age = factor(age, levels = c("0-3", "4-6", "7-9", "10-12", "13-15", "16-18", "19-21", "22-24", "25-27", "28-30", "31-33", "34-36", "37-39", "40-42", "43-45", "46-48"))) %>% 
  group_by(time_calendar, age) %>% 
  summarise(disease = sum(disease_mean)) %>% 
  ungroup() %>% 
  left_join(dates %>% 
              select(-level) %>% 
              unique() %>% 
              rename(time_calendar = time))

fig <- ggplot(traj) +
  geom_line(aes(x = yearmon, y = disease, colour = age), alpha = 0.7, size = 1) +
  theme_classic() +
  scale_colour_manual(values = colorRampPalette(brewer.pal(11, "RdBu"))(16)) +
  labs(x = "Time (Months)", y = "Disease Count", colour = "Age Group")
fig

# -------------------------------------------------------------------------

# disease proportion by age group
traj <- do.call(rbind, traj18) %>% 
  as.data.frame() %>% 
  group_by(time_calendar, birth_month, time_birth) %>% 
  mutate(disease_mean = mean(disease)) %>% 
  select(-c(waning, aging, infected, disease)) %>% 
  distinct() %>% 
  ungroup() %>% 
  select(-rate) %>% 
  mutate(age = case_when(time_birth <= 3 ~ "0-3",
                         time_birth > 3 & time_birth <= 6 ~ "4-6",
                         time_birth > 6 & time_birth <= 9 ~ "7-9",
                         time_birth > 9 & time_birth <= 12 ~ "10-12",
                         time_birth > 12 & time_birth <= 15 ~ "13-15",
                         time_birth > 15 & time_birth <= 18 ~ "16-18",
                         time_birth > 18 & time_birth <= 21 ~ "19-21",
                         time_birth > 21 & time_birth <= 24 ~ "22-24",
                         time_birth > 24 & time_birth <= 27 ~ "25-27",
                         time_birth > 27 & time_birth <= 30 ~ "28-30",
                         time_birth > 30 & time_birth <= 33 ~ "31-33",
                         time_birth > 33 & time_birth <= 36 ~ "34-36",
                         time_birth > 36 & time_birth <= 39 ~ "37-39",
                         time_birth > 39 & time_birth <= 42 ~ "40-42",
                         time_birth > 42 & time_birth <= 45 ~ "43-45",
                         time_birth > 45 & time_birth <= 48 ~ "46-48")
  ) %>%
  mutate(age = factor(age, levels = c("0-3", "4-6", "7-9", "10-12", "13-15", "16-18", "19-21", "22-24", "25-27", "28-30", "31-33", "34-36", "37-39", "40-42", "43-45", "46-48"))) %>% 
  left_join(dates %>% 
              select(-level) %>% 
              unique() %>% 
              rename(time_calendar = time),
            by = join_by(time_calendar)) %>% 
  group_by(age, time_calendar, yearmon) %>% 
  summarise(disease = sum(disease_mean)) %>% 
  ungroup()

fig <- ggplot() +
  geom_bar(data = traj, aes(x = yearmon, y = disease, fill = age), position = "dodge", stat = "identity") +
  scale_fill_manual(values = colorRampPalette(brewer.pal(11, "RdBu"))(16)) +
  # scale_fill_viridis_d(option = "C") +
  theme_classic() +
  labs(x = "Months", y = "Disease Proportion", fill = "Age Group")
fig

# -------------------------------------------------------------------------

# disease proportion by birth month

traj <- do.call(rbind, traj18) %>% 
  as.data.frame() %>% 
  group_by(time_calendar, birth_month, time_birth) %>% 
  mutate(disease_mean = mean(disease)) %>% 
  select(-c(waning, aging, infected, disease)) %>% 
  distinct() %>% 
  ungroup() %>% 
  select(-rate) %>% 
  left_join(dates %>% 
              select(-level) %>% 
              unique() %>% 
              rename(time_calendar = time),
            by = join_by(time_calendar)) %>% 
  group_by(birth_month, time_calendar, yearmon) %>% 
  summarise(disease = sum(disease_mean)) %>% 
  ungroup() %>% 
  mutate(birth_month = factor(birth_month, levels = c(1:12)))

fig <- ggplot() +
  geom_bar(data = traj, aes(x = yearmon, y = disease, fill = birth_month), position = "dodge", stat = "identity") +
  scale_fill_manual(values = colorRampPalette(brewer.pal(11, "RdBu"))(16)) +
  # scale_fill_viridis_d(option = "C") +
  theme_classic() +
  labs(x = "Months", y = "Disease Proportion", fill = "Birth Month")
fig

# -------------------------------------------------------------------------

# cumulative disease counts over time by birth month
traj <- do.call(rbind, traj18) %>% 
  as.data.frame() %>% 
  group_by(time_calendar, birth_month, time_birth) %>% 
  mutate(disease_mean = mean(disease)) %>% 
  select(-c(waning, aging, infected, disease)) %>% 
  distinct() %>% 
  ungroup() %>% 
  group_by(time_calendar, birth_month) %>% 
  summarise(disease = sum(disease_mean)) %>% 
  arrange(time_calendar, birth_month) %>% 
  group_by(birth_month) %>%  
  mutate(cumdisease = cumsum(disease)) %>% 
  ungroup() %>% 
  left_join(dates %>% 
              select(-level) %>% 
              unique() %>% 
              rename(time_calendar = time),
            by = join_by(time_calendar))

fig <- ggplot() +
  geom_line(data = traj, aes(x = yearmon, y = cumdisease, colour = as.factor(birth_month)), linetype = "solid") +
  scale_colour_manual(values = colorRampPalette(brewer.pal(11, "RdBu"))(12)) +
  theme_classic() +
  labs(x = "Time (Months)", y = "Cumulative Disease Counts", colour = "Birth Month")
fig
# this figure is not that helpful.
# Would be good to get an idea of how cumulative disease look for age groups pre-pandemics and post-pandemic.

# -------------------------------------------------------------------------

# pre-/post-pandemic cumulative disease counts over age by birth month
traj <- do.call(rbind, traj18) %>% 
  as.data.frame() %>% 
  group_by(time_calendar, birth_month, time_birth) %>% 
  mutate(disease_mean = mean(disease)) %>% 
  select(-c(waning, aging, infected, disease)) %>% 
  distinct() %>% 
  ungroup() %>% 
  left_join(dates %>% 
              select(-level) %>% 
              unique() %>% 
              rename(time_calendar = time),
            by = join_by(time_calendar)) %>% 
  mutate(period = case_when(yearmon >= as.yearmon("Mar 2017") & yearmon < as.yearmon("Mar 2020") ~ "pre-pandemic",
                            yearmon >= as.yearmon("Mar 2020") & yearmon <= as.yearmon("Mar 2021") ~ "pandemic",
                            yearmon > as.yearmon("Mar 2021") & yearmon <= as.yearmon("Mar 2024") ~ "post-pandemic")) %>% # create time periods for plot
  filter(!is.na(period)) %>% 
  group_by(time_birth, birth_month, period) %>% 
  summarise(disease = sum(disease_mean)) %>% 
  arrange(time_birth, birth_month, period) %>%  
  group_by(birth_month, period) %>%  
  mutate(cumdisease = cumsum(disease)) %>% 
  ungroup()

fig <- ggplot() +
  geom_line(data = traj, aes(x = time_birth, y = cumdisease, colour = as.factor(birth_month)), linetype = "solid") +
  scale_colour_manual(values = colorRampPalette(brewer.pal(11, "RdBu"))(12)) +
  theme_classic() +
  labs(x = "Age (Months)", y = "Cumulative Disease Counts", colour = "Birth Month") +
  facet_wrap(~period)
fig
# no major changes in cumulative disease counts over age comparing the pre-pandemic and post-pandemic period but the final range of disease counts vary signifying there might be a change in risk of disease post-pandemic.

# -------------------------------------------------------------------------

# pre-/post-pandemic birth month disease proportions by age

traj <- do.call(rbind, traj18) %>% 
  as.data.frame() %>% 
  group_by(time_calendar, birth_month, time_birth) %>% 
  mutate(disease_mean = mean(disease)) %>% 
  select(-c(waning, aging, infected, disease)) %>% 
  distinct() %>% 
  ungroup() %>% 
  left_join(dates %>% 
              select(-level) %>% 
              unique() %>% 
              rename(time_calendar = time),
            by = join_by(time_calendar)) %>% 
  mutate(period = case_when(yearmon >= as.yearmon("Mar 2017") & yearmon < as.yearmon("Mar 2020") ~ "pre-pandemic",
                            yearmon >= as.yearmon("Mar 2020") & yearmon < as.yearmon("Mar 2024") ~ "post-pandemic")) %>% # create time periods for plot
  filter(!is.na(period)) %>% 
  group_by(time_birth, birth_month, period) %>% 
  summarise(disease = sum(disease_mean)) %>% 
  arrange(time_birth, birth_month, period)

fig <- ggplot() +
  geom_bar(data = traj, aes(x = as.factor(birth_month), y = disease, fill = time_birth), position = "fill", stat = "identity") +
  scale_fill_viridis(discrete = FALSE, option = "C") +
  theme_classic() +
  labs(x = "Birth Month", y = "Disease Proportion", fill = "Age (Months)") +
  facet_wrap(~period)
fig

# -------------------------------------------------------------------------

# disease count age group/month

traj <- do.call(rbind, traj18) %>% 
  as.data.frame() %>% 
  group_by(time_calendar, birth_month, time_birth) %>% 
  mutate(disease_mean = mean(disease)) %>% 
  select(-c(waning, aging, infected, disease)) %>% 
  distinct() %>% 
  ungroup() %>% 
  mutate(age = case_when(time_birth <= 3 ~ "0-3",
                         time_birth > 3 & time_birth <= 6 ~ "4-6",
                         time_birth > 6 & time_birth <= 9 ~ "7-9",
                         time_birth > 9 & time_birth <= 12 ~ "10-12",
                         time_birth > 12 & time_birth <= 18 ~ "13-18",
                         time_birth > 18 & time_birth <= 24 ~ "19-24",
                         time_birth > 24 & time_birth <= 30 ~ "25-30",
                         time_birth > 30 & time_birth <= 36 ~ "31-36",
                         time_birth > 36 & time_birth <= 42 ~ "37-42",
                         time_birth > 42 & time_birth <= 48 ~ "42-48")
  ) %>% # create age groups
  left_join(dates %>% 
              select(-level) %>% 
              unique() %>% 
              rename(time_calendar = time),
            by = join_by(time_calendar)) %>% 
  mutate(period = case_when(yearmon >= as.yearmon("Mar 2017") & yearmon < as.yearmon("Mar 2020") ~ "pre-pandemic",
                            yearmon >= as.yearmon("Mar 2020") & yearmon <= as.yearmon("Mar 2021") ~ "pandemic",
                            yearmon > as.yearmon("Mar 2021") & yearmon <= as.yearmon("Mar 2024") ~ "post-pandemic")) %>% # create time periods for plot
  filter(!is.na(period)) %>% 
  group_by(age, birth_month, period) %>% 
  summarise(disease = sum(disease_mean)) %>% 
  ungroup() %>% 
  mutate(age = factor(age, levels = c("0-3", "4-6", "7-9", "10-12", "13-18", "19-24", "25-30", "31-36", "37-42", "42-48")))

fig <- ggplot(traj) +
  geom_bar(aes(x = age, y = disease, group = period, fill = period), stat = "identity", position = "dodge") +
  theme_classic() +
  labs(x = "Months", y = "Count", fill = "Period") +
  facet_wrap(~birth_month)
fig

# -------------------------------------------------------------------------

# heatmap: disease count by age and birth month

traj <- do.call(rbind, traj18) %>% 
  as.data.frame() %>% 
  group_by(time_calendar, birth_month, time_birth) %>% 
  mutate(disease_mean = mean(disease)) %>% 
  select(-c(waning, aging, infected, disease)) %>% 
  distinct() %>% 
  ungroup() %>% 
  left_join(dates %>% 
              select(-level) %>% 
              unique() %>% 
              rename(time_calendar = time),
            by = join_by(time_calendar)) %>%   mutate(period = case_when(yearmon >= as.yearmon("Mar 2017") & yearmon < as.yearmon("Mar 2020") ~ "pre-pandemic",
                                                                         yearmon >= as.yearmon("Mar 2020") & yearmon <= as.yearmon("Mar 2021") ~ "pandemic",
                                                                         yearmon > as.yearmon("Mar 2021") & yearmon <= as.yearmon("Mar 2024") ~ "post-pandemic")) %>% # create time periods for plot
  filter(!is.na(period)) %>% 
  group_by(time_birth, birth_month, period) %>% 
  summarise(disease = sum(disease_mean)) %>% 
  ungroup() %>% 
  mutate(birth_month = factor(birth_month, levels = c(1:12)))

fig <- ggplot() +
  geom_tile(data = traj, aes(x = time_birth, y = birth_month, fill = disease), colour = "white") +
  scale_fill_viridis(discrete = FALSE, option = "D") +
  theme_classic() +
  labs(x = "Age (Months)", y = "Birth Month", fill = "Disease Count") +
  facet_wrap(~period)
fig

# -------------------------------------------------------------------------

# birth month vs attack rate

birth_data <- births %>% 
  filter(year >= 2010) %>% 
  mutate(time = 1:178) %>% 
  filter(time %in% c((82-48):178)) %>% 
  group_by(birth_month) %>% 
  summarise(births = sum(births)) %>% 
  mutate(birth_month = as.numeric(birth_month))

traj <- do.call(rbind, traj18) %>% 
  as.data.frame() %>% 
  group_by(time_calendar, birth_month, time_birth) %>% 
  mutate(disease_mean = mean(disease)) %>% 
  select(-c(waning, aging, infected, disease)) %>% 
  distinct() %>% 
  ungroup() %>% 
  left_join(dates %>% 
              select(-level) %>% 
              unique() %>% 
              rename(time_calendar = time),
            by = join_by(time_calendar)) %>% 
  group_by(birth_month) %>% 
  summarise(disease = sum(disease_mean)) %>% 
  ungroup() %>% 
  left_join(birth_data) %>% 
  mutate(attack_rate = (disease/births)*100,
         birth_month = factor(birth_month, levels = c(1:12)))

fig <- ggplot() +
  geom_line(data = traj, aes(x = birth_month, y = attack_rate, group = 1)) +
  theme_classic() +
  labs(x = "Birth Month", y = "RSV Attack Rate (%)")
fig

# -------------------------------------------------------------------------

# age vs attack rate

birth_data <- births %>% 
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
  group_by(time_birth) %>% 
  summarise(births = sum(births))

ggplot(data = birth_age) +
  geom_line(aes(x = time_birth, y = births)) +
  theme_classic()

traj <- do.call(rbind, traj18) %>% 
  as.data.frame() %>% 
  group_by(time_calendar, birth_month, time_birth) %>% 
  mutate(disease_mean = mean(disease)) %>% 
  select(-c(waning, aging, infected, disease)) %>% 
  distinct() %>% 
  ungroup() %>% 
  left_join(dates %>% 
              select(-level) %>% 
              unique() %>% 
              rename(time_calendar = time),
            by = join_by(time_calendar)) %>% 
  group_by(time_birth) %>% 
  summarise(disease = sum(disease_mean)) %>% 
  ungroup() %>% 
  left_join(birth_age) %>% 
  mutate(attack_rate = (disease/births)*100)

fig <- ggplot() +
  geom_line(data = traj, aes(x = time_birth, y = attack_rate, group = 1)) +
  theme_classic() +
  labs(x = "Age (Months)", y = "RSV Attack Rate (%)")
fig

# -------------------------------------------------------------------------

# attack rate time series

birth_data <- births %>% 
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
  group_by(time_calendar) %>% 
  summarise(births = sum(births))

traj <- do.call(rbind, traj18) %>% 
  as.data.frame() %>% 
  group_by(time_calendar, birth_month, time_birth) %>% 
  mutate(disease_mean = mean(disease)) %>% 
  select(-c(waning, aging, infected, disease)) %>% 
  distinct() %>% 
  ungroup() %>% 
  left_join(dates %>% 
              select(-level) %>% 
              unique() %>% 
              rename(time_calendar = time),
            by = join_by(time_calendar)) %>% 
  group_by(time_calendar, yearmon) %>% 
  summarise(disease = sum(disease_mean)) %>% 
  ungroup() %>% 
  left_join(birth_age) %>% 
  mutate(attack_rate = (disease/births)*100)

fig <- ggplot() +
  geom_line(data = traj, aes(x = yearmon, y = attack_rate, group = 1)) +
  theme_classic() +
  labs(x = "Time (Months)", y = "RSV Attack Rate (%)")
fig

# -------------------------------------------------------------------------

#  polar plot: disease/age

traj <- do.call(rbind, traj18) %>% 
  as.data.frame() %>% 
  group_by(time_calendar, birth_month, time_birth) %>% 
  mutate(disease_mean = mean(disease)) %>% 
  select(-c(waning, aging, infected, disease)) %>% 
  distinct() %>% 
  ungroup() %>% 
  left_join(dates %>% 
              select(-level) %>% 
              unique() %>% 
              rename(time_calendar = time),
            by = join_by(time_calendar)) %>%   
  mutate(period = case_when(yearmon >= as.yearmon("Mar 2017") & yearmon < as.yearmon("Mar 2020") ~ "pre-pandemic",
                            yearmon >= as.yearmon("Mar 2020") & yearmon <= as.yearmon("Mar 2021") ~ "pandemic",
                            yearmon > as.yearmon("Mar 2021") & yearmon <= as.yearmon("Mar 2024") ~ "post-pandemic")) %>% # create time periods for plot
  filter(!is.na(period)) %>% 
  group_by(time_birth, period) %>% 
  summarise(disease = sum(disease_mean)) %>% 
  ungroup()

fig <- ggplot() +
  geom_col(data = traj, aes(x = time_birth, y = disease), width = 25, fill = "skyblue", color = "black") +
  coord_polar(start = 0) +
  scale_x_continuous(
    limits = c(0, 48),
    breaks = seq(0, 48, by = 6),
    minor_breaks = NULL) +
  theme_minimal() +
  labs(x = "Age (Months)", y = "Disease Count") +
  facet_wrap(~period)
fig

# -------------------------------------------------------------------------

#  polar plot: disease/birth month

traj <- do.call(rbind, traj18) %>% 
  as.data.frame() %>% 
  group_by(time_calendar, birth_month, time_birth) %>% 
  mutate(disease_mean = mean(disease)) %>% 
  select(-c(waning, aging, infected, disease)) %>% 
  distinct() %>% 
  ungroup() %>% 
  left_join(dates %>% 
              select(-level) %>% 
              unique() %>% 
              rename(time_calendar = time),
            by = join_by(time_calendar)) %>%   
  mutate(period = case_when(yearmon >= as.yearmon("Mar 2017") & yearmon < as.yearmon("Mar 2020") ~ "pre-pandemic",
                            yearmon >= as.yearmon("Mar 2020") & yearmon <= as.yearmon("Mar 2021") ~ "pandemic",
                            yearmon > as.yearmon("Mar 2021") & yearmon <= as.yearmon("Mar 2024") ~ "post-pandemic")) %>% # create time periods for plot
  filter(!is.na(period)) %>% 
  group_by(birth_month, period) %>% 
  summarise(disease = sum(disease_mean)) %>% 
  ungroup()

fig <- ggplot() +
  geom_col(data = traj, aes(x = birth_month, y = disease), fill = "skyblue", color = "black") +
  coord_polar(start = 0) +
  scale_x_continuous(
    limits = c(0, 12),
    breaks = seq(0, 12, by = 2),
    minor_breaks = NULL) +
  theme_minimal() +
  labs(x = "Birth Month", y = "Disease Count") +
  facet_wrap(~period)
fig

# -------------------------------------------------------------------------

# attack rate season/time series

birth_data <- births %>% 
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
  group_by(time_calendar) %>% 
  summarise(births = sum(births))

traj <- do.call(rbind, traj17subset) %>% 
  as.data.frame() %>% 
  group_by(time_calendar, birth_month, time_birth) %>% 
  mutate(disease_mean = mean(disease)) %>% 
  select(-c(waning, aging, infected, disease)) %>% 
  distinct() %>% 
  ungroup() %>% 
  left_join(dates %>% 
              select(-level) %>% 
              unique() %>% 
              rename(time_calendar = time),
            by = join_by(time_calendar)) %>% 
  group_by(time_calendar, yearmon, month, month_num, year) %>% 
  summarise(disease = sum(disease_mean)) %>% 
  ungroup() %>% 
  left_join(birth_age) %>% 
  mutate(attack_rate = (disease/births)*100000,
         season = cut(as.numeric(yearmon), 
                      breaks = as.yearmon(c("Jul 2017", "Jul 2018", "Jul 2019", "Jul 2020", "Jul 2021", "Jul 2022", "Jul 2023", "Jul 2024")), 
                      labels = c("2017_18", "2018_19", "2019_20", "2020_21", "2021_22", "2022_23", "2023_24"), 
                      right = FALSE)) %>% 
  filter(!is.na(season)) %>% 
  mutate(month = factor(month, levels = c(month.abb[7:12], month.abb[1:6])))

fig <- ggplot() +
  geom_line(data = traj, aes(x = month, y = attack_rate, colour = season, group = season), size = 1) +
  scale_colour_viridis_d(option = "C") +
  theme_classic() +
  labs(x = "Month", y = "Monthly RSV Attack Rate (per 100,000)", colour = "Season")
fig

# -------------------------------------------------------------------------

# attack rate season/age

birth_data <- births %>% 
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

traj <- do.call(rbind, traj) %>% 
  as.data.frame() %>% 
  group_by(time_calendar, birth_month, time_birth) %>% 
  mutate(disease_mean = mean(disease)) %>% 
  select(-c(waning, aging, infected, disease)) %>% 
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
  left_join(birth_age) %>% 
  group_by(time_birth, season) %>% 
  summarise(disease = sum(disease_mean),
            births = sum(births)) %>% 
  ungroup() %>% 
  mutate(attack_rate = (disease/births)*100000)

fig <- ggplot() +
  geom_line(data = traj, aes(x = time_birth, y = attack_rate, group = season, colour = season), size = 1) +
  scale_colour_viridis_d(option = "C") +
  theme_classic() +
  labs(x = "Age (Months)", y = "Annual RSV Attack Rate (per 100,000)", colour = "Season")
fig

# -------------------------------------------------------------------------

# attack rate season/birth month

birth_data <- births %>% 
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

traj <- do.call(rbind, traj17subset) %>% 
  as.data.frame() %>% 
  group_by(time_calendar, birth_month, time_birth) %>% 
  mutate(disease_mean = mean(disease)) %>% 
  select(-c(waning, aging, infected, disease)) %>% 
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
  left_join(birth_age) %>% 
  group_by(birth_month, season) %>% 
  summarise(disease = sum(disease_mean),
            births = sum(births)) %>% 
  ungroup() %>% 
  mutate(attack_rate = (disease/births)*100000,
         birth_month = factor(birth_month, levels = 1:12))

fig <- ggplot() +
  geom_line(data = traj, aes(x = birth_month, y = attack_rate, group = season, colour = season), size = 1) +
  scale_colour_viridis_d(option = "C") +
  theme_classic() +
  labs(x = "Birth Month", y = "Annual RSV Attack Rate (per 100,000)", colour = "Season")
fig

# -------------------------------------------------------------------------

# attack rate birth month/time series

birth_data <- births %>% 
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
  group_by(time_calendar, birth_month) %>% 
  summarise(births = sum(births)) %>% 
  mutate(birth_month = as.numeric(birth_month))

traj <- do.call(rbind, traj17subset) %>% 
  as.data.frame() %>% 
  group_by(time_calendar, birth_month, time_birth) %>% 
  mutate(disease_mean = mean(disease)) %>% 
  select(-c(waning, aging, infected, disease)) %>% 
  distinct() %>% 
  ungroup() %>% 
  left_join(dates %>% 
              select(-level) %>% 
              unique() %>% 
              rename(time_calendar = time),
            by = join_by(time_calendar)) %>% 
  group_by(time_calendar, yearmon, month, month_num, year, birth_month) %>% 
  summarise(disease = sum(disease_mean)) %>% 
  ungroup() %>% 
  left_join(birth_age) %>% 
  mutate(attack_rate = (disease/births)*100000,
         birth_month = factor(birth_month, levels = 1:12)) 

fig <- ggplot() +
  geom_line(data = traj, aes(x = yearmon, y = attack_rate, group = birth_month, colour = birth_month)) +
  scale_colour_viridis_d(option = "C") +
  theme_classic() +
  labs(x = "Time (Months)", y = "Monthly RSV Attack Rate (per 100,000)", colour = "Birth Month")
fig

# -------------------------------------------------------------------------

# attack rate birth month/age

birth_data <- births %>% 
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
  group_by(time_birth, birth_month) %>% 
  summarise(births = sum(births)) %>% 
  mutate(birth_month = as.numeric(birth_month))

traj <- do.call(rbind, traj17subset) %>% 
  as.data.frame() %>% 
  group_by(time_calendar, birth_month, time_birth) %>% 
  mutate(disease_mean = mean(disease)) %>% 
  select(-c(waning, aging, infected, disease)) %>% 
  distinct() %>% 
  ungroup() %>% 
  left_join(dates %>% 
              select(-level) %>% 
              unique() %>% 
              rename(time_calendar = time),
            by = join_by(time_calendar)) %>% 
  group_by(time_birth, birth_month) %>% 
  summarise(disease = sum(disease_mean)) %>% 
  ungroup() %>% 
  left_join(birth_age) %>% 
  mutate(attack_rate = (disease/births)*100000,
         birth_month = factor(birth_month, levels = 1:12)) 

fig <- ggplot() +
  geom_line(data = traj, aes(x = time_birth, y = attack_rate, group = birth_month, colour = birth_month)) +
  scale_colour_viridis_d(option = "C") +
  theme_classic() +
  labs(x = "Age (Months)", y = "RSV Attack Rate (per 100,000)", colour = "Birth Month")
fig

# -------------------------------------------------------------------------

# attack rate age/time series

birth_data <- births %>% 
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
  mutate(age = case_when(time_birth <= 3 ~ "0-3",
                         time_birth > 3 & time_birth <= 6 ~ "4-6",
                         time_birth > 6 & time_birth <= 9 ~ "7-9",
                         time_birth > 9 & time_birth <= 12 ~ "10-12",
                         time_birth > 12 & time_birth <= 18 ~ "13-18",
                         time_birth > 18 & time_birth <= 24 ~ "19-24",
                         time_birth > 24 & time_birth <= 30 ~ "25-30",
                         time_birth > 30 & time_birth <= 36 ~ "31-36",
                         time_birth > 36 & time_birth <= 42 ~ "37-42",
                         time_birth > 42 & time_birth <= 48 ~ "42-48")) %>% 
  group_by(time_calendar, age) %>% 
  summarise(births = sum(births))

traj <- do.call(rbind, traj17subset) %>% 
  as.data.frame() %>% 
  group_by(time_calendar, birth_month, time_birth) %>% 
  mutate(disease_mean = mean(disease)) %>% 
  select(-c(waning, aging, infected, disease)) %>% 
  distinct() %>% 
  ungroup() %>% 
  left_join(dates %>% 
              select(-level) %>% 
              unique() %>% 
              rename(time_calendar = time),
            by = join_by(time_calendar)) %>% 
  mutate(age = case_when(time_birth <= 3 ~ "0-3",
                         time_birth > 3 & time_birth <= 6 ~ "4-6",
                         time_birth > 6 & time_birth <= 9 ~ "7-9",
                         time_birth > 9 & time_birth <= 12 ~ "10-12",
                         time_birth > 12 & time_birth <= 18 ~ "13-18",
                         time_birth > 18 & time_birth <= 24 ~ "19-24",
                         time_birth > 24 & time_birth <= 30 ~ "25-30",
                         time_birth > 30 & time_birth <= 36 ~ "31-36",
                         time_birth > 36 & time_birth <= 42 ~ "37-42",
                         time_birth > 42 & time_birth <= 48 ~ "42-48")) %>% 
  group_by(time_calendar, yearmon, month, month_num, year, age) %>% 
  summarise(disease = sum(disease_mean)) %>% 
  ungroup() %>% 
  left_join(birth_age) %>% 
  mutate(attack_rate = (disease/births)*100000,
         age = factor(age, levels = c("0-3", "4-6", "7-9", "10-12", "13-18", "19-24", "25-30", "31-36", "37-42", "42-48"))) 

fig <- ggplot() +
  geom_line(data = traj, aes(x = yearmon, y = attack_rate, group = age, colour = age)) +
  scale_colour_viridis_d(option = "C") +
  theme_classic() +
  labs(x = "Time (Months)", y = "Montly RSV Attack Rate (per 100,000)", colour = "Age Group")
fig

# -------------------------------------------------------------------------

# heatmap: rate difference of two seasons by birth month and age

birth_data <- births %>% 
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

traj <- do.call(rbind, traj17subset) %>% 
  as.data.frame() %>% 
  group_by(time_calendar, birth_month, time_birth) %>% 
  mutate(disease_mean = mean(disease)) %>% 
  select(-c(waning, aging, infected, disease)) %>% 
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
  left_join(birth_age) %>% 
  group_by(birth_month, time_birth, season) %>% 
  summarise(disease = sum(disease_mean),
            births = sum(births)) %>% 
  ungroup() %>% 
  mutate(attack_rate = (disease/births)*100000,
         birth_month = factor(birth_month, levels = 1:12))

fig <- traj %>% 
  filter(season %in% c("2018_19", "2021_22")) %>% 
  select(-c(disease, births)) %>% 
  pivot_wider(names_from = season, values_from = attack_rate) %>% 
  mutate(rate_change = `2021_22` - `2018_19`) %>% 
  ggplot() +
  geom_tile(aes(x = time_birth, y = birth_month, fill = rate_change), colour = "white") +
  scale_fill_viridis(discrete = FALSE, option = "D") +
  theme_classic() +
  labs(x = "Age (Months)", y = "Birth Month", fill = "Annual Rate Difference (per 100,000)")

fig <- traj %>%
  filter(season %in% c("2018_19", "2021_22")) %>%
  select(-c(disease, births)) %>%
  pivot_wider(names_from = season, values_from = attack_rate) %>%
  mutate(rate_change = `2021_22` - `2018_19`,
         birth_month = as.numeric(birth_month)) %>%
  ggplot(aes(x = time_birth, y = birth_month, z = rate_change)) +
  geom_raster(aes(fill = rate_change)) + 
  geom_contour(colour = "white") +  
  scale_fill_viridis(discrete = FALSE, option = "D") +
  theme_classic() +
  labs(x = "Age (Months)", y = "Birth Month", fill = "Annual Rate Difference (per 100,000)")
fig

# -------------------------------------------------------------------------

# rate difference of two seasons over age by birth month

birth_data <- births %>% 
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

traj <- do.call(rbind, traj17subset) %>% 
  as.data.frame() %>% 
  group_by(time_calendar, birth_month, time_birth) %>% 
  mutate(disease_mean = mean(disease)) %>% 
  select(-c(waning, aging, infected, disease)) %>% 
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
  left_join(birth_age) %>% 
  group_by(birth_month, time_birth, season) %>% 
  summarise(disease = sum(disease_mean),
            births = sum(births)) %>% 
  ungroup() %>% 
  mutate(attack_rate = (disease/births)*100000,
         birth_month = factor(birth_month, levels = 1:12))

fig <- traj %>% 
  filter(season %in% c("2018_19", "2021_22")) %>% 
  select(-c(disease, births)) %>% 
  pivot_wider(names_from = season, values_from = attack_rate) %>% 
  mutate(rate_change = `2021_22` - `2018_19`) %>% 
  ggplot() +
  geom_line(aes(x = time_birth, y = rate_change, group = birth_month, colour = birth_month)) +
  scale_colour_viridis_d(option = "C") +
  theme_classic() +
  labs(x = "Age (Months)", y = "Annual Rate Difference (per 100,000)", colour = "Birth Month") 
  # facet_wrap(~birth_month, nrow = 3, ncol = 4)
fig


# -------------------------------------------------------------------------

# mother's time since last exposure over time

test <- women %>% 
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
  mutate(last_exp = factor(last_exp, levels = c(1:24, ">24")))

ggplot(data = test) +
  geom_line(aes(x = yearmon, y = value, group = last_exp, colour = last_exp)) +
  scale_colour_viridis_d(option = "C") +
  theme_classic() +
  labs(x = "Time (Months)", y = "Count", colour = "Time Since Last Exposure") 

# -------------------------------------------------------------------------

# mother's time since exposure over a season

test <- women %>% 
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
  mutate(last_exp = factor(last_exp, levels = c(1:24, ">24"))) %>% 
  group_by(season, last_exp) %>% 
  summarise(value = sum(value))

ggplot(data = test) +
  geom_bar(aes(x = last_exp, y = value, group = season, fill = season), stat = "identity", position = "dodge") +
  scale_fill_viridis_d(option = "C") +
  theme_classic() +
  labs(x = "Time (Months)", y = "Count", colour = "Time Since Last Exposure") 

# -------------------------------------------------------------------------

# babies born over mother's time since last exposure over time

test <- babies %>% 
  as.data.frame() %>% 
  left_join(dates %>% select(-c(rate, level)) %>% distinct(), by = join_by(time)) %>% 
  mutate(season = cut(as.numeric(yearmon), 
                      breaks = as.yearmon(c("Jul 2017", "Jul 2018", "Jul 2019", "Jul 2020", "Jul 2021", "Jul 2022", "Jul 2023", "Jul 2024")), 
                      labels = c("2017_18", "2018_19", "2019_20", "2020_21", "2021_22", "2022_23", "2023_24"), 
                      right = FALSE)) %>% 
  filter(!is.na(season)) %>% 
  pivot_longer(cols = `susceptible_reinf`:`I24`, names_to = "last_exp") %>% 
  mutate(last_exp = recode(last_exp, "susceptible_reinf" = ">24", "I24" = "24", "I23" = "23", "I22" = "22", "I21" = "21", "I20" = "20", "I19" = "19", "I18" = "18",
                           "I17" = "17", "I16" = "16", "I15" = "15", "I14" = "14", "I13" = "13", "I12" = "12", "I11" = "11", "I10" = "10", "I9" = "9", "I8" = "8",
                           "I7" = "7", "I6" = "6", "I5" = "5", "I4" = "4", "I3" = "3", "I2" = "2", "I1" = "1")) %>% 
  mutate(last_exp = factor(last_exp, levels = c(1:24, ">24")))

ggplot(data = test) +
  geom_line(aes(x = yearmon, y = value, group = last_exp, colour = last_exp)) +
  scale_colour_viridis_d(option = "C") +
  theme_classic() +
  labs(x = "Time (Months)", y = "Count", colour = "Time Since Last Exposure") 

# -------------------------------------------------------------------------

# babies born over mother's time since exposure by season

test <- babies %>% 
  as.data.frame() %>% 
  left_join(dates %>% select(-c(rate, level)) %>% distinct(), by = join_by(time)) %>% 
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
  group_by(season, last_exp) %>% 
  summarise(value = sum(value))

ggplot(data = test) +
  geom_bar(aes(x = last_exp, y = value, group = season, fill = season), stat = "identity", position = "dodge") +
  scale_fill_viridis_d(option = "C") +
  theme_classic() +
  labs(x = "Time (Months)", y = "Count", colour = "Time Since Last Exposure") 


# -------------------------------------------------------------------------

# disease count over time by mother's time since last exposure

test <- data %>% 
  as.data.frame() %>% 
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
  group_by(yearmon, last_exp) %>% 
  summarise(value = sum(value))
  
ggplot(data = test) +
  geom_line(aes(x = yearmon, y = value, group = last_exp, colour = last_exp)) +
  scale_colour_viridis_d(option = "C") +
  theme_classic() +
  labs(x = "Time (Months)", y = "Count", colour = "Time Since Last Exposure") 

# -------------------------------------------------------------------------

# disease count over mother's time since last exposure by season

test <- data %>% 
  as.data.frame() %>% 
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
  group_by(season, last_exp) %>% 
  summarise(value = sum(value))

ggplot(data = test) +
  geom_bar(aes(x = last_exp, y = value, group = season, fill = season), stat = "identity", position = "dodge") +
  scale_fill_viridis_d(option = "C") +
  theme_classic() +
  labs(x = "Time (Months)", y = "Count", colour = "Time Since Last Exposure") 
