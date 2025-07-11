library(cowplot)

n <- 17 # choose combination number
rep = 30 # number of years
factor = combinations[[n]]$factor # select factor for rate of exposure
n_interest <- combinations[[n]]$duration # duration of immunity in mothers

# helper data frame for dates and rates (without any disruption) to model babies - 2010 until 2024
dates <- as.data.frame(matrix(NA, 12*(rep+4), 6)) # add 4 years to account for modelling children until 4 years old
colnames(dates) <- c("time", "month", "month_num", "year", "yearmon", "date")

dates <- dates %>%
  mutate(time = 1:nrow(dates),
         month = rep(month.abb, nrow(dates)/12),
         month_num = rep(1:12, rep+4),
         year = rep(c(1995:(1995+(nrow(dates)/12)-1)), each =12), # model 30 years: 1995-2024
         yearmon = as.yearmon(paste(year, month, sep = "_"), "%Y_%b"),
         date = as.Date(yearmon),
         rate = (case_when(month == month.abb[1] ~ 0.06,
                           month %in% month.abb[2:3] ~ 0.02,
                           month %in% month.abb[4:8] ~ 0,
                           month == month.abb[9] ~ 0.04,
                           month == month.abb[10] ~ 0.08,
                           month %in% month.abb[11:12] ~ 0.18))) %>% # up until here to keep track of time/year when modelling mothers
  mutate(rate = rate * factor) %>%
  filter(year >= 2010) %>%
  mutate(time = 1:n_distinct(time)) %>%
  mutate(season = cut(as.numeric(yearmon), 
                      breaks = as.yearmon(c("Jul 2017", "Jul 2018", "Jul 2019", "Jul 2020", "Jul 2021", "Jul 2022", "Jul 2023", "Jul 2024", "Jul 2025", "Jul 2026", "Jul 2027", "Jul 2028")), 
                      labels = c("2017-18", "2018-19", "2019-20", "2020-21", "2021-22", "2022-23", "2023-24", "2024-25", "2025-26", "2026-27", "2027-28"), 
                      right = FALSE)) %>% 
  rename(time_calendar = time)

# monthly birth occurrences data (spans 1995 jan - 2024 oct)
birth_data <- read_excel(here("data", "monthly-births-october-24-tabs.xlsx"), sheet = "Table_3", skip = 4) %>%
  filter(`NHS Board area` == "Scotland") %>%
  filter(Year >= 1995) %>% # start at 1995 to match time/year when modelling mothers (full dataset starts 1991)
  select(-c(`NHS Board area`, `Column1`)) %>%
  rename(year = Year, month = Month, births = `Births occurring`) %>%
  mutate(yearmon = as.yearmon(paste(year, month, sep = "_"), "%Y_%B"),
         date = as.Date(yearmon),
         birth_month = format(as.Date(date, format = "%Y-%B-%d"), "%m")) %>%
  arrange(yearmon)

# laod helper scripts for intermediate data sets
lapply(list.files(here("code", "plot"), pattern = "^plot.*\\.R$", full.names = TRUE), source)
lapply(list.files(here("code", "model"), pattern = "^save.*\\.R$", full.names = TRUE), source)
lapply(list.files(here("code", "model"), pattern = "^create.*\\.R$", full.names = TRUE), source)

# extract posteriors
out <- readRDS(here("output", "data", "parameters", "15032025*", paste0("out", n, ".rds")))

posterior <- getSample(out, thin = 100)
posterior <- posterior[1:2000, ]
fixed <- matrix(combinations[[n]]$fixed[!combinations[[n]]$ind],
                nrow = nrow(posterior), 
                ncol = sum(!combinations[[n]]$ind),
                byrow = TRUE,
                dimnames = list(NULL, combinations[[n]]$name[!combinations[[n]]$ind]))
posterior <- cbind(posterior, fixed)

# set duration of maternal immunity
duration = combinations[[n]]$duration

# create fixed datasets
save_data <- create_data(n_interest = duration, rep = 30, factor = combinations[[n]]$factor)

# generate trajectory as model output
traj <- save_trajectory(out)

# generate trajectory of infection and disease
traj_infection<- mclapply(1:nrow(posterior),
                             function(r){
                               output <- create_trajectory_infection(lambda = exp(posterior[r, "disruption"]),
                                                                     theta1 = posterior[r, "inf_imm1"], theta2 = posterior[r, "inf_imm2"],
                                                                     omega1 = posterior[r, "waning1"], omega2 = posterior[r, "waning2"],
                                                                     alpha1 = posterior[r, "aging1"], alpha2 = posterior[r, "aging2"],
                                                                     stored_data = save_data,
                                                                     delta = 0.0075,
                                                                     n_interest = duration)[, 1] * posterior[r, "detection"]
                               return(output)
                             },
                             mc.cores = 4) 

plot_hdi(traj, traj_infection)

risk <- do.call(rbind, traj) %>% 
  as.data.frame() %>% 
  select(1:97) %>% 
  rowSums() %>% 
  bind_cols(do.call(rbind, traj_infection) %>% 
              as.data.frame() %>% 
              select(1:97) %>%
              rowSums()) %>% 
  rename(disease = `...1`, infection = `...2`) %>% 
  mutate(risk = disease / infection,
         age = "1-4 years") %>% 
  bind_rows(do.call(rbind, traj) %>% 
              as.data.frame() %>% 
              select(98:194) %>% 
              rowSums() %>% 
              bind_cols(do.call(rbind, traj_infection) %>% 
                          as.data.frame() %>% 
                          select(98:194) %>%
                          rowSums()) %>% 
              rename(disease = `...1`, infection = `...2`) %>% 
              mutate(risk = disease / infection,
                     age = "<1 years")) %>% 
  group_by(age) %>% 
  summarise(mean = mean(risk),
            lower = hdi(risk)[[1]],
            upper = hdi(risk)[[2]])

plot_shapes(out)

# parameter estimates summary statistics
colMeans(posterior)
hdi(posterior)

# generate trajectory with details (e.g., birth_month, etc.)
traj_birth_month <- mclapply(1:nrow(posterior),
                 function(r){
                   output <- create_trajectory_birth_month(lambda = exp(posterior[r, "disruption"]),
                                           theta1 = posterior[r, "inf_imm1"], theta2 = posterior[r, "inf_imm2"],
                                           omega1 = posterior[r, "waning1"], omega2 = posterior[r, "waning2"],
                                           alpha1 = posterior[r, "aging1"], alpha2 = posterior[r, "aging2"],
                                           stored_data = save_data,
                                           delta = 0.0075,
                                           n_interest = duration)
                   output[, c(3:(3+n_interest), (10+n_interest))] <- output[, c(3:(3+n_interest), (10+n_interest))] * posterior[r, "detection"]
                   return(output)
                 },
                 mc.cores = 4) 

plot_age_season(traj_birth_month, birth_data)

# additional plots for exploration
plot_traceplot(out)
plot_trajectories(traj)