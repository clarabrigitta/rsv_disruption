source(here("code", "plot", "plot_traceplot.R"))
source(here("code", "plot", "plot_shapes.R"))


# choose combination number
n <- 17

# extract posteriors
out <- readRDS(here("output", "data", "parameters", "19022025", paste0("out", n, ".rds")))

posterior <- getSample(out, thin = 1000)
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
  left_join(data.frame(level = rep(1:25, 228),
                       time = rep(1:228, each = 25))) # model 15yrs of births (2010-2024)

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

# generate trajectory as model output
traj <- save_trajectory(out)

# generate trajectory with details (e.g., birth_month, etc.)
traj_birth_month <- mclapply(1:nrow(posterior),
                 function(r){
                   output <- save_trajectory_birth_month(lambda = exp(posterior[r, "disruption"]),
                                           theta1 = posterior[r, "inf_imm1"], theta2 = posterior[r, "inf_imm2"],
                                           omega1 = posterior[r, "waning1"], omega2 = posterior[r, "waning2"],
                                           alpha1 = posterior[r, "aging1"], alpha2 = posterior[r, "aging2"],
                                           stored_data = save_data,
                                           delta = 0.0075,
                                           n_interest = duration)
                   output[, c(3:27, 34)] <- output[, c(3:27, 34)] * posterior[r, "detection"]
                   return(output)
                 },
                 mc.cores = 4) 

# generate maternal infection history
traj_women <-save_trajectory_women(lambda = exp(-4.3),
                        stored_data = save_data, 
                        delta = 0.0075,  
                        n_interest = duration)

# generate immunity level proportion of children
traj_babies <-save_trajectory_babies(lambda = exp(-4.3),
                              stored_data = save_data, 
                              delta = 0.0075,  
                              n_interest = duration)

plot_traceplot(out)
plot_shapes(out)

plot_trajectories(traj)
plot_hdi(traj)

plot_age_month(traj_birth_month, birth_data)
plot_age_season(traj_birth_month, birth_data)
plot_age_birth_month(traj_birth_month, birth_data)

plot_immunity_birth(traj_babies)
plot_immunity_rate(traj_babies, traj_birth_month, birth_data)