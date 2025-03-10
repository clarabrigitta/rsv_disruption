library(BayesianTools)
library(posterior)
library(bayesplot)
library(ggplot2)
library(gridExtra)
library(parallel)
library(here)

here()

source(here("code", "model", "create_data.R"))
source(here("code", "model", "create_combinations.R"))
source(here("code", "model", "model_function.R"))

n <- Sys.getenv("SLURM_ARRAY_TASK_ID")
n <- as.integer(n)

# -------------------------------------------------------------------------

# weekly rate of laboratory confirmed cases by age and pathogen aggregated to monthly
scotland_rate <- read.csv(here("data", "respiratory_age_20241218.csv")) %>%
  mutate(date = as.Date(as.character(WeekBeginning), "%Y%m%d")) %>% 
  filter(Pathogen == "Respiratory syncytial virus",
         AgeGroup %in% c("<1 years", "1-4 years")) %>% 
  rename(age = AgeGroup,
         rate = RatePer100000) %>%
  mutate(yearmon = as.yearmon(date),
         year = as.numeric(format(yearmon, "%Y"))) %>% 
  left_join(read_excel(here("data", "mid-year-population-estimates-time-series-data.xlsx"), sheet = "Table 1", skip = 5) %>% 
              filter(`Area name` == "Scotland", `Sex` == "Persons") %>% 
              select(c("Year", `0`:`4`)) %>% 
              pivot_longer(cols = `0`:`4`, names_to = "age_yr", values_to = "pop") %>% 
              mutate(age = ifelse(age_yr == 0, "<1 years", "1-4 years")) %>% 
              rename(year = Year) %>% 
              group_by(year, age) %>% 
              summarise(pop = sum(pop)) %>% 
              ungroup(),
            by = c("year", "age")) %>% 
  mutate(pop = case_when(year == 2024 & age == "<1 years" ~ 46197,
                         year == 2024 & age == "1-4 years" ~ 201071,
                         TRUE ~ pop), # using 2023 population estimate because 2024 not available
         count = rate / 100000 * pop) %>%  
  group_by(age, yearmon) %>% 
  summarise(count = sum(count)) %>% 
  ungroup() %>% 
  filter(yearmon <= as.yearmon("2024-10"))

# -------------------------------------------------------------------------

# create combinations to run
combinations <- create_combinations()

# set duration of maternal immunity
duration = combinations[[n]]$duration

# create fixed datasets
save_data <- create_data(n_interest = duration, rep = 30, factor = combinations[[n]]$factor)

print(paste("start iteration number", n, "time:", Sys.time()))
# poisson likelihood function
likelihood <- function(params){
  
  params <- as.list(params)
  names(params) <- combinations[[n]]$name[combinations[[n]]$ind]
  fixed_params <- as.list(combinations[[n]]$fixed[!combinations[[n]]$ind])
  names(fixed_params) <- combinations[[n]]$name[!combinations[[n]]$ind]
  combined_params <- c(params, fixed_params)
  
  likelihood <- dpois(round(scotland_rate$count, digits = 0),
                      as.numeric(round(model_function(
                        lambda = exp(combined_params$disruption),
                        theta1 = combined_params$inf_imm1, theta2 = combined_params$inf_imm2,
                        omega1 = combined_params$waning1, omega2 = combined_params$waning2, 
                        alpha1 = combined_params$aging1, alpha2 = combined_params$aging2, 
                        stored_data = save_data, 
                        delta = 0.0075, 
                        n_interest = duration)[, 1], digits = 0)) * combined_params$detection,
                      log = T)
  
  return(sum(likelihood))  
}

# fitting setup
setup <- createBayesianSetup(likelihood,
                             lower = combinations[[n]]$lb[combinations[[n]]$ind], 
                             upper = combinations[[n]]$ub[combinations[[n]]$ind], 
                             names = combinations[[n]]$name[combinations[[n]]$ind])

settings = list(iterations = 100000, nrChains = 1, message = TRUE, burnin = 50000) # don't thin for now

# fit model and save output
results <- mclapply(1:4,
                    function(x) {
                      runMCMC(bayesianSetup = setup, sampler = "DEzs", settings = settings)
                    },
                    mc.cores = 4)

out <- createMcmcSamplerList(results)

dir.create(here("output", "data", "parameters", format(Sys.Date(), "%d%m%Y")))
saveRDS(out, file = here("output", "data", "parameters", format(Sys.Date(), "%d%m%Y"), paste0("out", n, ".rds")))

print(paste("end iteration number", n, "time:", Sys.time()))