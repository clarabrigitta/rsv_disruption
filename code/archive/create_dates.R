# create table for dates reference

library(dplyr)
library(zoo)

dates <- as.data.frame(matrix(NA, 12*rep, 5))
colnames(dates) <- c("time", "month", "year", "yearmon", "date")

dates <- dates %>% 
  mutate(time = 1:nrow(dates),
         month = rep(month.abb, nrow(dates)/12),
         year = rep(c(2000:(2000+(nrow(dates)/12)-1)), each =12),
         yearmon = as.yearmon(paste(year, month, sep = "_"), "%Y_%b"),
         date = as.Date(yearmon))

saveRDS(dates, file = "./output/data/dates.rds")
