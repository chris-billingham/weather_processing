suppressPackageStartupMessages({
  library(tidyverse)
  library(darksky)
  library(here)
  library(lubridate)
  library(glue)
})

# set the longitude and latitude for Buxton
lng <- -1.911
lat <- 53.259

# load in functions
read_dates <- function(lat, lon, start, end) {
  obs <- seq(as.Date(start), as.Date(end), "day") %>%
    map(~get_forecast_for(lat, lon, units = "uk2", .x))
  return(obs)
}

print(glue("01. getting old data"))
old_hourly <- readRDS(here("data/buxton_hourly.rds"))
old_daily <- readRDS(here("data/buxton_daily.rds"))

# set start date for next day and end date for today - 1
start_date <- as.Date(max(old_hourly$time)) + days(1)
end_date <- Sys.Date() - days(1)

# work out how many days have we got
range <- as.integer(as.Date(end_date) - as.Date(start_date))

print(glue("02. truncating range to not use more than the free credits"))
# if it's more than 800, which means we would have to pay, then reset end_date
if(range > 800) {
  end_date <- start_date + days(800)
}

# annoying quirk
start_date <- as.character(start_date)
end_date <- as.character(end_date)

# read in the new data
print(glue("03. getting new data from darksky for {start_date} to {end_date}"))
update <- pmap(list(lat, lng, start_date, end_date), read_dates)

# split data
new_hourly <- update[[1]] %>% map_df("hourly")
new_daily <- update[[1]] %>% map_df("daily")

# combine the two
all_hourly <- bind_rows(old_hourly, new_hourly)
all_daily <- bind_rows(old_daily, new_daily)

# save the data off
print(glue("04. saving data to hdd"))
saveRDS(all_daily, here("data/buxton_daily.rds"))
saveRDS(all_hourly, here("data/buxton_hourly.rds"))
