library(tidyverse)
library(magrittr)
library(darksky)
library(lubridate)
library(arrow)

postcode <- readRDS("data/postcode_letter.rds")

# load in functions
read_dates <- function(lat, lon, start, end) {
  obs <- seq(as.Date(start), as.Date(end), "day") %>%
    map(~get_forecast_for(lat, lon, units = "uk2", .x))
  return(obs)
}

iterate_postcode <- function(c, level, object) {
  df_post <- eval(parse(text = object))[[c]] %>%
    map_df(level) %>%
    mutate(postcode_short = c)
  return(df_post)
}

# read in the old data
all_hourly <- read_parquet("data/all_hourly.parquet")
all_daily <- read_parquet("data/all_daily.parquet")

# work out the last date
latest <- max(all_daily$time)

# get the start date as last + 1 and end date as today
start_date <- latest + days(1)
end_date <- today() - days(1)

# work out how many days have we got
range <- as.integer(as.Date(end_date) - as.Date(start_date))

# if it's more than 8, which means we would have to pay, then reset end_date
if(range > 8) {
  end_date <- start_date + days(7)
}

# annoying quirk
start_date <- as.character(start_date)
end_date <- as.character(end_date)

# read in the new data
update <- pmap(list(postcode$lat, postcode$lon, start_date, end_date), read_dates)
names(update) <- postcode$letters

# process the new data
new_daily <- pmap_df(list(postcode$letters, "daily", "update"), iterate_postcode)
new_hourly <- pmap_df(list(postcode$letters, "hourly", "update"), iterate_postcode)

# and add it to the old data
all_hourly <- bind_rows(all_hourly, new_hourly)
all_daily <- bind_rows(all_daily, new_daily)

# save off the new data
write_parquet(all_hourly, "data/all_hourly.parquet")
write_parquet(all_daily, "data/all_daily.parquet")

