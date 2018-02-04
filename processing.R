# initial 5 year set up

library(tidyverse)
library(magrittr)
library(darksky)

# let's check if we've got what we need for post codes
if(file.exists("data/postcode_letter.rds")) {
  postcode <- readRDS("data/postcode_letter.rds")
} else {
  
# get the massive postcode file, download and unzip if it doesn't exist
  if(!file.exists("data/postcodes.csv")) {
  download.file("https://www.doogal.co.uk/files/postcodes.zip", "data/postcode_long.zip")
  unzip("data/postcode_long.zip", exdir = "data")
}

# remove not in use, and mad lattitudes, convert to first few characters and take an average of lat/lon across all
postcode <- read_csv("data/postcodes.csv") %>%
  mutate(post_outward = str_extract(Postcode, "([^ ]+)")) %>%
  filter(`In Use?` == "Yes") %>%
  group_by(post_outward) %>%
  summarise(lat_av = mean(Latitude), lon_av = mean(Longitude)) %>%
  ungroup(post_outward) %>% 
  filter(lat_av >= 47) %>% 
  mutate(letters = str_extract(post_outward, "([:alpha:]+)")) %>% 
  group_by(letters) %>% 
  summarise(lat_av = mean(lat_av), lon_av = mean(lon_av))

# change the column names
colnames(postcode) <- c("letters","lat","lon")

}

# let's create a helper function to download a date range of data if we need it
read_dates <- function(lat, lon, start, end) {
  obs <- seq(as.Date(start), as.Date(end), "day") %>%
    map(~get_forecast_for(lat, lon, units = "uk2", .x))
  return(obs)
}

# right let's get the data
# if exists then read file else run mapping
if(file.exists("data/obs_5yr.rds")) {
  obs_5yr <- readRDS("data/obs_5yr.rds")
} else {
  obs_5yr <- pmap(list(postcode$lat, postcode$lon, "2013-01-01", "2018-01-08"), read_dates)
  saveRDS(obs_5yr, file = "data/obs_5yr.rds")
}

# add the postcode letters as names on the list
names(obs_5yr) <- postcode$letters

# create helper function for processing the weather tidily
# i've made this generic so you can point at anything
iterate_postcode <- function(c, level, object) {
  df_post <- eval(parse(text = object))[[c]] %>%
    map_df(level) %>%
    mutate(postcode_short = c)
  return(df_post)
}

# tidy up the daily data
all_daily <- pmap_df(list(postcode$letters, "daily", "obs_5yr"), iterate_postcode)
saveRDS(all_daily, "data/all_daily.rds")

# tidy up the hourly data
all_hourly <- pmap_df(list(postcode$letters, "hourly", "obs_5yr"), iterate_postcode)
saveRDS(all_hourly, "data/all_hourly.rds")

# remove the massive list
rm(obs_5yr)
gc()

