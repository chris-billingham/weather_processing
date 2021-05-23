library(tidyverse)
library(rvest)
library(lubridate)
library(janitor)
library(here)

# read the latest buxton weather watch details
url <- "https://buxtonweather.co.uk/details.htm"

# get the page
page <- read_html(url)

# grab table 
bww_table <- page %>% html_table() %>% 
  pluck(1)

# find where the Last Updated bit is
last_updated <- bww_table %>% 
  filter(str_detect(X1, "Last Updated")) %>% 
  select(X1) %>% as.character()

# get the time and the date, looking for "on"
time <- str_extract(last_updated, ".+?(?= on)")
date <- str_extract(last_updated, "(?<=on ).*")

# create the time of obs and time now
date_time_obs <- dmy_hm(paste0(date, " ", time))
date_time_now <- Sys.time()

# clean up, this is a art
bww_final <- bww_table %>%
  # create a flag for whether we find the word "Parameter"
  mutate(parameter_flag = ifelse(X1 == "Parameter", 1, 0)) %>%
  # cumulatively sum that which essentially marks the section from the first Parameter to the second
  mutate(parameter_sum = cumsum(parameter_flag)) %>%
  # filter on section 1
  filter(parameter_sum == 1) %>%
  # get rid of what we don't need
  select(-parameter_flag, -parameter_sum) %>%
  # select and rename what we actually need
  select(parameter = X1, value = X2) %>%
  # and the remove the final Parameter
  filter(!parameter == "Parameter") %>%
  # take it wide
  pivot_wider(names_from = "parameter", values_from = "value") %>%
  # clean up the names
  clean_names() %>%
  # add in when the observation is from
  mutate(obs_datetime = date_time_obs) %>%
  # move the obs_datetime to the front because i like it
  relocate(obs_datetime, .before = wind_direction) %>%
  # convert everything except these three to numeric
  mutate(across(!c("obs_datetime", "wind_direction", "rain"), as.numeric)) %>%
  # and add in when the data is from :sunglasses:
  mutate(date_time_data = date_time_now)

# read in the old data
bww_old <- readRDS(here("data/buxton_weather_watch.rds"))

# bind to the new
bww_new <- bind_rows(bww_old, bww_final)

# and save off
saveRDS(bww_new, here("data/buxton_weather_watch.rds"))
