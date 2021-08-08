suppressPackageStartupMessages({
  library(magrittr)
})

# read the latest buxton weather watch details
url <- "https://buxtonweather.co.uk/details.htm"

# get the page
page <- rvest::read_html(url)

# grab table 
logger::log_info("Reading the latest Buxton Weather Watch data")
bww_table <- page %>% 
  rvest::html_table() %>% 
  purrr::pluck(1)

# find where the Last Updated bit is
logger::log_info("Process the data")
last_updated <- bww_table %>% 
  dplyr::filter(stringr::str_detect(X1, "Last Updated")) %>% 
  dplyr::select(X1) %>% 
  as.character()

# get the time and the date, looking for "on"
time <- stringr::str_extract(last_updated, ".+?(?= on)")
date <- stringr::str_extract(last_updated, "(?<=on ).*")

# create the time of obs and time now
date_time_obs <- lubridate::dmy_hm(paste0(date, " ", time))
date_time_now <- Sys.time()

# clean up, this is a art
bww_final <- bww_table %>%
  # create a flag for whether we find the word "Parameter"
  dplyr::mutate(parameter_flag = ifelse(X1 == "Parameter", 1, 0)) %>%
  # cumulatively sum that which essentially marks the section from the first Parameter to the second
  dplyr::mutate(parameter_sum = cumsum(parameter_flag)) %>%
  # filter on section 1
  dplyr::filter(parameter_sum == 1) %>%
  # get rid of what we don't need
  dplyr::select(-parameter_flag, -parameter_sum) %>%
  # select and rename what we actually need
  dplyr::select(parameter = X1, value = X2) %>%
  # and the remove the final Parameter
  dplyr::filter(!parameter == "Parameter") %>%
  # take it wide
  tidyr::pivot_wider(names_from = "parameter", values_from = "value") %>%
  # clean up the names
  janitor::clean_names() %>%
  # add in when the observation is from
  dplyr::mutate(obs_datetime = date_time_obs) %>%
  # move the obs_datetime to the front because i like it
  dplyr::relocate(obs_datetime, .before = wind_direction) %>%
  # convert everything except these three to numeric
  dplyr::mutate(dplyr::across(!c("obs_datetime", "wind_direction"), as.numeric)) %>%
  # and add in when the data is from :sunglasses:
  dplyr::mutate(date_time_data = date_time_now)

# read in the old data
logger::log_info("Reading in the old data")
bww_old <- readRDS(here::here("data/buxton_weather_watch.rds"))

# bind to the new
bww_new <- dplyr::bind_rows(bww_old, bww_final)

# remove dupes
logger::log_info("Removing duplicate entries")
bww_new_dupes <- bww_new %>%
  dplyr::group_by(obs_datetime) %>%
  dplyr::filter(date_time_data == min(date_time_data)) %>%
  dplyr::ungroup()

# and save off
logger::log_info("Saving updated data to disk and #fin")
saveRDS(bww_new_dupes, here::here("data/buxton_weather_watch.rds"))
