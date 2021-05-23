library(tidyverse)
library(rvest)
library(lubridate)
library(janitor)
library(here)

# read the latest buxton weather watch details
url <- "https://buxtonweather.co.uk/details.htm"

# get the page
page <- read_html(url)

# turn it into text
all_text <- page %>% html_elements("tr") %>%
  html_text()

# after this it's horrible
# parse line 3 to get when this was last updated
last_updated <- trimws(all_text[3]) %>% str_split("\r\n", simplify = TRUE) %>% .[2]
time <- str_extract(last_updated, ".+?(?= on)")
date <- str_extract(last_updated, "(?<=on ).*")
date_time_obs <- dmy_hm(paste0(date, " ", time))

# a function to convert the table and grab the latest value
bww_convert <- function(char) {
  df <- trimws(char) %>% str_split("\r\n", simplify = TRUE) %>% as.character()
  char_len <- length(df)
  first <- paste(df[1:(char_len-11)], collapse = " ")
  last <- df[(char_len-10)]
  return_df <- str_c(first, last, sep = "|")
  return(return_df)
}

# iterate through everything
bww_data <- pbapply::pblapply(all_text[c(5,6,7,8,9,10,11,12,13,23,24,25,26,27)], bww_convert) %>% 
  unlist() %>%
  as_tibble() %>%
  separate(value, c("parameter", "value"), sep = "\\|")

# clean up
bww_final <- pivot_wider(bww_data, names_from = "parameter", values_from = "value") %>%
  clean_names() %>%
  mutate(obs_datetime = date_time_obs) %>%
  relocate(obs_datetime, .before = wind_direction) %>%
  mutate(across(!c("obs_datetime", "wind_direction", "rain"), as.numeric))

bww_old <- readRDS(here("data/buxton_weather_watch.rds"))

bww_new <- bind_rows(bww_old, bww_final)

saveRDS(bww_new, here("data/buxton_weather_watch.rds"))
