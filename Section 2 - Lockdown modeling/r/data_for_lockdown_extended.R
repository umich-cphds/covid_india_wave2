librarian::shelf(tidyverse, janitor)

data_through <- as.Date("2021-08-15")

# ind_dat <- read_csv("http://data.covid19india.org/csv/latest/case_time_series.csv",
#                     show_col_types = FALSE) %>%
#   clean_names() %>%
#   select(-date) %>%
#   rename(
#     date         = date_ymd,
#     daily_cases  = daily_confirmed,
#     cases        = total_confirmed,
#     daily_deaths = daily_deceased,
#     deaths       = total_deceased
#   ) %>%
#   mutate(place = "India") %>%
#   filter(date <= data_through) %>%
#   select(place, date, everything())
# 
# mh_dat <- readr::read_csv("https://data.covid19india.org/csv/latest/state_wise_daily.csv",
#                    show_col_types = FALSE) %>%
#   janitor::clean_names() %>%
#   dplyr::select(date_ymd, status, mh) %>%
#   dplyr::rename(date = date_ymd) %>%
#   tidyr::pivot_wider(
#     values_from = "mh",
#     names_from = "status",
#     id_cols = "date"
#   ) %>%
#   dplyr::mutate(place = "Maharashtra") %>%
#   dplyr::select(place, date, daily_cases = Confirmed, daily_recovered = Recovered, daily_deaths = Deceased) %>%
#   dplyr::mutate(daily_cases = case_when(daily_cases < 0 ~ 0, T ~ daily_cases)) %>%
#   dplyr::mutate(
#     cases = cumsum(daily_cases),
#     recovered = cumsum(daily_recovered),
#     deaths = cumsum(daily_deaths)
#   ) %>%
#   filter(date <= data_through)

dat <- read_csv("https://raw.githubusercontent.com/umich-cphds/cov-ind-19-data/master/2021-08-18/everything.csv", show_col_types = FALSE) %>%
  mutate(daily_cases = case_when(daily_cases < 0 ~ 0, T ~ daily_cases)) %>%
  filter(date <= data_through)

write_csv(x = dat, file = "data_for_lockdown_extended.csv")



