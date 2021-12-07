# REQUIRES R 4.1.0+ TO RUN
# REQUIRES R 4.1.0+ TO RUN
# REQUIRES R 4.1.0+ TO RUN

library(tidyverse)
library(here)
library(glue)
library(janitor)
library(anytime)
library(zoo)
options(stringsAsFactors = FALSE)

mh <- FALSE
kl <- FALSE

d <- read_csv("https://api.covid19india.org/csv/latest/case_time_series.csv",
              col_types = cols()) %>%
  clean_names() %>%
  select(-date) %>%
  select(date = date_ymd, daily_cases = daily_confirmed, daily_deaths = daily_deceased,
         total_cases = total_confirmed, total_deaths = total_deceased, 
         everything())

if (kl == TRUE) {
  taco <- read_csv("https://api.covid19india.org/csv/latest/state_wise_daily.csv",
                   col_types = cols()) %>%
    clean_names() %>%
    select(date = date_ymd, status, value = kl) %>%
    pivot_wider(
      names_from  = "status",
      values_from = "value",
      id_cols     = "date"
    ) %>%
    select(date, daily_cases = Confirmed, daily_deaths = Deceased) %>%
    mutate(cfr = daily_deaths / daily_cases) %>%
    mutate(cfr_t7 = zoo::rollmean(cfr, k = 7, fill = NA, align = "right"))
  d <- d %>% left_join(taco %>% select(date, cfr, cfr_t7), by = "date")
} else if (mh == TRUE) {
  taco <- read_csv("https://api.covid19india.org/csv/latest/state_wise_daily.csv",
                   col_types = cols()) %>%
    clean_names() %>%
    select(date = date_ymd, status, value = mh) %>%
    pivot_wider(
      names_from  = "status",
      values_from = "value",
      id_cols     = "date"
    ) %>%
    select(date, daily_cases = Confirmed, daily_deaths = Deceased) %>%
    mutate(cfr = daily_deaths / daily_cases) %>%
    mutate(cfr_t7 = zoo::rollmean(cfr, k = 7, fill = NA, align = "right"))
  d <- d %>% left_join(taco %>% select(date, cfr, cfr_t7), by = "date")
} else {
  d <- d  %>%
    mutate(cfr = daily_deaths / daily_cases) %>%
    mutate(cfr_t7 = zoo::rollmean(x = cfr, k = 7, fill = NA, align = "right"))
}

scenarios <- c("2021-03-15", "2021-03-30", "no_intervention")

for (i in seq_along(scenarios)) {
  
  tmp_filename <- glue("{scenarios[i]}_smooth1_data.txt")
  
  if (i == 1) {
    p <- read_tsv(here("data", "early_lockdown",
                       tmp_filename),
                  col_types = cols()) %>%
      mutate(scenario = scenarios[i])
  } else {
    p <- bind_rows(p,
                   read_tsv(here("data", "early_lockdown",
                                 tmp_filename),
                            col_types = cols()) %>%
                     mutate(scenario = scenarios[i]))
  }
  
}

dates <- as.Date(c(
  "2021-03-30",
  "2021-04-15",
  "2021-04-30",
  "2021-05-15"
))

obs <- d %>%
  select(date, value = total_cases) %>%
  filter(date %in% dates) %>%
  mutate(
    scenario = "Observed"
  )

proj <- p %>%
  filter(date %in% dates & scenario != "no intervention") %>%
  mutate(scenario = paste(trimws(format(as.Date(scenario), '%B')), trimws(format(as.Date(scenario), '%e')))) %>%
  filter(!(date == "2021-05-15" & scenario == "NA NA"))

table_cases <- bind_rows(obs, proj) %>%
  mutate(scenario = case_when(
    scenario == "NA NA" ~ "No intervention",
    T ~ scenario
  ))

table_cases <- table_cases %>%
  mutate(scenario = case_when(
    tolower(scenario) %in% c("observed", "no intervention") ~ "Observed/No Intervention",
    T ~ scenario
  )) %>%
  pivot_wider(
    names_from  = "scenario",
    values_from = "value",
    id_cols     = "date"
  ) %>%
  mutate(across(.cols = where(is.numeric), ~ .x / 1e6))

table_cases_print <- table_cases %>%
  mutate_if(is.numeric, \(x) trimws(format(x, big.mark = ",", digits = 2, nsmall = 2))) %>%
  mutate_if(is.character, \(x) ifelse(x == "NA", "-", x))

table_deaths <- table_cases

obs_d <- rep(NA, length(table_deaths$date))
for (i in seq_along(table_deaths$date)) {
  
  if (table_deaths$date[i] <= "2021-05-15") {
    obs_d[i] <- d %>% filter(date == table_deaths$date[i]) %>% pull(total_deaths)
  } else{
    obs_d[i] <- (round(table_deaths$`Observed/No Intervention`[i] * (d %>% filter(date == "2021-05-23") %>% pull(total_deaths) / d %>% filter(date == "2021-05-23") %>% pull(total_cases))))
  }
  
}

table_deaths$`Observed/No Intervention` <- obs_d  

max_date <- d %>% filter(date == max(date)) %>% pull(date)
use_cfr <- d %>% filter(date == max_date) %>% pull(cfr_t7)

for (i in seq_along(scenarios)) {
  
  if (i == 3) next
  
  tmp_d <- d %>%
    filter(date <= as.Date(scenarios[i])) %>%
    select(date, daily_cases, daily_deaths, total_cases, total_deaths)
  tmp_p <- p %>%
    filter(scenario == scenarios[i]) %>%
    drop_na(incidence) %>%
    rename(total_cases = value, daily_cases = incidence) %>%
    select(date, daily_cases, total_cases)
  
  tmp <- bind_rows(tmp_d, tmp_p) %>%
    left_join(d %>% select(date, cfr_t7), by = "date") %>%
    mutate(
      cfr_t7 = case_when(
        date > max_date ~ use_cfr,
        T ~ cfr_t7
      )
    ) %>%
    mutate(
      daily_deaths = case_when(
        date > as.Date(scenarios[i]) ~ round(daily_cases * cfr_t7),
        T ~ daily_deaths
      )
    ) %>%
    mutate(
      death_est = cumsum(daily_deaths)
    )
  tmp
  
  table_deaths[[i+2]]  <- tmp %>% filter(date %in% dates) %>% pull(death_est)
  
}

table_deaths <- table_deaths %>%
  mutate(across(where(is.numeric), ~ .x / 1000))

for (i in 1:3) {
  table_deaths[[i + 1]] <- trimws(format(table_deaths[[i + 1]], nsmall = 2, digits = 2, big.mark = ","))
}

for (i in 1:2) {
  tmp_a <- table_deaths[[i + 2]]
  tmp_a <- c(rep("-", i), tmp_a[(i+1):length(tmp_a)])
  table_deaths[[i+2]] <- tmp_a
}

if (mh == TRUE) {
  case_out  <- here("lockdown", "output", "table_mh_cases.txt")
  death_out <- here("lockdown", "output", "table_mh_deaths.txt")
} else if (kl == TRUE) {
  case_out  <- here("lockdown", "output", "table_kl_cases.txt")
  death_out <- here("lockdown", "output", "table_kl_deaths.txt")
} else {
  case_out  <- here("lockdown", "output", "table_cases.txt")
  death_out <- here("lockdown", "output", "table_deaths.txt")
}

write_tsv(table_cases_print,
          case_out)
write_tsv(table_deaths,
          death_out)
